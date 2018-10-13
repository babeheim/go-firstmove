
library(rethinking)   # needed for model exploration
library(rstan)
library(RColorBrewer) # needed for figures
library(tictoc)

scaffold <- FALSE
save_temp <- TRUE

texttab <- function(input.matrix, alignment=NA, hlines=NA, caption="", scale=NA){
  output <- character(nrow(input.matrix))
  for(i in 1:nrow(input.matrix)){
    add.amps <- paste(input.matrix[i,], collapse=" & ")
    output[i] <- paste(add.amps, "\\\\", sep=" ")
  }
  if(all(!is.na(hlines))){
    for(i in 1:length(hlines)) output <- append(output, "\\hline", hlines[i]+(i-1))
  }
  return(output)
}

modal <- function(data){
  mode <- NA
  if(length(data) > 0 & !all(is.na(data))){
  mode <- names(sort(table(data),decreasing=T))[1]
  options(warn=-1)
  if(!is.na(as.numeric(mode))){
  mode <- as.numeric(mode)
  }
  options(warn=0)
  }
  return(mode)
}

dir_init <- function(path, verbose = FALSE, overwrite = TRUE){
  if(substr(path, 1, 2)!='./') stop('path argument must be formatted
    with "./" at beginning')
  contents <- dir(path, recursive=TRUE)
  if(dir.exists(path)){
    if(overwrite){
      if(verbose){
        if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
        if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
      }
      if(dir.exists(path)) unlink(path, recursive=TRUE)
      dir.create(path)
    }
  } else {
    if(verbose){
      print(paste('folder ', path, ' created.', sep=""))
    }
    dir.create(path)
  }
}

sapply_pb <- function(X, FUN, ...){
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)
  wrapper <- function(...){
    curVal <- get("counter", envir = env)
    assign("counter", curVal +1 ,envir=env)
    setTxtProgressBar(get("pb", envir=env), curVal +1)
    FUN(...)
  }
  res <- sapply(X, wrapper, ...)
  close(pb)
  res
}

sgf.string.to.list <- function(sgf.string){
  if(is.na(sgf.string)) return(NA)
  # eliminate metadata, it might have () in comments
  if(length(grep("C\\[", sgf.string))>0){
    comment.data <- gregexpr("C\\[.*?\\]", sgf.string)
    comment.start <- as.vector(comment.data[[1]])
    comment.stop <- comment.start + attributes(comment.data[[1]])$match.length-1
    for(i in 1:length(comment.start)) substr(sgf.string, comment.start[i], comment.stop[i]) <- paste(rep(" ", (comment.stop[i] - comment.start[i])+1), collapse="")
    sgf.string <- gsub(" ", "", sgf.string)
  }
  if(length(grep("EV\\[", sgf.string))>0){
    comment.data <- gregexpr("EV\\[.*?\\]", sgf.string)
    comment.start <- as.vector(comment.data[[1]])
    comment.stop <- comment.start + attributes(comment.data[[1]])$match.length-1
    for(i in 1:length(comment.start)) substr(sgf.string, comment.start[i], comment.stop[i]) <- paste(rep(" ", (comment.stop[i] - comment.start[i])+1), collapse="")
    sgf.string <- gsub(" ", "", sgf.string)
  }
  if(length(grep("OH\\[", sgf.string))>0){
    comment.data <- gregexpr("OH\\[.*?\\]", sgf.string)
    comment.start <- as.vector(comment.data[[1]])
    comment.stop <- comment.start + attributes(comment.data[[1]])$match.length-1
    for(i in 1:length(comment.start)) substr(sgf.string, comment.start[i], comment.stop[i]) <- paste(rep(" ", (comment.stop[i] - comment.start[i])+1), collapse="")
    sgf.string <- gsub(" ", "", sgf.string)
  }
  start.locations <- regexpr("AB\\[",sgf.string) # black stones before play begins
  start.locations[2] <- regexpr("AW\\[",sgf.string) # white stones before play begins
  start.locations[3] <- regexpr(";B\\[",sgf.string) # black moves
  start.locations[4] <- regexpr(";W\\[",sgf.string) # white moves
  if(all(start.locations)==-1) return(NA)
  # identify start and stop points, and truncate string
  start.point <- min(start.locations[start.locations!=-1])
  (ifelse(substr(sgf.string,nchar(sgf.string),nchar(sgf.string))==")",
    stop.point <- nchar(sgf.string)-1,
    stop.point <- nchar(sgf.string))
  )  # make sure there isn't an ) at the end of the sgf.string
  move.string <- substr(sgf.string,start.point,stop.point)
  if(regexpr("\\;", move.string)==1) start.locations[1:2] <- -1  # bugfix: if the first character is a colon, then there should not be AB and AW tags (but they can mistakenly show up inside the move.string)
  # first task: eliminate all but the main path of the game
  if(regexpr("\\)", move.string)!=-1){ # we know there's more than one path by a )
    cut <- regexpr("\\)",move.string)
    move.string <- substr(move.string,1,cut-1)
    move.string <- gsub("\\(", "", move.string)  # ok if there are no hits
  } # the error is because there's () inside comments...here the EV tag...
  # second task: make all passes with the "tt" coordinate...passes are denoted []
  move.string <- gsub("\\[\\]", "\\[tt\\]", move.string)
  # third task: pull out all moves into a vector, distinguishing between a handicap vector and game vector
  move.vector <- character(0)
  color.vector <- character(0)
  # if there are game moves
  if(start.locations[3] > 0 | start.locations[4] > 0){
    black.starts <- unlist(gregexpr(";B\\[",move.string))
    white.starts <- unlist(gregexpr(";W\\[",move.string))
    move.starts <- sort(c(black.starts,white.starts))
    if(any(move.starts==-1)) move.starts <- move.starts[move.starts>0]
    move.vector <- as.character(sapply(move.starts, function(z) substr(move.string, z+3, z+4)))
    color.vector <- as.character(sapply(move.starts, function(z) substr(move.string, z+1, z+1)))
    color.vector[color.vector=="B"] <- "black"
    color.vector[color.vector=="W"] <- "white"
    # trim the move string until it doesn't have a pass as its very last move
    while(move.vector[length(move.vector)]=="tt" & length(move.vector) > 1){
      move.vector <- move.vector[-length(move.vector)]
    }
    # diagnostic
    if(length(move.vector)==0 | (length(move.vector)==1 && move.vector[1]=="tt")){  # this line prompted me to understand how to use && instead of &
      # what if all moves are passes?
      move.vector <- character(0)
      color.vector <- character(0)
    }
  }
  # if there are pregame moves (handicap placements)
  if(start.locations[1] > 0 | start.locations[2] > 0){
    handi.zone.stop <- regexpr("\\;", move.string)
    if(handi.zone.stop==-1){  # this only happens in games with handicap stones but no moves
        handi.zone.stop <- max(unlist(gregexpr("\\]", move.string) ))
    }
    handi.string <- substr(move.string, 1, handi.zone.stop)
    potential.handi.stops <- c(unlist(gregexpr("[[:upper:]]",handi.string)), handi.zone.stop)  # there's a bug here: what if it feeds lowercase tags, e.g. aw[ and ab[
    handi.string <-gsub("\\[tt\\]", "", handi.string)      # bug fix: if a game's AB or AW tags contain passes for some reason
    if(nchar(handi.string) < 6) start.locations[1:2] <- c(-1,-1)   # if the AW/AB tags are ONLY passes, then take them out altogether
  }
  ## and for handicap stones, if any
  if(start.locations[1] > 0){    # i need to have it pull out just the handicap moves, and distinguish them between black and white
    b.handi.start <- as.numeric(regexpr("AB\\[",handi.string))
    b.handi.stop <- potential.handi.stops[min(which(potential.handi.stops>(b.handi.start+1)))]
    b.handi.string <- substr(handi.string, b.handi.start, b.handi.stop)
    b.handi.starts <- unlist(gregexpr("\\[", b.handi.string))
    b.handi.vector <- as.character(sapply(b.handi.starts, function(z) substr(b.handi.string, z+1, z+2)))
    move.vector <- c(b.handi.vector, move.vector)
    color.vector <- c(rep("BLACK", length(b.handi.vector)), color.vector)
  }
  if(start.locations[2] > 0){   # if there's a AW tag and its not inside the move string
    w.handi.start <- as.numeric(regexpr("AW\\[",handi.string))
    w.handi.stop <- potential.handi.stops[min(which(potential.handi.stops>(w.handi.start+1)))]
    w.handi.string <- substr(handi.string,w.handi.start,(w.handi.stop-1))
    w.handi.starts <- unlist(gregexpr("\\[", w.handi.string))
    w.handi.vector <- as.character(sapply(w.handi.starts, function(z) substr(w.handi.string, z+1, z+2)))
    move.vector <- c(w.handi.vector, move.vector)
    color.vector <- c(rep("WHITE", length(w.handi.vector)), color.vector)
  }
#   move.vector <- as.vector(na.omit(move.vector))
  # check for illegal moves
  illegal <- letters[21:26]
  should.be.zero <- length(unlist(sapply(illegal, function(z) grep(z, move.vector))))
  if(should.be.zero > 0){
    return(NA)
  }
  output <- list(move.vector, color.vector)
  output
}

nemo.finder <- function(pattern=NA, data, box=c("as", "sa"), exclude.handi = T, exclude.inversion = T, diagnostics = F, graph.type="om", cutoff=0.02, order.matters=T, search.once=F, nemo.trends=T, cum.trends=T){
# S1. Ensure inputs exist and are properly formatted.
  if(class(box)=="logical"){
    if(box==T){
      cat("Please specify bounding box...\n")
      lb <- 1:19
      corners <- bounding.box.maker()
      possible.coordinates <- sapply(letters[lb],function(x)paste(x,letters[lb],sep=""))
      my.box <- c(as.character(possible.coordinates[20-corners$y[1],corners$x[1]]), as.character(possible.coordinates[20-corners$y[2],corners$x[2]]))
    }
  }
  if(class(box)=="character"){
    x.letter <- substr(box,1,1)
    x.coord <- sapply(x.letter, function(z) which(letters==z))
    y.letter <- substr(box,2,2)
    y.coord <- sapply(y.letter, function(z) 20-which(letters==z))
    if(x.coord[1]==x.coord[2] | y.coord[1]==y.coord[2]) stop("Error! The bounding box must be a rectangle.")
    if(any(c(x.coord, y.coord) > 19 | c(x.coord, y.coord) < 1)) stop("Error! Bounding box exceeds valid board coordinates.")
    corners <- xy.coords(x=x.coord, y=y.coord)
    board.maker(clear.goban=F)
    rect(x.coord[1]-0.5, y.coord[1]-0.5, x.coord[2]+0.5, y.coord[2]+0.5, lwd=3, border="blue")
    my.box <- box
  }
  if(any(is.na(pattern[[1]]))){
    cat("No default search pattern detected; please enter a search pattern...\n")
    pattern <- pattern.saver(box=my.box)
  }
  if(any(is.na(pattern[[2]]))){
    cat("Something is wrong with the input pattern's colors.  Defaulting to 'black', 'white', 'black', etc.")
    pattern[[2]] <- rep(c("black", "white"), length=length(pattern[[1]]))
  }
  go.points(pattern)
  if(class(data) != "data.frame" & class(data) != "matrix") stop("Error! The game table must be a properly formatted data frame or matrix.")
  if(!"move.string" %in% colnames(data)) stop("Error! The data object must contain a column called 'move.string'.")
  if(!"ID" %in% colnames(data)) stop("Error! The data object must contain a column called 'ID'.")
  # should I also check that the sgfs are properly formatted
# S1.5 Recursive looping for multiples searches.
  if(nemo.trends==T){
    if(search.once==F) cat("Can't do iterative searches with nemo.trends on.\n")
    search.once <- T
  }
  if(search.once==F){
    recursive.box <- my.box
    recursive.pattern <- pattern
    recursive.data <- data
    recursive.exclude.handi <- exclude.handi
    recursive.exclude.inversion <- exclude.inversion
    recursive.diagnostics <- diagnostics
    recursive.cutoff <- 0.02
    recursive.order.matters <- order.matters
    stop.flag <- 0
    while(stop.flag == 0){
      invisible.output <- nemo.finder(pattern=recursive.pattern, data=recursive.data, box=recursive.box, exclude.handi=recursive.exclude.handi, exclude.inversion=recursive.exclude.inversion, cutoff=recursive.cutoff, order.matters=recursive.order.matters, diagnostics=recursive.diagnostics, graph.type="om", search.once=T, nemo.trends=F)
      stop.button <-  xy.coords(19, 19+1)
      points(stop.button,col="gray",pch=15,cex=2.5)
      points(stop.button,col="black",pch=4,cex=2,lwd=2)
      hits <- invisible.output[,1]
      if(length(hits)==1){
        cat("Only one game remains:\n")
        return(recursive.data[sort(unique(hits)),"ID"])
      }
      print(length(hits))
      cat("Click on a point to continue the walk.\n")
      next.move <- sapply(locator(1), round)
      stop.flag <- as.numeric(any(next.move > 19) | any(next.move < 1))
      if(stop.flag==1){
        return(invisible.output)
      }
      new.coord <- paste(letters[next.move[1]], letters[20-next.move[2]], sep="")
      recursive.data <- recursive.data[sort(unique(hits)),]
      if(nrow(recursive.data)==1){
        cat("Only one game remains:\n")
        return(recursive.data[1:5])
      }
      recursive.pattern[[1]] <- c(recursive.pattern[[1]], new.coord)
      ifelse(recursive.pattern[[2]][length(recursive.pattern[[2]])]=="white", new.color <- "black", new.color <- "white")
      recursive.pattern[[2]] <- c(recursive.pattern[[2]], new.color)
    }
  }
# S2. Initialize function objects.
  sgfs <- as.character(data[,"move.string"])
  id.data <- as.character(data[, "ID"])
  found.nemos <- character(0)
  final.nemo.initialization <- rep(NA, length(sgfs))
  hits <- integer(0)
  M1.player <- character(0)
  n.loops <- 8
  if(exclude.inversion==F) n.loops <- 16
  threads <- 1:n.loops
  original.orientation <- orienter(pattern)
# S2.1 - Initialize objects based on the bounding box.
  box.coord.variations <- integer(0)
  box.search.token <- NA
  lb <- 1:19
  letter.grid <- sapply(letters[lb], function(x) paste(x, letters[lb], sep=""))
  box.corner.variations <- letter.grid[20-corners$y[1], corners$x[1]]
  box.corner.variations[2] <- letter.grid[20-corners$y[2], corners$x[2]]
  box.corner.variations <- mirror.mirror(box.corner.variations, starting.orientation=original.orientation)
  x.letters <- substr(box.corner.variations, 1, 1)
  y.letters <- substr(box.corner.variations, 2, 2)
  for(i in 1:8){
    abcs <- sort(match(x.letters[i,], letters))
    x.side <- letters[abcs[1]:abcs[2]]
    abcs <- sort(match(y.letters[i,], letters))
    y.side <- letters[abcs[1]:abcs[2]]
    box.search.token[i] <- paste("[", x.side[1], "-", x.side[length(x.side)], "][", y.side[1], "-", y.side[length(y.side)], "]", sep="")
  }
  box.coord.variations <- paste(sort(rep(x.side, length(y.side))), rep(y.side, length(x.side)), sep="")  # this code describes the 8th orientation of the box
  box.coord.variations <- mirror.mirror(box.coord.variations, starting.orientation = 8)
  if(n.loops == 16){
    box.search.token <- c(box.search.token, box.search.token)
    box.coord.variations <- rbind(box.coord.variations, box.coord.variations)
  }
## S2.2 - Initialize objects based on the pattern.
  # if(original.orientation > 8) threads <- 1:(length(threads)/2)*2-1
  # if(original.orientation == 0 & n.loops == 8) threads <- 1
  # if(original.orientation == 0 & n.loops == 16) threads <- c(1,9)
  if(original.orientation > 8) original.orientation <- as.numeric(substr(original.orientation, 2, 2))
  if(original.orientation==0) original.orientation <- 1
  original.moves <- pattern[[1]]
  original.colors <- pattern[[2]]
  plot.colors <- original.colors
  first.move.colors <- rep(pattern[[2]][1], 8)
  n.pattern.moves <- length(original.moves)
  raw.coord.variations <- mirror.mirror(original.moves, starting.orientation = original.orientation)
  search.pattern.variations <- raw.coord.variations
  color.tags <- ifelse(original.colors=="black", "B\\[", "W\\[")
  search.pattern.variations <- t(apply(search.pattern.variations, 1, function(z) paste(color.tags, z, "\\]", sep="")))
  if(nrow(search.pattern.variations) == 1) search.pattern.variations <- t(search.pattern.variations)
  if(exclude.inversion==F){ # this expands the search.pattern.variations to include inversion searches
    inverted.color.variations <- raw.coord.variations
    color.tags <- ifelse(original.colors=="white", "B\\[", "W\\[")
    inverted.color.variations <- t(apply(inverted.color.variations, 1, function(z) paste(color.tags, z, "\\]", sep="")))
    search.pattern.variations <- rbind(search.pattern.variations, inverted.color.variations)
    first.move.colors <- c(first.move.colors, rep(ifelse(pattern[[2]][1]=="black", "white", "black"), 8))
    box.corner.variations <- rbind(box.corner.variations, box.corner.variations)
    raw.coord.variations <- rbind(raw.coord.variations, raw.coord.variations)
    inversion.colors <- ifelse(original.colors=="white", "black", "white")
  }
  last.move.colors <- substr(search.pattern.variations[,n.pattern.moves], 1, 1)
# S2.3 - Initalize objects based on handicap move specifications.
  # this is how we exclude handicap moves, if desired
  # i need to create a list of handicap moves, as long as the sgf lists...when I want to verify if something is the right color I need only check if it has a handicap move for that color...
  if(!exclude.handi){
    black.handis <- rep(NA, length(sgfs))
    white.handis <- rep(NA, length(sgfs))
    handi.raw <- substr(sgfs, 1, regexpr(";", sgfs)-1)
    has.handi <- nchar(handi.raw)>0
    has.black.handi <- regexpr("AB", handi.raw)>0
    has.white.handi <- regexpr("AW", handi.raw)>0
    # the way i have the sgfs cleaned, AW always preceeds AB
    black.handis[has.black.handi] <- substr(handi.raw[has.black.handi], regexpr("AB", handi.raw[has.black.handi]), nchar(handi.raw[has.black.handi]))
    white.stop <- regexpr("AB", handi.raw)-1
    white.stop[has.white.handi & !has.black.handi] <- nchar(handi.raw[has.white.handi & !has.black.handi])
    white.handis[has.white.handi] <- substr( handi.raw[has.white.handi], 1, white.stop[has.white.handi])
    handicap.registry <- cbind(black.handis, white.handis)
    colnames(handicap.registry) <- c("black", "white")
  }
  handicap.coords <- c("dd", "dj", "dp", "jd", "jj", "jp", "pd", "pj", "pp")
  handicap.columns <- which(raw.coord.variations[1,]%in%handicap.coords)
  if(exclude.handi==F & length(handicap.columns)!=0){
    search.pattern.variations[,handicap.columns] <- substr(search.pattern.variations[,handicap.columns], 2, 7)
  }
# S3 - Search the game records for the pattern.
  for(i in threads){  # The main search loop - refer to the package documentation to describe what it does.
# S3.1 - Initialize and check loop-specific objects.
    still.good <- 1:length(sgfs)
    search.pattern.i <- search.pattern.variations[i,]
    raw.coord.i <- raw.coord.variations[i,]
    box.coord.i <- box.coord.variations[i,]
    running.substrings <- sgfs
    box.nemo.locations <- rep(0, length(sgfs))
    if(i==9) plot.colors <- inversion.colors
    original.pattern.grayed <- list(original.moves, rep("gray", n.pattern.moves))
    board.maker()
    go.points(original.pattern.grayed)
    xs <- match(substr(box.corner.variations[i,], 1, 1), letters)
    ys <- 20-match(substr(box.corner.variations[i,], 2, 2), letters)
    rect(xs[1], ys[1], xs[2], ys[2], lwd=3, border="blue")
    plot.new <- list(raw.coord.i, plot.colors)
    go.points(plot.new)
    if(any(!raw.coord.i %in% box.coord.i)) stop("Search box must enclose all the pattern stones!")
      if(diagnostics){
      go.points(list(box.coord.variations[i, ], terrain.colors(length(box.coord.variations[i, ]))))
      xup <- substr(box.search.token[i], 2, 2)
      xdown <- substr(box.search.token[i], 4, 4)
      yup <- substr(box.search.token[i], 7, 7)
      ydown <- substr(box.search.token[i], 9, 9)
      uno <- paste(xup, yup, sep="")
      go.points(list(uno, "green"))
      dos <- paste(xup, ydown, sep="")
      go.points(list(dos, "green"))
      tres <- paste(xdown, yup, sep="")
      go.points(list(tres, "green"))
      catorce <- paste(xdown, ydown, sep="")
      go.points(list(catorce, "green"))
    }
# S3.2 - Move step by step through each move in the pattern, excluding games that do not contain it within the bounding box.
    for(j in 1:n.pattern.moves){
      if(diagnostics & !is.na(raw.coord.variations[i,1])){
        go.points(list(raw.coord.variations[i,j], "red"))
        Sys.sleep(0.2)
      }
      # step 1: in each sgf string, find the nchar from the left of the next move within the bounding box, and save that location
      box.nemo.locations[still.good] <- regexpr(box.search.token[i], running.substrings[still.good])
      # step 2: drop games which do not have another move inside the bounding box (these returned a "0" in regexpr)
      still.good <- still.good[box.nemo.locations[still.good] > 0]
      # step 3: extract the next move inside the bounding box from the remaining strings
      box.nemos <- substr(running.substrings[still.good], box.nemo.locations[still.good]-3, box.nemo.locations[still.good]+2)
      # step 4: verify that the color of each nemo in the string is the correct color according to the search pattern
      correct.color <- ifelse(plot.colors[j]=="black", "B", "W")
      color.keepers <- substr(box.nemos, 2, 2)==correct.color
      still.good <- still.good[color.keepers]
      box.nemos <- box.nemos[color.keepers]
      # step 5a: verify that this move is the next move in the search pattern, and if not drop those games
      if(order.matters){
        nemo.is.next.pattern.move <- substr(box.nemos, 4, 5) == raw.coord.i[j]
        still.good <- still.good[nemo.is.next.pattern.move]
        box.nemos <- box.nemos[nemo.is.next.pattern.move]
      }
      # step 5b: alternatively, if order does not matter, verify that this move is inside the search pattern, and if not drop these games
      if(!order.matters){
        nemo.is.in.pattern <- substr(box.nemos, 4, 5) %in% raw.coord.i
        still.good <- still.good[nemo.is.in.pattern]
        box.nemos <- box.nemos[nemo.is.in.pattern]
      }
      # step 4.5: uhh..something with handicaps
      # if(exclude.handi){
        # still.good <- still.good[grep(";", box.nemos)] # drop games whose jth move inside the box is a handicap move (either begins with "]..." or "A...", but not ";...")
        # box.nemos <- box.nemos[grep(";", box.nemos)]
      # }
      # step 5.5: uhh..something with handicaps?
      # if(!exclude.handi){
        # check.with.registry <- substr(box.nemos, 2, 2)=="]"
        # handi.string <- handicap.registry[still.good[check.with.registry],plot.colors[j]]
        # color.keepers[check.with.registry] <- !is.na(handi.string) & regexpr(search.pattern.i[j], handi.string) > 0
      # }
      # step 6: update list of games that are still good, and shorten them for the next cycle
      running.substrings[still.good] <- substr(running.substrings[still.good], box.nemo.locations[still.good]+2, nchar(running.substrings[still.good]))
      print(length(still.good))
    }
# S3.2 - Find the nemo among the games with the pattern.
    final.nemos <- final.nemo.initialization
    # step 1: in each sgf string, find the nchar from the left of the next move within the bounding box, and save that location (same as during the pattern search above)
    box.nemo.locations[still.good] <- regexpr(box.search.token[i], running.substrings[still.good])
    # step 2: categorize the "still good" games as either having a nemo, or not
    still.good.no.nemo <- still.good[box.nemo.locations[still.good] == -1]
    still.good.has.nemo <- still.good[box.nemo.locations[still.good] > 0]
    # step 3: for the games with nemos, get the color of the player who played that nemo
    nemo.colors <- as.character(substr(running.substrings[still.good.has.nemo],
    box.nemo.locations[still.good.has.nemo]-2, box.nemo.locations[still.good.has.nemo]-2))
    # step 4: further subdivide based on whether the nemo was played by the "pattern last move" player ("lm") or the "not pattern last move" player ("om"), and transform each nemo into the original orientation of the pattern
    nemo.is.lm <- nemo.colors == last.move.colors[i]
    still.good.has.nemo.om <- still.good.has.nemo[!nemo.is.lm]
    still.good.has.nemo.lm <- still.good.has.nemo[nemo.is.lm]
    final.nemos[still.good.has.nemo.om] <- as.character(substr(running.substrings[still.good.has.nemo.om], box.nemo.locations[still.good.has.nemo.om], box.nemo.locations[still.good.has.nemo.om]+1))
    nemo.box.indexes <- match(final.nemos[still.good.has.nemo.om], box.coord.i) # look up the location of the nemo within the bounding box for that orientation
    original.orientation.nemo.om <- box.coord.variations[original.orientation, nemo.box.indexes] # use those locations to look up the original orientation coordinates for that nemo
    final.nemos[still.good.has.nemo.om] <- original.orientation.nemo.om # save the nemo in the original orientation
    final.nemos[still.good.has.nemo.lm] <- as.character(substr(running.substrings[still.good.has.nemo.lm], box.nemo.locations[still.good.has.nemo.lm], box.nemo.locations[still.good.has.nemo.lm]+1))
    nemo.box.indexes <- match(final.nemos[still.good.has.nemo.lm], box.coord.i) # look up the location of the nemo within the bounding box for that orientation
    original.orientation.nemo.lm <- box.coord.variations[original.orientation, nemo.box.indexes] # use those locations to look up the original orientation coordinates for that nemo
    final.nemos[still.good.has.nemo.lm] <- toupper(original.orientation.nemo.lm) # save the nemo in the original orientation, and UPPERCASE it to indicate the lamo played it (which is kind of weird, indicates tenuki or pass)
# S3.3 - Save the results of each loop of the search.
    hits <- c(hits, still.good)
    found.nemos <- c(found.nemos, final.nemos[still.good])
    M1.player <- c(M1.player, rep(first.move.colors[i], length(still.good)))
    cat("Orientation ", i, " complete. \n", sep="")
  }
# S4 - Create output table and visualization of nemos....no idea what's going on here....
  if(length(hits)==0) stop("There are no hits for this pattern in the database.")
  if(graph.type=="both")  found.nemos.to.graph <- tolower(found.nemos[!is.na(found.nemos)]) # display nemos played by both lm and om
  if(graph.type=="om") found.nemos.to.graph <- found.nemos[substr(found.nemos, 1, 1) %in% letters] # only display the nemos played by the om, the other mover
  if(graph.type=="lm") found.nemos.to.graph <- tolower(found.nemos[substr(found.nemos, 1, 1) %in% LETTERS]) # only display the nemos played by the lm, the last mover
  nemo.freqs <- rev(sort(table(found.nemos.to.graph)))/length(na.omit(found.nemos.to.graph))
  nemo.freqs <- nemo.freqs[nemo.freqs > cutoff]
  nemo.names <- names(nemo.freqs)
  if(nemo.trends==T){
    year.data <- as.numeric(substr(data[,"DT"], 1, 4))
    if(any(is.na(year.data))) stop("Dates are improperly formatted!")
    years.of.found.nemos.to.graph <- year.data[hits[!is.na(found.nemos)]]
    nemo.trend.matrix <- table(found.nemos.to.graph, years.of.found.nemos.to.graph)
    nemo.trend.matrix <- nemo.trend.matrix[rownames(nemo.trend.matrix) %in% nemo.names,]
    nemo.trend.matrix <- t(t(nemo.trend.matrix)/colSums(nemo.trend.matrix))
    o.nemo <- order(rowSums(nemo.trend.matrix), decreasing=T)
    nemo.trend.matrix <- nemo.trend.matrix[o.nemo,]
    my.colors <- sample(rainbow(20), length(nemo.names))
    trend.years <- colnames(nemo.trend.matrix)
    trend.nemo.names <- rownames(nemo.trend.matrix)
    if(cum.trends==T) nemo.trend.plot.matrix <- apply(nemo.trend.matrix, 2, cumsum)
    if(cum.trends==F) nemo.trend.plot.matrix <- nemo.trend.matrix
    dev.new()
    plot(trend.years, nemo.trend.plot.matrix[1,], ylim=c(0,1), type="l", col=my.colors[1])
    x.point.sample <- sample(trend.years, 1)
    while(nemo.trend.plot.matrix[1,x.point.sample] < 0.1) x.point.sample <- sample(trend.years, 1)
    text( x=as.numeric(x.point.sample), y=nemo.trend.plot.matrix[1,x.point.sample], labels=trend.nemo.names[1], col=my.colors[1], pos=1)
    for(i in 2:nrow(nemo.trend.matrix)){
      points(trend.years, nemo.trend.plot.matrix[i,], type="l", col=my.colors[i])
      x.point.sample <- sample(trend.years, 1)
      while(nemo.trend.plot.matrix[i,x.point.sample] < 0.1) x.point.sample <- sample(trend.years, 1)
      text( x=as.numeric(x.point.sample), y=nemo.trend.plot.matrix[i,x.point.sample], labels=trend.nemo.names[i], col=my.colors[i])
    }
  }
  if(nemo.trends==F){
    x.letter <- substr(nemo.names,1,1)
    x.coord <- sapply(x.letter, function(z) which(letters==z))
    y.letter <- substr(nemo.names,2,2)
    y.coord <- sapply(y.letter, function(z) 20-which(letters==z))
    board.maker()
    go.points(pattern)
    color.scale <- rev(heat.colors(2000, alpha=.7))
    color.scale <- color.scale[1001:2000]
    weight.colors <- color.scale[round(nemo.freqs*1000)]
    title <- paste(length(found.nemos.to.graph), " nemos plotted.", sep="")
    mtext(title, NORTH<-3, adj=.05, line=0.25)
    points(x.coord, y.coord, pch=15, cex=3.3, col=weight.colors)
    text(x.coord, y.coord, labels=paste(round(nemo.freqs*100), "%", sep=""), cex=0.8, col="black")
  }
  output <- data.frame(hits, id.data[hits], M1.player, as.character(found.nemos))
    colnames(output) <- c("row", "ID", "M1", "nemo")
  return(output)
}

bounding.box.maker <- function(size=19){
  board.maker(clear.goban=F)
  options(locatorBell=TRUE)
  first.corner <- lapply(locator(1),round)
  if(first.corner$x < 1) first.corner$x <- 1
  if(first.corner$y < 1) first.corner$y <- 1
  if(first.corner$x > size) first.corner$x <- size
  if(first.corner$y > size) first.corner$y <- size
  points(first.corner, pch=3, col="royalblue1")
  second.corner <- lapply(locator(1),round)
  if(second.corner$x < 1) second.corner$x <- 1
  if(second.corner$y < 1) second.corner$y <- 1
  if(second.corner$x > size) second.corner$x <- size
  if(second.corner$y > size) second.corner$y <- size
  points(second.corner, pch=3, col="royalblue1")
  xs <- sort(c(first.corner$x, second.corner$x))
  ys <- sort(c(first.corner$y, second.corner$y))
  if(xs[1] == xs[2] | ys[1] == ys[2]) stop("Error: Bounding box must be a rectangle!")
  rect(xs[1]-0.5, ys[1]-0.5, xs[2]+0.5, ys[2]+0.5, lwd=3, border="royalblue")
  options(locatorBell=FALSE)
  return(xy.coords(x=xs, y=ys))
}

sgf.list.to.string <- function(sgf.list){
  move.vector <- sgf.list[[1]]
  color.vector <- sgf.list[[2]]
  handi.string <- character(0)
  if("WHITE" %in% color.vector | "BLACK" %in% color.vector){
    handis <- which(color.vector == "WHITE" | color.vector == "BLACK")
    handi.moves <- move.vector[handis]
    handi.colors <- color.vector[handis]
    move.vector <- move.vector[-handis]
    color.vector <- color.vector[-handis]
    w.handi.string <- character(0)
    b.handi.string <- character(0)
    if("WHITE" %in% handi.colors){
      w.handi.moves <- handi.moves[handi.colors=="WHITE"]
      w.handi.string <- paste("AW", paste("[", w.handi.moves, "]", sep="", collapse=""), sep="")
    }
    if("BLACK" %in% handi.colors){
      b.handi.moves <- handi.moves[handi.colors=="BLACK"]
      b.handi.string <- paste("AB", paste("[", b.handi.moves, "]", sep="", collapse=""), sep="")
    }
    handi.string <- paste(w.handi.string, b.handi.string, collapse="", sep="")
  }
  pasted.color <- color.vector
  pasted.color[pasted.color=="black"] <- "B"
  pasted.color[pasted.color=="white"] <- "W"
  output <- paste(";", pasted.color, "[", move.vector, "]", sep="", collapse="")
  output <- paste(handi.string, output, collapse="", sep="")
  output
}

tag.extractor <- function(tag, sgf.string){
  start.point <- as.numeric(regexpr(tag, sgf.string))  # question 1: where does the tag appear in the sgf.string?
  if(start.point == -1) return(NA)
  possible.stops <- unlist(gregexpr("\\]",sgf.string))
  likely.stop.point <- min(possible.stops[possible.stops > start.point ])   # question 2: what's the most likely matching right brace?
  tag.data <- substr(sgf.string,start.point+3,likely.stop.point-1)  # plus 3 because there's the two letters and the left brace, minus one because of the right brace
  tag.data
  # issue: what if there is no ] brace?  then the stop.point would fail
  # issue: as a check, if two [ appear before the ], then I know it screwed up
  # theoretically the sgf.string should be checked before tag.extractor runs on it...i should include that at the top of it
}

tree.flagger <- function(sgf.string){
  if(is.na(sgf.string)) return(NA)
  start.locations <- regexpr("AB\\[", sgf.string)
  start.locations[2] <- regexpr("AW\\[", sgf.string)
  start.locations[3] <- regexpr(";B\\[", sgf.string)
  start.locations[4] <- regexpr(";W\\[", sgf.string)
  if(all(start.locations)==-1) return(NA)
  start.point <- min(start.locations[start.locations!=-1])
  (ifelse(substr(sgf.string, nchar(sgf.string), nchar(sgf.string))==")", stop.point <- nchar(sgf.string)-1, stop.point <- nchar(sgf.string)))  # make sure there isn't an ) at the end of the sgf.string
  trunc.string <- substr(sgf.string, start.point, stop.point)
  tree.is.present <- 0
  if(regexpr("\\)", trunc.string) != -1) tree.is.present <- 1
  tree.is.present
}

orienter <- function(pattern){
  if(class(pattern)=="list"){
    moves <- pattern[[1]]
  }
  if(class(pattern)=="character" & length(pattern)[1]==1 & nchar(pattern)[1] > 2){
    sgf.as.list <- sgf.string.to.list(pattern)
    moves <- sgf.as.list[[1]]
  }
  if(class(pattern)=="character" & nchar(pattern)[1] == 2) moves <- pattern
  if(is.na(moves[1])) return(NA)  # the string contains no moves! i don't want it to return an error because I want hash maker to just be NA too
  board.size <- 19
  n.spots <- board.size^2
  lb <- 1:board.size
  possible.coordinates <- sapply(letters[lb],function(x)paste(x,letters[lb],sep=""))
  orientation.matrix  <- matrix(c(43, 4, 4, 4, 4, 4, 4, 4, 4, 54, 5, 5, 5, 5, 5, 5, 5, 5, 65, 3, 43, 4, 4, 4, 4, 4, 4, 4, 54, 5, 5, 5, 5, 5, 5, 5, 65, 6, 3, 3, 43, 4, 4, 4, 4, 4, 4, 54, 5, 5, 5, 5, 5, 5, 65, 6, 6, 3, 3, 3, 43, 4, 4, 4, 4, 4, 54, 5, 5, 5, 5, 5, 65, 6, 6, 6, 3, 3, 3, 3, 43, 4, 4, 4, 4, 54, 5, 5, 5, 5, 65, 6, 6, 6, 6, 3, 3, 3, 3, 3, 43, 4, 4, 4, 54, 5, 5, 5, 65, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, 43, 4, 4, 54, 5, 5, 65, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, 3, 43, 4, 54, 5, 65, 6, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, 3, 3, 43, 54, 65, 6, 6, 6, 6, 6, 6, 6, 6, 32, 32, 32, 32, 32, 32, 32, 32, 32, 0, 76, 76, 76, 76, 76, 76, 76, 76, 76, 2, 2, 2, 2, 2, 2, 2, 2, 21, 18, 87, 7, 7, 7, 7, 7, 7, 7, 7, 2, 2, 2, 2, 2, 2, 2, 21, 1, 18, 8, 87, 7, 7, 7, 7, 7, 7, 7, 2, 2, 2, 2, 2, 2, 21, 1, 1, 18, 8, 8, 87, 7, 7, 7, 7, 7, 7, 2, 2, 2, 2, 2, 21, 1, 1, 1, 18, 8, 8, 8, 87, 7, 7, 7, 7, 7, 2, 2, 2, 2, 21, 1, 1, 1, 1, 18, 8, 8, 8, 8, 87, 7, 7, 7, 7, 2, 2, 2, 21, 1, 1, 1, 1, 1, 18, 8, 8, 8, 8, 8, 87, 7, 7, 7, 2, 2, 21, 1, 1, 1, 1, 1, 1, 18, 8, 8, 8, 8, 8, 8, 87, 7, 7, 2, 21, 1, 1, 1, 1, 1, 1, 1, 18, 8, 8, 8, 8, 8, 8, 8, 87, 7, 21, 1, 1, 1, 1, 1, 1, 1, 1, 18, 8, 8, 8, 8, 8, 8, 8, 8, 87),nrow=19,ncol=19)
  # locate the first non-diagonal, non-center-line move's orientation
    i <- 1
  orientation <- NA
  while( (orientation == 0 | orientation > 9 | is.na(orientation)) & i <= length(moves) ){
    if(moves[i] != "tt") orientation <- orientation.matrix[which(letters==substr(moves[i], 2, 2)), which(letters==substr(moves[i], 1, 1))]
    i <- i + 1
  }
  if(is.na(orientation)) stop("error in orienter")
  orientation
}

mirror.mirror <- function(pattern, starting.orientation=NA){
  if(is.na(starting.orientation)) starting.orientation <- orienter(pattern)
  if(starting.orientation > 9) starting.orientation <- as.numeric(substr(starting.orientation, 2, 2))
  if(class(pattern)=="list"){
    moves <- pattern[[1]]
  }
  if(class(pattern)=="character" & length(pattern)[1]==1 & nchar(pattern)[1] > 2){
    sgf.as.list <- sgf.string.to.list(pattern)
    moves <- sgf.as.list[[1]]
  }
  if(class(pattern)=="character" & nchar(pattern)[1] == 2) moves <- pattern
  if(is.na(moves[1])) return(NA)  # the string contains no moves! i don't want it to return an error because I want hash maker to just be NA too
  x.letters <- substr(moves,1,1)
  y.letters <- substr(moves,2,2)
  x <- match(x.letters, letters)
  y <- 20-match(y.letters, letters)
  coordinates <- xy.coords(x,y)
  bis1.rot <- function(coordinates){
    # y = oldx, x = oldy
    xy.coords(x=(coordinates$y), y=(coordinates$x))
  }
  vert.rot <- function(coordinates){
    # x=20-xold, y=yold
    xy.coords(x=(20-coordinates$x), y=coordinates$y)
  }
  bisneg1.rot <- function(coordinates){
    # y = 20-oldx, x = 20-oldy
    xy.coords(x=(20-coordinates$y), y=(20-coordinates$x))
  }
  horiz.rot <- function(coordinates){
    # x=xold, y=20-yold
    xy.coords(x=coordinates$x, y=(20-coordinates$y))
  }
  # run seven transformations given a starting point, always in the order above...but where's the starting point?
  if(starting.orientation == 0) {
    or1 <- coordinates
    or2 <- coordinates
    or3 <- coordinates
    or4 <- coordinates
    or5 <- coordinates
    or6 <- coordinates
    or7 <- coordinates
    or8 <- coordinates
  }
  if(starting.orientation==1){
    or1 <- coordinates
    or2 <- bis1.rot(or1)
    or3 <- vert.rot(or2)
    or4 <- bisneg1.rot(or3)
    or5 <- horiz.rot(or4)
    or6 <- bis1.rot(or5)
    or7 <- vert.rot(or6)
    or8 <- bisneg1.rot(or7)
  }
  if(starting.orientation==2){
    or2 <- coordinates
    or3 <- vert.rot(or2)
    or4 <- bisneg1.rot(or3)
    or5 <- horiz.rot(or4)
    or6 <- bis1.rot(or5)
    or7 <- vert.rot(or6)
    or8 <- bisneg1.rot(or7)
    or1 <- horiz.rot(or8)
  }
  if(starting.orientation==3){
    or3 <- coordinates
    or4 <- bisneg1.rot(or3)
    or5 <- horiz.rot(or4)
    or6 <- bis1.rot(or5)
    or7 <- vert.rot(or6)
    or8 <- bisneg1.rot(or7)
    or1 <- horiz.rot(or8)
    or2 <- bis1.rot(or1)
  }
  if(starting.orientation == 4){
    or4 <- coordinates
    or5 <- horiz.rot(or4)
    or6 <- bis1.rot(or5)
    or7 <- vert.rot(or6)
    or8 <- bisneg1.rot(or7)
    or1 <- horiz.rot(or8)
    or2 <- bis1.rot(or1)
    or3 <- vert.rot(or2)
  }
  if(starting.orientation == 5){
    or5 <- coordinates
    or6 <- bis1.rot(or5)
    or7 <- vert.rot(or6)
    or8 <- bisneg1.rot(or7)
    or1 <- horiz.rot(or8)
    or2 <- bis1.rot(or1)
    or3 <- vert.rot(or2)
    or4 <- bisneg1.rot(or3)
  }
  if(starting.orientation == 6){
    or6 <- coordinates
    or7 <- vert.rot(or6)
    or8 <- bisneg1.rot(or7)
    or1 <- horiz.rot(or8)
    or2 <- bis1.rot(or1)
    or3 <- vert.rot(or2)
    or4 <- bisneg1.rot(or3)
    or5 <- horiz.rot(or4)
  }
  if(starting.orientation == 7){
    or7 <- coordinates
    or8 <- bisneg1.rot(or7)
    or1 <- horiz.rot(or8)
    or2 <- bis1.rot(or1)
    or3 <- vert.rot(or2)
    or4 <- bisneg1.rot(or3)
    or5 <- horiz.rot(or4)
    or6 <- bis1.rot(or5)
  }
  if(starting.orientation==8){
    or8 <- coordinates
    or1 <- horiz.rot(or8)
    or2 <- bis1.rot(or1)
    or3 <- vert.rot(or2)
    or4 <- bisneg1.rot(or3)
    or5 <- horiz.rot(or4)
    or6 <- bis1.rot(or5)
    or7 <- vert.rot(or6)
  }
  in.sgf <- function(coordinates){
      paste(letters[coordinates$x], letters[20-coordinates$y], sep="")
  }
  rbind(in.sgf(or1), in.sgf(or2), in.sgf(or3), in.sgf(or4), in.sgf(or5), in.sgf(or6), in.sgf(or7), in.sgf(or8))
}

standard.position <- function(pattern){
  board.size = 19
  n.spots = board.size^2
  orientation <- orienter(pattern)
  if(class(pattern)=="character" & length(pattern)==1){
    sgf.as.list <- sgf.string.to.list(pattern)
    moves <- sgf.as.list[[1]]
    colors <- sgf.as.list[[2]]
  }
  if(class(pattern)=="character" & length(pattern)>1 & all(nchar(pattern) == 2)){
    moves <- pattern
    colors <- NA
    cat("Warning! Without handicap information, standard position may vary unexpectedly.\n")
  }
  if(class(pattern)=="list"){
    moves <- pattern[[1]]
    colors <- pattern[[2]]
  }
  # remove passes
  if("tt" %in% moves){
    drop <- which(moves=="tt")
    moves <- moves[-drop]
    colors <- colors[-drop]
  }
  handi.length <- 0
  if(!is.na(colors)[1]) handi.length <- sum(regexpr("[[:upper:]]",colors)==1)
  lb <- 1:board.size
  coord.vec <- sapply(letters[lb], function(x) paste(x, letters[lb], sep=""))
  if(orientation > 8) coord.vec.adj <- coord.vec # this only is called if the entire game is played on dividing lines
  # now that we've established the orientation of this game, make the appropriate move.num.matix
  if(orientation==1) coord.vec.adj <- matrix(coord.vec[n.spots:1], ncol=board.size, byrow=T)
  if(orientation==2 | orientation == 0 | is.na(orientation)) coord.vec.adj <- matrix(coord.vec[1:n.spots], ncol=board.size)
  if(orientation==5) coord.vec.adj <- matrix(coord.vec[1:n.spots], ncol=board.size, byrow=T)
  if(orientation==6) coord.vec.adj <- matrix(coord.vec[n.spots:1], ncol=19)
  if(orientation==8){
    sector.5 <- matrix(coord.vec[1:n.spots], ncol=board.size, byrow=T)
    sector.8 <- sector.5[board.size:1,]
    coord.vec.adj <- sector.8
  }
  if(orientation==7){
    sector.6 <- matrix(coord.vec[n.spots:1], ncol=board.size)
    sector.7 <- sector.6[,board.size:1]
    coord.vec.adj <- sector.7
  }
  if(orientation==3){
    sector.2 <- matrix(coord.vec[1:n.spots], ncol=board.size)
    sector.3 <- sector.2[,board.size:1]
    coord.vec.adj <- sector.3
  }
  if(orientation==4){  # oddly ,this and 8 had to be flipped when I used the new match approach
    sector.1 <- matrix(coord.vec[n.spots:1], ncol=board.size,byrow=T)
    sector.4 <- sector.1[board.size:1,]
    coord.vec.adj <- sector.4
  }
  move.locations <- match(moves, coord.vec.adj)
  if(handi.length>0) move.locations[1:handi.length] <- sort(move.locations[1:handi.length])  # this is to standardize the hash values for games with handis...
  move.locations  # starting at the top left and counting down 1 by 1...
}

hash.maker <- function(pattern){
  if(class(pattern)=="character" & length(pattern)==1){
    sgf.as.list <- sgf.string.to.list(pattern)
    moves <- sgf.as.list[[1]]
    colors <- sgf.as.list[[2]]
    move.loc <- standard.position(sgf.as.list)
  }
  if(class(pattern)=="character" & length(pattern)>1 & all(nchar(pattern)==2)){
    stop("Hash making is impossible without handicap information!")
  }
  if(class(pattern)=="list"){
    moves <- pattern[[1]]
    colors <- pattern[[2]]
    move.loc <- standard.position(pattern)
  }
    # passes should appear as NA, but if the move locations are ALL NA then something is wrong
  if(all(is.na(move.loc))) return(NA)
  handi.length <- sum(regexpr("[[:upper:]]", colors)==1)
  game.length <- length(moves)-handi.length
  if(game.length==0) return(NA)
  move.loc <- move.loc[-(1:handi.length)]  # ignore the role of handicap stones in calculating the hash
  pos <- 1:length(move.loc)
  if(any(is.na(move.loc))){  # ignore passes and their locations in calculating the hash
    drop <- which(is.na(move.loc))
    move.loc <- move.loc[-drop]
    pos <- pos[-drop]
  }
  ##########
  numerator <- sum(move.loc*pos^7)
  divisor <- 999999937
  hash.tag <- as.integer(numerator%%divisor)
  ##########
  digits <- nchar(divisor)
  fill.hashtag <- digits-nchar(hash.tag)
  fill.moves <- 3-nchar(game.length)
  hash.tag <- paste( paste( paste(rep(0,fill.moves), collapse=""),game.length,sep=""), paste( paste(rep(0,fill.hashtag), collapse=""),hash.tag,sep=""), sep="-")
  hash.tag
}

board.maker <- function(size=19, clear.goban=T, goban.color="darkgoldenrod1"){
        lb <- 1:size
        starpoints <- c(4,10,16)  # 4, 7, 10 for 13x13, and 3, 5, 7 for 9x9
        if(size == 13){
                starpoints <- c(4, 7, 10)
        }
        if(size == 9){
                starpoints <- c(3,5,7)
        }
        title.height <- .25  # in inches
        goban.side <- 5   # in inches
        if(length(dev.list())!=0 & all(round(par("pin"))==5) & clear.goban==F) return()
        if(length(dev.list())==0 | any(round(par("pin"))!=5)){  # if a goban is not current open, create a window for one....
                graphics.off()
                if(.Platform$OS.type=="windows") {
                        windows(width = goban.side, height = goban.side+title.height, rescale="fixed")
                }
                if(.Platform$OS.type=="unix") {
                        quartz(width = goban.side, height = goban.side+title.height)
                }
        }
        par(mai=c(0, 0, title.height, 0))
        par(bg=goban.color)
        plot(lb, lb, type = "n",xaxt="n",yaxt="n",xlab="",ylab="")
        for(i in lb){
                lines(c(i,i), c(1,size))
                lines(c(1,size), c(i,i))
        }
        for(i in starpoints){
                for(j in starpoints){
                        points(i,j,pch=20,cex=1)
                }
        }
}

# SGF Processing

go.points <- function(pattern){
    colors <- NA
    if(class(pattern)=="character" & length(pattern) > 1 & all(nchar(pattern) == 2)) moves <- pattern
    if(class(pattern)=="character" & length(pattern) == 1){
      pattern.as.list <- sgf.string.to.list(pattern)
      moves <- pattern.as.list[[1]]
      colors <- pattern.as.list[[2]]
    }
    if(class(pattern)=="list"){
      moves <- pattern[[1]]
      colors <- pattern[[2]]
    }
    board.maker(clear.goban=F)
    goban.side <- par("pin")[1]
    stone.size <- goban.side/(19.44*0.076)  # the 5 here represents 5 inches, as specified in the board.maker as the fixed size of the board
    x.letter <- substr(moves,1,1)
    x.coord <- sapply(x.letter, function(z) which(letters==z))
    y.letter <- substr(moves,2,2)
    y.coord <- sapply(y.letter, function(z) 20-which(letters==z))
    if(is.na(colors)[1]) colors <- rep("gray", length(y.coord))
    points(x.coord, y.coord, cex=stone.size,pch=21,bg=colors)
}

game.table.maker <- function(my.path="./"){
  cat("Locating sgf files...\n")
  available.games <- list.files(path=my.path,pattern="\\.sgf$",recursive=T,full.names=TRUE)
  raw.sgfs <- rep(NA, length(available.games))
  cat("Reading sgf files:\n")
  raw.sgfs <- sapply_pb(available.games, function(z) paste(scan(z, what=" ", sep="\n", quiet=T), collapse=""))
  tags <- c("SZ","PW","WR","PB","BR","DT","PC","KM","RE","RU","OT","TM","HA", "GC")
  n.columns <- length(tags) + 1 + 1 + 1 + 1   # hash tag, tree flag, file name and move string
  column.names <- c("ID", tags, "filename", "tree.flag", "move.string")
  game.table <- matrix(NA,nrow=length(raw.sgfs),ncol=n.columns)
  colnames(game.table) <- column.names
  cat("Processing loaded games:\n")
  pb <- txtProgressBar(min = 0, max = length(raw.sgfs), style = 3)
  for(i in 1:length(raw.sgfs)){
    sgf <- raw.sgfs[i]
    ID <- hash.maker(sgf)
    tag.data <- sapply(tags, function(z) tag.extractor(z,sgf))
    tag.data <- gsub("\\,", "\\;", tag.data)  # sometimes metadata contains commas, which is a problem for csv files
    move.string <- sgf.list.to.string(sgf.string.to.list(sgf))
    # sgf.player(move.string,speed=100,title=ID)
    tree.flag <- tree.flagger(sgf)
    game.table[i,] <- c(ID, tag.data, available.games[i], tree.flag, move.string)
    setTxtProgressBar(pb,i)
  }
  close(pb)
  game.table
#  write.table(game.table,"gogod_gametable.csv",sep=",") # for when you want to do it manually
}

duplicate.detector <- function(data){
  ids <- data[,"ID"]
  potential.collisions <- unique(ids[which(duplicated(ids))])
  if(length(potential.collisions)==0) return("Zero hits, zero duplicates.")
  duplicate.register <- rep(NA, length(potential.collisions))
  pb <- txtProgressBar(min = 0, max = length(potential.collisions), style = 3)
  for(i in 1:length(potential.collisions)){
    check.me <- which(ids==potential.collisions[i])
    focal.sgfs <- data[check.me,"move.string"]
    standardized.sgfs <- lapply(focal.sgfs, standard.position)
    duplicate.register[i] <- length(unique(standardized.sgfs))
  #  print(round(i/length(potential.collisions),2))
    setTxtProgressBar(pb,i)
  }
  close(pb)
  print(paste(length(duplicate.register), " repeats, ", sep=""))
  if(any(duplicate.register > 0)) print(potential.collisions[which(duplicate.register>0)])
  print(paste(sum(duplicate.register>1), " game duplications.", sep=""))
  if(any(duplicate.register>1)) print(potential.collisions[which(duplicate.register>1)])
  # duplicate.register  # if the entry is 1, then there's no collision after all...
}

# Game DB Analysis

sgf.player <- function(data, speed=NA, nums=F, title=NA, stone.size=NA, stone.shape="circle", bg.color="darkgoldenrod1", halt=NA){
    my.halt <- halt
    my.nums <- nums
        if(class(data)=="matrix"){
                if(is.na(speed)) speed <- 100
                sgfs <- as.character(data[,"move.string"])
                pb <- txtProgressBar(min = 0, max = length(sgfs), style = 3)
                for(i in 1:length(sgfs)){
                        sgf.player(sgfs[i], speed=100, num=my.nums, title=data[i,"ID"], halt=my.halt)
                        setTxtProgressBar(pb,i)
                        if(speed!=100) Sys.sleep(1-speed/100)
                }
                close(pb)
                return()
        }
        if(is.na(speed)) speed <- 85
        moves <- NA
        if(class(data)=="character" & length(data) == 1){
                sgf <- data
                pattern.as.list <- sgf.string.to.list(data)
                moves <- pattern.as.list[[1]]
                colors <- pattern.as.list[[2]]
        }
        if(class(data)=="character" & length(data) > 1 & !is.na(match("move.string", names(data))) ){
                sgf <- data["move.string"]
                pattern.as.list <- sgf.string.to.list(data["move.string"])
                moves <- pattern.as.list[[1]]
                colors <- pattern.as.list[[2]]
        }
        if(class(data)=="list"){
                sgf <- data
                moves <- data[[1]]
                colors <- data[[2]]
        }
        if(is.na(moves)[1]) stop("The sgf of the game is not in a valid format.")
        x.cartesian <- match(substr(moves, 1, 1), letters)
        y.cartesian <- 20-match(substr(moves, 2, 2), letters)
        handi.positions <- which(colors==toupper(colors))
        n.handi <- length(handi.positions)
        move.labels <- c(rep("H", n.handi), 1:(length(moves)-n.handi))
        colors[colors=="BLACK"] <- "darkred"
        colors[colors=="WHITE"] <- "darksalmon"
        # farthest.coordinate <- max(sgf.xy$x, sgf.xy$y)
        # potential.sizes <- c(9,13,19)
        # board.size <- min(potential.sizes[which(potential.sizes>=farthest.coordinate)])
        board.size <- 19
        board.maker(size=board.size, goban.color=bg.color)
        if(is.na(title)) title <- hash.maker(sgf)
        mtext(title, NORTH<-3, adj=.05, line=0.25)
        goban.side <- par("pin")[1]
        if(board.size == 19 & is.na(stone.size)) stone.size <- goban.side/(19.44*0.076)  # the 5 here represents 5 inches, as specified in the board.maker as the fixed size of the board
        if(board.size == 13 & is.na(stone.size)){
                stone.size <- 5.6
        }
        if(board.size == 9 & is.na(stone.size)){
                stone.size <- 7.9
        }
    if(is.na(halt)) halt <- length(moves)
        steps <- (n.handi+1):halt
        if(speed == 100){
                steps <- halt
        }
    if(stone.shape=="circle") stone.pch <- 21
    if(stone.shape=="square") stone.pch <- 22
        for(i in steps){
                plot.me <- xy.coords(x=x.cartesian[1:i], y=y.cartesian[1:i])
                points(plot.me, cex=stone.size, pch=stone.pch, bg=colors[1:i])
                if(nums == T) text(plot.me$x+0.03, plot.me$y+0.03, labels=move.labels[1:i], col=ifelse(colors[1:i]=="white", "black", "white"), cex=.7)
                if(speed!=100) Sys.sleep(1-speed/100)
        }
   if(.Platform$OS.type=="windows") bringToTop(-1)
}

kifu.maker <- function(data, nums=F, title=NA, halt=NA, type="plot", show=F){
  my.halt <- halt
  my.nums <- nums
  my.title <- title
  # if(class(data)=="matrix"){
  # if(is.na(speed)) speed <- 100
  # sgfs <- as.character(data[,"move.string"])
  # pb <- txtProgressBar(min = 0, max = length(sgfs), style = 3)
  # for(i in 1:length(sgfs)){
  # sgf.player(sgfs[i], speed=100, title=data[i,"ID"], halt=my.halt, nums=my.nums)
  # setTxtProgressBar(pb,i)
  # if(speed!=100) Sys.sleep(1-speed/100)
  # }
  # close(pb)
  # return()
  # }
  moves <- NA
  if(class(data)=="character" & length(data) == 1){
  sgf <- data
  pattern.as.list <- sgf.string.to.list(data)
  moves <- pattern.as.list[[1]]
  colors <- pattern.as.list[[2]]
  }
  if(class(data)=="character" & length(data) > 1 & !is.na(match("move.string", names(data))) ){
  sgf <- data["move.string"]
  pattern.as.list <- sgf.string.to.list(data["move.string"])
  moves <- pattern.as.list[[1]]
  colors <- pattern.as.list[[2]]
  }
  if(class(data)=="list"){
  sgf <- data
  moves <- data[[1]]
  colors <- data[[2]]
  }
  if(is.na(moves)[1]) stop("The sgf of the game is not in a valid format.")
  x.cartesian <- match(substr(moves, 1, 1), letters)
  y.cartesian <- 20-match(substr(moves, 2, 2), letters)
  handi.positions <- which(colors==toupper(colors))
  n.handi <- length(handi.positions)
  move.labels <- c(rep("H", n.handi), 1:(length(moves)-n.handi))
  colors[colors=="BLACK"] <- "darkred"
  colors[colors=="WHITE"] <- "darksalmon"
  board.size <- 19
  size <- board.size
  clear.goban <- T
  lb <- 1:size
  starpoints <- c(4,10,16)  # 4, 7, 10 for 13x13, and 3, 5, 7 for 9x9
  title.height <- .25  # in inches
  goban.side <- 5   # in inches
  if(is.na(title)) my.title <- hash.maker(sgf)
  fn <- my.title
  if(type!="plot") fn <- paste(my.title, type, sep=".")
  if(type=="plot"){
  if(.Platform$OS.type=="windows") {
  windows(width = goban.side, height = goban.side+title.height, rescale="fixed")
  }
  if(.Platform$OS.type=="unix") {
  quartz(width = goban.side, height = goban.side+title.height)
  }
  }
  if(type=="png"){
  png(filename=fn, width = goban.side, height = goban.side + title.height, units="in", res=96) # ms windows has 96 dpi standard vs 76 for apple
  }
  if(type=="pdf"){
  pdf(file=fn, width = goban.side, height = goban.side+title.height)
  }
  if(type=="ps"){
  postscript(file=fn, width = goban.side, height = goban.side+title.height)
  }
  par(mai=c(0, 0, title.height, 0))
  par(bg="darkgoldenrod1")
  plot(lb, lb, type = "n",xaxt="n",yaxt="n",xlab="",ylab="")
  for(i in lb){
  lines(c(i,i), c(1,size))
  lines(c(1,size), c(i,i))
  }
  for(i in starpoints){
  for(j in starpoints){
  points(i,j,pch=20,cex=1)
  }
  }
  mtext(title, NORTH<-3, adj=.05, line=0.25)
  goban.side <- par("pin")[1]
  stone.size <- goban.side/(19.44*0.076)  # the 5 here represents 5 inches, as specified in the board.maker as the fixed size of the board
  if(is.na(halt)) halt <- length(moves)
  steps <- (n.handi+1):halt
  plot.me <- xy.coords(x=x.cartesian[1:halt], y=y.cartesian[1:halt])
  points(plot.me, cex=stone.size, pch=21, bg=colors[1:halt])
  if(nums == T) text(plot.me$x+0.03, plot.me$y+0.03, labels=move.labels[1:halt], col=ifelse(colors[1:halt]=="white", "black", "white"), cex=.7)
     # if(.Platform$OS.type=="windows") bringToTop(-1)
     graphics.off()
  if(show==T){
  sgf.player(sgf, nums=my.nums, halt=my.halt, title=my.title, speed=100)
  }
  if(type!="plot") cat("Saved to:", paste(getwd(), fn, sep="/"), "\n", sep=" ")
}

# the whole point of the box is to say "not only does the string we're looking for have to be present, but by the last move in that string there cannot be any other stones within that bounding box

pattern.saver <- function(box=NA){
  board.size <- 19
  stone.size <- 3.4
  xmax <- 19
  xmin <- 1
  ymax <- 19
  ymin <- 1
  black.button <- xy.coords(9, board.size+1)
  gray.button <-  xy.coords(10, board.size+1)
  white.button <-  xy.coords(11, board.size+1)
  stop.button <-  xy.coords(19, board.size+1)
  board.maker()
  par(xpd=T)
  points(gray.button,bg="gray",pch=21,cex=stone.size)
  points(black.button,bg="black",pch=21,cex=stone.size)
  points(white.button,bg="white",pch=21,cex=stone.size)
  points(stop.button,col="gray",pch=15,cex=2.5)
  points(stop.button,col="black",pch=4,cex=2,lwd=2)
  points(black.button,col="red",pch=20,cex=1)
  if(!all(is.na(box))){
    x.letter <- substr(box,1,1)
    x.coord <- sort(sapply(x.letter, function(z) which(letters==z)))
    y.letter <- substr(box,2,2)
    y.coord <- sort(sapply(y.letter, function(z) 20-which(letters==z)))
    if(x.coord[1]==x.coord[2] | y.coord[1]==y.coord[2]) stop("Error! The bounding box must be a rectangle.")
    if(any(c(x.coord, y.coord) > 19 | c(x.coord, y.coord) < 1)) stop("Error! Bounding box exceeds valid board coordinates.")
    corners <- xy.coords(x=x.coord, y=y.coord)
    rect(x.coord[1]-0.5, y.coord[1]-0.5, x.coord[2]+0.5, y.coord[2]+0.5, lwd=3, border="blue")
    xmax <- x.coord[2]
    xmin <-  x.coord[1]
    ymax <- y.coord[2]
    ymin <- y.coord[1]
  }
  current.color <- "black"
  options(locatorBell=FALSE)
  active.button <- black.button
  cols <- c("black","gray","white")
  sgf.coords <- NA
  sgf.coord.cols <- NA
  move <- xy.coords(1,1)
  moves <- xy.coords(integer(0))
  lb <- 1:19
  possible.coordinates <- sapply(letters[lb],function(x)paste(x,letters[lb],sep=""))
  looping <- T
  while(looping){
    points(gray.button,bg="gray",pch=21,cex=stone.size)
    points(black.button,bg="black",pch=21,cex=stone.size)
    points(white.button,bg="white",pch=21,cex=stone.size)
    points(active.button,col="red",pch=20,cex=1)
    move <- lapply(locator(1),round)
    if(move$y > 19){  # if its a button press, else its a move on the board
      if(move$x==gray.button$x | move$x==black.button$x | move$x==white.button$x){
        active.button <- move
        current.color <- cols[move$x-(black.button$x-1)]
      } else if(move$x == stop.button$x){
        looping <- F
        cat("Pattern Entered\n")
      }
    }
    if(move$x <= xmax & move$x >= xmin & move$y <= ymax & move$y >= ymin){
      move.coord <- as.character(possible.coordinates[20-move$y,move$x])
      if(!move.coord %in% sgf.coords){
        moves <- xy.coords(c(moves$x, move$x), c(moves$y, move$y))
        points(move, bg=current.color,pch=21,cex=stone.size)
        sgf.coords <- na.omit(c(sgf.coords, move.coord))
        sgf.coord.cols <- na.omit(c(sgf.coord.cols, current.color))
        if(current.color=="black"){
          current.color<-"white"
          active.button <- white.button
        } else if(current.color=="white"){
          current.color<-"black"
          active.button <- black.button
        }
      }
    }
  }
  return(list(sgf.coords, sgf.coord.cols))
}
# pattern.saver <- function(called.manually = T, board.size, stone.size, black.button, gray.button, white.button, stop.button, corners, local.box){
  # if(called.manually){
    # board.size <- 19
    # stone.size <- 3.4
    # black.button <- xy.coords(9, board.size+1)
    # gray.button <-  xy.coords(10, board.size+1)
    # white.button <-  xy.coords(11, board.size+1)
    # stop.button <-  xy.coords(19, board.size+1)
    # board.maker()
    # par(xpd=T)
    # points(gray.button,bg="gray",pch=21,cex=stone.size)
    # points(black.button,bg="black",pch=21,cex=stone.size)
    # points(white.button,bg="white",pch=21,cex=stone.size)
    # points(stop.button,col="gray",pch=15,cex=2.5)
    # points(stop.button,col="black",pch=4,cex=2,lwd=2)
  # }
  # points(black.button,col="red",pch=20,cex=1)
  # current.color <- "black"
  # options(locatorBell=FALSE)
  # active.button <- black.button
  # cols <- c("black","gray","white")
  # sgf.coords <- NA
  # sgf.coord.cols <- NA
  # move <- xy.coords(1,1)
  # moves <- xy.coords(integer(0))
  # lb <- 1:19
  # possible.coordinates <- sapply(letters[lb],function(x)paste(x,letters[lb],sep=""))
  # stoploop <- F
  # while(stoploop == F){
    # illegal <- F
    # points(gray.button,bg="gray",pch=21,cex=stone.size)
    # points(black.button,bg="black",pch=21,cex=stone.size)
    # points(white.button,bg="white",pch=21,cex=stone.size)
    # points(active.button,col="red",pch=20,cex=1)
    # move <- lapply(locator(1),round)
    # if(move$y > 19){  # if its a button press, else its a move on the board
      # if(move$x==gray.button$x | move$x==black.button$x | move$x==white.button$x){
        # active.button <- move
        # current.color <- cols[move$x-(black.button$x-1)]
      # } else if(move$x == stop.button$x){
        # stoploop <- T
        # cat("Pattern Entered\n")
      # }
    # } else if(move$x <= 19 & move$x >= 1 & move$y <= 19 & move$y >= 1){
      # if(called.manually == F){  # if its called inside string.search, it needs to use a bounding box.  When called manually there is no bounding box...
        # if(local.box == T){  # note that "local.box" will not exist unless we're inside string.search, when its either T or F
          # range.x <- sort(corners$x)
          # range.y <- sort(corners$y)
          # if(length(intersect(move$x,range.x[1]:range.x[2]))==0 | length(intersect(move$y,range.y[1]:range.y[2]))==0) illegal <- T
        # }
      # }
      # # check to see if a stone has already been placed at this location...
      # move.coord <- as.character(possible.coordinates[20-move$y,move$x])
      # if(length(grep(move.coord,sgf.coords))!=0) illegal <- T
      # if(illegal == F){
        # moves <- xy.coords(c(moves$x, move$x), c(moves$y, move$y))
        # points(move, bg=current.color,pch=21,cex=stone.size)
        # sgf.coords <- na.omit(c(sgf.coords, move.coord))
        # sgf.coord.cols <- na.omit(c(sgf.coord.cols, current.color))
        # if(current.color=="black"){
          # current.color<-"white"
          # active.button <- white.button
        # } else if(current.color=="white"){
          # current.color<-"black"
          # active.button <- black.button
        # }
      # }
    # }
  # }
  # return(list(sgf.coords, sgf.coord.cols))
# }

nemo.time <- function(hits, data, from=NA, to=NA, limit.to=NA){
  if(!is.na(limit.to) & limit.to=="om"){
    drop <- which(substr(hits[,"nemo"], 1, 1) %in% LETTERS)
    hits <- hits[-drop, ]
  }
  if(!is.na(limit.to) & limit.to=="lm"){ # lm nemos are in UPPERCASE
    drop <- which(substr(hits[,"nemo"], 1, 1) %in% letters)
    hits <- hits[-drop, ]
  }
  nemos <- tolower(hits[,"nemo"])
  hit.DT <- data[hits[,"row"], "DT"]
  hit.year <- as.numeric(substr(hit.DT, 1, 4))
  hit.year.list <- names(table(hit.year))
  crude.table <- sort(table(nemos), decreasing=T)
  coord.count.by.year <- t(table(nemos, hit.year))
  coord.frq.by.year <- coord.count.by.year/rowSums(coord.count.by.year)
  cols <- sample(rainbow(1000), length(crude.table))
  if(is.na(from)) from <- min(na.omit(hit.year))
  if(is.na(to)) to <- max(na.omit(hit.year))
  dev.new()
  plot(hit.year.list, coord.frq.by.year[,1], type="l", col=cols[1], ylim=c(0,1), xlim=c(from, to), ylab="Frequency of Appearance", las=1, xlab="")
  if(any(coord.frq.by.year[,1] > 0.1)) text(x=sample(from:to, 1), y=coord.frq.by.year[length(hit.year.list),1], labels=colnames(coord.frq.by.year)[1], col=cols[1], pos=3)
  for(i in 2:length(crude.table)){
    points(hit.year.list, coord.frq.by.year[,i], col=cols[i], type="l")
    ex <- sample(from:to, 1)
    if(any(coord.frq.by.year[,i] > 0.1)) text(x=ex, y=coord.frq.by.year[hit.year.list==ex,i], labels=colnames(coord.frq.by.year)[i], col=cols[i], pos=3)
  }
  return(coord.count.by.year)
}

chronography <- function(hits, data, focal.mover = "M1", p.name=NA, from=NA, to=NA, file=NA){  # this doesn't need to generate a giant career matrix...but a playre matrix by year could be used for this and performance counts, since both are margins of that matrix....player counts by year....game counts by year
  if(class(hits)=="data.frame"){
    hit.rows <- hits[,1]
    M1.color.tag <-  as.character(hits[,3])
    M1.color.tag[M1.color.tag=="black"] <- "B"
    M1.color.tag[M1.color.tag=="white"] <- "W"
    M2.color.tag <- rep(NA, length(hits[,3]))
    M2.color.tag[M1.color.tag=="B"] <- "W"
    M2.color.tag[M1.color.tag=="W"] <- "B"
  } else {
    stop("Search hits are not valid.")
  }
  hit.DT <- data[hit.rows, "DT"]
  hit.years <- as.numeric(substr( hit.DT, 1 , 4))
  M1.names <- c(data[hit.rows[M1.color.tag=="B"], "PB"], data[hit.rows[M1.color.tag=="W"], "PW"])
  M2.names <- c(data[hit.rows[M2.color.tag=="B"], "PB"], data[hit.rows[M2.color.tag=="W"], "PW"])
  if(focal.mover == "M1"){
    focal.names <- M1.names
    focal.colors <- M1.color.tag
  }
  if(focal.mover == "M2"){
    focal.names <- M2.names
    focal.colors <- M2.color.tag
  }
  winning.color <- substr(data[hit.rows, "RE"], 1,1)
  focal.win <- as.numeric(winning.color==focal.colors)
  # to correct for team games
  teams <- focal.names[grep("\\&", focal.names)]
#  teams.years <- hit.years[grep("\\&", focal.names)] # needed to copy
#  teams.result <- focal.win[grep("\\&", focal.names)]
#  first.teammate <- substr(teams, 1, regexpr("\\&", teams)-2)
#  second.teammate <- substr(teams, regexpr("\\&", teams)+2, nchar(teams))
#  focal.names[grep("\\&", focal.names)] <- first.teammate
#  focal.names <- c(focal.names, second.teammate)
#  hit.years <- c(hit.years, teams.years)
#  focal.win <- c(focal.win, teams.result)
  #
  hit.matrix <- table(focal.names, hit.years)
  hits.per.year <- colSums(hit.matrix)
  focal.movers.per.year <- colSums(hit.matrix>0) # ...what exactly does this mean?  it's the number of players who are focal movers per year
  focal.uses.per.player <- rowSums(hit.matrix)
  focal.win.matrix <- tapply(focal.win, list(focal.names, hit.years), sum)
  focal.wins.per.player <- rowSums(focal.win.matrix, na.rm=T)
  focal.wins.per.year <- colSums(focal.win.matrix, na.rm=T)
  # cool! it seems players who use smileyface a lot also do well with it....it becomes less and less normal...
  # for(i in 1:(max(focal.uses.per.player)-1)){
    # keep <- focal.uses.per.player > i
    # par(mfrow=c(2,1))
    # plot(1:length(focal.uses.per.player[keep]), sort(focal.wins.per.player[keep]/focal.uses.per.player[keep]), ylim=c(0,1))
    # abline(h=0.5, col="blue")
    # hist(focal.wins.per.player[keep]/focal.uses.per.player[keep], breaks=20, xlim=c(0,1))
    # abline(v=0.5, col="blue")
    # Sys.sleep(0.3)
  # }
  # double graph of hits, frq hits with frq win overlay
  focal.player.win.freq <- focal.wins.per.year/hits.per.year
  all.years <- as.numeric(substr(gogod[,"DT"], 1,4))
  total.games.per.year <- table(all.years[all.years%in%hit.years])
  hit.freq.per.year <- as.numeric(hits.per.year/total.games.per.year)  # bug? - if something appears multiple times in the same game it doesn't get properly accounted for....
  nonzero.years <- as.numeric(colnames(hit.matrix))
  hit.year.range <- min(hit.years):max(hit.years)
  z.hits.per.year <- rep(0, length(hit.year.range))
  z.hits.per.year[hit.year.range%in%nonzero.years] <- hits.per.year
  z.hit.freq.per.year <- rep(0, length(hit.year.range))
  z.hit.freq.per.year[hit.year.range%in%nonzero.years] <- hit.freq.per.year
  z.focal.player.win.freq <- rep(0, length(hit.year.range))
  z.focal.player.win.freq[hit.year.range%in%nonzero.years] <- focal.player.win.freq
  graph.frame <- data.frame(hit.year.range, z.hits.per.year, z.hit.freq.per.year, z.focal.player.win.freq)
  if(is.na(from)) from <- min(graph.frame[,1])
  time.distances <- abs(graph.frame[,1]-from)
  time.min.cutoff <- which(time.distances==min(time.distances))
  graph.frame <- graph.frame[time.min.cutoff:dim(graph.frame)[1],]
  if(is.na(to)) to <- max(graph.frame[,1])
  time.distances <- abs(graph.frame[,1]-to)
  time.max.cutoff <- which(time.distances==min(time.distances))
  graph.frame <- graph.frame[1:time.max.cutoff, ]
  dev.new()
  par(mfrow=c(2,1))
  n.bars <- dim(graph.frame)[1]
  barplot(graph.frame$z.hits.per.year, xlab="Years", space=0, xaxt="n", ylab="Hits", las=1, xlim=c(0, n.bars), ylim=c(0,max(graph.frame$z.hits.per.year)*1.05),  main=paste("Proportion of games with pattern and frequency of ", ifelse(focal.mover=="M1", "First Mover", "Second Mover"), " wins", sep=""))
  axis(1, at=(1:n.bars-0.5), labels=graph.frame$hit.year.range)
  par(new=T)
  plot(1:n.bars-0.5, graph.frame$z.focal.player.win.freq, xlim=c(0,n.bars), ylim=c(0,1), yaxt="n", ylab="", xlab="", type="l", col="blue", xaxt="n", pch=20)
  axis(4, at=c(0,0.5,1), las=1, col.ticks="blue", col.axis="blue", col="blue")
  abline(h=0.5, lty=3, col="lightblue")
  if(!is.na(p.name)){
    mtext(p.name, NORTH<-3, adj=.5, line=.5)
  }
  tentothe=0
  ramp <- graph.frame$z.hit.freq.per.year
  while(sum(ramp[ramp!=0]>1)/length(ramp[ramp!=0]) < .5){
    ramp <- ramp*10
    tentothe <- tentothe + 1
  }
  graph.frame$z.hit.freq.per.year <- ramp
  # second plot: frequency in the population of games over time
  barplot(graph.frame$z.hit.freq.per.year, xlab="Years", space=0, xaxt="n", ylab=paste("Hits per ", 10^tentothe, " games", sep=""), las=1, xlim=c(0,n.bars), ylim=c(0, max(graph.frame$z.hit.freq.per.year)*1.05), main=paste("Proportion of games with pattern and frequency of ", ifelse(focal.mover=="M1", "First Mover", "Second Mover"), " wins", sep=""))
  axis(1, at=(1:n.bars-0.5), labels=graph.frame$hit.year.range)
  par(new=T)
  plot(1:n.bars-0.5, graph.frame$z.focal.player.win.freq, xlim=c(0,n.bars), ylim=c(0,1), yaxt="n", ylab="", xlab="", type="l", col="blue", xaxt="n", pch=20)
  axis(4, at=c(0,0.5,1), las=1, col.ticks="blue", col.axis="blue", col="blue")
  abline(h=0.5, lty=3, col="lightblue")
  if(!is.na(p.name)){
    mtext(p.name, NORTH<-3, adj=.5, line=.5)
  }
  # player year map
  all.years <- as.numeric(substr(data[,"DT"], 1,4))
  white.years <- all.years[data[,"PW"]%in%focal.names]
  white.names <- data[data[,"PW"]%in%focal.names, "PW"]
  black.years <- all.years[data[,"PB"]%in%focal.names]
  black.names <- data[data[,"PB"]%in%focal.names, "PB"]
  lost.teammates <- character(0)
#  lost.teammates <- c(first.teammate[!first.teammate%in%data[,"PB"] & !first.teammate%in%data[,"PW"]], second.teammate[!second.teammate%in%data[,"PB"] & !second.teammate%in%data[,"PW"]])
  lost.years <- numeric(0)
  lost.names <- character(0)
  if(length(lost.teammates)>0){  # surprisingly this can take up an enormous amount of time with the big searchess...
    for(i in 1:length(lost.teammates)){
      found <- all.years[c(grep(lost.teammates[i], data[,"PB"]), grep(lost.teammates[i], data[,"PW"]))]
      lost.names <- c(lost.names, rep(lost.teammates[i], length(found)))
      lost.years <- c(lost.years, found)
    }
  }
  career.matrix <- table(c(black.names, white.names, lost.names), c(black.years, white.years, lost.years))
  hit.matrix.plot <- career.matrix
  hit.matrix.plot[hit.matrix.plot>0]<-0
  hit.matrix.plot[,colnames(career.matrix) %in% colnames(hit.matrix)] <- hit.matrix
  career.years <- as.numeric(colnames(career.matrix))
  hit.years <- as.numeric(colnames(hit.matrix))
  career.range <- min(colnames(career.matrix)):max(colnames(career.matrix))
  career.first <- apply(career.matrix, 1, function(z) min(which(z>0)))
  hit.first <- apply(hit.matrix.plot, 1, function(z) min(which(z>0)))
  o1 <- order(career.first, hit.first)
  o2 <- order(hit.first, career.first)
  n.players <- dim(career.matrix)[1]
    n.years <- dim(career.matrix)[2]
  career.years <- as.numeric(colnames(career.matrix))
  x.coords <- career.years[sort(rep(1:n.years, n.players))]
  y.coords <- rep(1:n.players, n.years)
  htw.ratio <- n.players/n.years
  W=11
  W.plot <- W-(39/32)
  H.plot <- W.plot*n.years/n.players
  H <- H.plot + (29/16)
  career.matrix.o1 <- career.matrix[o1, ]
  hit.matrix.plot.o1 <- hit.matrix.plot[o1,]
  career.matrix.o2 <- career.matrix[o2, ]
  hit.matrix.plot.o2 <- hit.matrix.plot[o2,]
  windows(width=W, height=2*H)
  par(mfrow=c(2,1))
  plot(y.coords[career.matrix.o1>0], x.coords[career.matrix.o1>0], pch=20, cex=0.5, col="gray", las=1, xlab="", ylab="", ylim=c(from, to))
  points(y.coords[hit.matrix.plot.o1>0], x.coords[hit.matrix.plot.o1>0], pch=20, cex=0.5, col="red")
  plot(y.coords[career.matrix.o2>0], x.coords[career.matrix.o2>0], pch=20, cex=0.5, col="gray", las=1, xlab="", ylab="", ylim=c(from, to))
  points(y.coords[hit.matrix.plot.o2>0], x.coords[hit.matrix.plot.o2>0], pch=20, cex=0.5, col="red")
  # wall output
  if(!is.na(file)) png(paste(file, "_graphs.png", sep=""))
  if(is.na(file)) windows(width=W, height=2*H)
  par(mfrow=c(2,1))
  barplot(graph.frame$z.hit.freq.per.year, xlab="Years", space=0, xaxt="n", ylab=paste("Hits per ", 10^tentothe, " games", sep=""), las=1, xlim=c(0,n.bars), ylim=c(0, max(graph.frame$z.hit.freq.per.year)*1.05), main=paste("Proportion of games with pattern and frequency of ", ifelse(focal.mover=="M1", "First Mover", "Second Mover"), " wins", sep=""))
  axis(1, at=(1:n.bars-0.5), labels=graph.frame$hit.year.range)
  par(new=T)
  plot(1:n.bars-0.5, graph.frame$z.focal.player.win.freq, xlim=c(0,n.bars), ylim=c(0,1), yaxt="n", ylab="", xlab="", type="l", col="blue", xaxt="n", pch=20)
  axis(4, at=c(0,0.5,1), las=1, col.ticks="blue", col.axis="blue", col="blue")
  abline(h=0.5, lty=3, col="lightblue")
  if(!is.na(p.name)){
    mtext(p.name, NORTH<-3, adj=.5, line=.5)
  }
  plot(y.coords[career.matrix.o2>0], x.coords[career.matrix.o2>0], pch=20, cex=0.5, col="gray", xlab="", ylab="", las=1, ylim=c(from, to))
  points(y.coords[hit.matrix.plot.o2>0], x.coords[hit.matrix.plot.o2>0], pch=20, cex=0.5, col="red", xlab="", ylab="", las=1)
  if(!is.na(file)) dev.off()
}

biography <- function(name){
  database <- gogod
  PW <- database[,"PW"]
  PB <- database[,"PB"]
  my.b.games <- which(PB==name)
  my.w.games <- which(PW==name)
  my.o <- order(c(my.b.games, my.w.games))
  my.games <- c(my.b.games, my.w.games)
  my.games <- my.games[my.o]
  n.b.games <- length(my.b.games)
  n.w.games <- length(my.w.games)
  my.b.ranks <- gogod[my.b.games,"BR"]
  my.w.ranks <- gogod[my.b.games,"BR"]
  my.ranks <- c(my.b.ranks, my.w.ranks)
  my.ranks <- my.ranks[my.o]
  my.dates <- c(gogod[my.b.games, "DT"], gogod[my.w.games, "DT"])
  my.dates <- my.dates[my.o]
  my.years <- na.omit(as.numeric(substr(my.dates, 1, 4)))
  my.b.re <- gogod[my.b.games,"RE"]
  my.b.wins <- as.numeric(substr(my.b.re, 1, 1) == "B")
  my.w.re <- gogod[my.w.games,"RE"]
  my.w.wins <- as.numeric(substr(my.w.re, 1, 1) == "W")
  my.wins <- c(my.b.wins, my.w.wins)
  my.wins <- my.wins[my.o]
  cat("-----------------------\n")
  cat(name, " Biography in the GoGoD Database\n", sep="")
  cat("-----------------------\n")
  cat(length(my.games), " games on record\n", sep="")
  cat(round(sum(my.wins)/length(my.wins)*100, 2), "% wins\n", sep="")
  cat(name, " plays Black ", round((n.b.games/(n.b.games+n.w.games))*100,1), "% of the time\n", sep="")
  cat(round(sum(my.b.wins)/length(my.b.wins)*100, 2), "% wins As Black\n", sep="")
  cat(round(sum(my.w.wins)/length(my.w.wins)*100, 2), "% wins As White\n", sep="")
  cat("Career spans ", length(min(my.years):max(my.years)), " years (", min(my.years), " to ", max(my.years), ")\n", sep="")
  start <- min(my.years) - min(my.years)%%5
  stop <- max(my.years) + (5-max(my.years)%%5)
  range <- start:stop
  ticks <- range[range%%5==0]
  labs <- range[range%%10==0]
  hist(my.years, breaks=bk(my.years), xaxt="n", col="gray", xlim=c(start, stop), ylab="Number of Games", xlab="",main=paste(name, "'s Games and Performance", sep=""))
  axis(1, at=ticks, labels=F)
  axis(1, at=labs, labels=labs)
  par(new=T)
  annual.win.rate <- tapply(my.wins, my.years, mean)
  plot(names(annual.win.rate), annual.win.rate, type="o", pch=20, ylim=c(0,1), xaxt="n", las=1, xlim=c(start, stop), col="blue", yaxt="n", ylab="", xlab="")
  axis(1, at=ticks, labels=F)
  axis(1, at=labs, labels=labs)
  abline(h=0.5, lty=3, col="lightblue")
  axis(4, at=c(0,0.5,1), las=1, col.ticks="blue", col.axis="blue", col="blue")
  # rank over time...
  # most common opponents
  my.opponents.list <- c(gogod[my.b.games, "PW"], gogod[my.w.games, "PB"])
  my.opponents.list <- my.opponents.list[my.o]
  my.opponents <- sort(table(my.opponents.list), decreasing=T)
  top.ten.opponents <- my.opponents[1:10]
  cat("-----------------------\n")
  cat(name, "'s Top Ten Opponents\n", sep="")
  cat("1. ", names(top.ten.opponents)[1], ", with ", top.ten.opponents[1], " games (", round(mean(my.wins[my.opponents.list==names(top.ten.opponents)[1]])*100, 2), "% won)\n", sep="")
  cat("2. ", names(top.ten.opponents)[2], ", with ", top.ten.opponents[2], " games (", round(mean(my.wins[my.opponents.list==names(top.ten.opponents)[2]])*100, 2), "% won)\n", sep="")
  cat("3. ", names(top.ten.opponents)[3], ", with ", top.ten.opponents[3], " games (", round(mean(my.wins[my.opponents.list==names(top.ten.opponents)[3]])*100, 2), "% won)\n", sep="")
  cat("4. ", names(top.ten.opponents)[4], ", with ", top.ten.opponents[4], " games (", round(mean(my.wins[my.opponents.list==names(top.ten.opponents)[4]])*100, 2), "% won)\n", sep="")
  cat("5. ", names(top.ten.opponents)[5], ", with ", top.ten.opponents[5], " games (", round(mean(my.wins[my.opponents.list==names(top.ten.opponents)[5]])*100, 2), "% won)\n", sep="")
  cat("6. ", names(top.ten.opponents)[6], ", with ", top.ten.opponents[6], " games (", round(mean(my.wins[my.opponents.list==names(top.ten.opponents)[6]])*100, 2), "% won)\n", sep="")
  cat("7. ", names(top.ten.opponents)[7], ", with ", top.ten.opponents[7], " games (", round(mean(my.wins[my.opponents.list==names(top.ten.opponents)[7]])*100, 2), "% won)\n", sep="")
  cat("8. ", names(top.ten.opponents)[8], ", with ", top.ten.opponents[8], " games (", round(mean(my.wins[my.opponents.list==names(top.ten.opponents)[8]])*100, 2), "% won)\n", sep="")
  cat("9. ", names(top.ten.opponents)[9], ", with ", top.ten.opponents[9], " games (", round(mean(my.wins[my.opponents.list==names(top.ten.opponents)[9]])*100, 2), "% won)\n", sep="")
  cat("10. ", names(top.ten.opponents)[10], ", with ", top.ten.opponents[10], " games (", round(mean(my.wins[my.opponents.list==names(top.ten.opponents)[10]])*100, 2), "% won)\n", sep="")
  # number of unique opponents
  # ratio of games to opponents, and graph that over time
  # rank over time, a la kgs...but it has to be cleaned up first!
  cat("-----------------------\n")
  cat(name, "'s ranks held:\n", sep="")
  cat(sort(unique(c(my.b.ranks, my.w.ranks))), "\n")
}


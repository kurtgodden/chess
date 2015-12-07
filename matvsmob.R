#  matvsmob.R
#
#  Analyze chess game data to determine how important material and
#  mobility are in determining a game's outcome. Given that I hypothesize that
#  when white wins, white material and white mobility minus black's
#  will be >0 and <0 when black wins, we would expect these two to relatively 
#  cancel each other out, and for draws we would expect them to be roughly
#  equal to zero.  But given that white begins with a small advantage, we
#  expect white minus black to be very slightly positive overall.  
#
#   rchess described at:
#   http://jkunst.com/r/rchess-a-chess-package-for-r/
#     and at http://rpubs.com/jbkunst/rchess2
#   https://github.com/jbkunst/rchess 
#   pgn parser/converter at:
#   https://github.com/jbkunst/chess-db 
#
# I downloaded 1,696,727 master level games (ELO>2000) from:
# http://www.kingbase-chess.net on 11/16/2015
# I preprocessed and saved that data into .RData format using Joshua Kunst's
# 01_pgn_parser.R code. That data resides in file chess-db.RData
#
# Here are the stats of 5000 randomly-selected games: (193 mins to compute)
#            w.mat     b.mat     w.mob     b.mob
# stats.mean 29.574640 29.553987 33.13604 30.839317
# stats.stdv  5.650526  5.680532  4.86725  4.354868
#
#  2000 random games:
#            w.mat     b.mat     w.mob     b.mob
# stats.mean 29.591335 29.583878 33.177407 30.775609
# stats.stdv  5.705259  5.710414  4.905983  4.369868
#
# And 1000 random games:
#            w.mat     b.mat     w.mob     b.mob
# stats.mean 29.363866 29.334931 32.943453 30.77301
# stats.stdv  5.736882  5.769887  4.967302  4.44095
#
# It took c. 21 mins to process 1,000 games with non-parallel code.  
# Thus, whole dfgames db would take roughly: 
# (1696727/1000) * 21 = 35,631 mins or 593.9 hours or 24.7 days
# The parallel version with 3 cores (b, w, d) would take for all games: 
# > (1696727/1000) * 8.052783 / 60 /24
# [1] 9.488454 DAYS for all 1.7M games
# 100,000 games should take 13.4213 hours or so.
# The revised parallel version using 6 cores:
# > (1696727/1000) * 5.533333 / 60 / 24
# [1] 6.51983, so only 6.5 days vs. 24.7 days for entire game database!
# 100,000 games should take about 9.2 hours   
#
# Revised again, using 8 cores: 5.2 mins/1000 games.
# (1696727/1000) * 5.2 / 60 / 24 = 6.1 days
# =============== Set up Environment and load Chess Game Data ==================

library("rchess")   # R package implementing interface to chess code
library("stringr")  # rchess wants this
library("parallel") # allows huge decrease in processing time
library(pryr)       # fns to assist in memory analysis
# detectCores()
# 8
setwd("~/Documents/R/Chess/MaterialVsMobility")

# ========================== Misc Parameters ==================================

load("chess-db.RData") # sets 'dfgames' to 1,696,727 observations of 11 vars
# 468441      black wins
# 620745      white wins
# 607422      draws
# First 4 games are: draw, draw, white, black
# Of first 1k games: 252 black wins, 354 white, 394 draws

# ============ define functions for material and mobility =====================

material <- function(board, piece.values) {
    # return the material of white & black for current position
    pn.val <- piece.values[piece.values$Piece=="p", 2]
    kt.val <- piece.values[piece.values$Piece=="n", 2]
    bp.val <- piece.values[piece.values$Piece=="b", 2]
    rk.val <- piece.values[piece.values$Piece=="r", 2]
    qn.val <- piece.values[piece.values$Piece=="q", 2]
    
    fen          <- board$fen()
    endRankData  <- regexec(" ", fen)[[1]][1] - 1
    pieces       <- substr(fen, 1, endRankData) 
    
    white.values <- (pn.val * str_count(pieces, "P") + 
                     kt.val * str_count(pieces, "N") +
                     bp.val * str_count(pieces, "B") +
                     rk.val * str_count(pieces, "R") +
                     qn.val * str_count(pieces, "Q"))
    
    black.values <- (pn.val * str_count(pieces, "p") + 
                     kt.val * str_count(pieces, "n") +
                     bp.val * str_count(pieces, "b") +
                     rk.val * str_count(pieces, "r") +
                     qn.val * str_count(pieces, "q"))
    
    c(white.values, black.values)
}

mobility <- function(board){
    # returns number of legal moves at current position
    length(board$moves())
}

material.mobility.game.matrix <- function(pgn) {
    # return matrix of material (row 1, white; row 2, black) and mobility (row 3)
    # before every move in game plus mat/mob of final position
    # In row 3, mobility, the odd numbered columns refer to white's mobility
    # Columns correspond to each half-move of the game
    board   <- Chess$new()
    board$load_pgn(pgn)
    all.san <- board$history()  # pgn as vector without move nums
    board   <- Chess$new()      # reset board to step thru moves
    
    mat.mob.matrix <- apply(as.matrix(all.san), 1, 
                            function(san){
                                #return position's material/mobility, then make next move
                                mat <- material(board, piece.values) # material of w & b
                                mob <- mobility(board)               # num legal moves on board
                                board$move(san)                      # make move taken by player
                                c(mat, mob)
                            })
    mat            <- material(board, piece.values) # material at end position
    mob            <- mobility(board)               # mobility at end position
    cbind(mat.mob.matrix, c(mat, mob))   # return mat.mob including end position 
}

mat.mob.by.result <- function(pgns){  
    # return array of stats PER MOVE of every game
    # pgns is a vector of pgn strings, i.e. games
    if (length(pgns)>1) {
        game.array  <- apply(as.matrix(pgns), 1, material.mobility.game.matrix)
    }
    else { #there is only one game in the group
        game.matrix <- list(material.mobility.game.matrix(pgns))
    }
}

game.stats.multiple <- function(game.array){
    # return mean material and mobility from mutliple games
    # vapply is faster than sapply and also provides names
    stats.matrix <- vapply(game.array, game.stats.single,
                           c("w.mat"=0, "b.mat"=0, "w.mob"=0,"b.mob"=0)) 
    # each column of matrix is a game
    # row 1 is mean white material for each game
    # rwo 2 is mean black material
    # row 3 is mean white mobility
    # row 4 is mean black mobility
    stats.mean <- rowMeans(stats.matrix) # faster than apply/mean
    stats.stdv <- apply(stats.matrix, 1, sd)
    rbind(stats.mean, stats.stdv) 
    #return mean and std dev for w/b material and mobility
}

game.stats.single <- function(game.matrix){
    # return mean material and mobility from single game
    w.mat <- mean(game.matrix[1,])                # mean white material in this game
    b.mat <- mean(game.matrix[2,])                # mean black material
    w.mob <- mean(game.matrix[3, c(TRUE, FALSE)]) # mean white mobility
    b.mob <- mean(game.matrix[3, c(FALSE, TRUE)]) # mean black mobility
    c(w.mat, b.mat, w.mob, b.mob)
}

# get.mat.mob <- function(df) {
#     # this is the top-level non-parallel function
#     tapply(df$pgn, list(df$result), mat.mob.by.result)
# }
# 
# system.time(mat.mob <- get.mat.mob(moves.results.df)) 

get.mat.mob.parallel <- function(df, game.result, num.games, num.cores) { 
    # df is a data frame of games with only one variable: pgn
    # game.result is a text string indicating the outcome of the games
    # being analyzed: black, white, draw, or all
    # num.games is the number of games being analyzed (==nrow(df))
    # num.cores is the number of cores we can use for parallel processing
    #   (currently 8 cores)
    
    # This parallel version takes 25% the time of non-parallel get.mat.mob!
    
    games.per.core <-  floor(num.games/num.cores)
    g1.first <- 1
    g1.last  <- games.per.core
    g2.first <- g1.last + 1
    g2.last  <- 2 * games.per.core
    g3.first <- g2.last + 1
    g3.last  <- 3 * games.per.core
    g4.first <- g3.last + 1
    g4.last  <- 4 * games.per.core
    g5.first <- g4.last + 1
    g5.last  <- 5 * games.per.core
    g6.first <- g5.last + 1
    g6.last  <- 6 * games.per.core
    g7.first <- g6.last + 1
    g7.last  <- 7 * games.per.core
    g8.first <- g7.last + 1
    g8.last  <- num.games
    
    # split df into 8 subsets to be processed by all 8 cores
    g1 <- df[g1.first:g1.last, ]
    g2 <- df[g2.first:g2.last, ] 
    g3 <- df[g3.first:g3.last, ] 
    g4 <- df[g4.first:g4.last, ] 
    g5 <- df[g5.first:g5.last, ] 
    g6 <- df[g6.first:g6.last, ] 
    g7 <- df[g7.first:g7.last, ] 
    g8 <- df[g8.first:g8.last, ] 
    
    mat.mob.array <- mclapply(list(g1, g2, g3, g4, g5, g6, g7, g8), 
                              mat.mob.by.result, 
                              mc.cores = num.cores)     
    # recombine results into single array
    print("Restoring data array...")
    g.array <- append(append(append(append(append(append(append(mat.mob.array[[1]], 
                                                                mat.mob.array[[2]]),
                                                         mat.mob.array[[3]]),
                                                  mat.mob.array[[4]]),
                                           mat.mob.array[[5]]),
                                    mat.mob.array[[6]]),
                             mat.mob.array[[7]]),
                      mat.mob.array[[8]])  
    print("Plotting histograms...")
     # plot histograms using stats from every move of every game
    visualize.move.data(g.array, num.games, game.result) 
    
    # get final statistics
    mat.mob.array <- array(list(g.array))  
    lapply(mat.mob.array, game.stats.multiple)
}   

visualize.move.data <- function(move.data, num.games, result){
    # plot histograms of data
    p.df      <- as.data.frame(move.data)
    num.moves <- ncol(p.df)
    wht.moves <- ceiling(num.moves/2)
    blk.moves <- num.moves - wht.moves
    if (result=="black") outcome<-paste("Won by Black")
    if (result=="white") outcome<-paste("Won by White")
    if (result=="draw")  outcome<-paste("that Resulted in a Draw")
    if (result=="all")   outcome<-paste("Randomly Selected")
    
    hist(as.numeric(p.df[1,]),                               # black material
         main=paste("Black Material in", num.games, "Master-Level Games",
                    "\n", outcome),
         xlab=paste("Black Material Computed at each of", num.moves, "Moves"),
         col="blue")
         
    hist(as.numeric(p.df[2,]),                               # white material
         main=paste("White Material in", num.games, "Master-Level Games",
                    "\n", outcome),
         xlab=paste("White Material Computed at each of", num.moves, "Moves"),
         col="light blue") 
    
    hist(as.numeric(p.df[3, c(FALSE, TRUE)]),                # black mobility
         main=paste("Black Mobility in", num.games, "Master-Level Games",
                    "\n", outcome),
         xlab=paste("Black Mobility Computed for", blk.moves, "Black Moves"),
         col="dark green") 
    
    hist(as.numeric(p.df[3, c(TRUE, FALSE)]),                # white mobility
         main=paste("White Mobility in", num.games, "Master-Level Games",
                    "\n", outcome),
         xlab=paste("White Mobility Computed for", wht.moves, " White Moves"),
         col="green")
}

analyze.chess.games <- function(allgames, num.games, result="all"){
    # this is the top-level function
    # analyze num.games from allgames where result is:
    # 'black', 'white', 'draw', or 'all' games regardless of outcome
    
    if (result=="black") games <- allgames[allgames$result == "0-1",     "pgn"]
    if (result=="white") games <- allgames[allgames$result == "1-0",     "pgn"]
    if (result=="draw")  games <- allgames[allgames$result == "1/2-1/2", "pgn"]
    if (result=="all")   games <- allgames[, "pgn"]
    
    if (num.games>nrow(games)) 
        return(print("Error: Sample size exceeds available games"))
    
    set.seed(42) # make this reproducible
    indices <- sample(nrow(games), num.games)
    get.mat.mob.parallel(as.data.frame(games[indices, ]), 
                         result, num.games, detectCores())
}

# ============================== Set Control Params ============================

piece.values <- data.frame(Piece=c("p", "n", "b", "r", "q"),
                           Value=c(1, 3, 3, 5, 9))
sample.size  <- 2000      # number of games to analyze from dfgames

result       <- "all"   # black, white, draw or all

# ============================= Analyze Chess Games ============================

system.time(mat.mob <- analyze.chess.games(dfgames, sample.size, result=result))

mat.mob[[1]]


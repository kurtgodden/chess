#  matvsmob.R
#
#  Analyze chess game data to determine how important material and
#  mobility are in determining a game's outcome. Given that I hypothesize that
#  when white wins, white material and white mobility minus black's
#  will be >0 and <0 when black wins, we would expect these two to relatively 
#  cancel each other out, and for draws we would expect them to be roughly
#  equal to zero.  But given that white begins with a small advantage, we
#  expect white minus black to be very slightly positive overall.  To test if
#  material and mobility are important factors in a win, we need to test:
#   H0_mat_w: material_white = material_population 
#   H0_mat_b: material_black = material_population
#   H0_mat_d: material_white = material_black
#   H0_mob_w: mobility_white = mobility_population 
#   H0_mat_b: mobility_black = mobility_population
#   H0_mat_d: mobility_white = mobility_black
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
# Here are the stats of the first 1000 games:
# > mat.mob[[1]] # stats for black wins
# w.mat     b.mat     w.mob     b.mob
# stats.mean 29.579637 29.888283 30.710644 33.283729
# stats.stdv  5.081028  4.958256  4.116537  4.269052
# > mat.mob[[2]] # stats for white wins
# w.mat     b.mat     w.mob     b.mob
# stats.mean 29.323026 28.978749 33.067158 30.381365
# stats.stdv  5.089341  5.258845  4.415415  4.229633
# > mat.mob[[3]] # stats for draws
# w.mat     b.mat     w.mob     b.mob
# stats.mean 29.457469 29.463118 30.440890 30.331888
# stats.stdv  6.902642  6.910577  4.310082  4.794257
# > mat.mob[[4]] # stats for all games
# w.mat     b.mat    w.mob     b.mob
# stats.mean 29.440662 29.398793 31.43857 31.093267
# stats.stdv  5.864516  5.904278  4.46326  4.643391

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
# =============== Set up Environment and load Chess Game Data ==================
rm(list = ls())
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
    # 1st item in array are wins by black, then wins by white then draws
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

get.mat.mob.parallel <- function(df) {
    # this is the top-level function
    # This parallel version takes 29% the time of non-parallel get.mat.mob!
    
    b  <- df[df$result == "0-1",     "pgn"]
    w  <- df[df$result == "1-0",     "pgn"]
    d  <- df[df$result == "1/2-1/2", "pgn"]
    
    # get indices to allow splitting each group into 2 subgroups
    blen <- length(b)     # number of black wins
    bmid <- floor(blen/2) # index to mid point of black wins
    bnxt <- bmid + 1      # index to next game after mid point
    
    wlen <- length(w)     # ditto for white
    wmid <- floor(wlen/2)
    wnxt <- wmid + 1
    
    dlen <- length(d)     # ditto for draws
    dmid <- floor(dlen/2)
    dnxt <- dmid + 1
    print(paste("black wins: ", blen, " white wins: ", wlen, " draws: ", dlen)) 
    
    # split the black, white, and draw game lists into 2 parts each
    # to maximize parallelization by using 2 cores each for b, w, and draws
    b1   <- b[1:bmid]     # first half of black games
    b2   <- b[bnxt:blen]  # second half of black games
    w1   <- w[1:wmid]     # ditto for white
    w2   <- w[wnxt:wlen]
    d1   <- d[1:dmid]     # ditto for draws
    d2   <- d[dnxt:dlen]
    
    # use one core to process each of these 6 groups
    mat.mob.array <- mclapply(list(b1, b2, w1, w2, d1, d2), mat.mob.by.result, 
                              mc.cores = 6) 
    # reconstitute the black, white and draw results
    b.array       <- append(mat.mob.array[[1]],  # black mat/mob results
                            mat.mob.array[[2]])
    w.array       <- append(mat.mob.array[[3]],  # white mat/mob results
                            mat.mob.array[[4]])
    d.array       <- append(mat.mob.array[[5]],  # draw mat/mob results
                            mat.mob.array[[6]])
    # combine all into population results so we don't recompute that
    p.array       <- append(append(b.array, w.array), d.array) 
   
     # plot histograms using stats from every move of every game
    visualize.move.data(p.array, blen + wlen + dlen)
    
    # get final statistics
    mat.mob.array <- array(list(b.array, w.array, d.array, p.array))
    mclapply(mat.mob.array, game.stats.multiple, mc.cores=4)
}

visualize.move.data <- function(move.data, num.games){
    # plot histograms of data
    p.df      <- as.data.frame(move.data)
    num.moves <- ncol(p.df)
    wht.moves <- round(num.moves/2)
    blk.moves <- num.moves - wht.moves
    hist(as.numeric(p.df[1,]),                               # black material
         main=paste("Black Material in", num.games, "Master-Level Games"),
         xlab=paste("Black Material Computed at each of", num.moves, "Moves"),
         col="blue")
         
    hist(as.numeric(p.df[2,]),                               # white material
         main=paste("White Material in", num.games, "Master-Level Games"),
         xlab=paste("White Material Computed at each of", num.moves, "Moves"),
         col="light blue") 
    
    hist(as.numeric(p.df[3, c(FALSE, TRUE)]),                # black mobility
         main=paste("Black Mobility in", num.games, "Master-Level Games"),
         xlab=paste("Black Mobility Computed for", blk.moves, "Black Moves"),
         col="dark green") 
    
    hist(as.numeric(p.df[3, c(TRUE, FALSE)]),                # white mobility
         main=paste("White Mobility in", num.games, "Master-Level Games"),
         xlab=paste("White Mobility Computed for", wht.moves, " White Moves"),
         col="green")
}

# ============================== Set Misc Params ===============================

piece.values     <- data.frame(Piece=c("p", "n", "b", "r", "q"),
                               Value=c(1, 3, 3, 5, 9))
set.seed(42)
sampleSize       <- 2000    # number of games to analyze from dfgames
indices          <- sample(nrow(dfgames), sampleSize)
# indices <- 1:1000
moves.results.df <- as.data.frame(dfgames[indices, c("pgn", "result")])
mem_change(rm(list = ("dfgames"))) # reclaims about 1G of memory for 1000 games

# ============================= Analyze Chess Games ============================

system.time(mat.mob <- get.mat.mob.parallel(moves.results.df))

# This code takes roughly 6 mins/1000 games
mat.mob[[1]] # stats for black wins
mat.mob[[2]] # stats for white wins
mat.mob[[3]] # stats for draws
mat.mob[[4]] # stats for all games


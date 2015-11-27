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
#   Chess Visualization Code from:
#   http://jkunst.com/r/visualizing-chess-data-with-ggplot/
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
# > 
#     > # now compute same stats but for population w/o regard for outcome
#     > # This code is a shame because it's actually recomputing what has already been done!
#     > system.time(mat.mob.population <- get.mat.mob.population(moves.results.df))
# user   system  elapsed 
# 1237.805   15.791 1246.195 
# > mat.mob.population[[1]]
# w.mat     b.mat    w.mob     b.mob
# stats.mean 29.440662 29.398793 31.43857 31.093267
# stats.stdv  5.864516  5.904278  4.46326  4.643391

# ========================== Set up Environment ===============================
library("rchess")
library("stringr")
library("parallel") 
cores <- detectCores()
# 8

setwd("~/Documents/R/Chess/MaterialVsMobility")

# ========================== Misc Parameters ==================================

load("chess-db.RData") # sets 'dfgames' to 1,696,727 observations of 11 vars

piece.values <- data.frame(Piece=c("p", "n", "b", "r", "q"),
                           Value=c(1, 3, 3, 5, 9))

# First 4 games are: draw, draw, white, black
moves.results.df <- as.data.frame(dfgames[1:5, c("pgn", "result")])
# of first 1k games: 252 black wins, 354 white, 394 draws

# It took c. 21 mins to process 1,000 games with non-parallel code.  
# Thus, whole dfgames db would take roughly: 
# (1696727/1000) * 21 = 35,631 mins or 593.9 hours or 24.7 days
# The parallel version took about 8 mins giving: 
# 13573.82 mins or 226.2303 hrs or 9.4 days!
# 100,000 games should take 13.3 hours or so.

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
    cbind(mat.mob.matrix, c(mat, mob))   # return mat.mob incl. final position 
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

# get.mat.mob.population <- function(df) { 
#     # this is a top-level function
#     # Compute stats for entire population of games,
#     # i.e. do not split by outomes.
#     
#     all <- df[, "pgn"]
#     # use one core for each group
#     lapply(list(all), mat.mob.by.result) 
# }

get.mat.mob.parallel <- function(df) {# debug printing doesn't work when parallel
    # this is the top-level function
    # This parallel version takes 38% the time of non-parallel get.mat.mob!
    
    b <- df[df$result == "0-1",     "pgn"]
    w <- df[df$result == "1-0",     "pgn"]
    d <- df[df$result == "1/2-1/2", "pgn"]
    # use one core for each group
    mat.mob.array <- mclapply(list(b, w, d), mat.mob.by.result, mc.cores = 3) 
#    print(mat.mob.array)
    
    mat.mob.population <- append(append(mat.mob.array[[1]], 
                                        mat.mob.array[[2]]), 
                                 mat.mob.array[[3]])
    
    mat.mob.array[[4]] <- mat.mob.population
    print(paste("black wins: ", length(b), " white wins: ", length(w), " draws: ", length(d))) 
    mclapply(mat.mob.array, game.stats.multiple, mc.cores=4)
} 

# =============================================================================

system.time(mat.mob <- get.mat.mob.parallel(moves.results.df))

mat.mob[[1]] # stats for black wins
mat.mob[[2]] # stats for white wins
mat.mob[[3]] # stats for draws
mat.mob[[4]] # stats for all games


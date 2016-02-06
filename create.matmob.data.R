#  create.matmob.data.R
#  This file is maintained at https://github.com/kurtgodden/chess
#
#   rchess described at:
#   http://jkunst.com/r/rchess-a-chess-package-for-r/
#     and at http://rpubs.com/jbkunst/rchess2
#   https://github.com/jbkunst/rchess 
#   pgn parser/converter at:
#   https://github.com/jbkunst/chess-db 
#
# I downloaded 1,696,727 master level games (ELO>=2000) from:
# http://www.kingbase-chess.net on 11/16/2015
# I preprocessed and saved that data into .RData format using Joshua Kunst's
# 01_pgn_parser.R code. That preprocessed data resides in file chess-db.RData
#
# This code reads chess-db.RData, which creates data frame 'dfgames'.
# creates basic material/mobility stats
# and writes that back out to file 'chess-matmob.csv'
# 
# The saved data is from a data frame with one game per row and these vars:
# dfgames.row:  integer, index to row of dfgames.
# w.mat:        numeric, vector of white material at every board position
# w.mat.mean:   numeric, mean of w.mat
# w.mat.sd:     numeric, standard deviation of w.mat
# b.mat:        numeric, vector of black material at every board position
# b.mat.mean:   numeric, mean of b.mat
# b.mat.sd:     numeric, standard deviation of b.mat
# w.mob:        integer, vector of move counts for each white move
# w.mob.mean:   numeric, mean of w.mob
# w.mob.sd:     numeric, standard deviation of w.mob
# b.mob:        numeric, vector of move counts for each black move
# b.mob.mean:   numeric, mean of b.mob
# b.mob.sd:     numeric, standard deviation of b.mob
# * NOT YET FULLY IMPLEMENTED *# fen: character, vector of fen board positions
# pgn:          character, pgn string
# result:       factor, outcome of game: "1-0", "0-1", or "1/2-1/2"
# 
# Another code file will read this data to perform statistical analysis.

# =============== Set up Environment and load Chess Game Data ==================

library("rchess")   # R package implementing interface to chess code
library("stringr")  # rchess wants this
library("parallel") # allows huge decrease in processing time
library("pryr")     # fns to assist in memory analysis
# detectCores()
# 8
setwd("~/Documents/R/Chess/MaterialVsMobility")

load("chess-db.RData") # sets 'dfgames' to 1,696,727 observations of 11 vars
# 468441      black wins
# 620745      white wins
# 607422      draws
# 119         games with unknown result, recorded in pgn as '*'

# ============ define functions for material and mobility =====================

material <- function(board, piece.values) { 
    # return the material of white & black for current position
    pn.val <- piece.values[piece.values$Piece=="p", 2]
    kt.val <- piece.values[piece.values$Piece=="n", 2]
    bp.val <- piece.values[piece.values$Piece=="b", 2]
    rk.val <- piece.values[piece.values$Piece=="r", 2]
    qn.val <- piece.values[piece.values$Piece=="q", 2]
    # parse fen and compute white/black material
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
#    list(c(white.values, black.values), fen)
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
    
#    fen.vec <- NULL # We're going to build a vector of the fen strings for this game
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
#                                fen.vec <<- c(fen.vec, mat[[2]])     # add fen to fen vector
#                                c(mat[[1]], mob) #return only the w/b mat for the matrix, ignoring fen
                                c(mat, mob)
                            })
    mat            <- material(board, piece.values) # material at end position
    mob            <- mobility(board)               # mobility at end position
    # return mat.mob.matrix, now including end position and fen strings
#    list(cbind(mat.mob.matrix, c(mat[[1]], mob)), fen.vec)   
    cbind(mat.mob.matrix, c(mat, mob))
}

mat.mob.by.result <- function(pgns){  
    # return array of stats PER MOVE of every game
    # pgns is a vector of pgn strings, i.e. games
    if (length(pgns)>1) {
        game.array  <- apply(as.matrix(pgns), 1, material.mobility.game.matrix)
        game.array
    }
    else { #there is only one game in the group
        game.matrix <- list(material.mobility.game.matrix(pgns))
        game.matrix
    }
}

game.stats.multiple <- function(game.array, write.data.to.file, 
                                result.vector, index.vector, pgn.vector){
    # return mean material and mobility from multiple games
    # vapply is faster than sapply and also provides names
    # write.data.to.file controls whether we write our data to a file or not

    stats.matrix <- vapply(game.array, game.stats.single,
                           c("w.mat.mean"=0, "w.mat.sd"=0,
                             "b.mat.mean"=0, "b.mat.sd"=0,
                             "w.mob.mean"=0, "w.mob.sd"=0,
                             "b.mob.mean"=0, "b.mob.sd"=0)) 
    # each column of matrix is a game
    # row 1 is mean white material for each game
    # row 2 is the white material std dev
    # row 3 is mean black material
    # row 4 is the black material std dev
    # row 5 is mean white mobility
    # row 6 is the white mobility std dev
    # row 7 is mean black mobility
    # row 8 is the black mobility std dev

    stats.df              <- as.data.frame(t(stats.matrix))
    stats.df$result       <- result.vector
    stats.df$dfgames.row  <- index.vector
    
    # In next lines, the game vectors of material and mobility
    # get saved as strings like: "c(1, 2, 3)"
    
    w.mat.vec <- as.character(lapply(game.array, get.matmob.vector, "w.mat"))
    b.mat.vec <- as.character(lapply(game.array, get.matmob.vector, "b.mat"))
    w.mob.vec <- as.character(lapply(game.array, get.matmob.vector, "w.mob"))
    b.mob.vec <- as.character(lapply(game.array, get.matmob.vector, "b.mob"))
    
    # So we strip off the c(), leaving strings like "1.1, 2.2, 3.3, ..."
    stats.df$w.mat.vector <- substr(w.mat.vec, 3, nchar(w.mat.vec)-1)
    stats.df$b.mat.vector <- substr(b.mat.vec, 3, nchar(b.mat.vec)-1)
    stats.df$w.mob.vector <- substr(w.mob.vec, 3, nchar(w.mob.vec)-1)
    stats.df$b.mob.vector <- substr(b.mob.vec, 3, nchar(b.mob.vec)-1)
    # Then when reading those back in from file, we can convert those strings 
    # back to numeric vectors using scan():
    # scan(text="1.1, 2.2, 3.3", sep=",") -> [1] 1.1 2.2 3.3 etc.
    
    stats.df$pgn          <- pgn.vector 
    
    if (write.data.to.file) {
        fname <- paste("stats", sample.size, "games", 
                       "result.all", "csv", sep=".")
        write.csv(stats.df, file=fname)
    }
    stats.mean <- rowMeans(stats.matrix) # faster than apply/mean
    stats.stdv <- apply(stats.matrix, 1, sd)
    rbind(stats.mean, stats.stdv) #return mean and std dev for w/b material and mobility
} 

get.matmob.vector <- function(game.matrix, vec){
    # game.matrix has 3 rows: white material, black material, combined mobility

    if (vec=="w.mat")
        game.matrix[1, ]
    else if (vec=="b.mat")
        game.matrix[2, ]
    else if (vec=="w.mob")
        game.matrix[3, c(TRUE, FALSE)]
    else if (vec=="b.mob")
        game.matrix[3, c(FALSE, TRUE)]
    else print(paste("Error: cannot determine requested vector",
                     vec, "in get.matmob.vector"))
}

game.stats.single <- function(game.matrix){
    # return mean material and mobility from single game
    # along with an indicator if game ended in checkmate
    w.mat       <- mean(game.matrix[1,])                # mean white material in this game
    w.mat.sd    <- sd(game.matrix[1,])                  # and its sd
    b.mat       <- mean(game.matrix[2,])                # mean black material
    b.mat.sd    <- sd(game.matrix[2,])                  # and its sd
    
    white.moves <- game.matrix[3, c(TRUE, FALSE)]
    black.moves <- game.matrix[3, c(FALSE, TRUE)]
    
    w.mob       <- mean(white.moves)   # mean white mobility
    w.mob.sd    <- sd(white.moves)     # and their sd
    b.mob       <- mean(black.moves)   # mean black mobility
    b.mob.sd    <- sd(black.moves)     # and their sd

    c(w.mat, w.mat.sd, b.mat, b.mat.sd, w.mob, w.mob.sd, b.mob, b.mob.sd)
}

get.mat.mob.parallel <- function(df, num.games, 
                                 num.cores, write.data.to.file) { 
    # df is a data frame of games with 3 vars: index (to db), result, pgn
    # num.games is the number of games being processed (==nrow(df))
    # num.cores is the number of cores we can use for parallel processing
    # (currently 8 cores)
    # write.data.to.file is boolean and controls whether we write our data
    games.per.core <-  floor(num.games/num.cores)
    g1.first       <- 1
    g1.last        <- games.per.core
    g2.first       <- g1.last + 1
    g2.last        <- 2 * games.per.core
    g3.first       <- g2.last + 1
    g3.last        <- 3 * games.per.core
    g4.first       <- g3.last + 1
    g4.last        <- 4 * games.per.core
    g5.first       <- g4.last + 1
    g5.last        <- 5 * games.per.core
    g6.first       <- g5.last + 1
    g6.last        <- 6 * games.per.core
    g7.first       <- g6.last + 1
    g7.last        <- 7 * games.per.core
    g8.first       <- g7.last + 1
    g8.last        <- num.games
    
    # split df into 8 subsets to process one per core
    g1 <- df[g1.first:g1.last, "pgn"]
    g2 <- df[g2.first:g2.last, "pgn"] 
    g3 <- df[g3.first:g3.last, "pgn"] 
    g4 <- df[g4.first:g4.last, "pgn"] 
    g5 <- df[g5.first:g5.last, "pgn"] 
    g6 <- df[g6.first:g6.last, "pgn"] 
    g7 <- df[g7.first:g7.last, "pgn"] 
    g8 <- df[g8.first:g8.last, "pgn"] 
    
    mat.mob.array <- mclapply(list(g1, g2, g3, g4, g5, g6, g7, g8), 
                              mat.mob.by.result, 
                              mc.cores = num.cores)   
    # serial version of mclapply, so I can print stuff during debugging
    mat.mob.array <- lapply(list(g1, g2, g3, g4, g5, g6, g7, g8), 
                            mat.mob.by.result)      
    # recombine results into single array
    # we need to omit the fen from this sequence of appends
    # If I start getting the fen, then the mat.mob.array will be
    # a list of 8 elements, one for each core.
    # Each element will be a list of games, where each game is actually
    # a 2-element list: the first elem is the matmob matrix, which is to 
    # be extracted, appended and saved into g.array, and the 2nd elem
    # are the vector of fen strings for that game.  These will have to be 
    # collected, combined and then saved into a new variable to be written
    # out as part of the saved data frame.
    g.array <- append(append(append(append(append(append(append(mat.mob.array[[1]], 
                                                                mat.mob.array[[2]]),
                                                         mat.mob.array[[3]]),
                                                  mat.mob.array[[4]]),
                                           mat.mob.array[[5]]),
                                    mat.mob.array[[6]]),
                             mat.mob.array[[7]]),
                      mat.mob.array[[8]])  
    # get final statistics
    mat.mob.array <- array(list(g.array))  
    lapply(mat.mob.array, game.stats.multiple, 
           write.data.to.file, df$result, df$df.row, df$pgn)
}   

data.overview <- function(games, num.games){
    # print high-level summary of games to screen before processing

        black.wins <- nrow(games[games$result == "0-1",     ])
        white.wins <- nrow(games[games$result == "1-0",     ])
        num.draws  <- nrow(games[games$result == "1/2-1/2", ])
        num.unk    <- nrow(games[games$result == "*",       ])
        descriptive.result <- paste("consisting of:\n",
                                    black.wins, "black wins\n",
                                    white.wins, "white wins\n",
                                    num.draws,  "draws, and\n",
                                    num.unk,    "unknown outcomes.")
    writeLines(paste("Processing", num.games, 
                     "chess games", descriptive.result))
}

process.chess.data <- function(allgames, num.games, write.data.to.file) {
    # process num.games from global dfgames

    #Note: These will include some games with result = "*"
    games <- allgames[, c("result", "pgn")] 
    
    if (num.games>nrow(games)) 
        return(print("Error: Sample size exceeds available games"))
   
    # Free up 1GB of memory used by dfgames
    mem_change(rm(allgames, inherits=TRUE))
    mem_change(rm(dfgames,  inherits=TRUE))
    
    set.seed(42) # make this code reproducible
#============ put indices into a loop either here or where this fn is called
#==================================== READ THIS!!!!!!!!!! ======================================================
#    indices <- sample(nrow(games), num.games)
    indices <- 1:32
    games   <- as.data.frame(games[indices, ]) # games to analyze
    data.overview(games, num.games)    # summarize high-level stats of these games
 
    # So 'games' df contains 3 vars: df.row, result and pgn.
    games$df.row <- indices                     
    
    get.mat.mob.parallel(games, num.games, 
                         detectCores(), write.data.to.file) # I have 8 cores
}

# ============================== Set Control Params ============================

piece.values <- data.frame(Piece=c("p", "n", "b", "r", "q"), 
                           Value=c( 1,   3,   3,   5,   9))
sample.size  <- 32     # number of games to process from dfgames per iteration
save.data    <- TRUE     # write new data to file or not

# ============================= Analyze Chess Games ============================

system.time(mat.mob <- process.chess.data(dfgames, sample.size, save.data))

# mat.mob <- analyze.chess.games(dfgames,   sample.size, result, do.t.test, 
#                                c(mat.diff.pop, mob.diff.pop), 
#                                save.data, linear.fits)


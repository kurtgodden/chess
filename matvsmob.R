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
# I downloaded 1.6M master level games (ELO>2000) from:
# http://www.kingbase-chess.net on 11/16/2015
# I preprocessed and saved that data into .RData format using Joshua Kunst's
# 01_pgn_parser.R code. That data resides in file chess-db.RData

# ========================== Set up Environment ===============================
library("rchess")
# library("dplyr") # DO I REALLY NEED THIS??????????????????????
library("stringr")

setwd("~/Documents/R/Chess/MaterialVsMobility")

# ========================== Misc Parameters ==================================

load("chess-db.RData") # sets 'dfgames' to 1,696,727 observations of 11 vars

piece.values <- data.frame(Piece=c("p", "n", "b", "r", "q"),
                           Value=c(1, 3, 3, 5, 9))

# In game 4 of dfgames, Karpov as white lost to Morozevich.
pgn <- dfgames[4, 11]$pgn # col 11 is the pgn data, returned as 1x1 df 

# 1.Nf3 d5 2.g3 c6 3.Bg2 Bf5 4.c4 e6 5.Qb3 Qb6 6.d3 Nf6 7.Be3 Qxb3 8.axb3 a6 
# 9.Nh4 Bg6 10.f4 Nbd7 11.h3 Bb4+ 12.Kd1 O-O 13.Nd2 Rfd8 14.Kc2 b5 15.Rhd1  a5 16.Nxg6 hxg6 
# 17.Nf3 Re8 18.Nd4 Rac8 19.Bd2 Nh5 20.cxb5 cxb5+ 21.Kb1  Nxg3 22.Nxb5 Rb8 23.Nd4 Nc5 
# 24.Be3 Nf5 25.Nxf5 gxf5 26.Ka2 Nxb3 27.Rab1  Bd6 28.Ba7 Rb4 29.e4 Ra8 30.Be3 Rab8

# ============ define functions for material and mobility =====================

material <- function(board, piece.values) {
    # return the relative material advantage of white vs. black
    # white advantage: material > 0
    # black advantage: material < 0
    # no advantage:    material = 0
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
    
#     material <- white.values - black.values
#     material
}

mobility <- function(board){
    # returns number of possible moves at current position
    length(board$moves())
}

material.mobility.game.matrix <- function(pgn) {
    # return matrix of material (row 1) and mobility (row 2)
    # before every move in game plus mat/mob of final position
    # In row 2, mobility, the odd numbered columns refer to white's mobility
    board   <- Chess$new()
    board$load_pgn(pgn)
    all.san <- board$history()  # pgn as vector without move nums
    board   <- Chess$new()      # reset board to step thru moves
    
    get.mat.mob <- function(san){  #called by apply in next line after this fn
       #return position's material/mobility, then make next move
        mat <- material(board, piece.values) # relative material: w - b
        mob <- mobility(board)               # num legal moves on board
        board$move(san)                      # make move taken by player
        c(mat, mob)
    }
    
    mat.mob.matrix <- apply(as.matrix(all.san), 1, get.mat.mob)
    mat            <- material(board, piece.values) # material at end position
    mob            <- mobility(board)               # mobility at end position
    cbind(mat.mob.matrix, c(mat, mob))   # return mat.mob incl. final position 
}

# =============================================================================

game.matrix <- material.mobility.game.matrix(pgn)
print(mean(game.matrix[1,]))                # mean white material in this game
print(mean(game.matrix[2,]))                # mean black material
print(mean(game.matrix[3, c(TRUE, FALSE)])) # mean white mobility
print(mean(game.matrix[3, c(FALSE, TRUE)])) # mean black mobility

# Some useful stuff I may need:
# turn          <- board$turn()                  # whose turn, w or b?
# num.moves     <- length(all.san) # number of half moves
# white.moves   <- ceiling(num.moves/2)      # number of white moves
# black.moves   <- floor(num.moves/2)        # number of black moves


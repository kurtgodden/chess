#  matvsmob.R
#
#  Analyze chess game data to determine which of material vs.
#  mobility is more important in determining a game's outcome.
#  Thus, H0: material = mobility

library("rchess")
library("dplyr") # for filtering and joining
library("stringr")

# ========================== Misc Parameters ==================================

piece.values <- data.frame(Piece=c("p", "n", "b", "r", "q"),
                           Value=c(1, 3, 3, 5, 9))

# ============ define functions for material and mobility =====================

material <- function(board, piece.values) {
    # return the relative material advantage of white vs. black
    # white advantage: material >  0
    # black advantage: material <  0
    # no advantage:    material == 0
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
    
    material <- white.values - black.values
    material
}

mobility <- function(board){
    # returns number of possible moves at current position
    length(board$moves())
}

pgn <- "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6
4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6
8. c3 O-O 9. h3 Nb8  10. d4 Nbd7
11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7
14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5
Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4"

board <- Chess$new()
board$load_pgn(pgn)
plot(board)

board$move("Nc6")
plot(board)
material(board, piece.values)
mobility(board)

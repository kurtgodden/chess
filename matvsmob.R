#  matvsmob.R
#
# I wrote this up at: http://rpubs.com/kurtgodden/135598
#
#  Analyze chess game data to determine how important material and
#  mobility are in determining a game's outcome. Given that I hypothesize that
#  when white wins, white material and white mobility minus black's
#  will be >0 (and <0 when black wins), we would expect these two to relatively 
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
# 1000 random games won by black:
#            w.mat     b.mat     w.mob     b.mob
# stats.mean 28.663276 29.038162 31.747328 32.336095
# stats.stdv  5.214587  5.073105  4.688989  4.410876
#
# 1000 random games won by white:
#            w.mat     b.mat     w.mob     b.mob
# stats.mean 29.45825 29.142364 34.362652 29.786741
# stats.stdv  5.34346  5.487446  4.900061  4.323617
#
# 1000 random games ending in a draw:
#            w.mat     b.mat     w.mob     b.mob
# stats.mean 29.902903 29.907774 32.494551 30.468773
# stats.stdv  6.500493  6.511622  4.805943  4.277875
# 
#
#  Hypothesis Test of Black mean mobility, using the 5k stats for the estimate of
#  sigma_population: 4.354868
#  (black sample mean - black population mean) / (sigma_pop/sqrt of sample size)
# (32.336095-30.839317)/(4.354868/1000^0.5)
#  [1] 10.86882
# So the black mobility in black wins is >10 SE from population black mobility!
#
# Same test, applied to wins by white, using 4.86725 estimate of sigma_pop:
# (34.362652 - 33.13604)/(4.86725 / 1000^0.5)
# [1] 7.969362, again very significant, way better than 99% level
#
# Hypothesis test of black material:
# (29.038162 - 29.553987) / (5.680532 / 1000^0.5)
# [1] -2.87153, there's a surprise!  Black has LESS material at >99% conf level!
#
#  White material:
#  (29.45825 - 29.574640) / (5.650526 / 1000^0.5)
# [1] -0.6513686  NOT statistically significant

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




# Paired t-tests of white - black under 1000 white wins with:
# alternative="greater", paired=TRUE, mu=0

# [1] "T-test of white-black material"
# 
# Paired t-test
# 
# data:  stats.matrix[1, ] and stats.matrix[2, ]
# t = 16.655, df = 999, p-value < 2.2e-16
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#     0.2846601       Inf
# sample estimates:
#     mean of the differences 
# 0.3158866 
# 
# [1] "T-test of white-black mobility"
# 
# Paired t-test
# 
# data:  stats.matrix[3, ] and stats.matrix[4, ]
# t = 31.598, df = 999, p-value < 2.2e-16
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#     4.337491      Inf
# sample estimates:
#     mean of the differences 
# 4.57591 

# [1] "T-test of white-black material"
# 
# Paired t-test
# 
# data:  stats.matrix[1, ] and stats.matrix[2, ]
# t = 15.566, df = 999, p-value < 2.2e-16
# alternative hypothesis: true difference in means is greater than 0.020653
# 95 percent confidence interval:
#     0.2846601       Inf
# sample estimates:
#     mean of the differences 
# 0.3158866 
# 
# [1] "T-test of white-black mobility"
# 
# Paired t-test
# 
# data:  stats.matrix[3, ] and stats.matrix[4, ]
# t = 15.739, df = 999, p-value < 2.2e-16
# alternative hypothesis: true difference in means is greater than 2.296723
# 95 percent confidence interval:
#     4.337491      Inf
# sample estimates:
#     mean of the differences 
# 4.57591 
# 

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

# game.stats.multiple <- function(game.array, game.result, 
#                                 do.t, matmob.diffs.pop){
#     # return mean material and mobility from multiple games
#     # vapply is faster than sapply and also provides names
#     # if do.t is TRUE, we perform paired t.test of white vs. black stats
#     # matmob.diffs.pop is sequence of differences in material and mobility 
#     # (white - black) from the estimates in the population
#     
#     stats.matrix <- vapply(game.array, game.stats.single,
#                            c("w.mat"=0, "b.mat"=0, "w.mob"=0,"b.mob"=0)) 
#     # each column of matrix is a game
#     # row 1 is mean white material for each game
#     # row 2 is mean black material
#     # row 3 is mean white mobility
#     # row 4 is mean black mobility
#     
#     if (do.t==TRUE) paired.t.test(game.result, stats.matrix, matmob.diffs.pop)
#     
#     stats.mean <- rowMeans(stats.matrix) # faster than apply/mean
#     stats.stdv <- apply(stats.matrix, 1, sd)
#     rbind(stats.mean, stats.stdv) #return mean and std dev for w/b material and mobility
# }

game.stats.multiple <- function(game.array, game.result, 
                                do.t, matmob.diffs.pop){
    # return mean material and mobility from multiple games
    # vapply is faster than sapply and also provides names
    # if do.t is TRUE, we perform paired t.test of white vs. black stats
    # matmob.diffs.pop is sequence of differences in material and mobility 
    # (white - black) from the estimates in the population
    
    stats.matrix <- vapply(game.array, game.stats.single,
                           c("w.mat"=0, "b.mat"=0, "w.mob"=0,"b.mob"=0, "mates"=0)) 
    # each column of matrix is a game
    # row 1 is mean white material for each game
    # row 2 is mean black material
    # row 3 is mean white mobility
    # row 4 is mean black mobility
    # row 5 is 1 if game ended in checkmate
    
    if (do.t==TRUE) paired.t.test(game.result, stats.matrix, matmob.diffs.pop)
    
######## I need to understand the following better before I write about it################
    stats.df <- as.data.frame(t(stats.matrix))
#    print(stats.df)
    lm.white.fit <- lm(w.mat ~ w.mob, data=stats.df)
    lm.black.fit <- lm(b.mat ~ b.mob, data=stats.df)
    
    plot(stats.df$w.mob, stats.df$w.mat)
    abline(lm.white.fit)
    plot(lm.white.fit)
    print(summary(lm.white.fit))
    
    plot(stats.df$b.mob, stats.df$b.mat)
    abline(lm.black.fit)
    plot(lm.black.fit)
    print(summary(lm.black.fit)) 
##########################################################################################
    
    stats.mean <- rowMeans(stats.matrix) # faster than apply/mean
    stats.stdv <- apply(stats.matrix, 1, sd)
    rbind(stats.mean, stats.stdv) #return mean and std dev for w/b material and mobility
}

# game.stats.single <- function(game.matrix){
#     # return mean material and mobility from single game
#     w.mat <- mean(game.matrix[1,])                # mean white material in this game
#     b.mat <- mean(game.matrix[2,])                # mean black material
#     w.mob <- mean(game.matrix[3, c(TRUE, FALSE)]) # mean white mobility
#     b.mob <- mean(game.matrix[3, c(FALSE, TRUE)]) # mean black mobility
#     c(w.mat, b.mat, w.mob, b.mob)
# }

game.stats.single <- function(game.matrix){
    # return mean material and mobility from single game
    # along with an indicator if game ended in checkmat
    w.mat <- mean(game.matrix[1,])                # mean white material in this game
    b.mat <- mean(game.matrix[2,])                # mean black material
    
    white.moves <- game.matrix[3, c(TRUE, FALSE)]
    black.moves <- game.matrix[3, c(FALSE, TRUE)]
    checkmate   <- sum(white.moves==0, black.moves==0) # did game end in mate?
    
    w.mob <- mean(white.moves) # mean white mobility
    b.mob <- mean(black.moves) # mean black mobility
    c(w.mat, b.mat, w.mob, b.mob, checkmate)
}


# get.mat.mob <- function(df) {
#     # this is the top-level non-parallel function
#     tapply(df$pgn, list(df$result), mat.mob.by.result)
# }
# 
# system.time(mat.mob <- get.mat.mob(moves.results.df)) 

get.mat.mob.parallel <- function(df, game.result, do.t, matmob.diffs.pop,
                                 num.games, num.cores) { 
    # df is a data frame of games with only one variable: pgn
    # game.result is a text string indicating the outcome of the games
    # being analyzed: black, white, draw, or all
    # do.t is boolean and controls whether paired t.test is performed
    # matmob.diffs.pop contains sequence of differences between
    # material and mobility (white minus black) in the population.
    # num.games is the number of games being analyzed (==nrow(df))
    # num.cores is the number of cores we can use for parallel processing
    # (currently 8 cores)
    # This parallel version takes 25% the time of non-parallel version!
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
    g.array <- append(append(append(append(append(append(append(mat.mob.array[[1]], 
                                                                mat.mob.array[[2]]),
                                                         mat.mob.array[[3]]),
                                                  mat.mob.array[[4]]),
                                           mat.mob.array[[5]]),
                                    mat.mob.array[[6]]),
                             mat.mob.array[[7]]),
                      mat.mob.array[[8]])  
    # plot histograms using stats from every move of every game
    visualize.move.data(g.array, num.games, game.result) 
    
    # get final statistics
    mat.mob.array <- array(list(g.array))  
    lapply(mat.mob.array, game.stats.multiple, game.result, 
           do.t, matmob.diffs.pop)
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
         main=paste("Black Material in", num.games, "Expert-Level Games",
                    "\n", outcome),
         xlab=paste("Black Material Computed at each of", num.moves, "Moves"),
         col="blue")
         
    hist(as.numeric(p.df[2,]),                               # white material
         main=paste("White Material in", num.games, "Expert-Level Games",
                    "\n", outcome),
         xlab=paste("White Material Computed at each of", num.moves, "Moves"),
         col="light blue") 
    
    hist(as.numeric(p.df[3, c(FALSE, TRUE)]),                # black mobility
         main=paste("Black Mobility in", num.games, "Expert-Level Games",
                    "\n", outcome),
         xlab=paste("Black Mobility Computed for", blk.moves, "Black Moves"),
         col="dark green") 
    
    hist(as.numeric(p.df[3, c(TRUE, FALSE)]),                # white mobility
         main=paste("White Mobility in", num.games, "Expert-Level Games",
                    "\n", outcome),
         xlab=paste("White Mobility Computed for", wht.moves, " White Moves"),
         col="green")
}

data.overview <- function(games, num.games, result){
    # print high-level summary of games to screen before deep analysis
    if (result=="black") descriptive.result <- "won by black."
    if (result=="white") descriptive.result <- "won by white."
    if (result=="draw")  descriptive.result <- "ending in a draw."
    if (result=="all")  {
        black.wins <- nrow(games[games$result == "0-1",     ])
        white.wins <- nrow(games[games$result == "1-0",     ])
        num.draws  <- nrow(games[games$result == "1/2-1/2", ])
        unk.result <- nrow(games[games$result == "*",       ])
        descriptive.result <- paste("consisting of:\n",
                                    black.wins, "black wins\n",
                                    white.wins, "white wins\n",
                                    num.draws, "draws, and\n",
                                    unk.result, "game(s) of unknown result.\n")} 
    
    writeLines(paste("Analyzing random sample of", num.games, 
                "chess games", descriptive.result))
}

analyze.chess.games <- function(allgames, num.games, result="all", 
                                do.t=TRUE, matmob.diffs.pop){
    # this is the top-level function
    # analyze num.games from global dfgames where result is:
    # 'black', 'white', 'draw', or 'all' games regardless of outcome
    # Perform paired t-test of black vs. white advantage if do.t is T,
    # using the population differences in matmob.diffs.pop for mu
    
    if (result=="black") games <- allgames[allgames$result == "0-1",     
                                              c("result", "pgn")]
    if (result=="white") games <- allgames[allgames$result == "1-0",      
                                              c("result", "pgn")]
    if (result=="draw")  games <- allgames[allgames$result == "1/2-1/2",  
                                              c("result", "pgn")]
    if (result=="all")   games <- allgames[,  c("result", "pgn")]
    
    if (num.games>nrow(games)) 
        return(print("Error: Sample size exceeds available games"))
   
    # Free up 1GB of memory used by dfgames
    mem_change(rm(allgames, inherits=TRUE))
    mem_change(rm(dfgames,  inherits=TRUE))
    
    set.seed(42) # make this code reproducible
    indices <- sample(nrow(games), num.games)
    games   <- as.data.frame(games[indices, ]) # games to analyze
    data.overview(games, num.games, result)    # summarize high-level stats of these games
    games   <- as.data.frame(games[ , "pgn"])  # keep only the pgn variable
    get.mat.mob.parallel(games, result, do.t, matmob.diffs.pop,
                         num.games, detectCores()) # I have 8 cores
}

# ==============================  t-test functions  ===========================

t.stat.Welch <- function(s1.mean, s2.mean, s1.sd, s2.sd, n1, n2){
    # return t-statistic for Welch's t-test
    # for comparing means of two populations with different variance
    # I'm using this to compare population means vis-a-vis the means
    # from subsets of the population where either black or white wins
    # s1.mean is mean of sample 1
    # s1.sd   is std dev of sample 1
    # n1      is sample size of sample 1
    # Similarly for sample 2
    mean.diff   <- s1.mean - s2.mean
    stdErr.diff <- sqrt(s1.sd^2/n1 + s2.sd^2/n2)
    mean.diff/stdErr.diff
}

deg.freedom <- function(s1.sd, s2.sd, n1, n2){
    # return degrees of freedom for Welch t-test using
    # Welch–Satterthwaite equation. 
    # s1.sd   is std dev of sample 1
    # n1      is sample size of sample 1
    # Similarly for sample 2
    s1.var.ratio <- s1.sd^2/n1
    s2.var.ratio <- s2.sd^2/n2
    numer        <- (s1.var.ratio + s2.var.ratio)^2
    denom        <- (s1.var.ratio^2 / (n1-1) + s2.var.ratio^2 / (n2-1))
    numer/denom
}

paired.t.test <- function(game.result, stats.matrix, matmob.diffs.pop) {   
    # I'm using this to determine if either black or white has a
    # statistically significant advantage in either material or mobility
    # in games that are won by either black or white, respectively
    # (as opposed to analyzing games without regard to winners, i.e. the
    # population or  games that result in a draw).
    # Each column of stats.matrix is a game
    # row 1 is mean white material for each game
    # row 2 is mean black material "
    # row 3 is mean white mobility "
    # row 4 is mean black mobility "
    # matmob.diffs.pop is sequence of values for mu from the difference
    # (white - black) of the previously computed point estimates of
    # mean material and mobility in the population 
    mat.diff.pop <- matmob.diffs.pop[1]
    mob.diff.pop <- matmob.diffs.pop[2]
    if (game.result=="white"){
        print("paired t-test of white - black material")
        print(t.test(stats.matrix[1,], stats.matrix[2, ], 
                     alternative="greater", paired=TRUE, mu = mat.diff.pop))
        
        print("paired t-test of white - black mobility")
        print(t.test(stats.matrix[3,], stats.matrix[4, ], 
                     alternative="greater", paired=TRUE, mu = mob.diff.pop))
    }
    if (game.result=="black"){
        print("paired t-test of black - white material")
        print(t.test(stats.matrix[2,], stats.matrix[1, ], 
                     alternative="greater", paired=TRUE, mu = -mat.diff.pop))
        
        print("paired t-test of black - white mobility")
        print(t.test(stats.matrix[4,], stats.matrix[3, ], 
                     alternative="greater", paired=TRUE, mu = -mob.diff.pop))
    }
}    

# samp.siz.b <- 1000
# samp.siz.p <- 5000
# sd.b       <- 4.410876
# sd.p       <- 4.354868
# B.mob_b    <- 32.336095
# B.mob_p    <- 30.839317
# df         <- deg.freedom(sd.b, sd.p, samp.siz.b, samp.siz.p)
# t.value    <- t.stat.Welch(B.mob_b, B.mob_p, sd.b, sd.p, samp.siz.b, samp.siz.p)
# cutoff     <- qt(0.95, df)
# t.value > cutoff

# ============================== Set Control Params ============================

piece.values <- data.frame(Piece=c("p", "n", "b", "r", "q"), 
                           Value=c( 1,   3,   3,   5,   9))
sample.size  <- 1000      # number of games to analyze from dfgames

result       <- "all"   # black, white, draw or all 

do.t.test    <- TRUE     # perform t.test or not on white vs. black stats

mat.diff.pop <- 0.020653 # diff in white - black material in population

mob.diff.pop <- 2.296723 # diff in white - black mobility in population

# ============================= Analyze Chess Games ============================

# system.time(mat.mob <- analyze.chess.games(dfgames, sample.size, 
#                                            result, do.t.test,
#                                            c(mat.diff.pop, mob.diff.pop)))
mat.mob <- analyze.chess.games(dfgames,   sample.size, result, 
                               do.t.test, c(mat.diff.pop, mob.diff.pop))

mat.mob[[1]][1,5] <- mat.mob[[1]][1,5] * sample.size

mat.mob[[1]]
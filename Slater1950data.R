# Elliot Slater's Data from 
# http://eliotslater.org/index.php/chess/147-statistics-for-the-chess-computer-and-the-factor-of-mobility
#
# Slater gives one reference to Shannon, which can be found here:
# http://vision.unipv.it/IA1/ProgrammingaComputerforPlayingChess.pdf
#
# Slater said that the linear model resulted in:
# 0.086 + 1.658 * Mobility
# but that is not per least squares fit, which is:
# 0.0369 + 1.6218 * Mobility
# Slater did not say if his fit was obtained by actually computing
# least squares or (more likely) by simply eyeballing the straight line.

# Mobility means from 78 random games
After.Move <- c(0, 5, 10, 15, 20, 25, 30, 35)
Winners    <- c(20, 34.2, 37.5, 39.7, 38.9, 39.6, 35.6, 31.7)
Losers     <- c(20, 33.9, 36, 35.2, 36.4, 31.9, 27.7, 23.2)
Difference <- Winners - Losers

plot(After.Move, Winners, pch=19, col="darkgreen", type="b", ylim=c(0, 40),
     main="Mobility Means of Winners (green) vs. Losers (red) in 78 Games
     with Difference in Blue, per E. Slater, 1950")
lines(After.Move, Losers, pch=19, col="red", type="b")
lines(After.Move, Difference, pch=19, col="blue", type="b")
#
#
Mobility <- c(.26, .19, .14, .09, .04, -.01, -.07, -.13, -.24)
Material <- c(.46, .35, .27, .17, .09, -.05, -.06, -.03, -.43)

mat.mob     <- data.frame(Mobility=Mobility, Material=Material)
mat.mob.fit <- lm(Material ~ Mobility, data=mat.mob)
plot(Mobility, Material, pch=19, col="blue", type="b",
     main="Means from 380 Games at 20th Move\nper E. Slater, 1950 with Regression Line")
abline(mat.mob.fit, col="red", lty=2)

summary(mat.mob.fit)


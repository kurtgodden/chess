# Elliot Slater's Data from 
# http://eliotslater.org/index.php/chess/147-statistics-for-the-chess-computer-and-the-factor-of-mobility
#
# Slater gives one reference to Shannon, which can be found here:
# http://vision.unipv.it/IA1/ProgrammingaComputerforPlayingChess.pdf
#
# Slater said that the linear model resulted in:
# 0.086 + 1.658 * MobilityAdvantage
# but that is not per least squares fit, which is:
# 0.0369 + 1.62182 * MobilityAdvantage
# Slater did not explain his calculation of the fit.

# Mobility means from 78 random games
After.Move <- c(0, 5, 10, 15, 20, 25, 30, 35)
Winners    <- c(20, 34.2, 37.5, 39.7, 38.9, 39.6, 35.6, 31.7)
Losers     <- c(20, 33.9, 36, 35.2, 36.4, 31.9, 27.7, 23.2)
Difference <- Winners - Losers

par(mfrow=c(1,1))
plot(After.Move, Winners, pch=19, col="darkgreen", type="b", ylim=c(0, 40),
     main="Mobility of Winners, Losers, and Difference\nin 78 Games, after E. Slater, 1950",
     xlab="After Move", ylab="Mean Mobiilty")
lines(After.Move, Losers, pch=19, col="red", type="b")
lines(After.Move, Difference, pch=19, col="blue", type="b")
legend("bottomright", col=c("darkgreen", "red", "blue"), pch=19, c("Winners", "Losers", "Difference"))
#
#
mob.adv        <- c(.26, .19, .14, .09, .04, -.01, -.07, -.13, -.24) # white - black mobility
fP             <- c(.46, .35, .27, .17, .09, -.05, -.06, -.03, -.43) # This is Shannon's function

fP.mob.adv     <- data.frame(mob.adv=mob.adv, fP=fP)
fP.mob.adv.fit <- lm(fP ~ mob.adv, data=fP.mob.adv)

plot(mob.adv, fP, pch=19, col="blue", type="b",
     main="Mean f(P) from 380 Games at 20th Move:
     Slater's Regression vs. Least Squares Regression",
     xlab="Advantage in Mobility (white - black)",
     ylab="Shannon's Evaluation Function f(P)")
abline(fP.mob.adv.fit, col="red", lty=2, lwd=2)   # This is R's least squares fit
slater.eq <- c(0.086, 1.658)                      # This is Slater's linear equation
abline(coef=slater.eq, col="black", lty=3, lwd=2)
legend("bottomright", fill=c("blue", "black", "red"),
       c("Mean f(P)", "Slater Reg.", "Least Sq. Reg."))


summary(fP.mob.adv.fit)


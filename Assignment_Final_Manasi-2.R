#Manasi Kulkarni
#Assignment1
#Setting the working directory
setwd("~/Desktop/R")

#Loading the Felix Dataset in R
a <- read.csv("FelixHernandez2015.csv")

#Analysing the dataset summary and top values
summary(a)
head(a)

#We see from the head command that Wins is a binary value. Hence finding the number of wins
sum(a$W) #The number of wins are 18

#Mean,median and more of strikeouts
mean(a$SO) #The answer is 6.1629
median(a$SO) #The answer is 6
Mode <- function(x)
{
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(a$SO) #The answer is 5

#Relationship plot between Innings Pitched and Strikeouts
plot(a$SO~a$IP, pch = 16, cex = 1.3, col = "blue", main = "PLOT OF INNINGS PITCHED vs STRIKE OUTS", xlab = "INNINGS PITCHED", ylab = "STRIKE OUTS")
#Drawing a line on this plot
model = lm(a$SO~a$IP)
abline(model)

#The relationship is increasing relationship for Innings pitched vs Strikeouts as seen from the trendline

#Relationship between Innings pitched and Walks
plot(a$BB~a$IP, pch = 16, cex = 1.3, col = "green", main = "PLOT OF INNINGS PITCHED vs WALKS", xlab = "INNINGS PITCHED", ylab = "BASE ON BALLS")
#Drawing a line on this plot
abline(lm(a$BB~a$IP))

#The relationship is a decreasing relationshio for Innings pitched vs Walks as seen from the trend line

#Correlation coefficients between innings pitched and strikeouts
cor(a$IP,a$SO) #The answer is 0.6816081
#This is a positive corelation coefficient which means that as Innings pitched increases, the strikeouts also increases. It aligns with the plot as well. 

#Correlation coefficients between innings pitched and walks
cor(a$IP,a$BB) #The answer is -0.2638496
#This is a negative corelation coefficient which means that as Innings pitched increases, the walks decreases. It aligns with the plot as well. 

#Mean and Variance
k <- by(a$BB,a$Month, mean)
plot(unique(k(a[,'Month'])))
l <- by(a$BB,a$Month, var)
plot(unique(l(a[,'Month'])))
#The pattern is not perfectly understandable in the sense that when I see specific mean points on the graph over Month, I see a steady increase in the start, a stagnant curve in the middle followed by a steep decline and rising slope. 
#This means that the month of August had lesser 'walks by month'. We would need to do a deeper analysis into this.
#Same applies for Variance as well


#Felix's wins on the home or road
by(a$W==1,a$away, sum)
#This gives a count of 11 for home and 7 for road. This means that Felix wins more at home

#Loaing the Randy Johnson data set
b <- read.csv("RandyJohnson1995.csv")
#Difference
sum_Felix_strikeouts <- sum(a$SO)
sum_Randy_strikeouts <- sum(b$SO)
difference <- sum_Felix_strikeouts - sum_Randy_strikeouts
difference
#This value is negative (-103). This means that Randy's strikeouts are more than Felix's strikeouts
#Yes! He outperforms.

#Question3
#For the Verbal Section
#Mean on the test=151
#Standard Deviation on the test=7
#Z-Score=(5/7)=0.714

mean =151
sd = 7
x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)
plot(x, hx, type="l", lty=2, xlab="Verbal GRE Scores", main="Verbal Scores")
lb=151
ub=158
i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 

#For the Quant section
#Mean on the test=153
#Standard Deviation on the test=7.67
#Z-Score=(4/7.67)=0.52

mean =153
sd = 7.67
x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)
plot(x, hx, type="l", lty=2, xlab="Quant GRE Scores", main="Quant Scores")
lb=153
ub=157
i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="blue") 

#Plotting the Z-Scores
curve(dnorm, from = -5, to=5)
abline(v=0.714, col="blue")
abline(v=0.52, col="red")
text(1.285714+1, 0.3, "Verbal: 0.714",col="blue") 
text(0.5215124-1.5, 0.1, "Quantitative: 0.52", col="red") 

#Finding percentiles
pnorm(0.714)
pnorm(0.52)






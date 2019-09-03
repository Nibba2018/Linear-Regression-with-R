
bonds = read.delim("bonds.txt", row.names = 1)
head(bonds)
tail(bonds)

str(bonds)

summary(bonds)

plot(bonds$CouponRate, bonds$BidPrice, main = "Bid Price vs Coupon Rate", xlab= "Coupon Rate", ylab="Bid Price")

bondsmod = lm(bonds$BidPrice ~ bonds$CouponRate)

plot(bonds$CouponRate, bonds$BidPrice, main = "Bid Price vs Coupon Rate", xlab= "Coupon Rate", ylab="Bid Price") + abline(bondsmod)

summary(bondsmod)

alpha = 0.05
n = 35
p = 1
qt(p = 1-(alpha/2), df = n-p-1)

3.0661 - (2.034515*0.3068)
3.0661 + (2.034515*0.3068)

plot(bondsmod$fitted.values, rstandard(bondsmod), main="Residual Plot", xlab="Predicted Values for Bid Price", ylab="Standardized Residuals") + abline(h=2, lty=2) + abline(h=-2, lty=2)

# Allows you to click on the points straight from the plotted graph in Rstudio
outliers = identify(bondsmod$fitted.values, rstandard(bondsmod))

outliers = which(as.matrix(rstandard(bondsmod)) > 2 | as.matrix(rstandard(bondsmod) < -2))
outliers = c(4, outliers) # Four was added as its very close to the +2 line
outliers

bonds_new = bonds[-outliers,]

bondsmod_new = lm(bonds_new$BidPrice ~ bonds_new$CouponRate)

summary(bondsmod)
summary(bondsmod_new)

plot(bonds_new$CouponRate, bonds_new$BidPrice, main = "Bid Price vs Coupon Rate without Outliers", xlab= "Coupon Rate", ylab="Bid Price") + abline(bondsmod_new)



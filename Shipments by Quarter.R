#3.1
#a
getwd()
setwd( "/Users/mirandamcwatters/Desktop/SCMA 854")
ship.df = read.csv("ApplianceShipments.csv")
summary(ship.df)
ship.df
ship.ts
ship.ts=ts(ship.df$Shipments, start = c(1985,1),end = c(1989,4), freq=4)
ship.lm = tslm(ship.ts ~ trend + I(trend^2))
plot(ship.ts, xlab= "Year", ylab="Shipments")
#b 
plot(ship.ts, xlab= "year", ylab="Shipments", ylim = c(3500,5000))
#It does look like there is a quarterly trend with shipments
#Quarters 2 and 3 have higher shipments then quarters 1 and 4
#c.
plot(ship.ts,xlab = "Year", ylab = "Shipment (in million $)", ylim = c(3500, 5000))
library(ggplot2)
par(oma = c(0,0,0,2))
xrange <-c(1,5)
yrange=range(ship.ts)
plot(xrange,  yrange,  main  =  "Shipments  by  Quarter",  type  =  "n",  xlab  =  "Year",  ylab  = "Shipments", bty = "l")
colors <-rainbow(4)
linetype <-c(1:4)
plotchar <-c(1:4)
for (i in 1:4) {
  current_quarter <- subset(ship.ts, cycle(ship.ts)==i)
  lines(current_quarter, type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i])
}  
legend(5.25 ,4800,   1:4,   cex=0.8,   col=colors,   pch=plotchar,   lty=linetype,   title="Quarter", xpd=NA)
##It looks like shipments in quarter 2 and 3 are larger than shipments 1 and 4
#d.
yearly <- aggregate(ship.ts, nfrequency=1, FUN=sum)
plot(yearly, bty="l")
#3.2
mowers.df=read.csv("RidingMowers.csv")
summary(mowers.df)
mower.plot= ggplot(mowers.df, aes(x=Income,y=Lot_Size,color=Ownership)) + geom_point()
print(mower.plot + labs(y="Lot Size (1000 ft^2)", x = "Income ($1000s)"))

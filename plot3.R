rm(list=ls())
require <- function(x) {if (!base::require(x, character.only = TRUE)) {install.packages(x, dep = TRUE) ; base::require(x, character.only = TRUE)}}#overwrites 'require' function to install missing packages

require("ggplot2")

##### Loads the data-files. Might take a while, be patient! #####
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

##### Of the four types of sources indicated by the #####
# type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

#Creates a dataframe with all the observations from the original dataframe from Baltimore City
Baltimore.city2 <- NEI[which(NEI$fips == 24510), ]

#Calculates the sums of emissions for each year from each emission type in Baltimore City
sum.baltimore <- as.data.frame(with(Baltimore.city2, tapply(Emissions, list(year,type), sum, na.rm=TRUE)))

#Adds points and lines for "Non-road" emissions
p <- ggplot(data=sum.baltimore, aes(x=c(1999,2002,2005,2008), y=`NON-ROAD`)) +
        geom_line(aes(color="Non-Road"))+
        geom_point(aes(color="Non-Road"))+
        theme_bw()+
        ggtitle("Emissions in Baltimore City")+
        labs(y= "Year", x = "PM2.5 emissions in tonnes")

#Adds points and lines for "Nonpoint" emissions
pp <- p + geom_point(data=sum.baltimore, aes(x=c(1999,2002,2005,2008), y=`NONPOINT`, color="Nonpoint")) +geom_line(data=sum.baltimore, aes(x=c(1999,2002,2005,2008), y=`NONPOINT`, color="Nonpoint"))

#Adds points and lines for "On-road" emissions
ppp <- pp + geom_point(data=sum.baltimore, aes(x=c(1999,2002,2005,2008), y=`ON-ROAD`, color="On-road")) +geom_line(data=sum.baltimore, aes(x=c(1999,2002,2005,2008), y=`ON-ROAD`, color="On-road"))

#Adds points and lines for "Point" emissions
pppp <- ppp + geom_point(data=sum.baltimore, aes(x=c(1999,2002,2005,2008), y=`POINT`, color="Point")) +geom_line(data=sum.baltimore, aes(x=c(1999,2002,2005,2008), y=`POINT`, color="Point"))

#Print the image file
png(filename = "plot3.png")
pppp
dev.off()

plot(x = c(1999,2002,2005,2008), y = sum.baltimore[2,], col=1, pch=20, type="n", ylim = c(0,2200), main = "Emissions in Baltimore City", xlab = "Year", ylab = "PM2.5 in tonnes")



#Adds points and lines for "Non-road" emissions
points(x = c(1999,2002,2005,2008), y = sum.baltimore[1,], col=1, pch=20)
lines(x = c(1999,2002,2005,2008), y = sum.baltimore[1,], col=1)

#Adds points and lines for "Nonpoint" emissions
points(x = c(1999,2002,2005,2008), y = sum.baltimore[2,], col=2, pch=20)
lines(x = c(1999,2002,2005,2008), y = sum.baltimore[2,], col=2)

#Adds points and lines for "On-road" emissions
points(x = c(1999,2002,2005,2008), y = sum.baltimore[3,], col=3, pch=20)
lines(x = c(1999,2002,2005,2008), y = sum.baltimore[3,], col=3)

#Adds points and lines for "Point" emissions
points(x = c(1999,2002,2005,2008), y = sum.baltimore[4,], col=4, pch=20)
lines(x = c(1999,2002,2005,2008), y = sum.baltimore[4,], col=4)

#Adds a legend
legend("left", legend=c("Non-road", "Nonpoint", "On-road", "Point"),
       col=c("black", "red", "green", "blue"), lty=1, cex=0.8,
       box.lty=1)


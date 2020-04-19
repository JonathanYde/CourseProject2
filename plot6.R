rm(list=ls())
require <- function(x) {if (!base::require(x, character.only = TRUE)) {install.packages(x, dep = TRUE) ; base::require(x, character.only = TRUE)}}#overwrites 'require' function to install missing packages


##### Loads the data-files. Might take a while, be patient! #####
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

##### Compare emissions from motor vehicle sources in Baltimore City with emissions #####
# from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

#Stores a dataframe with all entries with "vehicle" in the short name
vehicles <- SCC[which(grepl("Vehicle",x = SCC$Short.Name)),]

#Stores a dataframe of PM2.5 emissions where SCC matches the SCCs from "vehicles"
vehicle.emissions <- NEI[which(NEI$SCC %in% vehicles$SCC),]

#Stores a dataframe of PM2.5 emissions from vehicles in Baltimore City
Baltimore.city.vehicles <- vehicle.emissions[which(vehicle.emissions$fips == 24510), ]

#Calculates the sums of emissions for each year from vehicles in Baltimore City
sum.baltimore.vehicles <- with(Baltimore.city.vehicles, tapply(Emissions, as.factor(year), sum, na.rm=TRUE))

#Stores a dataframe of PM2.5 emissions from vehicles in Los Angeles County
LA.vehicles <- vehicle.emissions[which(vehicle.emissions$fips == "06037"), ]

#Calculates the sums of emissions for each year from vehicles in Los Angeles County
sum.LA.vehicles <- with(LA.vehicles, tapply(Emissions, as.factor(year), sum, na.rm=TRUE))

#Creates a plot with lines for vehicle emissions in Los Angeles County
png(filename = "plot6.png")
plot(x = c(1999,2002,2005,2008), y = sum.LA.vehicles, col=1, pch=20, main = "Vehicle missions in LA and Baltimore City", xlab= "Year", ylab = "PM2.5 in tonnes", ylim=c(0,1500))
lines(x = c(1999,2002,2005,2008), y = sum.LA.vehicles, col=1)

#Adds points and lines for vehicle emissions in Baltimore City
points(x = c(1999,2002,2005,2008), y = sum.baltimore.vehicles, col=2, pch=20)
lines(x = c(1999,2002,2005,2008), y = sum.baltimore.vehicles, col=2)

#Adds a legend
legend("left", legend=c("LA","Baltimore"),
       col=c("black", "red"), lty=1, cex=0.8,
       box.lty=1)
dev.off()

rm(list=ls())
require <- function(x) {if (!base::require(x, character.only = TRUE)) {install.packages(x, dep = TRUE) ; base::require(x, character.only = TRUE)}}#overwrites 'require' function to install missing packages


##### Loads the data-files. Might take a while, be patient! #####
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

##### How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? #####

#Stores a dataframe with all entries with "vehicle" in the short name
vehicles <- SCC[which(grepl("Vehicle",x = SCC$Short.Name)),]

#Stores a dataframe of PM2.5 emissions where SCC matches the SCCs from "vehicles"
vehicle.emissions <- NEI[which(NEI$SCC %in% vehicles$SCC),]

#Stores a dataframe of PM2.5 emissions from vehicles in Baltimore City
Baltimore.city.vehicles <- vehicle.emissions[which(vehicle.emissions$fips == 24510), ]

#Calculates the sums of emissions for each year from vehicles in Baltimore City
sum.baltimore.vehicles <- with(Baltimore.city.vehicles, tapply(Emissions, as.factor(year), sum, na.rm=TRUE))

#Creates a plot with lines for vehicle emissions in Baltimore City
png(filename = "plot5.png")
plot(x = c(1999,2002,2005,2008), y = sum.baltimore.vehicles, col=1, pch=20, main = "Vehicle missions in Baltimore City", xlab= "Year", ylab = "PM2.5 in tonnes")
lines(x = c(1999,2002,2005,2008), y = sum.baltimore.vehicles, col=1)
dev.off()

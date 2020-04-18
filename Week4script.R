rm(list=ls())
require <- function(x) {if (!base::require(x, character.only = TRUE)) {install.packages(x, dep = TRUE) ; base::require(x, character.only = TRUE)}}#overwrites 'require' function to install missing packages


##### Loads the data-files. Might take a while, be patient! #####
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

##### Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? #####
# Using the base plotting system, make a plot showing the total PM2.5 emission
# from all sources for each of the years 1999, 2002, 2005, and 2008.

#calculates the mean of values from each location for each year
mean.from.each.fip <- with(NEI, tapply(Emissions, list(fips,year), mean, na.rm=TRUE))

#creates a vector of which locations has values for each year
completes <- complete.cases(mean.from.each.fip)

#creates a matrix of means from each fip for each year, but only the locations with measures from all years
complete.mean.from.each.fip <- mean.from.each.fip[completes,]

#calculates the total emissions from each year by adding the means from each fip together
total.emissions <- colSums(complete.mean.from.each.fip)

#plots the total emissions from each year as a barchart
barplot(total.emissions, main = "Total PM2.5 emissions", ylab = "PM2.5 in tonnes")

boxplot(log10(complete.mean.from.each.fip))

##### Have total emissions from PM2.5 decreased in the Baltimore City, Maryland #####
# (fips == "24510") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.

#Creates a matrix with all the observations from Baltimore City
Baltimore.city <- complete.mean.from.each.fip["24510",]

#Plots the PM2.5 emissions in Baltimore City over the years
barplot(Baltimore.city, main = "Total PM2.5 emissions", ylab = "PM2.5 in tonnes")

##### Of the four types of sources indicated by the #####
# type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

#Creates a dataframe with all the observations from the original dataframe from Baltimore City
Baltimore.city2 <- NEI[which(NEI$fips == 24510), ]

#Calculates the sums of emissions for each year from each emission type in Baltimore City
sum.baltimore <- with(Baltimore.city2, tapply(Emissions, list(type,year), sum, na.rm=TRUE))

#Prepares to create a plot with the different emission types in Baltimore City
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


##### Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008? #####

#Stores a dataframe with all entries with "coal" in the short name
coal <- SCC[which(grepl("Coal",x = SCC$Short.Name)),]

#Stores a datafram with all entries with "combustion" in SCC.Level.One AND "coal" in the short name
coal.comb <- coal[which(grepl("Combustion", x = coal$SCC.Level.One)),]

head(coal.comb$SCC)

#Stores a dataframe of PM2.5 emissions where SCC matches the SCCs from "coal" and "combustion"
coal.emissions <- NEI[which(NEI$SCC %in% coal.comb$SCC),]

plot(as.factor(coal.emissions$year), coal.emissions$Emissions)

mean.from.each.fip2 <- with(coal.emissions, tapply(Emissions, list(fips,year), mean, na.rm=TRUE))

completes2 <- complete.cases(mean.from.each.fip2)

complete.mean.from.each.fip2 <- mean.from.each.fip2[completes2,]

total.emissions2 <- colSums(complete.mean.from.each.fip2)

barplot(total.emissions2, main = "PM2.5 emissions from coal combustion", ylab = "PM2.5 in tonnes")

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
plot(x = c(1999,2002,2005,2008), y = sum.baltimore.vehicles, col=1, pch=20, main = "Vehicle missions in Baltimore City", xlab= "Year", ylab = "PM2.5 in tonnes")
lines(x = c(1999,2002,2005,2008), y = sum.baltimore.vehicles, col=1)

##### Compare emissions from motor vehicle sources in Baltimore City with emissions #####
# from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

#Stores a dataframe of PM2.5 emissions from vehicles in Los Angeles County
LA.vehicles <- vehicle.emissions[which(vehicle.emissions$fips == "06037"), ]

#Calculates the sums of emissions for each year from vehicles in Los Angeles County
sum.LA.vehicles <- with(LA.vehicles, tapply(Emissions, as.factor(year), sum, na.rm=TRUE))

#Creates a plot with lines for vehicle emissions in Los Angeles County
plot(x = c(1999,2002,2005,2008), y = sum.LA.vehicles, col=1, pch=20, main = "Vehicle missions in LA and Baltimore City", xlab= "Year", ylab = "PM2.5 in tonnes", ylim=c(0,1500))
lines(x = c(1999,2002,2005,2008), y = sum.LA.vehicles, col=1)

#Adds points and lines for vehicle emissions in Baltimore City
points(x = c(1999,2002,2005,2008), y = sum.baltimore.vehicles, col=2, pch=20)
lines(x = c(1999,2002,2005,2008), y = sum.baltimore.vehicles, col=2)

#Adds a legend
legend("left", legend=c("LA","Baltimore"),
       col=c("black", "red"), lty=1, cex=0.8,
       box.lty=1)

#Creates a plot with lines for vehicle emissions in Los Angeles County
plot(x = c(1999,2002,2005,2008), y = log10(sum.LA.vehicles), col=1, pch=20, main = "Vehicle missions in LA and Baltimore City", xlab= "Year", ylab = "log10 (PM2.5 in tonnes)", ylim=c(1.3,3.2))
lines(x = c(1999,2002,2005,2008), y = log10(sum.LA.vehicles), col=1)

#Adds points and lines for vehicle emissions in Baltimore City
points(x = c(1999,2002,2005,2008), y = log10(sum.baltimore.vehicles), col=2, pch=20)
lines(x = c(1999,2002,2005,2008), y = log10(sum.baltimore.vehicles), col=2)

#Adds a legend
legend("right", legend=c("LA","Baltimore"),
       col=c("black", "red"), lty=1, cex=0.8,
       box.lty=1)

##### For each plot you should #####

# Construct the plot and save it to a PNG file.
# Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot, i.e. code in plot1.R constructs the plot1.png plot.
#Your code file should include code for reading the data so that the plot can be fully reproduced.
#You must also include the code that creates the PNG file.
#Only include the code for a single plot (i.e. plot1.R should only include code for producing plot1.png)
# Upload the PNG file on the Assignment submission page
# Copy and paste the R code from the corresponding R file into the text box at the appropriate point in the peer assessment.
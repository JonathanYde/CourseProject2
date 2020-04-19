rm(list=ls())
require <- function(x) {if (!base::require(x, character.only = TRUE)) {install.packages(x, dep = TRUE) ; base::require(x, character.only = TRUE)}}#overwrites 'require' function to install missing packages

##### Loads the data-files. Might take a while, be patient! #####
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

##### Have total emissions from PM2.5 decreased in the Baltimore City, Maryland #####
# (fips == "24510") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.

#calculates the mean of values from each location for each year
mean.from.each.fip <- with(NEI, tapply(Emissions, list(fips,year), mean, na.rm=TRUE))

#creates a vector of which locations has values for each year
completes <- complete.cases(mean.from.each.fip)

#creates a matrix of means from each fip for each year, but only the locations with measures from all years
complete.mean.from.each.fip <- mean.from.each.fip[completes,]

#Creates a matrix with all the observations from Baltimore City
Baltimore.city <- complete.mean.from.each.fip["24510",]

#Plots the PM2.5 emissions in Baltimore City over the years
png(filename = "plot2.png")
barplot(Baltimore.city, main = "PM2.5 emissions in Baltimore City", ylab = "PM2.5 in tonnes")
dev.off()

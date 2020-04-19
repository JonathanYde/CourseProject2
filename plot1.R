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
png(filename="plot1.png")
barplot(total.emissions, main = "Total PM2.5 emissions", ylab = "PM2.5 in tonnes")
dev.off()


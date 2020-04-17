rm(list=ls())
require <- function(x) {if (!base::require(x, character.only = TRUE)) {install.packages(x, dep = TRUE) ; base::require(x, character.only = TRUE)}}#overwrites 'require' function to install missing packages


##### Loads the data-files. Might take a while, be patient! #####
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

##### Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? #####
# Using the base plotting system, make a plot showing the total PM2.5 emission
# from all sources for each of the years 1999, 2002, 2005, and 2008.


##### Have total emissions from PM2.5 decreased in the Baltimore City, Maryland #####
# (fips == "24510"\color{red}{\verb|fips == "24510"|}fips=="24510") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.


##### Of the four types of sources indicated by the #####
# type\color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


##### Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008? #####


##### How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? #####


##### Compare emissions from motor vehicle sources in Baltimore City with emissions #####
# from motor vehicle sources in Los Angeles County, California (fips == "06037"\color{red}{\verb|fips == "06037"|}fips=="06037").
# Which city has seen greater changes over time in motor vehicle emissions?


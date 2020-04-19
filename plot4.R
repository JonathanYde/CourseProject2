rm(list=ls())
require <- function(x) {if (!base::require(x, character.only = TRUE)) {install.packages(x, dep = TRUE) ; base::require(x, character.only = TRUE)}}#overwrites 'require' function to install missing packages


##### Loads the data-files. Might take a while, be patient! #####
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

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

png(filename="plot4.png")
barplot(total.emissions2, main = "PM2.5 emissions from coal combustion", ylab = "PM2.5 in tonnes")
dev.off()

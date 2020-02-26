##################
# Load libraries #
##################

library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)

##############
# Load files #
##############

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

if (!file.exists("data")) {
  dir.create("data")
  download.file(url, "data/data.zip")
  unzip("data/data.zip",
        exdir = "data")
}

# Create data tables
nei <- readRDS("data/summarySCC_PM25.rds")
scc <- readRDS("data/Source_Classification_Code.rds")
nei <- tbl_df(nei)
scc <- tbl_df(scc)


##############
# Question 6 #
##############

# Find all vehicle SCCs and turn them into a character vector
vehicles <- grep("vehicle", scc$Short.Name, ignore.case=TRUE)
vehicles2 <- as.character(unlist(c(scc[vehicles, 1])))

# Filter data by Baltimore and Los Angeles
balt_la <- filter(nei, fips %in% c("06037", "24510"))

# Filter Baltimore-LA data by the motor vehicle SCCs
plot6 <- filter(balt_la, SCC %in% vehicles2)

# Convert "fips" into a factor for faceting
plot6$fips <- as.factor(plot6$fips)

# Relabel fips levels
levels(plot6$fips) <- c("Los Angeles", "Baltimore")

# Group data by type and year, and then sum emissions
plot6 <- plot6 %>%
  group_by(fips, year) %>%
  summarize(Emissions = sum(Emissions))

# Open PNG device
png(filename = "plot6.png")

# Create plot, faceting by fips into columns
p6 <- ggplot(plot6, aes(x = year, y = Emissions)) +
  geom_line() +
  ggtitle("Total Emissions") +
  facet_grid(cols = vars(fips))

p6

# Close PNG device
dev.off()
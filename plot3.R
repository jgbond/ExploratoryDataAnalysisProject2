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
# Question 3 #
##############

# Filter data by Baltimore
baltimore <- filter(nei, fips == "24510")

# Convert "type" into a factor for faceting
baltimore$type <- as.factor(baltimore$type)

# Group data by type and year, and then sum emissions
plot3 <- baltimore %>%
  group_by(type, year) %>%
  summarize(Emissions = sum(Emissions))

# Open PNG device
png(filename = "plot3.png")

# Create plot, faceting by type into columns
p3 <- ggplot(plot3, aes(x = year, y = Emissions)) +
  geom_line() +
  ggtitle("Baltimore Emissions") +
  facet_grid(cols = vars(type))

p3

# Close PNG device
dev.off()
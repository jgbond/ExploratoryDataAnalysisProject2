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

###############
# Question #1 #
###############

# Group data by year and find total emissions by year
plot1 <- nei %>%
  group_by(year) %>%
  summarize(Emissions = sum(Emissions))

# Open PNG device
png(filename = "plot1.png")

# Plot grouped data
plot(plot1$year,plot1$Emissions,
     type="n",
     xlab="Year",
     ylab="Emissions",
     main="Total PM2.5 Emissions")

# Add lines
lines(plot1$year,plot1$Emissions)

# Close PNG device
dev.off()
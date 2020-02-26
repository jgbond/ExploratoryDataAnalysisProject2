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
# Question 5 #
##############

# Find all vehicle SCCs and turn them into a character vector
vehicles <- grep("vehicle", scc$Short.Name, ignore.case=TRUE)
vehicles2 <- as.character(unlist(c(scc[vehicles, 1])))

# Filter data by Baltimore
baltimore <- filter(nei, fips == "24510")

# Filter Baltimore data by the motor vehicle SCCs
plot5 <- filter(baltimore, SCC %in% vehicles2)

# Group data by year and find total emissions by year
plot5 <- plot5 %>%
  group_by(year) %>%
  summarize(Emissions = sum(Emissions))

# Open PNG device
png(filename = "plot5.png")

# Plot grouped data
plot(plot5$year, plot5$Emissions,
     type="n",
     xlab="Year",
     ylab="Emissions",
     main="Baltimore Total Emissions from Motor Vehicles")

# Add lines
lines(plot5$year,plot5$Emissions)

# Close PNG device
dev.off()
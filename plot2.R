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
# Question 2 #
##############

# Filter data by Baltimore
baltimore <- filter(nei, fips == "24510")

# Group data by year and find total emissions by year
plot2 <- baltimore %>%
  group_by(year) %>%
  summarize(Emissions = sum(Emissions))

# Open PNG device
png(filename = "plot2.png")

# Plot grouped data
plot(plot2$year,plot2$Emissions,
     type="n",
     xlab="Year",
     ylab="Emissions",
     main="Baltimore Total PM2.5 Emissions")

# Add lines
lines(plot2$year,plot2$Emissions)

# Close PNG device
dev.off()
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
# Question 4 #
##############

# Find all coal and combustion SCCs and turn them into a character vector
coal <- grep("coal", scc$Short.Name, ignore.case = TRUE)
comb <- grep("comb", scc$Short.Name, ignore.case = TRUE)
intersect <- intersect(coal, comb)
coalcomb <- as.character(unlist(c(scc[intersect, 1])))

# Filter by the coal and combustion SCCs
plot4 <- filter(nei, SCC %in% coalcomb)

# Group data by year and find total emissions by year
plot4 <- plot4 %>%
  group_by(year) %>%
  summarize(Emissions = sum(Emissions))

# Open PNG device
png(filename = "plot4.png")

# Plot grouped data
plot(plot4$year,plot4$Emissions,
     type="n",
     xlab="Year",
     ylab="Emissions",
     main="Total Emissions from Coal Combustion")

# Add lines
lines(plot4$year,plot4$Emissions)

# Close PNG device
dev.off()
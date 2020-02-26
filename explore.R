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

# Plot grouped data
plot(plot1$year,plot1$Emissions,
     type="n",
     xlab="Year",
     ylab="Emissions",
     main="Total PM2.5 Emissions")

# Add lines
lines(plot1$year,plot1$Emissions)

##############
# Question 2 #
##############

# Filter data by Baltimore
baltimore <- filter(nei, fips == "24510")

# Group data by year and find total emissions by year
plot2 <- baltimore %>%
  group_by(year) %>%
  summarize(Emissions = sum(Emissions))

# Plot grouped data
plot(plot2$year,plot2$Emissions,
     type="n",
     xlab="Year",
     ylab="Emissions",
     main="Baltimore Total PM2.5 Emissions")

# Add lines
lines(plot2$year,plot2$Emissions)


##############
# Question 3 #
##############

# Convert "type" into a factor for faceting
baltimore$type <- as.factor(baltimore$type)

# Group data by type and year, and then sum emissions
plot3 <- baltimore %>%
  group_by(type, year) %>%
  summarize(Emissions = sum(Emissions))

# Create plot, faceting by type into columns
p3 <- ggplot(plot3, aes(x = year, y = Emissions)) +
  geom_line() +
  ggtitle("Baltimore Emissions") +
  facet_grid(cols = vars(type))

p3


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

# Plot grouped data
plot(plot4$year,plot4$Emissions,
     type="n",
     xlab="Year",
     ylab="Emissions",
     main="Total Emissions from Coal Combustion")

# Add lines
lines(plot4$year,plot4$Emissions)


##############
# Question 5 #
##############

# Find all vehicle SCCs and turn them into a character vector
vehicles <- grep("vehicle", scc$Short.Name, ignore.case=TRUE)
vehicles2 <- as.character(unlist(c(scc[vehicles, 1])))

# Filter Baltimore data by the motor vehicle SCCs
plot5 <- filter(baltimore, SCC %in% vehicles2)

# Group data by year and find total emissions by year
plot5 <- plot5 %>%
  group_by(year) %>%
  summarize(Emissions = sum(Emissions))

# Plot grouped data
plot(plot5$year, plot5$Emissions,
     type="n",
     xlab="Year",
     ylab="Emissions",
     main="Baltimore Total Emissions from Motor Vehicles")

# Add lines
lines(plot5$year,plot5$Emissions)



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

# Create plot, faceting by fips into columns
p6 <- ggplot(plot6, aes(x = year, y = Emissions)) +
  geom_line() +
  ggtitle("Total Emissions") +
  facet_grid(cols = vars(fips))

p6
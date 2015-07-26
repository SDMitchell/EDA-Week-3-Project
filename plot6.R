library(reshape2)
library(ggplot2)

xresDefault <- 480
yresDefault <- 480

readDataIntoEnvironment <- function() {
	NEI <<- readRDS("summarySCC_PM25.rds")
	SCC <<- readRDS("Source_Classification_Code.rds")
}

##
## Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in
## Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
##

constructPlot6 <- function(xres=xresDefault, yres=yresDefault) {
	# Just select all Data.Category where the value is "Onroad" from the SCC set
	sccIdsForOnRoad <- SCC[SCC$Data.Category == "Onroad", ]
	sccIdsForOnRoad <- sccIdsForOnRoad$SCC

	# Subset on the OnRoad sensors
	NEIOnRoad <- NEI[NEI$SCC %in% sccIdsForOnRoad, ]

	# Piece together the things we care about
	NEIByYear <- as.data.frame(cbind(NEIOnRoad$year, NEIOnRoad$fips, NEIOnRoad$Emissions))
	colnames(NEIByYear) <- c("year", "fips", "Emissions")

	# Grab some year labels while we're here
	years <- sort(unique(as.character(NEIByYear$year)))
	NEIByYear$year <- as.factor(NEIByYear$year)

	# Compute the sums per year that we need
	molten <- melt(NEIByYear, id.vars=c(1,2))
	molten$value <- as.numeric(molten$value)
	dataOnRoad <- dcast(molten, year+fips~variable, sum)

	# Filter out our city data sets and create a new year-over-year percent change column in each
	dataOnRoadComparisonBaltimore <- dataOnRoad[dataOnRoad$fips == "24510", ]
	dataOnRoadComparisonBaltimore <- transform(dataOnRoadComparisonBaltimore, YearOverYearChange=c(0.0,100.0*(Emissions[-1]/Emissions[-nrow(dataOnRoadComparisonBaltimore)]-1)))
	dataOnRoadComparisonLA <-  dataOnRoad[dataOnRoad$fips == "06037", ]
	dataOnRoadComparisonLA <- transform(dataOnRoadComparisonLA, YearOverYearChange=c(0.0,100.0*(Emissions[-1]/Emissions[-nrow(dataOnRoadComparisonLA)]-1)))

	# Combine them together into one data.frame we can plot
	dataOnRoadComparison <- rbind(dataOnRoadComparisonBaltimore, dataOnRoadComparisonLA)

	# Construct our plot object. Multiply year-over-year by -1 to turn it into an "improvement" statistic instead of a negative delta
	g <- ggplot(dataOnRoadComparison, aes(as.numeric(as.character(year)), -1.0 * YearOverYearChange, group=fips))
	g <- g + geom_area(aes(group=fips, fill=fips), position = "identity", alpha=0.5, colour="black") +
		scale_x_continuous(limits = c(1999, 2008), expand=c(0,0)) +
		scale_fill_manual(values=c("#999999", "#E69F00"),
						  name="City",
						  breaks=c("24510", "06037"),
						  labels=c("Baltimore", "Los Angeles")) +
		labs(x = "Year", y = "Year-Over-Year Improvement (in percent)", title="Year-Over-Year Motor Vehicle Emission Improvements") +
		theme(legend.background = element_rect(fill = "#FFFAF0", colour = "#000000"), panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), plot.background = element_rect(fill = "#FFFAF0", colour = "#000000"), panel.grid.major = element_line(colour="#FFDEAD"))

	# Print it out
	png(filename="plot6.png", width=xres, height=yres, type="cairo")
	print(g)
	dev.off()
}

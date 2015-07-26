library(reshape2)
library(ggplot2)

xresDefault <- 480
yresDefault <- 480

readDataIntoEnvironment <- function() {
	NEI <<- readRDS("summarySCC_PM25.rds")
	SCC <<- readRDS("Source_Classification_Code.rds")
}

## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

constructPlot5 <- function(xres=xresDefault, yres=yresDefault) {
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

	dataOnRoadBaltimoreCity <- dataOnRoad[dataOnRoad$fips == "24510", ]

	# Construct our plot object
	g <- ggplot(dataOnRoadBaltimoreCity, aes(as.numeric(years), Emissions, group=1))
	g <- g + geom_area(fill="#FFFAFA", colour="#000000") +
		scale_x_continuous(limits = c(1999, 2008), expand=c(0,0)) +
		scale_y_continuous(limits = c(0, 500), expand=c(0,0)) +
		labs(x = "Year", y = "Total Emissions (in tons)", title="Baltimore City Emissions Motor Vehicles") +
		theme(panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), plot.background = element_rect(fill = "#FFFAF0", colour = "#000000"), panel.grid.major = element_line(colour="#FFDEAD"))

	# Print it out
	png(filename="plot5.png", width=xres, height=yres, type="cairo")
	print(g)
	dev.off()
}

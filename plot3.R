library(reshape2)
library(ggplot2)

xresDefault <- 480
yresDefault <- 480

readDataIntoEnvironment <- function() {
	NEI <<- readRDS("summarySCC_PM25.rds")
	SCC <<- readRDS("Source_Classification_Code.rds")
}

##
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources
## have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use
## the ggplot2 plotting system to make a plot answer this question.
##

constructPlot3 <- function(xres=xresDefault, yres=yresDefault) {
	# Piece together the two things we care about
	NEIByYear <- as.data.frame(cbind(NEI$year, NEI$fips, NEI$type, NEI$Emissions))
	colnames(NEIByYear) <- c("year", "fips", "type", "Emissions")

	# Grab some year labels while we're here
	years <- sort(unique(as.character(NEIByYear$year)))
	NEIByYear$year <- as.factor(NEIByYear$year)

	# Compute the sums per year that we need
	molten <- melt(NEIByYear, id.vars=c(1,2,3))
	molten$value <- as.numeric(molten$value)
	frozen <- dcast(molten, year+fips+type~variable, sum)

	# Just select Baltimore City
	dataBaltimoreCity <- frozen[frozen$fips==24510, ]

	# Construct our plot object
	g <- ggplot(dataBaltimoreCity, aes(year, Emissions))
	g <- g + geom_line(aes(group=type)) +
		facet_wrap(~type, ncol=2) +
		labs(x = "Year", y = "Total Emissions (in tons)", title="Baltimore City Yearly Emissions By Source") +
		theme(panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), plot.background = element_rect(fill = "#FFFAF0", colour = "#000000"), panel.grid.major = element_line(colour="#FFDEAD"))

	# Print it out
	png(filename="plot3.png", width=xres, height=yres, type="cairo")
	print(g)
	dev.off()
}

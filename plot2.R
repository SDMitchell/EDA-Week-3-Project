library(reshape2)

xresDefault <- 480
yresDefault <- 480

readDataIntoEnvironment <- function() {
	NEI <<- readRDS("summarySCC_PM25.rds")
	SCC <<- readRDS("Source_Classification_Code.rds")
}

##
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
## from 1999 to 2008? Use the base plotting system to make a plot answering this question.
##

constructPlot2 <- function(xres=xresDefault, yres=yresDefault) {
	# Piece together the three things we care about
	NEIByYear <- as.data.frame(cbind(NEI$year, NEI$fips, NEI$Emissions))
	colnames(NEIByYear) <- c("year", "fips", "Emissions")

	# Grab some year labels while we're here
	years <- sort(unique(as.character(NEIByYear$year)))
	NEIByYear$year <- as.factor(NEIByYear$year)

	# Compute the sums per year that we need
	molten <- melt(NEIByYear, id.vars=c(1,2))
	molten$value <- as.numeric(molten$value)
	frozen <- dcast(molten, year+fips~variable, sum)
	dataBaltimoreCity <- frozen[frozen$fips==24510, ]

	png(filename="plot2.png", width=xres, height=yres, type="cairo")

	# Floral White background colour (pure white is just ugly and hard on the eyes)
	par(bg="#FFFAF0")

	# Plot in kilotons, to get rid of the ugly exponents. Also set the y-axis wider so that the difference year-over-year isn't quite so dishonest looking
	plot(y=as.vector(dataBaltimoreCity$Emissions)/1000, ylim=c(0,5), x=years, xlab="Year", ylab="Total Emissions (in kilotons)", type="l", main="Baltimore City PM2.5 Emissions By Year")
	polygon(c(1999, years, 2008), c(0, as.vector(dataBaltimoreCity$Emissions)/1000, 0),  col = "#FFFAFA")
	dev.off()
}

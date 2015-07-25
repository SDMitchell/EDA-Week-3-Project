library(reshape2)

xresDefault <- 480
yresDefault <- 480

readDataIntoEnvironment <- function() {
	NEI <<- readRDS("summarySCC_PM25.rds")
	SCC <<- readRDS("Source_Classification_Code.rds")
}

##
## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
## Using the base plotting system, make a plot showing the total PM2.5 emission from all
## sources for each of the years 1999, 2002, 2005, and 2008.
##

## Note: SCC is useless for the first question, as it is just a year-by-year comparison
constructPlot1 <- function(xres=xresDefault, yres=yresDefault) {
	# Piece together the two things we care about
	NEIByYear <- as.data.frame(cbind(NEI$year, NEI$Emissions))
	colnames(NEIByYear) <- c("year", "Emissions")

	# Grab some year labels while we're here
	years <- sort(unique(as.character(NEIByYear$year)))
	NEIByYear$year <- as.factor(NEIByYear$year)

	# Compute the sums per year that we need
	molten <- melt(NEIByYear, id.vars=c(1))
	sums <- tapply(molten$value, molten$year, sum)

	png(filename="plot1.png", width=xres, height=yres, type="cairo")

	# Floral White background colour (pure white is just ugly and hard on the eyes)
	par(bg="#FFFAF0")

	# Plot in Megatons, to get rid of the ugly exponents. Also set the y-axis wider so that the difference year-over-year isn't quite so dishonest looking
	plot(y=as.vector(sums)/1000000, ylim=c(0,10), x=years, xlab="Year", ylab="Total Emissions (in megatons)", type="l", main="Total PM2.5 Emissions By Year")
	polygon(c(1999, years, 2008), c(0, as.vector(sums)/1000000, 0),  col = "#FFFAFA")

	dev.off()
}


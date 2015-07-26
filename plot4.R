library(reshape2)
library(ggplot2)

xresDefault <- 480
yresDefault <- 480

readDataIntoEnvironment <- function() {
	NEI <<- readRDS("summarySCC_PM25.rds")
	SCC <<- readRDS("Source_Classification_Code.rds")
}

## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?


constructPlot4 <- function(xres=xresDefault, yres=yresDefault) {
	# Just select any extractive industry sector (EI.Sector) containing the word [Cc]oal from the SCC set
	sccIdsForCoal <- SCC[grep("[cC]oal", SCC$EI.Sector), ]
	sccIdsForCoal <- sccIdsForCoal$SCC

	# Subset on the Coal sectors
	NEICoal <- NEI[NEI$SCC %in% sccIdsForCoal, ]

	# Piece together the things we care about
	NEIByYear <- as.data.frame(cbind(NEICoal$year, NEICoal$Emissions))
	colnames(NEIByYear) <- c("year", "Emissions")

	# Grab some year labels while we're here
	years <- sort(unique(as.character(NEIByYear$year)))
	NEIByYear$year <- as.factor(NEIByYear$year)

	# Compute the sums per year that we need
	molten <- melt(NEIByYear, id.vars=c(1))
	molten$value <- as.numeric(molten$value)
	dataCoalSector <- dcast(molten, year~variable, sum)

	# Construct our plot object
	g <- ggplot(dataCoalSector, aes(as.numeric(years), Emissions/100000, group=1))
	g <- g + geom_area(fill="#FFFAFA", colour="#000000") +
		scale_x_continuous(limits = c(1999, 2008), expand=c(0,0)) +
		scale_y_continuous(limits = c(0, 10), expand=c(0,0)) +
		labs(x = "Year", y = "Total Emissions (in megatons)", title="Extractive Industry Yearly Emissions Total") +
		theme(panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), plot.background = element_rect(fill = "#FFFAF0", colour = "#000000"), panel.grid.major = element_line(colour="#FFDEAD"))

	# Print it out
	png(filename="plot4.png", width=xres, height=yres, type="cairo")
	print(g)
	dev.off()
}

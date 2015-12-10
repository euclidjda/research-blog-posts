#!/usr/bin/env Rscript

library(zoo);
library(xts);
library(PerformanceAnalytics);
library(ggplot2);

# Load Data
raw <- read.table("data-1962.dat",
			sep = " ",
                       	header = TRUE,
                       	na.strings="NULL",
                       	colClasses=c(DATE="character"))

date.seq <- as.Date(paste(raw$DATE,"01",sep=""),"%Y%m%d")

# Create the time series' ...

Hi10SP500.ts <- xts(raw$Hi10-raw$SP500,date.seq)
data.ts      <- xts( cbind(raw$Hi10,raw$SP500), date.seq );

pdf("chart1.pdf");

par(mfrow=c(2,1),mar=c(1,4,2,1), oma=c(2,1,2,1));

chart.CumReturns(data.ts,
                 #main="Value Investing: Simulated Performance and Active Drawdowns",
                 cex.main=1.1,
                 sub="",
                 ylab="Growth of $1 Invested",
                 geometric=TRUE,
                 wealth.index=TRUE,
                 xlab="",
		 col=c('#34BBA7','#617994') );

legend(150,500,
       legend = c("Value Approach", "SP500 Total Return"),
       col=c('#34BBA7','#617994'),
       lty = c("solid", "solid"),
       lwd = c( 2, 2),
       xjust = .5, yjust = .5)

chart.Drawdown(Hi10SP500.ts,ylab="Drawdown Relative to SP500 TR",
               col=c('#DD592D'),yaxis=FALSE);

yLabels <- c(-.5, -.4, -.3, -.2, -.1, 0)
axis(2, at=yLabels, labels=sprintf(round(100*yLabels), fmt="%d%%") )

dev.off();

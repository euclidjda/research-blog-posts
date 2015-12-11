#!/usr/bin/env Rscript
# Prototype R script for use at command line in Linux, Mac OS X, UNIX

library(zoo);
library(xts);
library(PerformanceAnalytics);
library(ggplot2);

# IMPORTANT!!!
# THIS WAS OUR FIRST BLOG POST AND IT WAS WRITTEN BEFORE WE DECIDED TO ONLY USE
# PUBLICLY AVAILABLE DATA.
# UNLIKE THE OTHERS, THIS BLOG DEPENDS ON PROPRIETRARY DATA THAT WE CANNOT
# SHAR ON GITHUB. CHECK OUT THE OTHER BLOG POST SCRIPTS AS THEY ALL USE
# PUBLICLY AVAILABLE DATA AND ARE FULLY SELF CONTAINED.
# OR USE THIS AS A TEMPLATE FOR YOUR OWN SCRIPTS.

data <- read.table("FILE_DOESNT_EXIST", sep = " ", header = TRUE);

ey <- data$EY;
sp500 <- data$SP500;

dates = seq(from=as.Date("1973-01-01"), by = "month", length.out=NROW(ey));

data_ts <- cbind(ey,sp500);
data_xts <- xts( data_ts, order.by=dates );

ey_ts <- ts(ey,start = c(1973,1), frequency=12);
ey_xts <- as.xts( ey_ts );

sp500_ts <- ts(sp500,start = c(1973,1), frequency=12);
sp500_xts <- as.xts( sp500_ts );

ex <- ey - sp500;

# dates  = seq(from=as.Date("1973-01-01"), by = "month", length.out=NROW(ex));
ex_xts = xts(ex,order.by=dates);

pdf("performance.pdf");

# proxima nova font

par(mfrow=c(2,1),mar=c(1,4,2,1), oma=c(2,1,2,1));

chart.CumReturns(data_xts,
                 main="Value Investing: Simulated Performance and Active Drawdowns",
                 cex.main=1.1,
                 sub="",
                 ylab="Growth of $1 Invested",
                 geometric=TRUE,
                 wealth.index=TRUE,
                 xlab="",
		 col=c('#34BBA7','#617994') );

legend(100,500,
       legend = c("Value Approach", "SP500 Total Return"),
       col=c('#34BBA7','#617994'),
       lty = c("solid", "solid"),
       lwd = c( 2, 2),
       xjust = .5, yjust = .5)

chart.Drawdown(ex_xts,ylab="Drawdown Relative to SP500 TR",col=c('#DD592D'),
               yaxis=FALSE);

yLabels <- c(-.5, -.4, -.3, -.2, -.1, 0)
axis(2, at=yLabels, labels=sprintf(round(100*yLabels), fmt="%d%%") )

dev.off();

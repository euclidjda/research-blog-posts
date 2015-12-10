#!/usr/bin/env Rscript

library(stringr)
library(scales)
library(ggthemes)
library(PerformanceAnalytics)
library(RColorBrewer)
library(plyr)

raw <- read.table("data-1962.dat",
                       sep = " ",
                       header = TRUE,
                       na.strings="NULL",
                       colClasses=c(DATE="character"))

date.seq <- as.Date(paste(raw$DATE,"01",sep=""),"%Y%m%d")

# Create the time series' ...

Hi10SP500.ts <- xts(raw$Hi10-raw$SP500,date.seq)
Hi10.ts      <- xts(raw$Hi10,date.seq)
SP500.ts     <- xts(raw$SP500,date.seq)

win.ts    <- window(Hi10SP500.ts,start=date.seq[1],end="2009-12-31")
drawdowns <- table.Drawdowns(win.ts,top=6)

start.dates <- sort( drawdowns[,2] )
end.dates   <- sort( drawdowns[,3] )

# Loop through the big drawdowns
series <- c()
type   <- c()
months <- c()
comp   <- c()

for (i in 1:length(start.dates)) {

    start <- start.dates[i]

    end.idx <- match(start,date.seq) + 2*12

    if (end.idx > length(date.seq)) {

        end <- date.seq[length(date.seq)]

    } else {

        end <- date.seq[end.idx]
        
    }
    
    series.label <- paste(paste(i,format(start, ": %b, %Y "),sep=""),
                          format(end, " %Y"), sep="-")

    for ( type.label in c("SP500","VALUE")) {

        series   <- c(series,series.label)
        type     <- c(type,type.label)
        months   <- c(months,0)
        cur      <- 0
        comp     <- c(comp,0)

        if (type.label == "SP500") {
            win <- window(SP500.ts,start=start,end=end)
        } else {
            win <- window(Hi10.ts,start=start,end=end)
        }
        
        for (j in 1:length(win)) {
            
            cur      <- (cur+1)*(win[[j]]+1)-1
            
            series   <- c(series,series.label)
            type     <- c(type,type.label)
            months   <- c(months,j)
            comp     <- c(comp,cur)
            
        }

    }

}

all.data <- data.frame(series,type,months,comp)

colors=c('#617994','#DD592D')

pdf("chart2.pdf");
                                        # Plot the data
ggplot(all.data,aes(x=months,y=comp,group=type,colour=type)) +
    geom_line(size=2)+
        scale_colour_manual(values=colors )+
        scale_y_continuous("",label=percent)+
            scale_x_discrete("",breaks=c(0,6,12,18,24))+
                facet_wrap(~series,scales="free")+
                     theme_bw()+guides(colour=FALSE)

dev.off()

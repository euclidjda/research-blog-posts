# Load libraries
library(stringr)
library(scales)
library(ggthemes)
library(PerformanceAnalytics)
library(RColorBrewer)
library(plyr)

# Define geometric mean
geomean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

# Download the data and unzip it
raw <- read.table("~/work/research/research-blog-posts/2015-10-oct/m400-m500.dat",
                      sep = " ",
                      header = TRUE,
                      na.strings="NULL",
                      colClasses=c(DATE="character"))

date.seq <- as.Date(paste(raw$DATE,"01",sep=""),"%Y%m%d")

tseries <- list( )

tseries[["SP500"]] <- xts(raw$SP500 , date.seq)
tseries[["M400"]]  <- xts(raw$M400  , date.seq)
tseries[["M500"]]  <- xts(raw$M500  , date.seq)

drawdowns <- table.Drawdowns(tseries[["SP500"]],top=9)

start.dates <- drawdowns[,1]
end.dates   <- drawdowns[,3]

# Loop through the big drawdowns
series <- c()
type   <- c()
months <- c()
cumm   <- c()

for (i in 1:length(start.dates)) {

    start <- start.dates[i]
    end   <- end.dates[i]

    series.label <- paste( letters[i],") ",
                          format(start,"%b '%y")," to ",
                          format(end,"%b '%y"), sep="")
         # paste(format(start, "%Y"),format(end, "%Y"), sep="-")

    for ( type.label in c("SP500","M400","M500")) {

        series   <- c(series,series.label)
        type     <- c(type,type.label)
        months   <- c(months,0)
        cur.cumm <- 0
        cumm     <- c(cumm,cur.cumm)

        win <- window(tseries[[type.label]],start=start,end=end)
        
        for (j in 1:length(win)) {
            
            cur.cumm <- (cur.cumm+1)*(win[[j]]+1)-1
            
            series   <- c(series,series.label)
            type     <- c(type,type.label)
            months   <- c(months,j)
            cumm     <- c(cumm,cur.cumm)
            
        }

    }

}

all.data <- data.frame(series,type,months,cumm)

all.data$type   <- factor( all.data$type )
all.data$series <- factor( all.data$series )

# Plot the data
ggplot(all.data,aes(x=months,y=cumm,group=type,colour=type)) +
    geom_line()+
        scale_y_continuous("Cummulative Return",label=percent) +
        facet_wrap(~series,scales="free") +
            ggtitle("Top 9 Biggest SP500 Drawdowns Peak to Trough to Recovery\nCompared with Model 400 and Model 500 Over the Sampe Period\n")

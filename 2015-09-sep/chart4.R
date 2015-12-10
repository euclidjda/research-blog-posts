#!/usr/bin/env Rscript

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


# The URL for the data
url.name     <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp"
file.name    <- "F-F_Benchmark_Factors_Monthly.zip"
full.url     <- paste(url.name, file.name, sep="/")

# You can change the ending date of the analysis by changing these variables
# Beware, however, that the source datafile may only be updated annually
# and therefore the most recent monthly may not be available

end.year     <- 2015 # Change these to bring up-to-date
end.month    <- 8
window.width <- 5*12 # The rolling window width. 5 Years in this case

# Download the data and unzip it

temp.file <- tempfile()
download.file(full.url, temp.file)
file.list <- unzip(temp.file, list=TRUE)

# Parse the data
french.data   <- read.csv(unzip(temp.file,files=as.character(file.list[1,1])),
                            sep = "",
                            header=TRUE )
names(french.data)[[1]] <- "DATE"

# Now we want to remove all the data below the end date
ds.year     <- as.numeric(substr(french.data$DATE[[1]],1,4))
ds.month    <- as.numeric(substr(french.data$DATE[[1]],5,6))
num.rows    <- 12*(end.year-ds.year)+(end.month-ds.month)+1
french.data <- head(french.data,num.rows)
date.seq    <- as.Date(paste(french.data$DATE,"01",sep=""),"%Y%m%d")
french.data$DATE <- date.seq

# Transform the data so that the return cells are numeric decimal format
for (i in 2:ncol(french.data)) french.data[,i] <- as.numeric(str_trim(french.data[,i]))
for (i in 2:ncol(french.data)) french.data[,i] <- french.data[,i]/100

# Now we calculate the the HML sequence from the factor sequences
french.data$Hi.Lo <- french.data$HML

# Now create a time series of the HML data that we can pass off to apply.rolling 
# and other PerformanceAnalytics functions
ts.data <- data.frame(french.data$Hi.Lo)
row.names(ts.data) <- date.seq

hi.lo.monthly <- xts(ts.data$french.data.Hi.Lo,date.seq)

hi.lo.annual  <- aggregate(hi.lo.monthly+1,
                           as.integer(format(index(hi.lo.monthly),"%Y")), prod)-1

cpi.raw <- read.table("cpi_data.dat",
                      sep = " ",
                      header = TRUE,
                      na.strings="NULL",
                      colClasses=c(date="character"))

cpidate.seq    <- as.Date(paste(cpi.raw$date,"01",sep=""),"%Y%m%d")
cpi.data       <- xts(cpi.raw$cpi,cpidate.seq)
cpi.monthly    <- Return.calculate(cpi.data, method="compound")
cpi.monthly    <- cpi.monthly[2:nrow(cpi.monthly),]
cpi.annual     <- aggregate(cpi.monthly+1,
                            as.integer(format(index(cpi.monthly),"%Y")), prod)-1

data.annual    <- merge.zoo( cpi.annual, hi.lo.annual )
df.annual      <- data.frame(cpi=data.annual$cpi.annual,hilo=data.annual$hi.lo.annual)

df.annual$year <- rownames(df.annual)

######################################################################################
# 
# Calc regression for 10 year lagged periods

result.all <- data.frame( size=c(), value=c() )

window.size <- 10

lag.size    <- window.size / 2 
num.years   <- nrow(df.annual)
num.windows <- num.years/window.size

samp <- df.annual
samp$period <- gl(num.windows,window.size,length=num.years)

result <- data.frame( cpi=c(), hilo=c(), year=c(), size=c() )

mid <- lag.size+1
end <- window.size

for ( p in levels(samp$period) ) {

    sub <- subset( samp, period == p )
    new.row <- data.frame( cpi   = geomean( sub[1:lag.size,]$cpi+1 )-1 ,
                          hilo   = geomean( sub[mid:end,]$hilo+1 )-1   ,
                          year   = as.numeric( sub[1,]$year )          ,
                          size = window.size                           )
    
    result <- rbind( result, new.row )
    
}

format.year <- function(year,window.size) {
    
    yr1 <- substr(toString(year+window.size/2-1),3,4)
    yr2 <- substr(toString(year+window.size/2),3,4)
    yr3 <- substr(toString(year+window.size-1),3,4)
    str <- paste(year,"-",year+9,sep="")
}

pdf("chart4.pdf")

p <- ggplot()
p <- p + geom_point(data=result,aes(x=cpi,y=hilo),color="#DD592D",size=3)
p <- p + geom_text(data=subset(result,year %in% c(1926,1946,1956,1966,1996,2006)),
                   aes(x=cpi,y=hilo,label=format.year(year,window.size),
                       vjust=-.4,hjust=-0.1),size=4,color='#617994')
# lower right
p <- p + geom_text(data=subset(result,year %in% c(1936)),
                   aes(x=cpi,y=hilo,label=format.year(year,window.size),
                       vjust=1.1,hjust=-0.1),size=4,color='#617994')
# lower left
p <- p + geom_text(data=subset(result,year %in% c(1976,1986)),
                   aes(x=cpi,y=hilo,label=format.year(year,window.size),
                       vjust=1.1,hjust=1.1),size=4,color='#617994')

p <- p + scale_x_continuous("Annualized Change in CPI for First Five Years of Period",
                            label=percent)

p <- p + scale_y_continuous("Compound Annualized Value Factor for Last Five Years of Period\n",
                            label=percent)

p <- p + ggtitle("Lagged Relationship Between Inflation and Value Performance\n")

p

dev.off();

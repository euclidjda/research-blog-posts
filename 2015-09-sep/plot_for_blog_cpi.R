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
file.name    <- "25_Portfolios_5x5_CSV.zip"
full.url     <- paste(url.name, file.name, sep="/")

# You can change the ending date of the analysis by changing these variables
# Beware, however, that the source datafile may only be updated annually
# and therefore the most recent monthly may not be available

end.year     <- 2015
end.month    <- 7
window.width <- 5*12 # The rolling window width. 5 Years in this case

# Download the data and unzip it

temp.file <- tempfile()
download.file(full.url, temp.file)
file.list <- unzip(temp.file, list=TRUE)

# Parse the data
french.data   <- read.csv(unzip(temp.file,
                              files=as.character(file.list[1,1])),
                        skip=19,
                        header=TRUE,
                        stringsAsFactors=FALSE)
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
# Column names are: DATE SMALL.LoBM ME1.BM2 SMALL.HiBM BIG.LoBM ME2.BM2 BIG.HiBM
fd <- french.data

french.data$Hi.Lo <-
    0.2*( fd$SMALL.HiBM + fd$ME2.BM5 + fd$ME3.BM5 + fd$ME4.BM5 + fd$BIG.HiBM ) -
    0.2*( fd$SMALL.LoBM + fd$ME2.BM1 + fd$ME3.BM1 + fd$ME4.BM1 + fd$BIG.LoBM )

# Now create a time series of the HML data that we can pass off to apply.rolling 
# and other PerformanceAnalytics functions
ts.data <- data.frame(french.data$Hi.Lo)
row.names(ts.data) <- date.seq

hi.lo.monthly <- xts(ts.data$french.data.Hi.Lo,date.seq)

hi.lo.annual  <- aggregate(hi.lo.monthly+1, as.integer(format(index(hi.lo.monthly),"%Y")), prod)-1

cpi.raw <- read.table("~/work/research/research-blog-posts/2015-09-sep/cpi_data.dat",
                      sep = " ",
                      header = TRUE,
                      na.strings="NULL",
                      colClasses=c(date="character"))

cpidate.seq <- as.Date(paste(cpi.raw$date,"01",sep=""),"%Y%m%d")

cpi.data <- xts(cpi.raw$cpi,cpidate.seq)

cpi.monthly <- Return.calculate(cpi.data, method="compound")
cpi.monthly <- cpi.monthly[2:nrow(cpi.monthly),]

cpi.annual  <- aggregate(cpi.monthly+1, as.integer(format(index(cpi.monthly),"%Y")), prod)-1

data.annual <- merge.zoo( cpi.annual, hi.lo.annual )

df.annual <- data.frame(cpi=data.annual$cpi.annual,hilo=data.annual$hi.lo.annual)

df.annual$year <- rownames(df.annual)

p.annual <- ggplot(df.annual,aes(x=cpi,y=hilo))+
    geom_text(aes(label=year),size=2.5)+
        geom_smooth(method = "lm")+
            scale_x_continuous("Annual Change in CPI",label=percent)+
                scale_y_continuous("Outperformance: Value vs. Growth",label=percent)+
                    ggtitle("The Relationship Between Inflation and Value Investing")

cpi.decade <- aggregate(cpi.annual+1, as.integer(substr(index(cpi.annual),1,3)), geomean)-1
hi.lo.decade <- aggregate(hi.lo.annual+1, as.integer(substr(index(hi.lo.annual),1,3)), geomean)-1

data.decade <- merge.zoo( cpi.decade, hi.lo.decade )

df.decade <- data.frame(cpi=data.decade$cpi.decade,hilo=data.decade$hi.lo.decade)

df.decade$year <- rownames(df.decade)

p.decade <- ggplot(df.decade,aes(x=cpi,y=hilo))+
      geom_text(aes(label=paste(year,"0s",sep="")),size=2.5)+
          geom_smooth(method = "lm", se = FALSE)+
              scale_x_continuous("Annual Change in CPI",label=percent)+
                  scale_y_continuous("Outperformance: Value vs. Growth",label=percent)+
                      ggtitle("The Relationship Between Inflation and Value Investing")

######################################################################################

# Calc correlation coeffienct for bootstrapped periods

result.all <- data.frame( size=c(), boot=c(), value=c() )

for (window.size in c(2,4,6,8,10)) {

    for ( n in 1:300 ) {
        
        num.years   <- nrow(df.annual)
        num.windows <- floor(num.years/window.size)
        end.index   <- num.years-window.size
        indexes     <- sort( sample(1:end.index,num.windows,replace=TRUE) )
        indorig     <- indexes

        for (i in 1:(window.size-1)) {
            indexes <- c(indexes,indorig+i)
        }

        indexes <- c( t( matrix( indexes, ncol=window.size ) ) )

        samp <- df.annual[indexes,]
        samp$period <- gl(num.windows,window.size)

        result <- aggregate(samp[,c("cpi","hilo")]+1, by=list(samp$period), FUN=geomean)
        result <- result[,c("cpi","hilo")]-1
        result$year <- df.annual[indorig,]$year

        new.row <- data.frame( size=c(window.size), boot=c(n), value=c(cor(result$cpi,result$hilo)))
        result.all <- rbind( result.all, new.row )
    }
                 
}

ggplot(data=result.all,aes(x=value,color=size,fill=size))+
    geom_histogram(binwidth=0.01)+
        facet_wrap(~size,ncol=5)+
            coord_flip()

cdata <- ddply(result.all,.(size),summarise,N=length(value),
               min=min(value),
               lower=quantile(value,0.05),
               y=mean(value),
               middle=median(value),
               upper=quantile(value,0.95),
               max=max(value)               )

######################################################################################

# Calc correlation coeffienct for lagged bootstrapped periods

result.all <- data.frame( size=c(), boot=c(), value=c() )

for (window.size in c(2,4,6,8,10)) {

    for ( n in 1:1000 ) {
        
        lag.size    <- window.size / 2
        num.years   <- nrow(df.annual)
        num.windows <- floor(num.years/window.size)
        end.index   <- num.years-window.size
        indexes     <- sort( sample(1:end.index,num.windows,replace=TRUE) )
        indorig     <- indexes

        for (i in 1:(window.size-1)) {
            indexes <- c(indexes,indorig+i)
        }

        indexes <- c( t( matrix( indexes, ncol=window.size ) ) )

        samp <- df.annual[indexes,]
        samp$period <- gl(num.windows,window.size)

        result <- data.frame( cpi=c(), hilo=c(), year=c() )

        for ( p in levels(samp$period) ) {

            sub <- subset( samp, period == p )
            new.row <- data.frame( cpi  = geomean( sub[1:lag.size,]$cpi+1 )-1              ,
                                   hilo = geomean( sub[lag.size+1:length(sub),]$hilo+1 )-1 ,
                                  year = sub[1,]$year                                      )
            result <- rbind( result, new.row )

        }

        new.row <- data.frame( size=c(window.size), boot=c(n), value=c(cor(result$cpi,result$hilo)))
        result.all <- rbind( result.all, new.row )
    }
                 
}

aggregate(result.all[c("value")],by=list(result.all$size),mean)

ggplot(data=result.all,aes(x=value,color=size,fill=size))+
    geom_histogram(binwidth=0.01)+
        facet_wrap(~size,ncol=5)+
            coord_flip()

cdata <- ddply(result.all,.(size),summarise,N=length(value),
               min=min(value),
               lower=quantile(value,0.05),
               y=mean(value),
               middle=median(value),
               upper=quantile(value,0.95),
               max=max(value)               )

######################################################################################

# Calc correlation coeffienct for sequential periods

result.all <- data.frame( size=c(), value=c() )

for (window.size in 1:10) {

    num.years   <- nrow(df.annual)
    num.windows <- ceiling(num.years/window.size)

    samp <- df.annual
    samp$period <- gl(num.windows,window.size,length=num.years)

    result <- aggregate(samp[,c("cpi","hilo")]+1, by=list(samp$period), FUN=geomean)
    result <- result[,c("cpi","hilo")]-1
    result$year <- df.annual[seq(from=1,to=num.years,by=window.size),]$year
    
    new.row <- data.frame( size=c(window.size), value=c(cor(result$cpi,result$hilo)))
    result.all <- rbind( result.all, new.row )
                 
}

######################################################################################
# Build table of all data point for sequential periods then do regression

result.all <- data.frame( size=c(), cpi=c(), hilo=c(), year=c() )

for (window.size in 1:10) {

    num.years   <- nrow(df.annual)
    num.windows <- ceiling(num.years/window.size)

    samp <- df.annual
    samp$period <- gl(num.windows,window.size,length=num.years)

    result <- aggregate(samp[,c("cpi","hilo")]+1, by=list(samp$period), FUN=geomean)
    result <- result[,c("cpi","hilo")]-1
    result$year <- df.annual[seq(from=1,to=num.years,by=window.size),]$year
    result$size <- window.size
    
    result.all <- rbind( result.all, result )
                 
}

result.all$size <- factor( result.all$size )

ggplot(result.all,aes(x=cpi,y=hilo))+
    geom_text(aes(label=year),size=1.5)+
        facet_wrap(~size,ncol=5,scales="free")+
            geom_smooth(method=lm,se=FALSE,fullrange=TRUE)+
                scale_x_continuous(labels=percent)+
                    scale_y_continuous(labels=percent)
                    

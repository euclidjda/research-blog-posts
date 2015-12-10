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

end.year     <- 2015
end.month    <- 8
window.width <- 5*12 # The rolling window width. 5 Years in this case

# Download the data and unzip it

temp.file <- tempfile()
download.file(full.url, temp.file)
file.list <- unzip(temp.file, list=TRUE)

# Parse the data
french.data   <- read.csv(unzip(temp.file,
                              files=as.character(file.list[1,1])),
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

hi.lo.annual  <- aggregate(hi.lo.monthly+1, as.integer(format(index(hi.lo.monthly),"%Y")), prod)-1
#hi.lo.annual  <- aggregate(hi.lo.monthly, as.integer(format(index(hi.lo.monthly),"%Y")), sum)

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
# hi.lo.decade <- aggregate(hi.lo.annual, as.integer(substr(index(hi.lo.annual),1,3)), mean)

data.decade <- merge.zoo( cpi.decade, hi.lo.decade )

df.decade <- data.frame(cpi=data.decade$cpi.decade,hilo=data.decade$hi.lo.decade)

df.decade$year <- rownames(df.decade)

### '#617994','#DD592D' '#34BBA7'


p.decade <- ggplot(df.decade,aes(x=cpi,y=hilo))+
    geom_point(color="#DD592D",size=3)+
     geom_text(data=subset(df.decade,year<195.5),aes(label=paste(year,"0s",sep=""),vjust=-.8,hjust=0.8),size=4,color='#617994')+
     geom_text(data=subset(df.decade,year>195.5),aes(label=paste(year,"0s",sep=""),vjust=-.8,hjust=0.1),size=4,color='#617994')+
         scale_x_continuous("Annual Change in CPI",label=percent,c(-0.04,-0.02,0,0.02,0.04,0.06,0.08))+
                  scale_y_continuous("Compound Annualized Value Factor",
                                     label=percent,breaks=c(-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.10))+ggtitle("\n\n\n\n")

p.decade <- ggplot(df.decade,aes(x=cpi,y=hilo))+
    geom_point()+
#      geom_text(aes(label=paste(year,"0s",sep="")),size=2.5)+
              scale_x_continuous("Annual Change in CPI",label=percent,c(-0.04,-0.02,0,0.02,0.04,0.06,0.08))+
                  scale_y_continuous("Compound Annualized Value Factor",
                                     label=percent,breaks=c(-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.10))


######################################################################################
# 
# Calc correlation coeffienct for sequential periods

result.all <- data.frame( size=c(), value=c() )

for ( window.size in 1:10 ) {

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

result.all

result.all$size <- factor(result.all$size)

### '#617994','#DD592D' '#34BBA7'

background <- data.frame( lower = c(0.0,0.5,0.8) , 
                          upper = c(0.5,0.8,1.0),
                          col = letters[1:3]  )

colors = c("#DD592D","#617994","#34BBA7")

ggplot(result.all)+
    geom_rect(data = background ,
              mapping = aes(
                  xmin = 1 ,
                  xmax = 10 ,
                  ymin = lower ,
                  ymax = upper ,
                  fill = col   ),
              alpha = 0.3 ) +
                  scale_fill_manual( values=colors ) +
                  geom_point(aes(x=size,y=value,color=value),size=6) +
#                      scale_color_gradient(low="blue", high="") +
geom_smooth(aes(x=size,y=value,group=1),method="lm",se=FALSE,color="grey",alpha=0.7) +
    scale_x_discrete("Time Period (Horizon) in Years") +
        scale_y_continuous("Correlation Between Inflation & Value Investing Performance\n",limit=c(-0.0,1.0),breaks=c(0.0,0.5,0.8,1.0),expand=c(0,0))+
                theme_bw() +
                    theme(legend.position="none",
                          panel.border=element_blank() )+ggtitle("\n\n\n\n")

######################################################################################
# 
# Calc correlation coeffienct for lagged sequential periods

result.all <- data.frame( size=c(), value=c() )

for ( window.size in c(2,4,6,8,10) ) {

    lag.size    <- ceiling( window.size / 2 )
    num.years   <- nrow(df.annual)
    num.windows <- floor(num.years/window.size)

    samp <- df.annual
    samp$period <- gl(num.windows,window.size,length=num.years)

    result <- data.frame( cpi=c(), hilo=c(), year=c(), size=c() )

    mid <- lag.size+1
    end <- window.size
    
    for ( p in levels(samp$period) ) {

        sub <- subset( samp, period == p )
        new.row <- data.frame( cpi  = geomean( sub[1:lag.size,]$cpi+1 )-1             ,
                              hilo = geomean( sub[mid:end,]$hilo+1 )-1 ,
                              year = sub[1,]$year                                     ,
                              size = window.size                                      )
        
        result <- rbind( result, new.row )
        
    }

    new.row <- data.frame( size=c(window.size), value=c(cor(result$cpi,result$hilo)))
    result.all <- rbind( result.all, new.row )
                 
}

result.all

background <- data.frame( lower = c(-0.25,0.5,0.8) , 
                          upper = c(0.5,0.8,1.0),
                          col = letters[1:3]  )

colors = c("#DD592D","#617994","#34BBA7")

ggplot(result.all)+
     geom_rect(data = background ,
                   mapping = aes(
                        xmin = 1 ,
                        xmax = 10 ,
                        ymin = lower ,
                        ymax = upper ,
                        fill = col   ),
               alpha = .3 ) + geom_point(aes(x=size,y=value,color=value),size=6) +
                   #scale_color_gradient(low="grey", high="orangered") +
                   geom_smooth(aes(x=size,y=value,group=1),method="lm",se=FALSE,color="grey",alpha=0.7,fullrange=TRUE) +
geom_hline(aes(yintercept=0.0),color="grey",linetype=2,size=0.5,alpha=0.4) +
     scale_x_discrete("Time Period (Horizon) in Years") +
         scale_y_continuous("Correlation Between Inflation in First Half of Period \n & Value Investing Performance in Second Half of Period\n",
                            limit=c(-0.25,1.0),breaks=c(0.0,0.5,0.8,1.0),expand=c(0,0))+
                                scale_fill_manual( values=colors ) +
                                    theme_bw() +
                 theme(legend.position="none",
                       panel.border=element_blank() )+ggtitle("\n\n\n\n")

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

ggplot()+
    geom_point(data=result,aes(x=cpi,y=hilo),color="#DD592D",size=3)+
# upper right
geom_text(data=subset(result,year %in% c(1926,1946,1956,1966,1996,2006)),
          aes(x=cpi,y=hilo,label=format.year(year,window.size),vjust=-.4,hjust=-0.1),size=4,color='#617994')+
# lower right
geom_text(data=subset(result,year %in% c(1936)),aes(x=cpi,y=hilo,label=format.year(year,window.size),vjust=1.1,hjust=-0.1),size=4,color='#617994')+
# lower left
geom_text(data=subset(result,year %in% c(1976,1986)),aes(x=cpi,y=hilo,label=format.year(year,window.size),vjust=1.1,hjust=1.1),size=4,color='#617994')+
         scale_x_continuous("Annualized Change in CPI for First Five Years of Period",label=percent)+
                  scale_y_continuous("Compound Annualized Value Factor for Last Five Years of Period\n",
                                     label=percent)+ggtitle("\n\n\n\n")



######################################################################################
# Build table of all data point for sequential periods then do regression

result.all <- data.frame( size=c(), cpi=c(), hilo=c(), year=c() )

for ( window.size in c(2,4,6,8,10) ) {

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

ggplot(result.all,aes(x=cpi,y=hilo,color=size))+
    geom_text(aes(label=year),size=1.5)+
            geom_smooth(method=lm,se=FALSE,fullrange=TRUE)+
                scale_x_continuous(labels=percent)+
                    scale_y_continuous(labels=percent)
                    

######################################################################################

# Calc correlation coeffienct for bootstrapped periods

result.all <- data.frame( size=c(), boot=c(), value=c() )

for (window.size in 1:10) {

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
#
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

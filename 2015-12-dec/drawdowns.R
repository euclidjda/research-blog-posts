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
file.name    <- "Portfolios_Formed_on_BE-ME_CSV.zip"
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
                        skip=23,
                        header=TRUE,
                        stringsAsFactors=FALSE)
names(french.data)[[1]] <- "DATE"
names(french.data)[[2]] <- "OTHER"

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
french.data$Hi.Lo <- french.data$Hi.30 - french.data$Lo.30

french.data$All <- (french.data$Lo.30+french.data$Med.40+french.data$Hi.30) / 3.0

ts.market <- xts(french.data$All,date.seq)
ts.hibm   <- xts(french.data$Hi.Lo,date.seq)

drawdowns <- table.Drawdowns(ts.market,top=9)

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

    series.label <- paste(format(start, "%Y"),format(end, "%Y"), sep="-")

    for ( type.label in c("market","hibm")) {

        series   <- c(series,series.label)
        type     <- c(type,type.label)
        months   <- c(months,1)
        cur.cumm <- 0
        cumm     <- c(cumm,cur.cumm)

        if (type.label == "market") {
            win <- window(ts.market,start=start,end=end)
        } else {
            win <- window(ts.hibm,start=start,end=end)
        }
        
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

# Plot the data
ggplot(all.data,aes(x=months,y=cumm,group=type,colour=type)) +
    geom_line()+
        facet_wrap(~series,scales="free")

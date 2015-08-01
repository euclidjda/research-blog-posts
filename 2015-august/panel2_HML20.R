# library for 
library(stringr)
library(scales)
library(ggthemes)
library(PerformanceAnalytics)
library(RColorBrewer)

# Globals to set for data end date (check the data file for last date)
end.year  <- 2015
end.month <- 6

# The URL for the data.
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_5x5_CSV.zip
url.name  <- paste("http://mba.tuck.dartmouth.edu", "pages/faculty/ken.french/ftp", sep="/")
file.name <- "25_Portfolios_5x5_CSV.zip"

# Download the data and unzip it
ff.url <- paste(url.name, file.name, sep="/")
f <- tempfile()
download.file(ff.url, f)
file.list <- unzip(f, list=TRUE)
 
# Parse the data
fama.data <- read.csv(unzip(f, files=as.character(file.list[1,1])), skip=19,header=TRUE,stringsAsFactors=FALSE)
names(fama.data)[[1]] <- "DATE"

# Now we want to remove all the data below the end date

start.year  <- as.numeric(substr(fama.data$DATE[[1]],1,4))
start.month <- as.numeric(substr(fama.data$DATE[[1]],5,6))
num.rows <- 12*(end.year-start.year)+(end.month-start.month)+1
fama.data <- head(fama.data,num.rows)

date.seq <- as.Date(paste(fama.data$DATE,"01",sep=""),"%Y%m%d")
fama.data$DATE <- date.seq

# Transform the data soe that the return cells are numeric decimal format

for (i in 2:ncol(fama.data)) fama.data[,i] <- as.numeric(str_trim(fama.data[,i]))
for (i in 2:ncol(fama.data)) fama.data[,i] <- fama.data[,i]/100

# Now we calculate the the HML sequence from the factor sequences
# Column names are: DATE SMALL.LoBM ME1.BM2 SMALL.HiBM BIG.LoBM ME2.BM2 BIG.HiBM
fd <- fama.data

fama.data$Hi.Lo <- 0.2*( fd$SMALL.HiBM + fd$ME2.BM5 + fd$ME3.BM5 + fd$ME4.BM5 + fd$BIG.HiBM ) -
    0.2*( fd$SMALL.LoBM + fd$ME2.BM1 + fd$ME3.BM1 + fd$ME4.BM1 + fd$BIG.LoBM )

# Now create a time series of the HML data that we can pass off to apply.rolling and other PerformanceAnalytics functions

ts.data <- data.frame(fama.data$Hi.Lo)
row.names(ts.data) <- date.seq

width <- 5*12

# So that we can use geometric returns
Return.custom <- function(R, scale = NA, geometric = TRUE) {
    Return.annualized(R, scale = NA, geometric = FALSE)
}

z <- apply.rolling(ts.data, width=width, FUN = "Return.custom")
fama.data$Hi.Lo.5YrRolling <- z[[1]]

rows <- nrow(fama.data)

start.indexes <- c()
end.indexes   <- c()

i <- 60

while ( i < rows ) {

    if (fama.data$Hi.Lo.5YrRolling[i]<0) {

        j         <- i+1
        start.idx <- i
        end.idx   <- j
        min.val   <- fama.data$Hi.Lo.5YrRolling[i]
        max.val   <- fama.data$Hi.Lo.5YrRolling[i]

        while ( j < i + 12*8) {

            if ( j < rows ) {
                if (fama.data$Hi.Lo.5YrRolling[j]>max.val) {
                    max.val <- fama.data$Hi.Lo.5YrRolling[j]
                    end.idx <- j
                }

            } else {
                    max.val <- fama.data$Hi.Lo.5YrRolling[rows]
                    end.idx <- rows
            }

            j <- j+1

        }

        start.indexes <- c(start.indexes,start.idx)
        end.indexes   <- c(end.indexes,end.idx)
        
        str <- paste("Start=",fama.data$DATE[start.idx],
                     "End=",fama.data$DATE[end.idx],
                     "EndValue=",fama.data$Hi.Lo.5YrRolling[end.idx],
                     "Duration=",(end.idx-start.idx)/12)
        print(str)

        # Go back three years to get earliest negative point
        i <- j - 12*3
        
    } else {

        i <- i+1

    }

}

                                        # Create Series
series <- c()
months <- c()
values <- c()

for (i in 2:length(start.indexes)) {

    start.idx <- start.indexes[i]
    end.idx   <- end.indexes[i]
    str       <- paste(format(fama.data$DATE[start.idx], "%Y"),
                       format(fama.data$DATE[end.idx]  , "%Y"), sep="-")
    cur.value <- 0

    series <- c(series,str)
    months <- c(months,1)
    values <- c(values,cur.value)
    
    for (j in start.idx:end.idx) {

        # cur.value <- (cur.value+1)*(fama.data$Hi.Lo[j]+1)-1
        cur.value <- cur.value + fama.data$Hi.Lo[j]
        
        series <- c(series,str)
        months <- c(months,j-start.idx+2)
        values <- c(values,cur.value)

    }
    
}

recov.data <- data.frame(series,months,values)

colors <- rev( brewer.pal(6,"Set1") )
#colors <- c(colors,'#617994')
#colors <- c('blue','green','yellow','#34BBA7','#617994','#DD592D')

# color=c('#34BBA7','#617994','#DD592D')

g <- ggplot(recov.data,aes(x=months,y=values,size=series,group=series,colour=series))
g <- g + geom_line()
g <- g + scale_size_manual(values=c(1,1,1,1,1,2))
g <- g + scale_colour_manual(values=colors )
g <- g + scale_x_discrete( "", breaks=c(1,12,24,48,72,96) )
g <- g + scale_y_continuous( "", labels=percent )    
g <- g + guides(fill=FALSE)
#g <- g + guides(fill=guide_legend(title=NULL)) +
#  theme(legend.position=c(1,2),legend.justification=c(1,1))

g+theme_fivethirtyeight()

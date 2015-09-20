# Load libraries
library(stringr)
library(scales)
library(ggthemes)
library(PerformanceAnalytics)
library(RColorBrewer)

# The URL for the data
url.name     <- paste("http://mba.tuck.dartmouth.edu",
                   "pages/faculty/ken.french/ftp", sep="/")
file.name    <- "25_Portfolios_5x5_CSV.zip"
full.url     <- paste(url.name, file.name, sep="/")

# You can change the starting date of the analysis by changing these variables
# Beware, however, that the source datafile starts in 1926

start.year   <- 1945
start.month  <- 1

# You can change the ending date of the analysis by changing these variables
# Beware, however, that the source datafile may only be updated annually
# and therefore the most recent monthly may not be available

end.year     <- 2015
end.month    <- 6

window.width <- 5*12 # The rolling window width. 5 Years in this case
look.forward <- 8*12 # How far to look forward for the end of a value recovery

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

# Now we want to remove all the data above start date and below the end date
ds.year    <- as.numeric(substr(french.data$DATE[[1]],1,4))
ds.month   <- as.numeric(substr(french.data$DATE[[1]],5,6))
num.rows   <- 12*(end.year-ds.year)+(end.month-ds.month)+1
french.data  <- head(french.data,num.rows)
date.seq   <- as.Date(paste(french.data$DATE,"01",sep=""),"%Y%m%d")
french.data$DATE <- date.seq

# Transform the data soe that the return cells are numeric decimal format
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

# Now calculate the a time series of rolling 5-year annualized returns
z <- apply.rolling(ts.data, width=window.width, FUN = "Return.annualized")
french.data$Hi.Lo.5YrRolling <- z[[1]]

rows <- nrow(french.data)

# The following loop programatically finds the value recoveries and fills
# the lists start.indexes and end.indexes with the start and end index of these
# recoveries in the french.data data frame.
# Beware that the results of this search are sensitive to the parameters
# window.width and look.forward and may change significantly if these
# are altered from the default values of 60 months and 96 months, respectively.

start.indexes <- c()
end.indexes   <- c()

i <- max( (start.year-ds.year)*12+(start.month-ds.month), window.width)

while ( i < rows ) {

    if (french.data$Hi.Lo.5YrRolling[i]<0) {

        # A new periods starts when 5YrRolling turns negative
        j         <- i+1
        start.idx <- i
        end.idx   <- j
        min.val   <- french.data$Hi.Lo.5YrRolling[i]
        max.val   <- french.data$Hi.Lo.5YrRolling[i]

        while ( j < i + look.forward) {

            if ( j < rows ) {
                if (french.data$Hi.Lo.5YrRolling[j]>max.val) {
                    max.val <- french.data$Hi.Lo.5YrRolling[j]
                    end.idx <- j
                }

            } else {
                    max.val <- french.data$Hi.Lo.5YrRolling[rows]
                    end.idx <- rows
            }

            j <- j+1

        }

        start.indexes <- c(start.indexes,start.idx)
        end.indexes   <- c(end.indexes,end.idx)

        # print info on the value recovery we found
        str <- paste("Start=",french.data$DATE[start.idx],
                     "End=",french.data$DATE[end.idx],
                     "EndValue=",french.data$Hi.Lo.5YrRolling[end.idx],
                     "Duration=",(end.idx-start.idx)/12)
        print(str)

        # Restart search from where value recovery ends
        i <- end.idx + 1
        
    } else {

        i <- i+1

    }

}

# This last loop creates a long format data frame of all the value recoveries
# so that it is suitable for plotting with ggplot2
series <- c()
months <- c()
values <- c()

for (i in 1:length(start.indexes)) {

    start.idx <- start.indexes[i]
    end.idx   <- end.indexes[i]
    str       <- paste(format(french.data$DATE[start.idx], "%Y"),
                       format(french.data$DATE[end.idx]  , "%Y"), sep="-")
    cur.value <- 0

    series <- c(series,str)
    months <- c(months,1)
    values <- c(values,cur.value)

    # for each month in the recovery we calculate the cur.value of 
    # the cumulative compounded return
    for (j in start.idx:end.idx) {

        cur.value <- (cur.value+1)*(french.data$Hi.Lo[j]+1)-1
        
        series <- c(series,str)
        months <- c(months,j-start.idx+2)
        values <- c(values,cur.value)

    }
    
}

# Create the data frame with the three columns series, months, and value
recov.data <- data.frame(series,months,values)

# Create a suitable color palette 
colors <- rev( brewer.pal(7,"Blues") )[1:5]
colors <- c(colors, '#DD592D' )

# The most current line size will be 2 while the rest are 1
sizes  <- rep( 1, length(start.indexes)-1 )
sizes  <- c(sizes, 2)

# Plot the data
g <- ggplot(recov.data,aes(x=months,y=values,size=series,group=series,colour=series))
g <- g + geom_hline(aes(yintercept=0.0),color="grey",linetype=1)
g <- g + geom_line()
g <- g + scale_size_manual(values=sizes)
g <- g + scale_colour_manual(values=colors )
g <- g + scale_x_discrete( "", breaks=c( 1, 25, 50, 75 ) )
g <- g + scale_y_continuous( "", labels=percent )
g <- g + guides(fill=FALSE)

g + theme_bw() + theme(panel.background=element_blank() ,
                       panel.border=element_blank() )

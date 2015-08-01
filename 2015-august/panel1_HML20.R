# library for 
library(stringr)
library(scales)
library(ggthemes)

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
# Column names are:     DATE SMALL.LoBM ME1.BM2 SMALL.HiBM BIG.LoBM ME2.BM2 BIG.HiBM
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

#pd <- levels(fama.data$DATE)

indexes <- seq(from=width,to=length(date.seq),by=7*12)
breaks  <- c(date.seq[indexes])
breaks  <- c(breaks,as.Date('2022-01-01'))
years   <- (indexes-width)/12+1931
years   <- c(years,2022)

g <- ggplot(fama.data,aes(x=DATE,y=Hi.Lo.5YrRolling,group=1))
g <- g + geom_line()
g <- g + geom_hline(aes(yintercept=0.049),color="red",linetype=2) 
g <- g + scale_x_date(breaks=breaks,labels=years,limits=as.Date(c('1931-06-01','2022-01-01')),expand=c(0,1) )
g <- g + scale_y_continuous("Annualized 5 Year Rolling Value Premium",labels=percent)
g <- g + guides(fill=guide_legend(title=NULL)) +
  scale_colour_discrete(labels=c("")) +
  theme(legend.position=c(1,1),legend.justification=c(1,1))

# g <- g + scale_x_discrete(breaks = breaks, labels=years,limits = as.Date(c('1931-06-01','2022-01-01'))

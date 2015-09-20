# library for 
library(stringr)
library(scales)

# The URL for the data.
url.name <- paste("http://mba.tuck.dartmouth.edu", "pages/faculty/ken.french/ftp", sep="/")
file.name <- "Portfolios_Formed_on_BE-ME_CSV.zip"

# Download the data and unzip it
ff.url <- paste(url.name, file.name, sep="/")
f <- tempfile()
download.file(ff.url, f)
file.list <- unzip(f, list=TRUE)
 
# Parse the data
factor.data <- read.csv(unzip(f, files=as.character(file.list[1,1])), skip=23,header=TRUE,stringsAsFactors=FALSE)
names(factor.data)[[1]] <- "DATE"
names(factor.data)[[2]] <- "NEG.BE"

start.year     <- as.numeric(substr(factor.data$DATE[[1]],1,4))
start.month <- as.numeric(substr(factor.data$DATE[[1]],5,6))

end.year     <- 2015
end.month <- 6

num.rows <- 12*(end.year-start.year)+(end.month-start.month)+1

factor.data <- head(factor.data,num.rows)
factor.data$DATE <- as.Date(paste(factor.data$DATE,"01",sep=""),"%Y%m%d")
row.names(factor.data) <- factor.data$DATE

# Clean the data

for (i in 2:ncol(factor.data)) factor.data[,i] <- as.numeric(str_trim(factor.data[,i]))
for (i in 2:ncol(factor.data)) factor.data[,i] <- factor.data[,i]/100

factor.data$Hi.10.Rolling <- apply.rolling(factor.data[c("Hi.10")], width=12*5, FUN = "Return.annualized")[[1]]
factor.data$Lo.10.Rolling <- apply.rolling(factor.data[c("Lo.10")], width=12*5, FUN = "Return.annualized")[[1]]

factor.data$Hi.20.Rolling <- apply.rolling(factor.data[c("Hi.20")], width=12*5, FUN = "Return.annualized")[[1]]
factor.data$Lo.20.Rolling <- apply.rolling(factor.data[c("Lo.20")], width=12*5, FUN = "Return.annualized")[[1]]

factor.data$Hi.30.Rolling <- apply.rolling(factor.data[c("Hi.30")], width=12*5, FUN = "Return.annualized")[[1]]
factor.data$Lo.30.Rolling <- apply.rolling(factor.data[c("Lo.30")], width=12*5, FUN = "Return.annualized")[[1]]

factor.data$Hi.Lo.10.Rolling <- factor.data$Hi.10.Rolling - factor.data$Lo.10.Rolling
factor.data$Hi.Lo.20.Rolling <- factor.data$Hi.20.Rolling - factor.data$Lo.20.Rolling
factor.data$Hi.Lo.30.Rolling <- factor.data$Hi.30.Rolling - factor.data$Lo.30.Rolling

temp1 <- na.omit( factor.data[c("DATE","Hi.Lo.10.Rolling","Hi.Lo.20.Rolling","Hi.Lo.30.Rolling")] )

temp1$DATE <- factor(temp1$DATE)

factor.long <- gather(temp1,strategy,perf,Hi.Lo.10.Rolling,Hi.Lo.20.Rolling,Hi.Lo.30.Rolling)

pd <- levels(factor.long$DATE)
sz <- length(pd)

indexes <- seq(from=1,to=length(pd),by=7*12)
breaks <- c(pd[indexes])

years <- (indexes-1)/12+1931

g <- ggplot(factor.long,aes(x=DATE,y=perf,group=strategy,colour=strategy,fill=strategy))
g <- g + geom_line()
g <- g + geom_hline(aes(yintercept=0.05),color="grey",linetype=2) 
g <- g + scale_x_discrete(breaks = breaks, labels=years)
g <- g + scale_y_continuous("Annualized 5 Year Rolling Value Premium",labels=percent)
g <- g + guides(fill=guide_legend(title=NULL)) +
  scale_colour_discrete(labels=c("Top/Bottom 10%","Top/Bottom 20%","Top/Bottom 30%")) +
  theme(legend.position=c(1,1),legend.justification=c(1,1))

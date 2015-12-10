#!/usr/bin/env Rscript
# Prototype R script for use at command line in Linux, Mac OS X, UNIX

library(ggplot2)
library(plyr)
library(RColorBrewer)
library(scales)
library(grid)
library(gridExtra)

data <- read.table("data-all.dat",sep = " ", header = TRUE,na.strings="NULL")

aspect.ratio <- 0.5

x.axis   <- seq(0.01,0.99,by=0.01)
ymin     <- 0.10
ymax     <- 0.18
ratio    <- (1/(ymax-ymin))*aspect.ratio # This will give golden ratio for chart
data$X   <- data$fund_rtn
Label    <- 'Annualized Return'
title    <- paste("Simulated",Label,"as a Function of Portfolio Size")
labels   <- percent # function (x) { x } # percent

size     <- length(x.axis)
repeats  <- nrow(data)/size

data$quantile <- rep(x.axis,repeats)

# None of the below should need to be changed per run

cdata1 <- ddply(data,c("quantile"),summarise,
                N=length(X),type="fund",
                value=median(X),
                lo=quantile(X,0.05),
                hi=quantile(X,0.95))
mrkt   <- cdata1$value[[length(cdata1$value)]]

cdata2 <- data.frame(quantile = x.axis                  ,
                     N        = rep(1,size)             ,
                     type     = rep("ewuv",size)        ,
                     value    = mrkt                    ,
                     lo       = rep(NA,size)            ,
                     hi       = rep(NA,size)            )

cdata <-  rbind.fill(cdata1,cdata2)

cdata$type <- factor( cdata$type, levels=c("fund","ewuv"),
                     labels=c("Earnings Yield","EW Universe") ) 

g <- ggplot(cdata,aes(x=quantile,group=type,colour=type,shape=type,linetype=type))

g <- g + geom_line(aes(y=value)) # ,alpha=0.5)

g <- g + geom_ribbon(aes(ymin=lo,ymax=hi,fill=type),
                     alpha=0.1,linetype = 2,colour='#617994',fill='#34BBA7')

g <- g + scale_x_continuous(element_blank(),labels=percent)

g <- g + scale_y_continuous(Label,labels=labels,limits=c(ymin,ymax))

g <- g + ggtitle("")

g <- g + theme(legend.title=element_blank() ,
               legend.position=c(1,1)       ,
               legend.justification=c(1,1)  ,
               plot.margin=unit(c(4,1,0,0),"line"))

g <- g + scale_color_manual(values=c('#617994','#DD592D'))

g <- g + scale_linetype_manual(values=c(1,6))

p <- g

# NOW DO THE WHOLE THING OVER AGAIN BUT WITH SHARPE RATIO

x.axis   <- seq(0.01,0.99,by=0.01)
ymin     <- 0.30
ymax     <- 0.60
ratio    <- (1/(ymax-ymin))*aspect.ratio # This will give golden ratio for chart
data$X   <- data$fund_sharpe
Label    <- 'Sharpe Ratio'
title    <- paste("Simulated",Label,"as a Function of Portfolio Size")
labels   <- function (x) { x } # percent

size     <- length(x.axis)
repeats  <- nrow(data)/size

data$quantile <- rep(x.axis,repeats)

# None of the below should need to be changed per run

cdata1 <- ddply(data,c("quantile"),summarise,N=length(X),type="fund",
                value=median(X),
                lo=quantile(X,0.05),
                hi=quantile(X,0.95))

mrkt   <- cdata1$value[[length(cdata1$value)]]

cdata2 <- data.frame(quantile = x.axis                  ,
                     N        = rep(1,size)             ,
                     type     = rep("ewuv",size)        ,
                     value    = mrkt                    ,
                     lo       = rep(NA,size)            ,
                     hi       = rep(NA,size)            )

cdata <-  rbind.fill(cdata1,cdata2)

cdata$type <- factor( cdata$type,
                     levels=c("fund","ewuv"),
                     labels=c("Earnings Yield","EW Universe") ) 

g <- ggplot(cdata,aes(x=quantile,group=type,colour=type,shape=type,linetype=type))

g <- g + geom_line(aes(y=value)) # ,alpha=0.5)

g <- g + geom_ribbon(aes(ymin=lo,ymax=hi,fill=type),
                     alpha=0.1,linetype = 2,colour='#617994',fill='#34BBA7')

g <- g + scale_x_continuous(element_blank(),labels=percent)

g <- g + scale_y_continuous(Label,labels=labels,limits=c(ymin,ymax))

g <- g + theme(legend.title=element_blank() ,
               legend.position=c(1,1)       ,
               legend.justification=c(1,1)  ,
               plot.margin=unit(c(0,1,5,0.4),"line"))

g <- g + scale_color_manual(values=c('#617994','#DD592D'))

g <- g + scale_linetype_manual(values=c(1,6))

pdf("both_charts.pdf")

grid.arrange(p,g, nrow=2)

dev.off()

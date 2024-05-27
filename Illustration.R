#########################################################
#### Illustration of the Applicative example of the paper:
###Titled:  New Risk Measures Derived 
###        from the $g$- and $h$-Index
### Authors: F. Bartolucci, F. Pennoni, F. Cortese (2024)
#########################################################
rm(list=ls())
if(!("tidyverse"%in%installed.packages())) install.packages("tydiverse")
if(!("dplyr"%in%installed.packages())) install.packages("dplyr")
if(!("ggplot2"%in%installed.packages())) install.packages("ggplot2")

# Load functions and data -------------------------------------------------
source("DD.R")
source("gh_index.R")
source("port_weights.R")

load("example_data.RData") 
library(ggplot2) 
library(tidyquant) 
library(dplyr)

# Compute drawdowns of type 1, 2 and 3 for each asset ---------------------------------------------

dd1_asset1=DD(example_data$Asset1,type=1,out.plot = T)
dd1_asset2=DD(example_data$Asset2,type=1,out.plot = T)
dd1_asset3=DD(example_data$Asset3,type=1,out.plot = T)
dd1_asset4=DD(example_data$Asset4,type=1,out.plot = T)

dd2_asset1=DD(example_data$Asset1,type=2,out.plot = T)
dd2_asset2=DD(example_data$Asset2,type=2,out.plot = T)
dd2_asset3=DD(example_data$Asset3,type=2,out.plot = T)
dd2_asset4=DD(example_data$Asset4,type=2,out.plot = T)

dd3_asset1=DD(example_data$Asset1,type=3,out.plot = T)
dd3_asset2=DD(example_data$Asset2,type=3,out.plot = T)
dd3_asset3=DD(example_data$Asset3,type=3,out.plot = T)
dd3_asset4=DD(example_data$Asset4,type=3,out.plot = T)


# Plot drawdowns of type 1 2 and 3 for the first asset ------------------------------------------------

# Plots similar to those of Figures 1, 2 and 3 of the paper 
dd1_asset1$plot
dd2_asset1$plot
dd3_asset1$plot

# Compute g- and h-indices based on drawdown of type 1, 2 and 3 --------------------

gh1_asset1=ghindex(-dd1_asset1$dd,out.plot = F)
gh1_asset2=ghindex(-dd1_asset2$dd,out.plot = F)
gh1_asset3=ghindex(-dd1_asset3$dd,out.plot = F)
gh1_asset4=ghindex(-dd1_asset4$dd,out.plot = F)

gh2_asset1=ghindex(-dd2_asset1$dd,out.plot = F)
gh2_asset2=ghindex(-dd2_asset2$dd,out.plot = F)
gh2_asset3=ghindex(-dd2_asset3$dd,out.plot = F)
gh2_asset4=ghindex(-dd2_asset4$dd,out.plot = F)

gh3_asset1=ghindex(-dd3_asset1$dd,out.plot = F)
gh3_asset2=ghindex(-dd3_asset2$dd,out.plot = F)
gh3_asset3=ghindex(-dd3_asset3$dd,out.plot = F)
gh3_asset4=ghindex(-dd3_asset4$dd,out.plot = F)

# Arrange all results in a dataframe
ghs=data.frame(Asset1=c(gh1_asset1$g,gh1_asset1$h,gh2_asset1$g,gh2_asset1$h,gh3_asset1$g,gh3_asset1$h),
               Asset2=c(gh1_asset2$g,gh1_asset2$h,gh2_asset2$g,gh2_asset2$h,gh3_asset2$g,gh3_asset2$h),
               Asset3=c(gh1_asset3$g,gh1_asset3$h,gh2_asset3$g,gh2_asset3$h,gh3_asset3$g,gh3_asset3$h),
               Asset4=c(gh1_asset4$g,gh1_asset4$h,gh2_asset4$g,gh2_asset4$h,gh3_asset4$g,gh3_asset4$h))

rownames(ghs)=c("g1","h1","g2","h2","g3","h3")

# Results shown in a similar way as in Table 2 of the paper
ghs

# Compute returns ---------------------------------------------------------

example_data.ret=apply(example_data[,-1],2,function(x)c(NA,diff(log(x))))
example_data.ret=data.frame(Date=1:dim(example_data)[1],example_data.ret)
colnames(example_data.ret)=c("Date","Asset1","Asset2","Asset3","Asset4")

# Dynamic computation of risk measures for asset 1 ------------------------------------

asset1.mw=gh_mw(example_data$Asset1,example_data.ret$Asset1,MW=30)

# Results shown as in Figures 7 to 10 of the paper

plot(asset1.mw$g1,main="Asset 1 - g1 index",type='l',
     ylab = "g1",xlab = "Time",lwd=2,col="red4")
plot(asset1.mw$h1,main="Asset 1 - h1 index",type='l',
     ylab = "h1",xlab = "Time",lwd=2,col="red1")
plot(asset1.mw$g2,main="Asset 1 - g2 index",type='l',
     ylab = "g2",xlab = "Time",lwd=2,col="green4")
plot(asset1.mw$h2,main="Asset 1 - h2 index",type='l',
     ylab = "h2",xlab = "Time",lwd=2,col="green1")
plot(asset1.mw$g3,main="Asset 1 - g3 index",type='l',
     ylab = "g3",xlab = "Time",lwd=2,col="blue4")
plot(asset1.mw$h3,main="Asset 1 - h3 index",type='l',
     ylab = "h3",xlab = "Time",lwd=2,col="blue1")
plot(asset1.mw$std,main="Asset 1 - Standard deviation",type='l',
     ylab = "Standard deviation",xlab = "Time",lwd=2,col="black")
plot(asset1.mw$VaR,main="Asset 1 - VaR",type='l',
     ylab = "VaR",xlab = "Time",lwd=2,col="grey50")


# Portfolio analysis ------------------------------------------------------

port.an100=port_weights(example_data,example_data.ret,in.sample=250, 
                     out.sample=100)
port.an200=port_weights(example_data,example_data.ret,in.sample=250, 
                     out.sample=200)

# Results shown as in Table 3 of the paper

rbind(port.an100$port.returns,port.an200$port.returns)


#########################################################
#### Function to perform a dynamic analyses with the g- and h-index according to drawdown of type 1, 2 and 3 proposed in the paper:
###Titled: New Risk Measures Derived 
###        from the g- and h-index
### Authors: F. Bartolucci, F. Pennoni, F. Cortese (2024)
#########################################################


port_weights=function(all.data,all.data.ret,in.sample=1045, 
                      out.sample=c(100,200,400)){
  
  # This function computes the portfolio weights, and relative portfolio returns, for the Maillard et al. (2010) strategy,
  # considering the following risk measures: g, h indices (for each drawdown type), VaR and sd. 
  
  # Arguments:
  # all.data: data frame with the time series of the assets prices, first column is the date
  # all.data.ret: data frame with the time series of the assets returns, first column is the date
  # in.sample: number of observations to use for in-sample estimation
  # out.sample: vector with the number of observations to use for out-of-sample estimation (estimation is made at time in.sample+out.sample[i])
  
  # Value:
  # list with the following elements:
  # maill.weights: dataframe with the portfolio weights for each asset
  # port.returns: dataframe with the portfolio returns for each out-of-sample period
  # risk.meas: dataframe with the risk measures for each asset
  # in.sample: vector with the last date of the in-sample period
  # out.sample: vector with the dates of the out-of-sample periods
  
  # Compute each drawdown type for each asset
  all.data.insample=all.data[1:in.sample,]
  dd1=apply(all.data.insample[,-1],2,DD,type=1,out.plot=F)
  dd2=apply(all.data.insample[,-1],2,DD,type=2,out.plot=F)
  dd3=apply(all.data.insample[,-1],2,DD,type=3,out.plot=F)
  
  all.data.ret.insample=all.data.ret[1:in.sample,]
  
  # compute g, h indexes, VaR and sd, for each asset, for each drawdown type
  g1=rep(0,length(dd1))
  h1=rep(0,length(dd1))
  g2=rep(0,length(dd1))
  h2=rep(0,length(dd1))
  g3=rep(0,length(dd1))
  h3=rep(0,length(dd1))
  
  # Percentage sd and VaR
  pstd=rep(0,length(dd1))
  pVaR=rep(0,length(dd1))
  
  for(i in 1:length(dd1)){
    temp=ghindex(-dd1[[i]]$dd,out.plot=F)
    g1[i]=temp$g
    h1[i]=temp$h
    temp=ghindex(-dd2[[i]]$dd,out.plot=F)
    g2[i]=temp$g
    h2[i]=temp$h
    temp=ghindex(-dd3[[i]]$dd,out.plot=F)
    g3[i]=temp$g
    h3[i]=temp$h
    
    pstd[i]=sd(all.data.ret.insample[,(i+1)],na.rm=T)*100
    pVaR[i]=(-VaR(R=all.data.ret.insample[-1,(i+1)],method="historical",p=.95)*100)
    
  }
  
  # Join all risk measures in a data frame
  all.risk.meas=rbind(g1,h1,g2,h2,g3,h3,pstd,pVaR)
  all.risk.meas=data.frame(all.risk.meas)
  colnames(all.risk.meas)=colnames(all.data)[-1]
  
  # Compute weights as in Maillard et al. (2010)
  maill.weights=1/all.risk.meas
  
  maill.weights[sapply(maill.weights, is.infinite)] <- 0
  
  temp=unlist(rowSums(maill.weights))
  maill.weights=maill.weights/temp
  
  # Compute portfolio returns at times in.sample+out.sample
  port.ret=matrix(0,nrow=nrow(maill.weights),ncol=length(out.sample))
  
  for(t in 1:length(out.sample)){
    port.ret[,t]=as.matrix(maill.weights)%*%t(all.data.ret[in.sample+out.sample[t],-1])*100
  }
  
  port.ret=data.frame(Date=all.data$Date[in.sample+out.sample],t(port.ret))
  colnames(port.ret)[-1]=rownames(maill.weights)
  
  
  return(list(maill.weights=maill.weights,
              port.returns=port.ret,
              risk.meas=all.risk.meas,
              in.sample=all.data$Date[in.sample],
              out.sample=all.data$Date[in.sample+out.sample]
  )
  )
  
}
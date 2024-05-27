#########################################################
#### Function to compute drawdown of type 1, 2 and 3 proposed in the paper:
###Titled: New Risk Measures Derived 
###        from the g- and h-index
### Authors: F. Bartolucci, F. Pennoni, F. Cortese (2024)
#########################################################

# FP cambiato qui co
DD = function(x,type,out.plot=F){
  
  # This function computes the drawdowns based on local maxima of type 1,2 or 3.
  
  # Arguments:
  # x is the series of prices
  # type is the desired type of local maximum 
  # out.plot is a logical variable to plot the results
  
  # Value:
  # List with the drawdowns, if out.plot is TRUE, it also returns a plot for the drawdowns.
  # tmax is a vector with times of the local maxima, tmin is a vector with times of the local minima.
  
  TT=length(x)
  
  switch(type, 
         '1'={
           tmin = tmax = NULL
           dd = NULL
           for(t in 1:(TT-1)){
             if(x[t]>x[t+1]){
               tmax = c(tmax,t)
               tmin = c(tmin,t+1)
               dd = c(dd,100*(x[t+1]/x[t]-1))
             }
             
           }
         },
         '2'={
           tmin = tmax = NULL
           dd = NULL
           for(t in 2:(TT-1)){
             if(x[t]<x[t-1] & x[t]<x[t+1]){
               tmin = c(tmin,t)
               if(!is.null(tmax)) dd = c(dd,100*(x[t]/x[tpmax]-1))
             }
             if(x[t]>x[t-1] & x[t]>x[t+1]){
               tmax = c(tmax,t)
               tpmax = t
             }
           }
         },
         '3'={
           tmax = NULL
           for(t in 2:(TT-1)){
             if(all(x[t]>x[1:(t-1)]) & x[t]>x[t+1]) tmax = c(tmax,t)
           }
           if(is.null(tmax)){
             tmax=1
           }
           tmin = NULL
           dd = NULL
           if(length(tmax)==1){
             tmin=which.min(x)
             dd=100*(x[tmin]/x[tmax]-1)
           }
           else{
             for(j in 2:length(tmax)){
               t1 = tmax[j-1]+which.min(x[(tmax[j-1]+1):(tmax[j]-1)]) #time of the jth loc min
               tmin = c(tmin,t1)
               dd = c(dd,100*(x[t1]/x[tmax[j]]-1))
             }
           }
         },
         {
           return(print('Not available'))
         }
  )
  if(out.plot){
    maxTime=rep(0,length(x))
    maxTime[tmax]=1
    minTime=rep(0,length(x))
    minTime[tmin]=2
    data=data.frame(Price=x,Time=1:length(x),category=as.factor(maxTime+minTime))
    if(type==1){
      data$category=recode(data$category,"1"="max","2"="min","3"="both")
      plot=ggplot(data=data,aes(x=Time))+
        geom_line(aes(y=Price))+
        geom_point(aes(y=Price,color=category,shape=category),size=3)+
        scale_color_manual(breaks=c("max","min","both"),values = c("0"="grey1","max" = "green3", "min" = "red2","both"="yellow2"))+
        scale_shape_manual(guide="none",values = c(1,16,16,16))+ 
        theme_bw()+ theme(axis.text=element_text(size=18),
                          axis.title = element_text(size=18),
                          legend.position = "top",
                          legend.title = element_blank(),
                          legend.text = element_text(size=16))+ 
        scale_fill_discrete(labels=c("none",'max', 'min', 'both'))
    }
    else{
      data$category=recode(data$category,"1"="max","2"="min")
      plot=ggplot(data=data,aes(x=Time))+
        geom_line(aes(y=Price))+
        geom_point(aes(y=Price,color=category,shape=category),size=3)+
        scale_color_manual(breaks=c("max","min"),values = c("0"="grey1","max" = "green3", "min" = "red2"))+
        scale_shape_manual(guide="none",values = c(1,16,16,16))+ 
        theme_bw()+ theme(axis.text=element_text(size=18),
                          axis.title = element_text(size=18),
                          legend.position = "top",
                          legend.title = element_blank(),
                          legend.text = element_text(size=16))+ 
        scale_fill_discrete(labels=c("none",'max', 'min'))
    }
    
    return(list(dd=dd,tmin=tmin,tmax=tmax,plot=plot))
  }
  else{
    return(list(dd=dd,tmin=tmin,tmax=tmax))
  }
}

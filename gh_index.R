#########################################################
#### Function to compute g- and h-index according to drawdown of type 1, 2 and 3 proposed in the paper:
###Titled: New Risk Measures Derived 
###        from the g- and h-index
### Authors: F. Bartolucci, F. Pennoni, F. Cortese (2024)
#########################################################

ghindex <- function(x,out.plot=T,legend=F
                    #,...
){
  
  # Function to compute g and h indexes.
  
  # Arguments:
  # x is the series of drawdowns
  # out.plot is a logical variable to plot the results
  # legend is a logical variable to show the legend
  
  # Value:
  # List with g and h indexes, if out.plot is TRUE, it also returns a plot for the two indexes.
  
  n = length(x)
  sx = sort(x,decreasing = TRUE)
  # If all drawdowns are less than 1, then g=h=0
  if(all(sx<1)){
    h = 0
    g = 0
  }
  else{
    h = max(which(sx>=1:n)) 
    g = max(which(cumsum(sx)>=(1:n)^2)) 
  }
  mx = cumsum(sx)/(1:n)
  # plot histogram
  if(out.plot){
    dat=data.frame(sx=sx,mx=mx,ind=1:length(x))
    wts=data.frame(ind=c(0,sx[1]),y=c(0,sx[1]))
  
    plot=ggplot(data = dat, aes(x = ind)) +
      geom_line(aes(y=mx,color="g-"),size=1.1) +
      geom_line(aes(y=sx,color="h-"),size=1.1) +
      geom_point(aes(y=mx,color="g-"),size=2)+
      geom_point(aes(y=sx,color="h-"),size=2)+
      geom_line(aes(y=ind),size=.8) +
      geom_vline(xintercept = g, linetype="dotted", 
                 color = "#F8766D", size=1.5)+
      geom_vline(xintercept = h, linetype="dotted", 
                 color = "#00BFC4", size=1.5)+
      geom_text(aes(x = g, y = 0, label = "o",color="g-"),size=8, show.legend = FALSE)+
      geom_text(aes(x = h, y = 0, label = "o",color="h-"),size=8, show.legend = FALSE)+
      scale_y_continuous(limits=c(0,sx[1]+1),
                         breaks=round(seq(0,sx[1]+1,length.out=5))
      )+
      scale_x_continuous(limits=c(0,max(g,h)+2),
                         breaks=round(seq(0,max(g,h)+2,by=3)))+
      labs(x = " ", y= " ",colour = "Index")+
      theme_bw()+ theme(axis.text=element_text(size=14),
                        legend.position = c(0.9, 0.8),
                        legend.title = element_text(size=14), #change legend title font size
                        legend.text = element_text(size=14))
    if(legend==F){
      plot=plot+theme(axis.text=element_text(size=14),
                      legend.position = "none")
    }
    out = list(g=g,h=h,zoomplot=plot)
    return(out)
  }
  # return output
  else{
    out = list(g=g,h=h)
    return(out)
  }
}






# moving window ------------------------------------------------------------------


gh_mw=function(x,rx,MW=500,out.plot=F){
  
  # This function computes the g and h indexes, VaR and sd, for a moving window of size MW.
  
  # Arguments:
  # x is the series of prices.
  # rx is the series of returns.
  # MW is the size of the moving window.
  # out.plot is a logical variable to plot the results.
  
  # Value:
  # List with the g and h indexes, VaR and sd.
  
  N=length(x)

  g1=rep(0,N-MW)
  g2=rep(0,N-MW)
  g3=rep(0,N-MW)
  
  h1=rep(0,N-MW)
  h2=rep(0,N-MW)
  h3=rep(0,N-MW)
  
  std=rep(0,N-MW)
  VaR=rep(0,N-MW)
  
  for(i in 1:(N-MW)){
    mw=(1:MW)+i-1

    dd1=DD(x[mw],type=1)
    dd1=-dd1$dd
    gh1=ghindex(dd1,out.plot = out.plot)
    
    dd2=DD(x[mw],type=2)
    dd2=-dd2$dd
    gh2=ghindex(dd2,out.plot = out.plot)
    
    dd3=DD(x[mw],type=3)
    dd3=-dd3$dd
    gh3=ghindex(dd3,out.plot = out.plot)
    
    g1[i]=gh1$g
    h1[i]=gh1$h
    g2[i]=gh2$g
    h2[i]=gh2$h
    g3[i]=gh3$g
    h3[i]=gh3$h

    std[i]=(sd(rx[mw])*100)
    VaR[i]=(-VaR(R=rx[mw],method="historical")*100)
  }
  
  # Create dataframe with all results
  res=data.frame(g1=g1,g2=g2,g3=g3,h1=h1,h2=h2,h3=h3,std=std,VaR=VaR)
  
  return(res)
}


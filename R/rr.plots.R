rr.plots <- function(set.ds1, set.ds2, set.ds3){
  p2 <- RR_plot(ds=set.ds1 ) + 
    facet_wrap(~agec, scales='fixed')
  
  p4 <- RR_plot(ds=set.ds2[ set.ds2$race_recode=='1' & set.ds2$sex=='M',] ,datemin='2020-01-01') + 
    facet_grid(agec~region, scales='fixed')
  
  p6 <- RR_plot(ds=set.ds3[set.ds3$agec !='Under 25 Years',], datemin='2020-01-01') + 
    facet_grid(agec~region, scales='fixed')
  plot.list =list('p2'=p2,'p4'=p4,'p6'=p6)
  return(plot.list)
  
}
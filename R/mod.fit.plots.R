
mod.fit.plots <- function(set.ds1,set.ds2, set.ds3 ){
  p1 <- obs_exp_plot(ds=set.ds1, xvar='date',yvar='obs_inc', groupvar='agec', colvar='agec') + 
    facet_wrap(~agec, scales='free_y')
  
  p3 <- obs_exp_plot(ds=set.ds2[ set.ds2$race_recode=='1' & set.ds2$sex=='M',], xvar='date',yvar='obs_inc', groupvar='agec', colvar='agec', datemin='2020-01-01')  + 
    facet_grid(agec~region, scales='free_y')
  
  p5 <- obs_exp_plot(ds=set.ds3[ set.ds3$agec !='Under 25 Years',], xvar='date',yvar='obs_inc', groupvar='agec', colvar='agec', datemin='2020-01-01') + 
    facet_grid(agec~region, scales='free_y')
  
    plot.list =list('p1'=p1,'p3'=p3,'p5'=p5)
  return(plot.list)
}

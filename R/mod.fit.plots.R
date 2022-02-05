
mod.fit.plots <- function(){
  p1 <- obs_exp_plot(ds=out.ds$preds.age, xvar='date',yvar='obs_inc', groupvar='agec', colvar='agec') + 
    facet_wrap(~agec, scales='free_y')
  
  p3 <- obs_exp_plot(ds=out.ds$preds.age.race.region[ out.ds$preds.age.race.region$race_recode=='1' &out.ds$preds.age.race.region$sex=='M',], xvar='date',yvar='obs_inc', groupvar='agec', colvar='agec', datemin='2020-01-01')  + 
    facet_grid(agec~region, scales='free_y')
  
  p5 <- obs_exp_plot(ds=out.ds$preds.age.region[ out.ds$preds.age.region$agec !='Under 25 Years',], xvar='date',yvar='obs_inc', groupvar='agec', colvar='agec', datemin='2020-01-01') + 
    facet_grid(agec~region, scales='free_y')
  
    plot.list =list('p1'=p1,'p3'=p3,'p5'=p5)
  return(plot.list)
}

rr.plots <- function(){
  p2 <- RR_plot(ds=out.ds$preds.age ) + 
    facet_wrap(~agec, scales='fixed')
  
  p4 <- RR_plot(ds=out.ds$preds.age.race.region[ out.ds$preds.age.race.region$race_recode=='1' & out.ds$preds.age.race.region$sex=='M',] ,datemin='2020-01-01') + 
    facet_grid(agec~region, scales='fixed')
  
  p6 <- RR_plot(ds=out.ds$preds.age.region[out.ds$preds.age.region$agec !='Under 25 Years',], datemin='2020-01-01') + 
    facet_grid(agec~region, scales='fixed')
  plot.list =list('p2'=p2,'p4'=p4,'p6'=p6)
  return(plot.list)
  
}
RR_plot <- function(ds, xvar,  groupvar, colvar, add.CIs=T,datemin='2014-01-01', ylab1="Rate Ratio"){
  p1 <- ds %>%
    mutate('agec'=as.factor(agec)) %>%
    ggplot(aes(x=date, y=RR_median , group=source, col=source)) +
    geom_line() +
    #  geom_point()+
    ylab(ylab1) +
    xlab("Date") +
    theme_classic() +
    theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
    geom_hline(yintercept=0, col='gray', lty=2) +
    geom_vline(xintercept=as.Date('2019-11-01'), col='gray', lty=2) +
    geom_hline(yintercept=1, col='gray', lty=2) +
    
        xlim(as.Date(datemin), as.Date('2020-12-31')) +
    ylim(0.9,1.75)+
    theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) 

  if(add.CIs==T){
   p1 +  geom_ribbon(aes(ymin=RR_lcl, ymax=RR_ucl), alpha=0.2, col='gray') 
      
  }  
  return(p1)
}

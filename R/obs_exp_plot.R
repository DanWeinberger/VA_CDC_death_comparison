obs_exp_plot <- function(ds, xvar, yvar, groupvar, colvar, datemin='2014-01-01', ylab1="Number of all-cause deaths"){
  ds %>%
    mutate('agec'=as.factor(agec)) %>%
    ggplot(aes_string(x=xvar, y=yvar , group=groupvar, col=colvar)) +
    geom_line() +
    #  geom_point()+
    geom_line(aes(x=date, y=pred_median, group=agec), lty=2, col='black') +
    geom_ribbon(aes(ymin=pred_lcl, ymax=pred_ucl), alpha=0.2, col='gray') +
    ylab(ylab1) +
    xlab("Date") +
    theme_classic() +
    theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
    geom_hline(yintercept=0, col='gray', lty=2) +
    geom_vline(xintercept=as.Date('2019-11-01'), col='gray', lty=2) +
    xlim(as.Date(datemin), as.Date('2020-12-31')) +
    theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) 
}
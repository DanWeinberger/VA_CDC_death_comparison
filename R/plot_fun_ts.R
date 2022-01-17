plot.fun <- function(ds=ds1,xvar='date', yvar, ylab='N deaths',plot.type='line'){
  ggplot(ds, aes_string(x=xvar, y=yvar)) +
    {if(plot.type=='line')geom_line() } +
    {if(plot.type=='point')geom_point() } +
    ylab(ylab) +
    xlab("Date") +
    theme_classic() +
    facet_wrap(agec~region , scales='free') +
    theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
    geom_hline(yintercept=0, col='gray', lty=2) 
}

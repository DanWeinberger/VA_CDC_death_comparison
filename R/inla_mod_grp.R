inla_mod_group <- function(select.grp, ds.in){
  
  # for(i in unique(ds.in$age_region_source)){
  #   print(i)
  #   select.grp=i
    
   ds <- ds.in[ds.in$age_region_source==select.grp,]
  

  test1 <- inla(all_cause_pre ~ 
                   time_scale + sin1 + sin2 + sin3 + cos1 +cos2+ cos3  , family='nbinomial', data=ds,
                control.predictor=list(link=1, compute=TRUE),
                #control.fixed = list(mean=0,mean.intercept=8.6,prec.intercept=1, prec=1),
                control.family=list(link='log'),
                control.compute=list(config = TRUE))
  
  pred.sample.list <- inla.posterior.sample(n=100, test1, seed=123)
  
  pred.interval.func <- function(sample.ds, dist=c('nb','poisson')){
    mu1 <- exp(sample.ds$latent[grep('Predictor', row.names(sample.ds$latent))])
    if(dist=='nb'){
      nb.size1 = sample.ds$hyperpar
      pred <- replicate(10, rnbinom(n=length(mu1), mu=mu1, size=nb.size1), simplify = 'array')
    }else{
      pred <- replicate(10, rpois(n=length(mu1), lambda=mu1), simplify = 'array')
    }
    return(pred)
  }
  posterior.preds <- sapply(pred.sample.list,pred.interval.func,dist='nb', simplify='array')
  posterior.preds.m <- reshape2::melt(posterior.preds)
  posterior.preds.c <- reshape2::dcast(posterior.preds.m, Var1 ~ Var3 + Var2) 
  posterior.preds.c$Var1 <- NULL
    
  colnames(posterior.preds.c) <- paste0('ColA', 1: ncol(posterior.preds.c))
  posterior.preds.df <- cbind.data.frame(ds, posterior.preds.c)

  #Pred by date
  preds.summary <- (t(apply(posterior.preds.c,1, quantile, probs=c(0.025,0.5,0.975))))
  preds.summary <- as.data.frame(preds.summary)
  names(preds.summary) <- c('pred_lcl','pred_mean','pred_ucl')
  preds.summary$age_region_source <- select.grp
  preds.summary$all_cause <- ds$all_cause
  preds.summary$week_end <- ds$week_end
  

  options(dplyr.summarise.inform = FALSE)
  

   posterior.preds.df.qtr <- posterior.preds.df %>%
      group_by(year ,quarter) %>%
      summarise(across(c(all_cause,starts_with("ColA")), sum ) ) %>%
      ungroup() %>%
      rowwise() %>%
      mutate( pred_mean= mean(c_across(starts_with("ColA"))), pred_lcl= quantile(c_across(starts_with("ColA")),probs=0.025), pred_ucl= quantile(c_across(starts_with("ColA")),probs=0.975), age_region_source=select.grp ) %>%
      ungroup() %>%
      select(year, quarter, pred_mean, pred_lcl, pred_ucl,all_cause ,age_region_source)
    
  posterior.preds.total <- posterior.preds.df[is.na(ds$all_cause_pre),] %>%
    summarise(across(c(all_cause,starts_with("ColA")), sum ) ) %>%
    mutate( pred_mean= mean(c_across(starts_with("ColA"))), pred_lcl= quantile(c_across(starts_with("ColA")),probs=0.025),pred_ucl= quantile(c_across(starts_with("ColA")),probs=0.975) , age_region_source=select.grp) %>%
    select(pred_mean, pred_lcl, pred_ucl,all_cause,age_region_source )
  
  #}
  
  out.ls = list('preds_qtr'=posterior.preds.df.qtr, 'preds_week'=preds.summary, 'preds_total'=posterior.preds.total )
  return(out.ls)

}


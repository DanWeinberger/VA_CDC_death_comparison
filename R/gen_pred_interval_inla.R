gen_pred_interval_inla <- function(inla_obj, covar.df, mod.mat, source,log.offset1=NULL){
  
  nrep1=20
  nrep2=21
  
  r.samples = inla.posterior.sample(nrep1, inla_obj)
  
  all.names <- dimnames(r.samples[[1]]$latent)[[1]] 
  
  pred.names <- all.names[-grep('Predictor', all.names) ] 
  # pred.names.check <- gsub(':1','', pred.names)
  #  sum(pred.names.check==colnames(mod.mat)) #order is correct
  
  sample.ds1 <- sapply(r.samples, function(x){
    betas <- x$latent[pred.names,, drop=F]
    pred1 <- exp(as.vector(mod.mat %*% betas) + log.offset1 )
   #  pred1 <- exp(as.vector(mod.mat %*% betas)  )
    
    return(pred1)
  }, simplify='array')
  
  mod.family <- inla_obj$.args$family
  
  sample.ds1.m <- melt(sample.ds1) #vectorize...
  sample.ds1.m.rep <- sapply(sample.ds1.m,rep.int,times=nrep2)
  
  if(mod.family=='nb'){
    nb.size1 = sample.ds$hyperpar
    pred <- rnbinom(n=length(sample.ds1.m.rep[,3]), mu=sample.ds1.m.rep[,3], size=nb.size1)
  }else{
    pred <- rpois(n=length(sample.ds1.m.rep[,3]), lambda=sample.ds1.m.rep[,3])
  }
  
  pred.df <- cbind.data.frame('id1'=sample.ds1.m.rep[,1],'rep'=rep(1:(nrep1*nrep2) , each=nrow(mod.mat)), 'pred'=pred)
  
  posterior.preds.c <- reshape2::dcast(pred.df, id1~rep, value.var='pred')
  posterior.preds.c$id1 <- NULL
  
  colnames(posterior.preds.c) <- paste0('ColA', 1: ncol(posterior.preds.c))
  posterior.preds.df <- cbind.data.frame(covar.df, posterior.preds.c)
  
  #Pred by date
  preds.summary <- (t(apply(posterior.preds.c,1, quantile, probs=c(0.025,0.5,0.975))))
  preds.summary <- as.data.frame(preds.summary)
  names(preds.summary) <- c('pred_lcl','pred_mean','pred_ucl')
  preds.summary <- cbind.data.frame(preds.summary,covar.df)
  
  
  #sex, race_recode, agec
  preds_covars <- cbind.data.frame(posterior.preds.c, covar.df[,c('sex','region', 'race_recode','agec', 'date','N_deaths')])
  preds_covars.m <- melt(preds_covars, id.vars=c('sex','region', 'race_recode','agec', 'date','N_deaths'))
  
  preds.summary$source <- source
  preds_covars.m$source <- source
  
    out.list= list('preds_time'=preds.summary,'preds_covars.m'=preds_covars.m)
  return(out.list)
}


# 
# 
# pred.interval.func <- function(sample.ds,  dist=c('nbinomial','poisson')){
#   mu1 <- exp(sample.ds$latent[grep('Predictor', row.names(sample.ds$latent))])
#   if(dist=='nbinomial'){
#     nb.size1 = sample.ds$hyperpar
#     pred <- replicate(10, rnbinom(n=length(mu1), mu=mu1, size=nb.size1), simplify = 'array')
#   }else{
#     pred <- replicate(10, rpois(n=length(mu1), lambda=mu1), simplify = 'array')
#   }
#   return(pred)
# }


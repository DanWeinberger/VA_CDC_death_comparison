gen_pred_interval_inla_ridge_ar1 <- function(inla_obj, X,Za,Zb, Zc,covar.df, mod.mat, source,log.offset1=NULL){
  
  nrep1=1000
  nrep2=1
  
  r.samples = inla.posterior.sample(nrep1, inla_obj)
  
  all.names <- dimnames(r.samples[[1]]$latent)[[1]] 
  
  X.names <- paste0(dimnames(X)[[2]], ':1')
  Za.names <- all.names[grep('idx.Za', all.names) ][-c(1:nrow(Za))] 
  Zb.names <- all.names[grep('idx.Zb', all.names) ][-c(1:nrow(Zb))] 
  Zc.names <- all.names[grep('idx.Zc', all.names) ][-c(1:nrow(Zc))] 

  t.names <- all.names[which(substring(all.names,1,2)=='t:')] #ar1 component from training data
  
  # #Ar1 components
  # ar1.prec.pos <- grep('Precision for t', names(r.samples[[1]]$hyperpar)) 
  # ar1.rho.pos <- grep('Rho for t', names(r.samples[[1]]$hyperpar)) 
  # 
  # ar1.prec.samp <- sapply(r.samples, function(x) x$hyperpar[ar1.prec.pos])
  # ar1.rho.samp <- sapply(r.samples, function(x) x$hyperpar[ar1.rho.pos])
  # 
  # unique.t <- unique(mod.mat[, 'time_scale'])
  
  #AR1.obs is fit to the data pre-pandemic, then extrapolate after
  ar1.obs <- sapply(r.samples, function(x){
    x$latent[t.names,, drop=F]
  })
  #matplot(ar1.obs, type='l')
  
  ar1.obs <- as.data.frame(ar1.obs)
  names(ar1.obs) <- paste0('sampleAR', 1:ncol(ar1.obs))
    
  ar1.obs$date <- sort(unique(covar.df$date))


  pred.names <- c(X.names,Za.names,Zb.names,Zc.names)
  # pred.names.check <- gsub(':1','', pred.names)
  #  sum(pred.names.check==colnames(mod.mat)) #order is correct
  
  sample.ds1 <- sapply(r.samples, function(x){
    betas <- x$latent[pred.names,, drop=F]
    pred1 <- exp(as.vector(mod.mat %*% betas) + log.offset1 )
   #  pred1 <- exp(as.vector(mod.mat %*% betas)  )
    
    return(pred1)
  }, simplify='array')
  
  sample.ds1 <- as.data.frame(sample.ds1)
  sample.ds1$date <- covar.df$date
  
  covar.ar <- merge(sample.ds1, ar1.obs, by='date', sort=F) #ensure everything correctly matched by date
  
  sample.ds1$date <- NULL 
  
  covar.ar <- covar.ar[,grep('sampleAR', names(covar.ar)) ]
  
  sample.ds2 <- sample.ds1 *exp( covar.ar)
  
  mod.family <- inla_obj$.args$family
  
  sample.ds1.m <- melt(as.matrix(sample.ds2)) #vectorize...
  
  sample.ds1.m.rep <- sapply(sample.ds1.m, rep.int,times=nrep2)
  
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
  
  preds_covars.m <- cbind.data.frame(preds_covars.m, log.offset1)
  
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


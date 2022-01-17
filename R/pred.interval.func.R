pred.interval.func <- function(sample.ds,  dist=c('nb','poisson')){
  mu1 <- exp(sample.ds$latent[grep('Predictor', row.names(sample.ds$latent))])
  if(dist=='nb'){
    nb.size1 = sample.ds$hyperpar
    pred <- replicate(10, rnbinom(n=length(mu1), mu=mu1, size=nb.size1), simplify = 'array')
  }else{
    pred <- replicate(10, rpois(n=length(mu1), lambda=mu1), simplify = 'array')
  }
  return(pred)
}


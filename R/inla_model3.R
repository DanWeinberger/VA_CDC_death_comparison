inla_model3 <- function(ds,prec.prior1=1){
  #http://julianfaraway.github.io/brinla/examples/ridge.html
  #ds=ag3

  ds$t <- ds$year + ds$qtr/4 - 1/4 -2014
  ds$t <- ds$t / max(ds$t)
  

  X <- model.matrix(~
                             1 + time_scale + qtr2 + qtr3 +qtr4 +
                             agec + race_recode + sex +  subgroup_combo , data=ds)

  #using the : instead of * ensure main effect is not included, just the interaction. this is needed bc we have a fixed effect already
  Za <- model.matrix(~
                             -1+
                             agec:time_scale + agec:qtr2 + agec:qtr3 +agec:qtr4  , data=ds)
  Zb <- model.matrix(~
                               -1+
                               race_recode:time_scale + race_recode:qtr2 + race_recode:qtr3 +race_recode:qtr4  , data=ds)
    
  Zc <- model.matrix(~
                               -1+
                               sex:time_scale + sex:qtr2 + sex:qtr3 + sex:qtr4  , data=ds)

  mod.mat.comb <- cbind(X, Za, Zb, Zc)
  n <- nrow(ds)

  y <- ds$all_cause_pre
  formula <- y ~ -1 + offset(log_pop) + X +  f(idx.Za, model="z", Z=Za) +  f(idx.Zb, model="z", Z=Zb)+  f(idx.Zc, model="z", Z=Zc) + f(t,model='ar1')
  
  mod.inla2 <- inla(formula, data = list(y=y, idx.Za = 1:n,idx.Zb = 1:n, 
                                         idx.Zc = 1:n,  
                                         t=ds$t,
                                         X=X, 
                                         log_pop=ds$log_pop), 
                    family='poisson', 
                    control.predictor = list(compute=TRUE),
                    control.compute=list(config = TRUE, dic=T, waic=T))  
  

 waic = mod.inla2$waic$waic
  dic = mod.inla2$dic$dic
  
  all.ts <- sort(unique(ds$t))
  t.increment <- all.ts[length(all.ts)] - all.ts[length(all.ts)-1] 
  
  res1 <- gen_pred_interval_inla_ridge_ar1(inla_obj=mod.inla2, covar.df=ds,X=X,Za=Za,Zb=Zb, Zc=Zc, mod.mat=mod.mat.comb , source= unique(ds$source) ,log.offset1=ds$log_pop)

  preds.inla2= c(res1, 'waic'=waic, 'dic'=dic,'inla_obj'=mod.inla2)
  return(preds.inla2)
}

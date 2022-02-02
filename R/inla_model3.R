inla_model3 <- function(ds,prec.prior1=1){
  #http://julianfaraway.github.io/brinla/examples/ridge.html
  #ds=ag3

  mod.mat1 <- model.matrix(~
                             1 + time_scale + qtr2 + qtr3 +qtr4 +
                             agec + race_recode + sex +  subgroup_combo +
                             agec*(time_scale + qtr2 + qtr3 +qtr4) +
                             race_recode*(time_scale + qtr2 + qtr3 +qtr4)+
                             region*(time_scale + qtr2 + qtr3 +qtr4), data=ds)
  
  form1.X <- as.formula(all_cause_pre ~
                          1 + time_scale + qtr2 + qtr3 +qtr4 +
                          agec + race_recode + sex +  subgroup_combo +
                          agec*(time_scale + qtr2 + qtr3 +qtr4) +
                          race_recode*(time_scale + qtr2 + qtr3 +qtr4)+
                          region*(time_scale + qtr2 + qtr3 +qtr4)  +
                          offset(log_pop))
  X <- model.matrix(~
                             1 + time_scale + qtr2 + qtr3 +qtr4 +
                             agec + race_recode + sex +  subgroup_combo , data=ds)
 # X <- as(mod.mat.X, "sparseMatrix")

  
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
  formula <- y ~ -1 + offset(log_pop) + X +  f(idx.Za, model="z", Z=Za) +  f(idx.Zb, model="z", Z=Zb)+  f(idx.Zc, model="z", Z=Zc)
  
  mod.inla2 <- inla(formula, data = list(y=y, idx.Za = 1:n,idx.Zb = 1:n, 
                                         idx.Zc = 1:n, 
                                         X=X, 
                                         log_pop=ds$log_pop), 
                    family='poisson', 
                    control.predictor = list(compute=TRUE),
                    control.compute=list(config = TRUE, dic=T, waic=T))  
  

 waic = mod.inla2$waic$waic
  dic = mod.inla2$dic$dic
  
  res1 <- gen_pred_interval_inla_ridge(inla_obj=mod.inla2, covar.df=ds,X=X,Za=Za,Zb=Zb, Zc=Zc, mod.mat=mod.mat.comb , source= unique(ds$source) ,log.offset1=ds$log_pop)

  preds.inla2= c(res1, 'waic'=waic, 'dic'=dic)
  return(preds.inla2)
}

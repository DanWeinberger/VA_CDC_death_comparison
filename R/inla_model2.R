inla_model2 <- function(ds,prec.prior1=1){
  #http://julianfaraway.github.io/brinla/examples/ridge.html
  #ds=ag3
  form1 <- as.formula(all_cause_pre ~
                        1 + time_scale + qtr2 + qtr3 +qtr4 +
                        agec + race_recode + sex +  subgroup_combo +
                        agec*(time_scale + qtr2 + qtr3 +qtr4) +
                        race_recode*(time_scale + qtr2 + qtr3 +qtr4)+
                        region*(time_scale + qtr2 + qtr3 +qtr4)  +
                        offset(log_pop))
  
  
  mod.mat1 <- model.matrix(~
                             1 + time_scale + qtr2 + qtr3 +qtr4 +
                             agec + race_recode + sex +  subgroup_combo +
                             agec*(time_scale + qtr2 + qtr3 +qtr4) +
                             race_recode*(time_scale + qtr2 + qtr3 +qtr4)+
                             region*(time_scale + qtr2 + qtr3 +qtr4), data=ds)  
  mod.mat1 <- as(mod.mat1, "sparseMatrix")
  
  n <- nrow(ds)
  X <- matrix(1,nrow = n, ncol= 1)
  Z <- as.matrix(mod.mat1)[,-1] #remove intercept from Z 
  
  y <- ds$all_cause_pre
  formula <- y ~ -1 + offset(log_pop) + X +  f(idx.Z, model="z", Z=Z)
  
  mod.inla2 <- inla(formula, data = list(y=y, idx.Z = 1:n, X=X, log_pop=ds$log_pop), 
                    family='poisson', 
                    control.predictor = list(compute=TRUE),
                    control.compute=list(config = TRUE, dic=T, waic=T))  
  

 waic = mod.inla2$waic$waic
  dic = mod.inla2$dic$dic
  
  res1 <- gen_pred_interval_inla_ridge(inla_obj=mod.inla2, covar.df=ds,Z=Z, mod.mat=mod.mat1 , source= unique(ds$source) ,log.offset1=ds$log_pop)

  preds.inla2= c(res1, 'waic'=waic, 'dic'=dic)
  return(preds.inla2)
}

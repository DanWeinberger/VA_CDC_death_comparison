inla_model3 <- function(ds,prec.prior1=1){
  #http://julianfaraway.github.io/brinla/examples/ridge.html
  #ds=ag3

  ds$t <- ds$year + ds$qtr/4 - 1/4 -2014
  ds$t <- ds$t / max(ds$t)
  
  mod.mat1 <- model.matrix(~
                             1 + time_scale + qtr2 + qtr3 +qtr4 + pandemic_effect +
                             agec + race_recode + sex +  subgroup_combo +
                             agec*(time_scale + qtr2 + qtr3 +qtr4) +
                             race_recode*(time_scale + qtr2 + qtr3 +qtr4)+
                             region*(time_scale + qtr2 + qtr3 +qtr4) , data=ds)
  
  form1.X <- as.formula(N_deaths  ~
                          1 + time_scale + qtr2 + qtr3 +qtr4 + pandemic_effect+
                          agec + race_recode + sex +  subgroup_combo +
                          agec*(time_scale + qtr2 + qtr3 +qtr4) +
                          race_recode*(time_scale + qtr2 + qtr3 +qtr4)+
                          region*(time_scale + qtr2 + qtr3 +qtr4)  +
                          f(t, model='ar1')+
                          offset(log_pop))
  
  X <- model.matrix(~
                             1 + time_scale + qtr2 + qtr3 +qtr4 + pandemic_effect+
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

  y <- ds$N_deaths
  formula <- y ~ -1 + offset(log_pop) + X +  f(idx.Za, model="z", Z=Za) +  f(idx.Zb, model="z", Z=Zb)+  f(idx.Zc, model="z", Z=Zc) + f(t,model='ar1')
  
  mod.inla2 <- inla(formula, data = list(y=y, idx.Za = 1:n,idx.Zb = 1:n, 
                                         idx.Zc = 1:n,  
                                         t=ds$t,
                                         X=X, 
                                         log_pop=ds$log_pop), 
                    family='poisson', 
                    control.predictor = list(compute=TRUE),
                    control.compute=list(config = TRUE, dic=T, waic=T))  
  
  coefs <- mod.inla2$summary.fixed
  pandemic.effect <- coefs[grep('pandemic',row.names(coefs)),c('0.5quant','0.025quant' ,"0.975quant")]
  pandemic_pct_increase <- round(100*(exp(pandemic.effect) -1))
  
 waic = mod.inla2$waic$waic
  dic = mod.inla2$dic$dic
  
  all.ts <- sort(unique(ds$t))
  t.increment <- all.ts[length(all.ts)] - all.ts[length(all.ts)-1] 
  

  preds.inla2= c('coefs'=coefs, 'pandemic_pct_increase'=pandemic_pct_increase,waic'=waic, 'dic'=dic)
  return(preds.inla2)
}

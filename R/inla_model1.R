inla_model1 <- function(ds,prec.prior1=1){
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
  
  mod.inla2 <- inla(form1, 
                    family='poisson', data=ds,
                    control.compute=list(config = TRUE, dic=T, waic=T),
                    control.fixed=list(mean=0, prec=prec.prior1) #INFORMATIVE PRIOR
  )
  #summary(mod.inla2)
  
  waic = mod.inla2$waic$waic
  dic = mod.inla2$dic$dic
  
  res1 <- gen_pred_interval_inla(inla_obj=mod.inla2, covar.df=ds, mod.mat=mod.mat1 , source= unique(ds$source) ,log.offset1=ds$log_pop)

  preds.inla2= c(res1, 'waic'=waic, 'dic'=dic)
  return(preds.inla2)
}
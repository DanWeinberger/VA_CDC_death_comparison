#Adopted from Zhe Zheng RSV burden analysis
model_string_non_nested<-"
model {
  for (i in 1:n.age){
    for(j in 1:n.race){
      for(k in 1:2){
      for (t in 1:n.date) { 
    
        log_lambda[i,j,k,t] <-  log_pop[i,j,k,t] + beta[1,i,j,k] +
                     beta[2,i,j,k]*qtr2[t] +
                     beta[3,i,j,k]*qtr3[t] +
                     beta[4,i,j,k]*qtr4[t] +
                     beta[5,i,j,k]*time[t] +
                     phi[i,j,k,t]
                     
        y[i,j,k,t] ~ dpois(exp(log_lambda[i,j,k,t]))  ## likelihood 
        phi[i,j,k,t] ~ dnorm(0,tau0)
      }

  #The coeeficient for each state/age has some global effect, effect that varies with age, effect that varies with state, and random variation
      for(q in 1:5){ #q for each of the regression effects
        beta[q,i,j,k] <- alpha0[q] + alpha_race[q,j] + alpha_agec[q,i] +  alpha_sex[q,k] + phi2[q,i,j]
      }
  
    }
    }
  }
  
  
    for(q in 1:5){
      
      alpha_sex[q,1] <- 0
      alpha_sex[q,2]~ dnorm(0, 1e-4)
      alpha0[q] ~ dnorm(0, 1e-4)
      tau2[q] ~ dgamma(0.01, 0.01)


      for (i in 1:n.age){
         alpha_agec[q,i] ~ dnorm(mu_agec, tau_agec)
      }
    
      for(j in 1:n.race){
         alpha_race[q,j] ~ dnorm(mu_race, tau_race)
      }
      
      for (i in 1:n.age){
            for(j in 1:n.race){
        phi2[q,i,j] ~dnorm(0, tau2[q])
            }
      }

    }
    
    tau0 ~ dgamma(0.01, 0.01)

    tau_agec ~ dgamma(0.01, 0.01)
    tau_race ~ dgamma(0.01, 0.01)
  
  #fairly tight priors
    mu_agec ~ dnorm(0, 1e-4)
    mu_race ~ dnorm(0, 1e-4)

}
"

##############################################################
#Model Fitting
##############################################################
inits1=list(".RNG.seed"=c(123), ".RNG.name"='base::Wichmann-Hill')
inits2=list(".RNG.seed"=c(456), ".RNG.name"='base::Wichmann-Hill')
inits3=list(".RNG.seed"=c(789), ".RNG.name"='base::Wichmann-Hill')

model_spec<-textConnection(model_string_non_nested)

model_jags<-jags.model(model_spec, 
                       inits=list(inits1,inits2, inits3),
                       data=list('y' = ag3.c['N_deaths',,,,],
                                 'qtr2' =time.vars$qtr2,
                                 'qtr3' =time.vars$qtr3,
                                 'qtr4' =time.vars$qtr4,
                                 'time' =time.vars$time_Scale,
                                 'log_pop' =ag3.c['log_pop',,,,],
                                 'n.age'=dim(ag3.c)[2],
                                 'n.race'=dim(ag3.c)[3],
                                 'n.date'=nrow(time.vars)
                                 ),
                       n.adapt=10000, 
                       n.chains=3)

params<-c('log_lambda')


##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=10000)
posterior_samples.all<-do.call(rbind,posterior_samples)

###################################################
#POSTERIOR INFERENCE
###################################################
#post_means<-colMeans(posterior_samples.all)
post_means<-apply(posterior_samples.all, 2, median)
sample.labs<-names(post_means)


coords <- stringr::str_extract(string = sample.labs, pattern = "(?<=\\[).*(?=\\])" ) 
coords.spl <- stringr::str_split(coords,',')
coords.df <- as.data.frame(do.call(rbind, coords.spl))
names(coords.df) <- c('agec', 'race','sex','time')

ci<-t(hdi(posterior_samples.all, credMass = 0.95))
ci<-matrix(sprintf("%.1f",round(ci,1)), ncol=2)
row.names(ci)<-sample.labs
post_means<-sprintf("%.1f",round(post_means,1))
names(post_means)<-sample.labs

post_samples <- cbind.data.frame(coords.df, t(posterior_samples.all))
post_samples.m <- reshape2::melt(post_samples, id.vars=names(coords.df))

#aggregate by age and iteration
post_sample_age <- post_samples.m %>%
  group_by(agec, time, variable) %>%
  summarise(  'pred'=sum(value) ) %>%
  ungroup() %>%
  group_by(agec, time) %>%
  summarise('pred_median'=median(pred),  'pred_lcl'=quantile(pred,probs=0.025), 'pred_ucl'=quantile(pred,probs=0.975)) %>%
  ungroup()
  
post_sample_age$time <- as.numeric(as.character(post_sample_age$time))  
post_sample_age$agec <- as.numeric(as.character(post_sample_age$agec))  

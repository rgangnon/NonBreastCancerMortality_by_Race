library(tidyverse)
library(mgcv)

fit_binomial_APC <-  function(y,n,
                     age=period-cohort,period=age+cohort,cohort=period-age,
                     ages=NULL,periods=NULL,cohorts=NULL,
                     BIC=TRUE,offset=NULL,
                     mean_delta_age=FALSE,mean_delta_period=TRUE,mean_delta_cohort=TRUE,
                     ref_age=NULL,ref_period=NULL,
                     trace=FALSE) {
  
  if (is.null(ages)) ages <- sort(unique(age))
  if (is.null(periods)) periods <- sort(unique(period))
  if (is.null(cohorts)) cohorts <- sort(unique(cohort))
  
  N_age <- length(ages)
  N_period <- length(periods)
  N_cohort <- length(cohorts)
  
  gamma <- ifelse(BIC,log(sum(y))/2,1)
  
  if (is.null(ref_age)) ref_age <- ages[floor((N_age+1)/2)]
  if (is.null(ref_period)) ref_period <- periods[floor((N_period+1)/2)]
  ref_cohort <- ref_period-ref_age

  APC_gam <- gam(cbind(y,n-y)~s(age,k=N_age,bs="cr",pc=ref_age)+
                   s(cohort,k=N_cohort,bs="cr",pc=ref_cohort)+
                   s(period,k=N_period,bs="cs",pc=ref_period),
                 family="binomial",offset=offset,
                 knots=list(age=ages,period=periods,cohort=cohorts),
                 gamma=gamma,control=list(trace=trace))
  
  tmp <- tibble(age=ref_age,period=ref_period,cohort=ref_cohort,pop=1)
  X_ref <- matrix(predict(APC_gam,tmp,type="lpmatrix"),nrow=1)
  rownames(X_ref) <- "intercept"
  
  tmp <- tibble(age=ages,period=ref_period,cohort=ref_cohort,pop=1)
  X_age <- sweep(predict(APC_gam,tmp,type="lpmatrix"),MARGIN=2,FUN="-",X_ref)
  X_delta_age <- diff(X_age)
  if (mean_delta_age) {
    X_age <- rbind(X_age,apply(X_delta_age,MARGIN=2,FUN=mean),apply(X_delta_age,MARGIN=2,FUN=mean))
  } else {
    X_age <- rbind(X_age,X_delta_age[1,],X_delta_age[nrow(X_delta_age),])
  }
  rownames(X_age) <- c(paste("a",ages),"a slope1","a slope2")
  
  tmp <- tibble(age=ref_age,period=periods,cohort=ref_cohort,pop=1)
  X_period <- sweep(predict(APC_gam,tmp,type="lpmatrix"),MARGIN=2,FUN="-",X_ref)
  X_delta_period <- diff(X_period)
  if (mean_delta_period) {
    X_period <- rbind(X_period,apply(X_delta_period,MARGIN=2,FUN=mean),apply(X_delta_period,MARGIN=2,FUN=mean))
  } else {
    X_period <- rbind(X_period,X_delta_period[1,],X_delta_period[nrow(X_delta_period),])
  }
  rownames(X_period) <- c(paste("p",periods),"p slope1","p slope2")
  
  tmp <- tibble(age=ref_age,period=ref_period,cohort=cohorts,pop=1)
  X_cohort <- sweep(predict(APC_gam,tmp,type="lpmatrix"),MARGIN=2,FUN="-",X_ref)
  X_delta_cohort <- diff(X_cohort)
  if (mean_delta_cohort) {
    X_cohort <- rbind(X_cohort,apply(X_delta_cohort,MARGIN=2,FUN=mean),apply(X_delta_cohort,MARGIN=2,FUN=mean))
  } else {
    X_cohort <- rbind(X_cohort,X_delta_cohort[1,],X_delta_cohort[nrow(X_delta_cohort),])
  }
  rownames(X_cohort) <- c(paste("c",cohorts),"c slope1","c slope2")

  X <- rbind(X_ref,X_age,X_period,X_cohort)
  B <- X %*% coef(APC_gam)
  V <- X %*% vcov(APC_gam) %*% t(X)
  
  return(list(B=B,V=V,ages=ages,periods=periods,cohorts=cohorts,
              gam_fit=APC_gam,
              settings=list(BIC=BIC,
                            mean_delta_age=mean_delta_age,mean_delta_period=mean_delta_period,
                            mean_delta_cohort=mean_delta_cohort,
                            ref_age=ref_age,ref_period=ref_period,ref_cohort=ref_cohort)))
}

fit_poisson_APC <-  function(y,pop,
                              age=period-cohort,period=age+cohort,cohort=period-age,
                              ages=NULL,periods=NULL,cohorts=NULL,
                              BIC=TRUE,rate_scale=100000,
                              mean_delta_age=FALSE,mean_delta_period=TRUE,mean_delta_cohort=TRUE,
                              ref_age=NULL,ref_period=NULL,
                              trace=FALSE) {
  
  if (is.null(ages)) ages <- sort(unique(age))
  if (is.null(periods)) periods <- sort(unique(period))
  if (is.null(cohorts)) cohorts <- sort(unique(cohort))
  
  N_age <- length(ages)
  N_period <- length(periods)
  N_cohort <- length(cohorts)
  
  gamma <- ifelse(BIC,log(sum(y))/2,1)
  
  if (is.null(ref_age)) ref_age <- ages[floor((N_age+1)/2)]
  if (is.null(ref_period)) ref_period <- periods[floor((N_period+1)/2)]
  ref_cohort <- ref_period-ref_age
  
  pop <- pop/rate_scale
  
  APC_gam <- gam(y~s(age,k=N_age,bs="cr",pc=ref_age)+
                   s(cohort,k=N_cohort,bs="cr",pc=ref_cohort)+
                   s(period,k=N_period,bs="cs",pc=ref_period),
                 family="poisson",offset=log(pop),
                 knots=list(age=ages,period=periods,cohort=cohorts),
                 gamma=gamma,control=list(trace=trace))
  
  tmp <- tibble(age=ref_age,period=ref_period,cohort=ref_cohort,pop=1)
  X_ref <- matrix(predict(APC_gam,tmp,type="lpmatrix"),nrow=1)
  rownames(X_ref) <- "intercept"
  
  tmp <- tibble(age=ages,period=ref_period,cohort=ref_cohort,pop=1)
  X_age <- sweep(predict(APC_gam,tmp,type="lpmatrix"),MARGIN=2,FUN="-",X_ref)
  X_delta_age <- diff(X_age)
  if (mean_delta_age) {
    X_age <- rbind(X_age,apply(X_delta_age,MARGIN=2,FUN=mean),apply(X_delta_age,MARGIN=2,FUN=mean))
  } else {
    X_age <- rbind(X_age,X_delta_age[1,],X_delta_age[nrow(X_delta_age),])
  }
  rownames(X_age) <- c(paste("a",ages),"a slope1","a slope2")
  
  tmp <- tibble(age=ref_age,period=periods,cohort=ref_cohort,pop=1)
  X_period <- sweep(predict(APC_gam,tmp,type="lpmatrix"),MARGIN=2,FUN="-",X_ref)
  X_delta_period <- diff(X_period)
  if (mean_delta_period) {
    X_period <- rbind(X_period,apply(X_delta_period,MARGIN=2,FUN=mean),apply(X_delta_period,MARGIN=2,FUN=mean))
  } else {
    X_period <- rbind(X_period,X_delta_period[1,],X_delta_period[nrow(X_delta_period),])
  }
  rownames(X_period) <- c(paste("p",periods),"p slope1","p slope2")
  
  tmp <- tibble(age=ref_age,period=ref_period,cohort=cohorts,pop=1)
  X_cohort <- sweep(predict(APC_gam,tmp,type="lpmatrix"),MARGIN=2,FUN="-",X_ref)
  X_delta_cohort <- diff(X_cohort)
  if (mean_delta_cohort) {
    X_cohort <- rbind(X_cohort,apply(X_delta_cohort,MARGIN=2,FUN=mean),apply(X_delta_cohort,MARGIN=2,FUN=mean))
  } else {
    X_cohort <- rbind(X_cohort,X_delta_cohort[1,],X_delta_cohort[nrow(X_delta_cohort),])
  }
  rownames(X_cohort) <- c(paste("c",cohorts),"c slope1","c slope2")
  
  X <- rbind(X_ref,X_age,X_period,X_cohort)
  B <- X %*% coef(APC_gam)
  V <- X %*% vcov(APC_gam) %*% t(X)
  
  return(list(B=B,V=V,ages=ages,periods=periods,cohorts=cohorts,
              gam_fit=APC_gam,
              settings=list(BIC=BIC,rate_scale=rate_scale,
                            mean_delta_age=mean_delta_age,mean_delta_period=mean_delta_period,
                            mean_delta_cohort=mean_delta_cohort,
                            ref_age=ref_age,ref_period=ref_period,ref_cohort=ref_cohort)))
}

make_dummy_matrix <- function(x,vals) {
  t(sapply(x,function(x,vals) 1*(x==vals),vals=vals))
}

predict_APC <- function(APC,newdata) {
  X <- cbind(1,
             make_dummy_matrix(pmin(pmax(newdata$age,min(APC$ages)),max(APC$ages)),APC$ages),
             pmin(newdata$age-min(APC$ages),0),
             pmax(newdata$age-max(APC$ages),0),
             make_dummy_matrix(pmin(pmax(newdata$period,min(APC$periods)),max(APC$periods)),APC$periods),
             pmin(newdata$period-min(APC$periods),0),
             pmax(newdata$period-max(APC$periods),0),
             make_dummy_matrix(pmin(pmax(newdata$cohort,min(APC$cohorts)),max(APC$cohorts)),APC$cohorts),
             pmin(newdata$cohort-min(APC$cohorts),0),
             pmax(newdata$cohort-max(APC$cohorts),0))
  newdata$lp <- c(X %*% APC$B)
  newdata$se_lp <- sqrt(diag(X %*% APC$V %*% t(X)))
  return(newdata)
}


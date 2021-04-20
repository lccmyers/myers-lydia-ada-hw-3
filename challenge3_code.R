library(tidyverse)
library(broom)

d<-read_csv("https://raw.githubusercontent.com/difiore/ada-2021-datasets/main/KamilarAndCooperData.csv", col_names=T)

boot_lm<-function (d, model, conf.level=0.95, reps=1000){
  df<-data.frame(coefficients = c(0,0), SE = c(0,0), lowerCI = c(0,0), upperCI = c(0,0),
        bootcoeff = c(mean(bootVals[,2]), mean(bootVals[,1])), bootSE = c(0,0),
        bootlowerCI = c(0,0), bootupperCI = c(0,0), row.names=c("Intercept", "Slope"))
  r<-lm(model, data=d)
  tidyr<-tidy(r)
  df$coefficients[1]<-tidyr[1,2]
  df$coefficients[2]<-tidyr[2,2]
  df$SE[1]<-tidyr[1,3]
  df$SE[2]<-tidyr[2,3]
  CI<-confint(r, level=conf.level)
  df$lowerCI[1]<-CI[1,1]
  df$lowerCI[2]<-CI[2,1]
  df$upperCI[1]<-CI[1,2]
  df$upperCI[2]<-CI[2,2]
  set.seed(1)
  bootVals<-data.frame(boot_slope = 1:1000, boot_intercept = 1:1000)
  for (i in 1:reps){
    s<-sample_n(d, size=nrow(d), replace=T)
    bootr<-lm(model, data=s)
    vals<-bootr$coefficients
    bootVals$boot_slope[[i]]<-vals[1]
    bootVals$boot_intercept[[i]]<-vals[2]}
  slope_se<-sd(bootVals$boot_slope)
  intercept_se<-sd(bootVals$boot_intercept)
  df$bootSE[1]<-intercept_se
  df$bootSE[2]<-slope_se
  slope_lower<-quantile(bootVals$boot_slope, ((1-conf.level)/2))
  slope_upper<-quantile(bootVals$boot_slope, (1-(1-conf.level)/2))
  intercept_lower<-quantile(bootVals$boot_intercept, ((1-conf.level)/2))
  intercept_upper<-quantile(bootVals$boot_intercept, (1-(1-conf.level)/2))
  df$bootlowerCI[1]<-intercept_lower
  df$bootlowerCI[2]<-slope_lower
  df$bootupperCI[1]<-intercept_upper
  df$bootupperCI[2]<-slope_upper
  return(df)
 
}


boot_lm(d=d, model=log(MeanGroupSize) ~ log(Body_mass_female_mean))
boot_lm(d=d, model=log(DayLength_km) ~ log(Body_mass_female_mean))
boot_lm(d=d, model=log(DayLength_km) ~ log(Body_mass_female_mean) + log(MeanGroupSize))


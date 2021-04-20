library(tidyverse)
library(broom)
library(infer)

d<-read_csv("https://raw.githubusercontent.com/difiore/ada-2021-datasets/main/KamilarAndCooperData.csv", col_names=T)
d$log_MeanGroupSize<-log(d$MeanGroupSize)
d$log_Body_mass_female_mean<-log(d$Body_mass_female_mean)

r<-lm(log_MeanGroupSize~log_Body_mass_female_mean, data=d)
r$coefficients

#slope is .5063, intercept is -1.7773

#bootstrap coefficients
set.seed(1)
bootVals<-data.frame(boot_slope = 1:1000, boot_intercept = 1:1000)

for (i in 1:1000){
  s<-sample_n(d, size=nrow(d), replace=T)
  bootr<-lm(log_MeanGroupSize~log_Body_mass_female_mean, data=s)
  vals<-bootr$coefficients
  bootVals$boot_slope[[i]]<-vals[1]
  bootVals$boot_intercept[[i]]<-vals[2]
}

hist(bootVals$boot_slope, main = "Bootstraped Slope Values", xlab = "Slope", breaks = 15)
hist(bootVals$boot_intercept, main = "Bootstrapped Intercept Values", xlab = "Intercept", breaks = 15)

#getting standard error
slope_se<-sd(bootVals$boot_slope)
intercept_se<-sd(bootVals$boot_intercept)
SE<-data.frame(Slope = slope_se, Intercept = intercept_se, row.names = "SE")
SE
#getting SE fro original model
summary(r)

#the SE values from the bootstrap are similar to those from the lm() function, but
#not entirely the same


#getting CIs from bootstrap
slope_lower<-quantile(bootVals$boot_slope, (.05/2))
slope_upper<-quantile(bootVals$boot_slope, (1-.05/2))
intercept_lower<-quantile(bootVals$boot_intercept, (.05/2))
intercept_upper<-quantile(bootVals$boot_intercept, (1-.05/2))

CI<-data.frame(Lower = c(slope_lower, intercept_lower), Upper = c(slope_upper, intercept_upper), 
               row.names = c("Intercept", "Slope"))

CI
#getting CIs from original model
confint(r, level=.95)
#values are again very similar, but not exactly the same. values are more similar for CIs than SEs

head(bootVals)


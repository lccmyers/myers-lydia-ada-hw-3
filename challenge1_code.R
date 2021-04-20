library(tidyverse)
library(broom)

d<-read_csv("https://raw.githubusercontent.com/difiore/ada-2021-datasets/main/KamilarAndCooperData.csv", col_names=T)
head(d)
d<-na.omit(d)

d$log_Brain_Size_Species_Mean<-log(d$Brain_Size_Species_Mean)
d$log_WeaningAge_d<-log(d$WeaningAge_d)

r<-lm(WeaningAge_d ~ Brain_Size_Species_Mean, data=d)
#point estimate of slope for not logged = 2.637
logr<-lm(log_WeaningAge_d ~ log_Brain_Size_Species_Mean, data=d)
#point estimate of slope for logged = .5712


ggplot(d, aes(x=Brain_Size_Species_Mean, y=WeaningAge_d))+geom_point()+
  geom_smooth(method="lm",formula=y~x, se=F, color="red")+
  annotate(geom="text",x=200,y=1500,label="Weaning age ~ Brain size",color="black",fontface=2)+
  labs(y= "Weaining Age in Days", x = "Brain  Size")

ggplot(d, aes(x=log(Brain_Size_Species_Mean), y=log(WeaningAge_d)))+geom_point()+
  geom_smooth(method="lm",formula=y~x, se=F, color="red")+
  annotate(geom="text",x=2,y=7,label="Log Weaning age ~ Log Brain size",color="black",fontface=2)+
  labs(y= "Log Weaining Age in Days", x = "Log Brain  Size")

summary(r)
summary(logr)
#for both models, the value of B1 is statistically significantly different from zero,
#which does not support the null hypothesis that B1 = 0

confint(r,'Brain_Size_Species_Mean',level=0.90)
confint(logr,"log_Brain_Size_Species_Mean",level=0.90)
#list confidence intervals

Brain_Size_Species_Mean<-d$Brain_Size_Species_Mean
df <- augment(r, se_fit = TRUE)
alpha<-0.05

#creating confidence intervals
ci <- predict(r, newdata = data.frame(brain_size = d$Brain_Size_Species_Mean),
  interval = "confidence", level = .9) 
ci <- data.frame(ci)
ci<-na.omit(ci)
ci <- cbind(df$Brain_Size_Species_Mean, ci)
names(ci) <- c("brain_size", "c.fit", "c.lwr", "c.upr")

#creating prediction intervals
pi <- predict(r, newdata = data.frame(brain_size = d$Brain_Size_Species_Mean),
              interval = "prediction", level = .9) 
pi <- data.frame(pi)
pi<-na.omit(pi)
pi <- cbind(df$Brain_Size_Species_Mean, pi)
names(pi) <- c("brain_size", "p.fit", "p.lwr", "p.upr")

#plotting
ggplot(data = df, aes(x = Brain_Size_Species_Mean, y = WeaningAge_d))+
  geom_point() + geom_line(data=ci, aes(x=Brain_Size_Species_Mean, y=c.fit))+
  geom_line(data=ci, aes(x=Brain_Size_Species_Mean, y=c.lwr, color="blue"))+
  geom_line(data=ci, aes(x=Brain_Size_Species_Mean, y=c.upr, color="red"))+
  geom_line(data = pi, aes(x = brain_size, y = p.lwr, color="darkblue"))+
  geom_line(data = pi, aes(x = brain_size, y = p.upr, color = "darkred"))+
  scale_color_discrete(name = "Confidence & Prediction Intervals",
  labels = c("Lower CI", "Lower PI", "Upper PI", "Upper CI"))+
  labs(y= "Weaning Age in Days", x = "Brain  Size")
  
#doing the same for tthe logged stuff
log_Brain_Size_Species_Mean<-log(d$Brain_Size_Species_Mean)
df <- augment(logr, se_fit = TRUE)

#creating confidence intervals
ci <- predict(logr, newdata = data.frame(log_brain_size = log(d$Brain_Size_Species_Mean)),
              interval = "confidence", level = .9) 
ci <- data.frame(ci)
ci<-na.omit(ci)
ci <- cbind(df$log_Brain_Size_Species_Mean, ci)
names(ci) <- c("log_brain_size", "c.fit", "c.lwr", "c.upr")
  
#creating prediction intervals
pi <- predict(logr, newdata = data.frame(log_brain_size = log(d$Brain_Size_Species_Mean)),
              interval = "prediction", level = .9) 
pi <- data.frame(pi)
pi<-na.omit(pi)
pi <- cbind(df$log_Brain_Size_Species_Mean, pi)
names(pi) <- c("log_brain_size", "p.fit", "p.lwr", "p.upr")
  
#plotting
ggplot(data = df, aes(x = log_Brain_Size_Species_Mean, y = log_WeaningAge_d))+
  geom_point() + geom_line(data=ci, aes(x=log_Brain_Size_Species_Mean, y=c.fit))+
  geom_line(data=ci, aes(x=log_Brain_Size_Species_Mean, y=c.lwr, color="blue"))+
  geom_line(data=ci, aes(x=log_Brain_Size_Species_Mean, y=c.upr, color="red"))+
  geom_line(data = pi, aes(x = log_brain_size, y = p.lwr, color="darkblue"))+
  geom_line(data = pi, aes(x = log_brain_size, y = p.upr, color = "darkred"))+
  scale_color_discrete(name = "Confidence & Prediction Intervals",
  labels = c("Lower CI", "Lower PI", "Upper PI", "Upper CI"))+
  labs(y= "Log Weaning Age in Days", x = "Log Brain  Size")  


#point estimates
predict(r,newdata = data.frame(Brain_Size_Species_Mean = 750),interval = "prediction", level = 0.90)
  
#logged is better

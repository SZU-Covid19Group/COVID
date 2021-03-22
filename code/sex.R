sex<-read.csv("D:/DATA/Sex/mobility pattern by sex_revised.csv")
head(sex)

library(ggplot2)
library(cowplot)
library(ggpubr)

#distance and cases
p1 <- ggplot(sex, aes(x=insidecity_m,y=male_cases)) + ylim(0,60)+
  xlab("Male inside city") + ylab("Cases")+ geom_point() + 
  stat_smooth(method=lm)+ stat_cor(method = "pearson",label.x = 6750,label.y = 60,p.digits = 2)
p2 <- ggplot(sex, aes(x=intercity_m,y=male_cases)) + ylim(0,60)+
  xlab("Male intercity") + ylab("Cases")+ geom_point() + stat_smooth(method=lm)+ 
  stat_cor(method = "pearson", label.x = 89000, label.y = 60,p.digits = 2)
p3 <- ggplot(sex, aes(x=average_m,y=male_cases)) + ylim(0,60)+
  xlab("Male average") + ylab("Cases")+ geom_point() + stat_smooth(method=lm)+ 
  stat_cor(method = "pearson", label.x = 7500, label.y = 60,p.digits = 2)

p4 <- ggplot(sex, aes(x=insidecity_f,y=female_cases)) + ylim(0,60)+
  xlab("Female inside city") + ylab("Cases")+ geom_point() + stat_smooth(method=lm)+ 
  stat_cor(method = "pearson", label.x = 6250, label.y = 60,p.digits = 2)
p5 <- ggplot(sex, aes(x=intercity_f,y=female_cases)) + ylim(0,60)+
  xlab("Female intercity") + ylab("Cases")+ geom_point() + stat_smooth(method=lm)+ 
  stat_cor(method = "pearson", label.x = 91000, label.y = 60,p.digits = 2)
p6 <- ggplot(sex, aes(x=average_f,y=female_cases)) + ylim(0,60)+
  xlab("Female average") + ylab("Cases")+ geom_point() + stat_smooth(method=lm)+ 
  stat_cor(method = "pearson", label.x = 7000, label.y = 60,p.digits = 2)
plot_grid(p1,p2,p3,p4,p5,p6,nrow=3,ncol=2,byrow=FALSE,labels = c("A","B","C","D","E","F"))

par(mfrow=c(2,3))
plot(rstudent(lm(male_cases~insidecity_m,data=sex)),main="male inside city",ylab = "rstudent")
abline(h=c(0,-2,2), lty=2)
plot(rstudent(lm(male_cases~intercity_m,data=sex)),main="male intercity",ylab = "rstudent")
abline(h=c(0,-2,2), lty=2)
plot(rstudent(lm(male_cases~average_m,data=sex)),main="male average",ylab = "rstudent")
abline(h=c(0,-2,2), lty=2)
plot(rstudent(lm(female_cases~insidecity_f,data=sex)),main="female inside city",ylab = "rstudent")
abline(h=c(0,-2,2), lty=2)
plot(rstudent(lm(female_cases~intercity_f,data=sex)),main="female intercity",ylab = "rstudent")
abline(h=c(0,-2,2), lty=2)
plot(rstudent(lm(female_cases~average_f,data=sex)),main="female average",ylab = "rstudent")
abline(h=c(0,-2,2), lty=2)

library(car)
par(mfrow=c(2,3))
qqPlot(lm(male_cases~insidecity_m,data=sex),simulate=TRUE,col=1)
qqPlot(lm(male_cases~intercity_m,data=sex),simulate=TRUE,col=1)
qqPlot(lm(male_cases~average_m,data=sex),simulate=TRUE,col=1)
qqPlot(lm(female_cases~insidecity_f,data=sex),simulate=TRUE,col=1)
qqPlot(lm(female_cases~intercity_f,data=sex),simulate=TRUE,col=1)
qqPlot(lm(female_cases~average_f,data=sex),simulate=TRUE,col=1)

#distance and mobility volumn
p7 <- ggplot(sex, aes(x=average_m,y=v_male)) +
  xlab("Male") + ylab("Mobidity")+ geom_point() + 
  stat_smooth(method=lm)+ stat_cor(method = "pearson",label.x = 6750,label.y = 60,p.digits = 2)
p8 <- ggplot(sex, aes(x=average_f,y=v_female)) + 
  xlab("Female") + ylab("Mobidity")+ geom_point() + stat_smooth(method=lm)+ 
  stat_cor(method = "pearson", label.x = 7000, label.y = 60,p.digits = 2)
plot_grid(p7,p8,nrow=1,ncol=2,byrow=FALSE)

pp7 <- ggplot(sex, aes(x=insidecity_m,y=v_male)) +
  xlab("Male_insidecity") + ylab("Mobidity")+ geom_point() + 
  stat_smooth(method=lm)+ stat_cor(method = "pearson",label.x = 6750,label.y = 60,p.digits = 2)
pp8 <- ggplot(sex, aes(x=insidecity_f,y=v_female)) + 
  xlab("Female_insidecity") + ylab("Mobidity")+ geom_point() + stat_smooth(method=lm)+ 
  stat_cor(method = "pearson", label.x = 6000, label.y = 60,p.digits = 2)
plot_grid(pp7,pp8,nrow=1,ncol=2,byrow=FALSE)

ppp7 <- ggplot(sex, aes(x=intercity_m,y=v_male)) +
  xlab("Male_intercity") + ylab("Mobidity")+ geom_point() + 
  stat_smooth(method=lm)+ stat_cor(method = "pearson",label.x = 85000,label.y = 60,p.digits = 2)
ppp8 <- ggplot(sex, aes(x=intercity_f,y=v_female)) + 
  xlab("Female_intercity") + ylab("Mobidity")+ geom_point() + stat_smooth(method=lm)+ 
  stat_cor(method = "pearson", label.x = 85000, label.y = 60,p.digits = 2)
plot_grid(ppp7,ppp8,nrow=1,ncol=2,byrow=FALSE)

#Interaction added
fit1<-lm(male_cases~v_male+average_m+v_male*average_m,data = sex)
summary(fit1)
vif(fit1)

fit2<-lm(female_cases~v_female+average_f+v_female*average_f,data = sex)
summary(fit2)
vif(fit2)

library(effects)
p9<-plot(effect("v_male:average_m",fit1,,list(average_m=c(5000,6000,7000,8000))),multiline=TRUE)
p10<-plot(effect("v_female:average_f",fit2,,list(average_f=c(4000,5000,6000,7000))),multiline=TRUE)
plot_grid(p9,p10,nrow=1,ncol=2,byrow=TRUE)

#lag creation
library(xts)
time<-seq(as.Date('2020-01-01'), length=91, by='day')
mc<-xts(sex$male_cases,time)
mv<-xts(sex$vm_log10,time)
fc<-xts(sex$female_cases,time)
fv<-xts(sex$vf_log10,time)
ma<-xts(sex$average_m,time)
fa<-xts(sex$average_f,time)
date<-cbind(mc,mv,fc,fv,ma,fa)
for(i in 0:30){
  mc_lag<-lag(mc,k=-i)
  date<-cbind(date,mc_lag)
}
for(j in 0:30){
  fc_lag<-lag(fc,k=-j)
  date<-cbind(date,fc_lag)
}

date<-date[,-c(7,38)]

date2<-window(date, start = as.Date('2020-01-01'),end = as.Date('2020-02-29'))

#lag without interaction
library(dplyr)
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:29){
  fit_m<-lm(date2[,7+i]~mv+ma,data=date2)
  coef_1<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=coef(summary(fit_m))[2,4],pma=coef(summary(fit_m))[3,4])
  summary1<-rbind(summary1,coef_1)
  fit_f<-lm(date2[,37+i]~fv+fa,data=date2)
  coef_2<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=coef(summary(fit_f))[2,4],pfa=coef(summary(fit_f))[3,4])
  summary2<-rbind(summary2,coef_2)
  setwd("D:/Rplot")
  plot1<-ggplot(date2, aes(x=mv,y=date2[,7+i])) + ylim(0,60)+
    xlab("Male volumn") + ylab("Male Cases")+ geom_point() + stat_smooth(method=lm)+ 
    stat_cor(method = "pearson", label.x = 7500, label.y = 60,p.digits = 2)
  plot2<-ggplot(date2, aes(x=fv,y=date2[,37+i])) + ylim(0,60)+
    xlab("Female volumn") + ylab("Female Cases")+ geom_point() + stat_smooth(method=lm)+ 
    stat_cor(method = "pearson", label.x = 7000, label.y = 60,p.digits = 2)
  plot3<-plot_grid(plot1,plot2,ncol=2,byrow=T)
  yourfilename=paste("Lag lg aver_no interaction",i+1,".jpeg",sep="")
  jpeg(file=yourfilename)
  print(plot3)
  dev.off()
}


#lag with interaction
library(dplyr)
summary_m<-data.frame()
summary_f<-data.frame()
for(i in 0:29){
  fit_m<-lm(date2[,7+i]~mv+ma+mv*ma,data=date2)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=coef(summary(fit_m))[2,4],pma=coef(summary(fit_m))[3,4],pmvma=coef(summary(fit_m))[4,4])
  summary_m<-rbind(summary_m,coef_m)
  fit_f<-lm(date2[,37+i]~fv+fa+fv*fa,data=date2)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=coef(summary(fit_f))[2,4],pfa=coef(summary(fit_f))[3,4],pfvfa=coef(summary(fit_f))[4,4])
  summary_f<-rbind(summary_f,coef_f)
  setwd("D:/Rplot")
  plot1<-plot(effect("mv:ma",fit_m,,list(ma=c(5000,6000,7000,8000))),multiline=TRUE,title="male")  
  plot2<-plot(effect("fv:fa",fit_f,,list(fa=c(4000,5000,6000,7000))),multiline=TRUE,title="female")
  plot3<-plot_grid(plot1,plot2,ncol=2,byrow=T)
  yourfilename=paste("Lag lg aver",i+1,".jpeg",sep="")
  jpeg(file=yourfilename)
  print(plot3)
  dev.off()
}

#most-fit day for modeling
max_m<-which.max(summary_m$adj_r)
max_f<-which.max(summary_f$adj_r)
max_m
max_f
day14<-as.numeric(date2[,6+max_m])
day13<-as.numeric(date2[,21+max_f])
male_volumn<-as.numeric(date2$mv)
female_volumn<-as.numeric(date2$fv)
male_average_distance<-as.numeric(date2$ma)
female_average_distance<-as.numeric(date2$fa)
date3<-data.frame()[c(1:60),]
date3<-cbind(date3,day14,day13,male_volumn,female_volumn,male_average_distance,female_average_distance)
time2<-seq(as.Date('2020-01-01'), length=60, by='day')
row.names(date3)<-time2
mostfit_m<-lm(day14~male_volumn+male_average_distance+male_volumn*male_average_distance,data=date3)
summary(mostfit_m)
vif(mostfit_m)
mostfit_f<-lm(day13~female_volumn+female_average_distance+female_volumn*female_average_distance,data=date3)
summary(mostfit_f)
vif(mostfit_f)

plot1<-plot(effect("male_volumn*male_average_distance",mostfit_m,,list(male_average_distance=c(5000,6000,7000,8000))),multiline=TRUE,title="male")  
plot2<-plot(effect("female_volumn*female_average_distance",mostfit_f,,list(female_average_distance=c(4000,5000,6000,7000))),multiline=TRUE,title="female")
plot3<-plot_grid(plot1,plot2,ncol=2,byrow=T)
plot3

#qqplot
par(mfrow=c(1,2))
qqPlot(mostfit_m,simulate=TRUE,col=1)
qqPlot(mostfit_f,simulate=TRUE,col=1)

#residue plot
par(mfrow=c(1,2))
plot(rstudent(mostfit_m))
abline(h=c(0,-2,2), lty=2)
plot(rstudent(mostfit_f))
abline(h=c(0,-2,2), lty=2)
shapiro.test(rstudent(mostfit_m))$p.value>=0.5
shapiro.test(rstudent(mostfit_f))$p.value>=0.5
#multicollinearity and centering
vif(mostfit_m)
vif(mostfit_f)

male_volumn_scale<-scale(male_volumn,scale=F)
female_volumn_scale<-scale(female_volumn,scale=F)
date3<-cbind(date3,male_volumn_scale,female_volumn_scale)
male_average_distance_scale<-scale(male_average_distance,scale=F)
female_average_distance_scale<-scale(female_average_distance,scale=F)
date3<-cbind(date3,male_average_distance_scale,female_average_distance_scale)

scale_m<-lm(day14~male_volumn+male_average_distance_scale+male_volumn*male_average_distance_scale,data=date3)
summary(scale_m)
vif(scale_m)
scale_f<-lm(day13~female_volumn+female_average_distance_scale+female_volumn*female_average_distance_scale,data=date3)
summary(scale_f)
vif(scale_f)

sp1<-plot(effect("male_volumn*male_average_distance_scale",scale_m,,list(male_average_distance=c(5000,6000,7000,8000))),multiline=TRUE,title="male")  
sp2<-plot(effect("female_volumn*female_average_distance_scale",scale_f,,list(female_average_distance=c(4000,5000,6000,7000))),multiline=TRUE,title="female")
plot1<-plot(effect("male_volumn*male_average_distance",mostfit_m),multiline=TRUE,title="male")  
plot2<-plot(effect("female_volumn*female_average_distance",mostfit_f),multiline=TRUE,title="female")
plot_grid(sp1,plot1,sp2,plot2,ncol=2,byrow=T)

#male and female
gender1 <- ggplot(sex, aes(x=v_male,y=female_cases)) + ylim(0,60)+
  xlab("Male volumn") + ylab("Female Cases")+ geom_point() + stat_smooth(method=lm)+ 
  stat_cor(method = "pearson", label.x = 7500, label.y = 60,p.digits = 2)
gender2 <- ggplot(sex, aes(x=v_female,y=male_cases)) + ylim(0,60)+
  xlab("Female volumn") + ylab("Male Cases")+ geom_point() + stat_smooth(method=lm)+ 
  stat_cor(method = "pearson", label.x = 7000, label.y = 60,p.digits = 2)
plot_grid(gender1,gender2,nrow=1,ncol=2,byrow=FALSE)

library(dplyr)
gender_1<-data.frame()
gender_2<-data.frame()
for(i in 0:14){
  male_move<-lm(date2[,22+i]~mv,data=date2)
  coef1<-data.frame(r=summary(male_move)$r.squared,adj_r=summary(male_move)$adj.r.squared)
  summary_f<-rbind(gender_1,coef1)
  female_move<-lm(date2[,7+i]~fv,data=date2)
  coef2<-data.frame(r=summary(female_move)$r.squared,adj_r=summary(female_move)$adj.r.squared)
  summary_m<-rbind(gender_2,coef2)
  setwd("D:/Rplot")
  plot1<-ggplot(date2, aes(x=mv,y=date2[,22+i])) + ylim(0,60)+
    xlab("Male volumn") + ylab("Female Cases")+ geom_point() + stat_smooth(method=lm)+ 
    stat_cor(method = "pearson", label.x = 7500, label.y = 60,p.digits = 2)
  plot2<-ggplot(date2, aes(x=fv,y=date2[,7+i])) + ylim(0,60)+
    xlab("Female volumn") + ylab("Male Cases")+ geom_point() + stat_smooth(method=lm)+ 
    stat_cor(method = "pearson", label.x = 7000, label.y = 60,p.digits = 2)
  plot3<-plot_grid(plot1,plot2,ncol=2,byrow=T)
  yourfilename=paste("gender",i+1,".jpeg",sep="")
  jpeg(file=yourfilename)
  print(plot3)
  dev.off()
}


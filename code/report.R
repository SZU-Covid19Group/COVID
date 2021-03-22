sex<-read.csv("D:/DATA/Sex/mobility pattern by sex_revised.csv")
head(sex)
library(car)
library(effects)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(xts)
library(ggrepel)
library(MASS)
library(caret)

sex$Date<-seq(as.Date('2020-01-01'), length=91, by='day')
sex1<-sex[c(1:60),]
options(scipen=200)
p1<-ggplot(sex1, aes(x=Date,y=v_male))+ theme_classic()+scale_y_continuous(limits=c(0,20000000),breaks = c(5000000,10000000,15000000,20000000),expand = c(0,0))+
  annotate("text",x=as.Date("2020-01-16"),y=19000000 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-01-16"),y=17500000 ,label= "Travel Rush")+
  annotate("text",x=as.Date("2020-02-01"),y=19000000 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-02-01"),y=17500000 ,label= "Vacation")+
  annotate("text",x=as.Date("2020-02-20"),y=17500000 ,label= "Resume work")+
  scale_x_date(limits = as.Date(c("2020-01-01","2020-02-29")),date_breaks = "5 days",minor_breaks = "1 day",date_labels = "%Y-%m-%d",expand = c(0,1))+ 
  geom_vline(xintercept=as.numeric(as.Date(c("2020-01-10","2020-01-14","2020-01-24","2020-02-09"))),lty=2,col=2)+
  ylab("Male Mobility")+ xlab(NULL)+ geom_point()+geom_line()+theme(axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.9))
p2<-ggplot(sex1, aes(x=Date,y=vm_log10))+ theme_classic()+scale_y_continuous(limits=c(6,8),breaks = c(6,6.5,7,7.5,8),expand = c(0,0))+
  scale_x_date(date_breaks = "5 days",minor_breaks = "1 day",date_labels = "%Y-%m-%d",limits = as.Date(c("2020-01-01","2020-02-29")),expand = c(0,1))+
  annotate("text",x=as.Date("2020-01-16"),y=7.65 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-01-16"),y=7.5 ,label= "Travel Rush")+
  annotate("text",x=as.Date("2020-02-01"),y=7.65 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-02-01"),y=7.5 ,label= "Vacation")+
  annotate("text",x=as.Date("2020-02-20"),y=7.5 ,label= "Resume work")+
  ylab("Male Mobility(log)")+ xlab(NULL)+ geom_point()+geom_line()+geom_vline(xintercept=as.numeric(as.Date(c("2020-01-10","2020-01-14","2020-01-24","2020-02-09"))),lty=2,col=2)+theme(axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.9))
p3<-ggplot(sex1, aes(x=Date,y=male_cases))+ theme_classic()+scale_y_continuous(limits=c(0,80),breaks = c(20,40,60,80),expand = c(0,0))+
  scale_x_date(date_breaks = "5 days",minor_breaks = "1 day",date_labels = "%Y-%m-%d",limits = as.Date(c("2020-01-01","2020-02-29")),expand = c(0,1))+
  annotate("text",x=as.Date("2020-01-16"),y=78 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-01-16"),y=73 ,label= "Travel Rush")+
  annotate("text",x=as.Date("2020-02-01"),y=78 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-02-01"),y=73 ,label= "Vacation")+
  annotate("text",x=as.Date("2020-02-20"),y=73 ,label= "Resume work")+
  ylab("Male Cases")+ xlab(NULL)+ geom_point()+geom_line()+geom_vline(xintercept=as.numeric(as.Date(c("2020-01-10","2020-01-14","2020-01-24","2020-02-09"))),lty=2,col=2)+theme(axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.9))
p4<-ggplot(sex1, aes(x=Date,y=v_female))+ theme_classic()+scale_y_continuous(limits=c(0,10000000),breaks = c(2500000,5000000,7500000,10000000),expand = c(0,0))+
  annotate("text",x=as.Date("2020-01-16"),y=9000000 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-01-16"),y=8300000 ,label= "Travel Rush")+
  annotate("text",x=as.Date("2020-02-01"),y=9000000 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-02-01"),y=8300000 ,label= "Vacation")+
  annotate("text",x=as.Date("2020-02-20"),y=8300000,label= "Resume work")+
  scale_x_date(date_breaks = "5 days",minor_breaks = "1 day",date_labels = "%Y-%m-%d",limits = as.Date(c("2020-01-01","2020-02-29")),expand = c(0,1))+ 
  geom_vline(xintercept=as.numeric(as.Date(c("2020-01-10","2020-01-14","2020-01-24","2020-02-09"))),lty=2,col=2)+
  ylab("Female Mobility")+ xlab(NULL)+ geom_point()+geom_line()+theme(axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.9))
p5<-ggplot(sex1, aes(x=Date,y=vf_log10))+ theme_classic()+scale_y_continuous(limits=c(6,8),breaks = c(6,6.5,7,7.5,8),expand = c(0,0))+
  scale_x_date(date_breaks = "5 days",minor_breaks = "1 day",date_labels = "%Y-%m-%d",limits = as.Date(c("2020-01-01","2020-02-29")),expand = c(0,1))+
  annotate("text",x=as.Date("2020-01-16"),y=7.65 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-01-16"),y=7.5 ,label= "Travel Rush")+
  annotate("text",x=as.Date("2020-02-01"),y=7.65 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-02-01"),y=7.5 ,label= "Vacation")+
  annotate("text",x=as.Date("2020-02-20"),y=7.5 ,label= "Resume work")+
  ylab("Female Mobility(log)")+ xlab(NULL)+ geom_point()+geom_line()+geom_vline(xintercept=as.numeric(as.Date(c("2020-01-10","2020-01-14","2020-01-24","2020-02-09"))),lty=2,col=2)+theme(axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.9))
p6<-ggplot(sex1, aes(x=Date,y=female_cases))+ theme_classic()+scale_y_continuous(limits=c(0,80),breaks = c(20,40,60,80),expand = c(0,0))+
  scale_x_date(date_breaks = "5 days",minor_breaks = "1 day",date_labels = "%Y-%m-%d",limits = as.Date(c("2020-01-01","2020-02-29")),expand = c(0,1))+
  annotate("text",x=as.Date("2020-01-16"),y=78 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-01-16"),y=73 ,label= "Travel Rush")+
  annotate("text",x=as.Date("2020-02-01"),y=78 ,label= "Spring Festival")+annotate("text",x=as.Date("2020-02-01"),y=73 ,label= "Vacation")+
  annotate("text",x=as.Date("2020-02-20"),y=73 ,label= "Resume work")+
  ylab("Female Cases")+ xlab(NULL)+ geom_point()+geom_line()+geom_vline(xintercept=as.numeric(as.Date(c("2020-01-10","2020-01-14","2020-01-24","2020-02-09"))),lty=2,col=2)+theme(axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.9))
p7<-ggplot(sex1, aes(x=Date,y=average_m))+
  xlab("Male Distance") + ylab("Mobidity")+ geom_point()+geom_line()+ geom_text_repel(label=sex1$Date)
p8<-ggplot(sex1, aes(x=Date,y=average_f))+ 
  xlab("FeMale Distance") + ylab("Mobidity")+ geom_point()+geom_line()+ geom_text_repel(label=sex1$Date)
library(cowplot)
plot_grid(p1,p2,p3,p4,p5,p6,ncol=2,byrow = F,align = "hv",labels = c("A","D","B","E","C","F"))

#point plot

sex0<-sex[c(1:60),]
splot1<-ggplot(sex0, aes(x=vm_log10,y=male_cases,color=factor(average_male))) + ylim(0,60)+
  xlab("Male Mobility(log)") + ylab("Male Cases") + 
  stat_smooth(method=lm)+ geom_point(size = 2.0, shape = 16)
splot2<-ggplot(sex0, aes(x=vm_log10,y=male_cases,color=factor(insidecity_male)))+ ylim(0,60)+
  xlab("Male Mobility(log)") + ylab("Male Cases") + 
  stat_smooth(method=lm)+ geom_point(size = 2.0, shape = 16)
splot3<-ggplot(sex0, aes(x=vm_log10,y=male_cases,color=factor(intercity_male)))+ ylim(0,60)+
  xlab("Male Mobility(log)") + ylab("Male Cases") + 
  stat_smooth(method=lm)+ geom_point(size = 2.0, shape = 16)

splot4<-ggplot(sex0, aes(x=vf_log10,y=female_cases,color=factor(average_female))) + ylim(0,60)+
  xlab("FeMale Mobility(log)") + ylab("FeMale Cases") + 
  stat_smooth(method=lm)+ geom_point(size = 2.0, shape = 16)
splot5<-ggplot(sex0, aes(x=vf_log10,y=female_cases,color=factor(insidecity_female)))+ ylim(0,60)+
  xlab("FeMale Mobility(log)") + ylab("FeMale Cases") + 
  stat_smooth(method=lm)+ geom_point(size = 2.0, shape = 16)
splot6<-ggplot(sex0, aes(x=vf_log10,y=female_cases,color=factor(intercity_female)))+ ylim(0,60)+
  xlab("FeMale Mobility(log)") + ylab("FeMale Cases") + 
  stat_smooth(method=lm)+ geom_point(size = 2.0, shape = 16)

plot_grid(splot1,splot2,splot3,splot4,splot5,splot6,nrow = 3,byrow = F,labels = c("A", "B","C","D","E","F"))

#Model comparison_WITH or WITHOUT interaction
fit1<-lm(male_cases~vm_log10+average_m,data = sex0)
summary(fit1)
fit2<-lm(male_cases~vm_log10+insidecity_m,data = sex0)
summary(fit2)
fit3<-lm(male_cases~vm_log10+intercity_m,data = sex0)
summary(fit3)
fit4<-lm(male_cases~vm_log10*average_m,data = sex0)
summary(fit4)
fit5<-lm(male_cases~vm_log10*insidecity_m,data = sex0)
summary(fit5)
fit6<-lm(male_cases~vm_log10*intercity_m,data = sex0)
summary(fit6)
fit7<-lm(female_cases~vf_log10+average_f,data = sex0)
summary(fit7)
fit8<-lm(female_cases~vf_log10+insidecity_f,data = sex0)
summary(fit8)
fit9<-lm(female_cases~vf_log10+intercity_f,data = sex0)
summary(fit9)
fit10<-lm(female_cases~vf_log10*average_f,data = sex0)
summary(fit10)
fit11<-lm(female_cases~vf_log10*insidecity_f,data = sex0)
summary(fit11)
fit12<-lm(female_cases~vf_log10*intercity_f,data = sex0)
summary(fit12)

library(effects)
p9<-plot(effect("vm_log10:average_m_cate",fit1,,list(average_m=c(5000,6000,7000,8000))),multiline=TRUE)
p10<-plot(effect("vf_log10:average_f_cate",fit2,,list(average_m=c(4000,5000,6000,7000))),multiline=TRUE)
plot_grid(p9,p10,nrow=1,ncol=2,byrow=TRUE)

#lag
library(xts)
time<-seq(as.Date('2020-01-01'), length=91, by='day')
mc<-xts(sex$male_cases,time)
mv<-xts(sex$vm_log10,time)
fc<-xts(sex$female_cases,time)
fv<-xts(sex$vf_log10,time)
ma<-xts(sex$average_m,time)
fa<-xts(sex$average_f,time)
mi<-xts(sex$insidecity_m,time)
fi<-xts(sex$insidecity_f,time)
mii<-xts(sex$intercity_m,time)
fii<-xts(sex$intercity_f,time)
date<-cbind(mc,mv,fc,fv,ma,fa)
date0<-cbind(mc,mv,fc,fv,mi,fi)
date00<-cbind(mc,mv,fc,fv,mii,fii)
lag<-c(0:30)
lag<-as.data.frame(lag)

for(i in 0:30){
  mc_lag<-lag(mc,k=-i)
  date<-cbind(date,mc_lag)
  date0<-cbind(date0,mc_lag)
  date00<-cbind(date00,mc_lag)
}
for(j in 0:30){
  fc_lag<-lag(fc,k=-j)
  date<-cbind(date,fc_lag)
  date0<-cbind(date0,fc_lag)
  date00<-cbind(date00,fc_lag)
}

date<-date[,-c(7,38)]#备用#

date2<-window(date, start = as.Date('2020-01-01'),end = as.Date('2020-02-29'))
date2<-window(date, start = as.Date('2020-01-01'),end = as.Date('2020-02-26'))
date2<-window(date, start = as.Date('2020-01-01'),end = as.Date('2020-02-16'))
date2<-window(date, start = as.Date('2020-01-01'),end = as.Date('2020-02-09'))
date3<-window(date0, start = as.Date('2020-01-01'),end = as.Date('2020-02-29'))
date3<-window(date0, start = as.Date('2020-01-01'),end = as.Date('2020-02-26'))
date3<-window(date0, start = as.Date('2020-01-01'),end = as.Date('2020-02-16'))
date3<-window(date0, start = as.Date('2020-01-01'),end = as.Date('2020-02-09'))
date4<-window(date00, start = as.Date('2020-01-01'),end = as.Date('2020-02-29'))
date4<-window(date00, start = as.Date('2020-01-01'),end = as.Date('2020-02-26'))
date4<-window(date00, start = as.Date('2020-01-01'),end = as.Date('2020-02-16'))
date4<-window(date00, start = as.Date('2020-01-01'),end = as.Date('2020-02-09'))

#lag 
library(dplyr)
#1
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date2[,7+i]~mv*ma,data=date2)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date2[,7+i]~mv+ma,data=date2)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date2[,38+i]~fv*fa,data=date2)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date2[,38+i]~fv+fa,data=date2)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}

date2<-window(date, start = as.Date('2020-01-01'),end = as.Date('2020-02-29'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date2[,7+i]~mv*ma,data=date2)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date2[,7+i]~mv+ma,data=date2)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date2[,38+i]~fv*fa,data=date2)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date2[,38+i]~fv+fa,data=date2)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total_with29<-as.matrix(summary_m$r)
total_with29<-cbind(total_with29,summary_m$pmvma,summary_f$r,summary_f$pfvfa,lag)
colnames(total_with29)<-c("male29_r","male29_p","female29_r","female29_p","lag")


date2<-window(date, start = as.Date('2020-01-01'),end = as.Date('2020-02-26'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date2[,7+i]~mv*ma,data=date2)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date2[,7+i]~mv+ma,data=date2)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date2[,38+i]~fv*fa,data=date2)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date2[,38+i]~fv+fa,data=date2)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total_with26<-as.matrix(summary_m$r)
total_with26<-cbind(total_with26,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total_with26)<-c("male26_r","male26_p","female26_r","female26_p")


date2<-window(date, start = as.Date('2020-01-01'),end = as.Date('2020-02-16'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date2[,7+i]~mv*ma,data=date2)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date2[,7+i]~mv+ma,data=date2)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date2[,38+i]~fv*fa,data=date2)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date2[,38+i]~fv+fa,data=date2)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total_with16<-as.matrix(summary_m$r)
total_with16<-cbind(total_with16,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total_with16)<-c("male16_r","male16_p","female16_r","female16_p")

date2<-window(date, start = as.Date('2020-01-01'),end = as.Date('2020-02-09'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date2[,7+i]~mv*ma,data=date2)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date2[,7+i]~mv+ma,data=date2)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date2[,38+i]~fv*fa,data=date2)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date2[,38+i]~fv+fa,data=date2)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total_with09<-as.matrix(summary_m$r)
total_with09<-cbind(total_with09,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total_with09)<-c("male09_r","male09_p","female09_r","female09_p")

total_with29<-as.data.frame(total_with29)
total_with26<-as.data.frame(total_with26)
total_with16<-as.data.frame(total_with16)
total_with09<-as.data.frame(total_with09)
total_with<-as.data.frame(matrix(numeric(0),nrow = 31))
total_with<-cbind(total_with,total_with09,total_with16,total_with26,total_with29)

#r2 and pvalue plot_with interaction
cols=c('02-09'="darkblue", '02-16'="red", '02-26'="orange",'02-29'="darkgreen")
shapes=c('R Square'=16,'P-value'=15)
linetypes=c('R Square'="solid",'P-value'="dashed")
male_with_average<-ggplot(total_with)+
  geom_point(aes(x=lag,y=male29_r,shape="R Square",colour="02-29"))+geom_line(aes(x=lag,y=male29_r,colour="02-29",linetype="R Square"))+
  geom_point(aes(x=lag,y=male26_r,shape="R Square",colour="02-26"))+geom_line(aes(x=lag,y=male26_r,colour="02-26",linetype="R Square"))+
  geom_point(aes(x=lag,y=male16_r,shape="R Square",colour="02-16"))+geom_line(aes(x=lag,y=male16_r,colour="02-16",linetype="R Square"))+
  geom_point(aes(x=lag,y=male09_r,shape="R Square",colour="02-09"))+geom_line(aes(x=lag,y=male09_r,colour="02-09",linetype="R Square"))+
  scale_y_continuous(limits = c(0,1),breaks = c(0.25,0.5,0.75,1),expand = c(0,0),sec.axis = sec_axis(~.*2,name="P-value",breaks = c(0.5,1,1.5,2)))+
  geom_point(aes(x=lag,y=male29_p/2,shape="P-value",color="02-29"))+geom_line(aes(x=lag,y=male29_p/2,colour="02-29",linetype="P-value"))+
  geom_point(aes(x=lag,y=male26_p/2,shape="P-value",color="02-26"))+geom_line(aes(x=lag,y=male26_p/2,colour="02-26",linetype="P-value"))+
  geom_point(aes(x=lag,y=male16_p/2,shape="P-value",color="02-16"))+geom_line(aes(x=lag,y=male16_p/2,colour="02-16",linetype="P-value"))+
  geom_point(aes(x=lag,y=male09_p/2,shape="P-value",color="02-09"))+geom_line(aes(x=lag,y=male09_p/2,colour="02-09",linetype="P-value"))+
  labs(x="Lag",y="R Square",title="Coefficients of lag model under interaction of average distance(male)")+
  scale_x_continuous(limits = c(0,30),breaks = c(0,5,10,15,20,25,30),expand = c(0,1))+
  scale_colour_manual(name="class",values=cols)+
  scale_shape_manual(name="class",values = shapes)+
  scale_linetype_manual(name="class",values=linetypes)+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())
 

  
female_with_average<-ggplot(total_with)+
  geom_point(aes(x=lag,y=female29_r,shape="R Square",colour="02-29"))+geom_line(aes(x=lag,y=female29_r,colour="02-29",linetype="R Square"))+
  geom_point(aes(x=lag,y=female26_r,shape="R Square",colour="02-26"))+geom_line(aes(x=lag,y=female26_r,colour="02-26",linetype="R Square"))+
  geom_point(aes(x=lag,y=female16_r,shape="R Square",colour="02-16"))+geom_line(aes(x=lag,y=female16_r,colour="02-16",linetype="R Square"))+
  geom_point(aes(x=lag,y=female09_r,shape="R Square",colour="02-09"))+geom_line(aes(x=lag,y=female09_r,colour="02-09",linetype="R Square"))+
  scale_y_continuous(limits = c(0,1),breaks = c(0.25,0.5,0.75,1),expand = c(0,0),sec.axis = sec_axis(~.*2,name="P-value",breaks = c(0.5,1,1.5,2)))+
  geom_point(aes(x=lag,y=female29_p/2,shape="P-value",color="02-29"))+geom_line(aes(x=lag,y=female29_p/2,colour="02-29",linetype="P-value"))+
  geom_point(aes(x=lag,y=female26_p/2,shape="P-value",color="02-26"))+geom_line(aes(x=lag,y=female26_p/2,colour="02-26",linetype="P-value"))+
  geom_point(aes(x=lag,y=female16_p/2,shape="P-value",color="02-16"))+geom_line(aes(x=lag,y=female16_p/2,colour="02-16",linetype="P-value"))+
  geom_point(aes(x=lag,y=female09_p/2,shape="P-value",color="02-09"))+geom_line(aes(x=lag,y=female09_p/2,colour="02-09",linetype="P-value"))+
  labs(x="Lag",y="R Square",title="Coefficients of lag model under interaction of average distance(female)")+
  scale_x_continuous(limits = c(0,30),breaks = c(0,5,10,15,20,25,30),expand = c(0,1))+
  scale_colour_manual(name="class",values=cols)+
  scale_shape_manual(name="class",values = shapes)+
  scale_linetype_manual(name="class",values=linetypes)+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())

plot_grid(male_with_average,female_with_average,ncol=1)




#2
library(dplyr)

date3<-window(date0, start = as.Date('2020-01-01'),end = as.Date('2020-02-29'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date3[,7+i]~mv*mi,data=date3)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date3[,7+i]~mv+mi,data=date3)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date3[,38+i]~fv*fi,data=date3)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date3[,38+i]~fv+fi,data=date3)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total2_with29<-as.matrix(summary_m$r)
total2_with29<-cbind(total2_with29,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total2_with29)<-c("male29_r","male29_p","female29_r","female29_p")

date3<-window(date0, start = as.Date('2020-01-01'),end = as.Date('2020-02-26'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date3[,7+i]~mv*mi,data=date3)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date3[,7+i]~mv+mi,data=date3)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date3[,38+i]~fv*fi,data=date3)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date3[,38+i]~fv+fi,data=date3)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total2_with26<-as.matrix(summary_m$r)
total2_with26<-cbind(total2_with26,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total2_with26)<-c("male26_r","male26_p","female26_r","female26_p")

date3<-window(date0, start = as.Date('2020-01-01'),end = as.Date('2020-02-16'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date3[,7+i]~mv*mi,data=date3)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date3[,7+i]~mv+mi,data=date3)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date3[,38+i]~fv*fi,data=date3)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date3[,38+i]~fv+fi,data=date3)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total2_with16<-as.matrix(summary_m$r)
total2_with16<-cbind(total2_with16,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total2_with16)<-c("male16_r","male16_p","female16_r","female16_p")

date3<-window(date0, start = as.Date('2020-01-01'),end = as.Date('2020-02-09'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date3[,7+i]~mv*mi,data=date3)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date3[,7+i]~mv+mi,data=date3)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date3[,38+i]~fv*fi,data=date3)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date3[,38+i]~fv+fi,data=date3)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total2_with09<-as.matrix(summary_m$r)
total2_with09<-cbind(total2_with09,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total2_with09)<-c("male09_r","male09_p","female09_r","female09_p")

total2_with29<-as.data.frame(total2_with29)
total2_with26<-as.data.frame(total2_with26)
total2_with16<-as.data.frame(total2_with16)
total2_with09<-as.data.frame(total2_with09)
total2_with<-as.data.frame(matrix(numeric(0),nrow = 31))
total2_with<-cbind(total2_with,total2_with09,total2_with16,total2_with26,total2_with29,lag)

#r2 and pvalue plot_with interaction
cols=c('02-09'="darkblue", '02-16'="red", '02-26'="orange",'02-29'="darkgreen")
shapes=c('R Square'=16,'P-value'=15)
linetypes=c('R Square'="solid",'P-value'="dashed")
male_with_insidecity<-ggplot(total2_with)+
  geom_point(aes(x=lag,y=male29_r,shape="R Square",colour="02-29"))+geom_line(aes(x=lag,y=male29_r,colour="02-29",linetype="R Square"))+
  geom_point(aes(x=lag,y=male26_r,shape="R Square",colour="02-26"))+geom_line(aes(x=lag,y=male26_r,colour="02-26",linetype="R Square"))+
  geom_point(aes(x=lag,y=male16_r,shape="R Square",colour="02-16"))+geom_line(aes(x=lag,y=male16_r,colour="02-16",linetype="R Square"))+
  geom_point(aes(x=lag,y=male09_r,shape="R Square",colour="02-09"))+geom_line(aes(x=lag,y=male09_r,colour="02-09",linetype="R Square"))+
  scale_y_continuous(limits = c(0,1),breaks = c(0.25,0.5,0.75,1),expand = c(0,0),sec.axis = sec_axis(~.*2,name="P-value",breaks = c(0.5,1,1.5,2)))+
  geom_point(aes(x=lag,y=male29_p/2,shape="P-value",color="02-29"))+geom_line(aes(x=lag,y=male29_p/2,colour="02-29",linetype="P-value"))+
  geom_point(aes(x=lag,y=male26_p/2,shape="P-value",color="02-26"))+geom_line(aes(x=lag,y=male26_p/2,colour="02-26",linetype="P-value"))+
  geom_point(aes(x=lag,y=male16_p/2,shape="P-value",color="02-16"))+geom_line(aes(x=lag,y=male16_p/2,colour="02-16",linetype="P-value"))+
  geom_point(aes(x=lag,y=male09_p/2,shape="P-value",color="02-09"))+geom_line(aes(x=lag,y=male09_p/2,colour="02-09",linetype="P-value"))+
  labs(x="Lag",y="R Square",title="Coefficients of lag model under interaction of inside-city distance(male)")+
  scale_x_continuous(limits = c(0,30),breaks = c(0,5,10,15,20,25,30),expand = c(0,1))+
  scale_colour_manual(name="class",values=cols)+
  scale_shape_manual(name="class",values = shapes)+
  scale_linetype_manual(name="class",values=linetypes)+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())

female_with_insidecity<-ggplot(total2_with)+
  geom_point(aes(x=lag,y=female29_r,shape="R Square",colour="02-29"))+geom_line(aes(x=lag,y=female29_r,colour="02-29",linetype="R Square"))+
  geom_point(aes(x=lag,y=female26_r,shape="R Square",colour="02-26"))+geom_line(aes(x=lag,y=female26_r,colour="02-26",linetype="R Square"))+
  geom_point(aes(x=lag,y=female16_r,shape="R Square",colour="02-16"))+geom_line(aes(x=lag,y=female16_r,colour="02-16",linetype="R Square"))+
  geom_point(aes(x=lag,y=female09_r,shape="R Square",colour="02-09"))+geom_line(aes(x=lag,y=female09_r,colour="02-09",linetype="R Square"))+
  scale_y_continuous(limits = c(0,1),breaks = c(0.25,0.5,0.75,1),expand = c(0,0),sec.axis = sec_axis(~.*2,name="P-value",breaks = c(0.5,1,1.5,2)))+
  geom_point(aes(x=lag,y=female29_p/2,shape="P-value",color="02-29"))+geom_line(aes(x=lag,y=female29_p/2,colour="02-29",linetype="P-value"))+
  geom_point(aes(x=lag,y=female26_p/2,shape="P-value",color="02-26"))+geom_line(aes(x=lag,y=female26_p/2,colour="02-26",linetype="P-value"))+
  geom_point(aes(x=lag,y=female16_p/2,shape="P-value",color="02-16"))+geom_line(aes(x=lag,y=female16_p/2,colour="02-16",linetype="P-value"))+
  geom_point(aes(x=lag,y=female09_p/2,shape="P-value",color="02-09"))+geom_line(aes(x=lag,y=female09_p/2,colour="02-09",linetype="P-value"))+
  labs(x="Lag",y="R Square",title="Coefficients of lag model under interaction of inside-city distance(female)")+
  scale_x_continuous(limits = c(0,30),breaks = c(0,5,10,15,20,25,30),expand = c(0,1))+
  scale_colour_manual(name="class",values=cols)+
  scale_shape_manual(name="class",values = shapes)+
  scale_linetype_manual(name="class",values=linetypes)+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())

plot_grid(male_with_insidecity,female_with_insidecity,ncol=1)


#3
library(dplyr)
date4<-window(date00, start = as.Date('2020-01-01'),end = as.Date('2020-02-29'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date4[,7+i]~mv*mii,data=date4)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date4[,7+i]~mv+mii,data=date4)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date4[,38+i]~fv*fii,data=date4)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date4[,38+i]~fv+fii,data=date4)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total3_with29<-as.matrix(summary_m$r)
total3_with29<-cbind(total3_with29,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total3_with29)<-c("male29_r","male29_p","female29_r","female29_p")

date4<-window(date00, start = as.Date('2020-01-01'),end = as.Date('2020-02-26'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date4[,7+i]~mv*mii,data=date4)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date4[,7+i]~mv+mii,data=date4)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date4[,38+i]~fv*fii,data=date4)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date4[,38+i]~fv+fii,data=date4)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total3_with26<-as.matrix(summary_m$r)
total3_with26<-cbind(total3_with26,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total3_with26)<-c("male26_r","male26_p","female26_r","female26_p")

date4<-window(date00, start = as.Date('2020-01-01'),end = as.Date('2020-02-16'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date4[,7+i]~mv*mii,data=date4)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date4[,7+i]~mv+mii,data=date4)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date4[,38+i]~fv*fii,data=date4)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date4[,38+i]~fv+fii,data=date4)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total3_with16<-as.matrix(summary_m$r)
total3_with16<-cbind(total3_with16,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total3_with16)<-c("male16_r","male16_p","female16_r","female16_p")

date4<-window(date00, start = as.Date('2020-01-01'),end = as.Date('2020-02-09'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date4[,7+i]~mv*mii,data=date4)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date4[,7+i]~mv+mii,data=date4)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date4[,38+i]~fv*fii,data=date4)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date4[,38+i]~fv+fii,data=date4)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total3_with09<-as.matrix(summary_m$r)
total3_with09<-cbind(total3_with09,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total3_with09)<-c("male09_r","male09_p","female09_r","female09_p")

total3_with29<-as.data.frame(total3_with29)
total3_with26<-as.data.frame(total3_with26)
total3_with16<-as.data.frame(total3_with16)
total3_with09<-as.data.frame(total3_with09)
total3_with<-as.data.frame(matrix(numeric(0),nrow = 31))
total3_with<-cbind(total3_with,total3_with09,total3_with16,total3_with26,total3_with29,lag)

#r2 and pvalue plot_with interaction
cols=c('02-09'="darkblue", '02-16'="red", '02-26'="orange",'02-29'="darkgreen")
shapes=c('R Square'=16,'P-value'=15)
linetypes=c('R Square'="solid",'P-value'="dashed")
male_with_intercity<-ggplot(total3_with)+
  geom_point(aes(x=lag,y=male29_r,shape="R Square",colour="02-29"))+geom_line(aes(x=lag,y=male29_r,colour="02-29",linetype="R Square"))+
  geom_point(aes(x=lag,y=male26_r,shape="R Square",colour="02-26"))+geom_line(aes(x=lag,y=male26_r,colour="02-26",linetype="R Square"))+
  geom_point(aes(x=lag,y=male16_r,shape="R Square",colour="02-16"))+geom_line(aes(x=lag,y=male16_r,colour="02-16",linetype="R Square"))+
  geom_point(aes(x=lag,y=male09_r,shape="R Square",colour="02-09"))+geom_line(aes(x=lag,y=male09_r,colour="02-09",linetype="R Square"))+
  scale_y_continuous(limits = c(0,1),breaks = c(0.25,0.5,0.75,1),expand = c(0,0),sec.axis = sec_axis(~.*2,name="P-value",breaks = c(0.5,1,1.5,2)))+
  geom_point(aes(x=lag,y=male29_p/2,shape="P-value",color="02-29"))+geom_line(aes(x=lag,y=male29_p/2,colour="02-29",linetype="P-value"))+
  geom_point(aes(x=lag,y=male26_p/2,shape="P-value",color="02-26"))+geom_line(aes(x=lag,y=male26_p/2,colour="02-26",linetype="P-value"))+
  geom_point(aes(x=lag,y=male16_p/2,shape="P-value",color="02-16"))+geom_line(aes(x=lag,y=male16_p/2,colour="02-16",linetype="P-value"))+
  geom_point(aes(x=lag,y=male09_p/2,shape="P-value",color="02-09"))+geom_line(aes(x=lag,y=male09_p/2,colour="02-09",linetype="P-value"))+
  labs(x="Lag",y="R Square",title="Coefficients of lag model under interaction of inter-city distance(male)")+
  scale_x_continuous(limits = c(0,30),breaks = c(0,5,10,15,20,25,30),expand = c(0,1))+
  scale_colour_manual(name="class",values=cols)+
  scale_shape_manual(name="class",values = shapes)+
  scale_linetype_manual(name="class",values=linetypes)+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())

female_with_intercity<-ggplot(total3_with)+
  geom_point(aes(x=lag,y=female29_r,shape="R Square",colour="02-29"))+geom_line(aes(x=lag,y=female29_r,colour="02-29",linetype="R Square"))+
  geom_point(aes(x=lag,y=female26_r,shape="R Square",colour="02-26"))+geom_line(aes(x=lag,y=female26_r,colour="02-26",linetype="R Square"))+
  geom_point(aes(x=lag,y=female16_r,shape="R Square",colour="02-16"))+geom_line(aes(x=lag,y=female16_r,colour="02-16",linetype="R Square"))+
  geom_point(aes(x=lag,y=female09_r,shape="R Square",colour="02-09"))+geom_line(aes(x=lag,y=female09_r,colour="02-09",linetype="R Square"))+
  scale_y_continuous(limits = c(0,1),breaks = c(0.25,0.5,0.75,1),expand = c(0,0),sec.axis = sec_axis(~.*2,name="P-value",breaks = c(0.5,1,1.5,2)))+
  geom_point(aes(x=lag,y=female29_p/2,shape="P-value",color="02-29"))+geom_line(aes(x=lag,y=female29_p/2,colour="02-29",linetype="P-value"))+
  geom_point(aes(x=lag,y=female26_p/2,shape="P-value",color="02-26"))+geom_line(aes(x=lag,y=female26_p/2,colour="02-26",linetype="P-value"))+
  geom_point(aes(x=lag,y=female16_p/2,shape="P-value",color="02-16"))+geom_line(aes(x=lag,y=female16_p/2,colour="02-16",linetype="P-value"))+
  geom_point(aes(x=lag,y=female09_p/2,shape="P-value",color="02-09"))+geom_line(aes(x=lag,y=female09_p/2,colour="02-09",linetype="P-value"))+
  labs(x="Lag",y="R Square",title="Coefficients of lag model under interaction of inter-city distance(female)")+
  scale_x_continuous(limits = c(0,30),breaks = c(0,5,10,15,20,25,30),expand = c(0,1))+
  scale_colour_manual(name="class",values=cols)+
  scale_shape_manual(name="class",values = shapes)+
  scale_linetype_manual(name="class",values=linetypes)+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())

plot_grid(male_with_intercity,female_with_intercity,ncol=1)




#Lag2
library(xts)
time<-seq(as.Date('2020-01-01'), length=91, by='day')
mc<-xts(sex$male_cases,time)
mv<-xts(sex$vm_log10,time)
fc<-xts(sex$female_cases,time)
fv<-xts(sex$vf_log10,time)
ma<-xts(sex$average_m,time)
fa<-xts(sex$average_f,time)
mi<-xts(sex$insidecity_m,time)
fi<-xts(sex$insidecity_f,time)
mii<-xts(sex$intercity_m,time)
fii<-xts(sex$intercity_f,time)
date<-cbind(mc,mv,fc,fv,ma,fa)
date0<-cbind(mc,mv,fc,fv,mi,fi)
date00<-cbind(mc,mv,fc,fv,mii,fii)
lag<-c(0:30)
lag<-as.data.frame(lag)

for(i in 0:30){
  mc_lag<-lag(mc,k=-i)
  date<-cbind(date,mc_lag)
  date0<-cbind(date0,mc_lag)
  date00<-cbind(date00,mc_lag)
}
for(j in 0:30){
  fc_lag<-lag(fc,k=-j)
  date<-cbind(date,fc_lag)
  date0<-cbind(date0,fc_lag)
  date00<-cbind(date00,fc_lag)
}

date<-date[,-c(7,38)]#备用#



#lag2 
library(dplyr)
#1
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date2[,7+i]~mv*ma,data=date2)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date2[,7+i]~mv+ma,data=date2)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date2[,38+i]~fv*fa,data=date2)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date2[,38+i]~fv+fa,data=date2)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}

date2<-window(date, start = as.Date('2020-01-10'),end = as.Date('2020-02-29'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date2[,7+i]~mv*ma,data=date2)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date2[,7+i]~mv+ma,data=date2)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date2[,38+i]~fv*fa,data=date2)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date2[,38+i]~fv+fa,data=date2)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total_with29<-as.matrix(summary_m$r)
total_with29<-cbind(total_with29,summary_m$pmvma,summary_f$r,summary_f$pfvfa,lag)
colnames(total_with29)<-c("male29_r","male29_p","female29_r","female29_p","lag")


date2<-window(date, start = as.Date('2020-01-10'),end = as.Date('2020-02-26'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date2[,7+i]~mv*ma,data=date2)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date2[,7+i]~mv+ma,data=date2)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date2[,38+i]~fv*fa,data=date2)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date2[,38+i]~fv+fa,data=date2)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total_with26<-as.matrix(summary_m$r)
total_with26<-cbind(total_with26,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total_with26)<-c("male26_r","male26_p","female26_r","female26_p")


date2<-window(date, start = as.Date('2020-01-10'),end = as.Date('2020-02-16'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date2[,7+i]~mv*ma,data=date2)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date2[,7+i]~mv+ma,data=date2)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date2[,38+i]~fv*fa,data=date2)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date2[,38+i]~fv+fa,data=date2)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total_with16<-as.matrix(summary_m$r)
total_with16<-cbind(total_with16,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total_with16)<-c("male16_r","male16_p","female16_r","female16_p")

date2<-window(date, start = as.Date('2020-01-10'),end = as.Date('2020-02-09'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date2[,7+i]~mv*ma,data=date2)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date2[,7+i]~mv+ma,data=date2)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date2[,38+i]~fv*fa,data=date2)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date2[,38+i]~fv+fa,data=date2)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total_with09<-as.matrix(summary_m$r)
total_with09<-cbind(total_with09,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total_with09)<-c("male09_r","male09_p","female09_r","female09_p")

total_with29<-as.data.frame(total_with29)
total_with26<-as.data.frame(total_with26)
total_with16<-as.data.frame(total_with16)
total_with09<-as.data.frame(total_with09)
total_with<-as.data.frame(matrix(numeric(0),nrow = 31))
total_with<-cbind(total_with,total_with09,total_with16,total_with26,total_with29)

#r2 and pvalue plot_with interaction
cols=c('01/10-02/09'="darkblue", '01/10-02/16'="red", '01/10-02/26'="orange",'01/10-02/29'="darkgreen")
shapes=c('R Square'=16,'P-value'=15)
linetypes=c('R Square'="solid",'P-value'="dashed")
male_with_average<-ggplot(total_with)+
  geom_point(aes(x=lag,y=male29_r,shape="R Square",colour="01/10-02/29"))+geom_line(aes(x=lag,y=male29_r,colour="01/10-02/29",linetype="R Square"))+
  geom_point(aes(x=lag,y=male26_r,shape="R Square",colour="01/10-02/26"))+geom_line(aes(x=lag,y=male26_r,colour="01/10-02/26",linetype="R Square"))+
  geom_point(aes(x=lag,y=male16_r,shape="R Square",colour="01/10-02/16"))+geom_line(aes(x=lag,y=male16_r,colour="01/10-02/16",linetype="R Square"))+
  geom_point(aes(x=lag,y=male09_r,shape="R Square",colour="01/10-02/09"))+geom_line(aes(x=lag,y=male09_r,colour="01/10-02/09",linetype="R Square"))+
  scale_y_continuous(limits = c(0,1),breaks = c(0.25,0.5,0.75,1),expand = c(0,0),sec.axis = sec_axis(~.*2,name="P-value",breaks = c(0.5,1,1.5,2)))+
  geom_point(aes(x=lag,y=male29_p/2,shape="P-value",color="01/10-02/29"))+geom_line(aes(x=lag,y=male29_p/2,colour="01/10-02/29",linetype="P-value"))+
  geom_point(aes(x=lag,y=male26_p/2,shape="P-value",color="01/10-02/26"))+geom_line(aes(x=lag,y=male26_p/2,colour="01/10-02/26",linetype="P-value"))+
  geom_point(aes(x=lag,y=male16_p/2,shape="P-value",color="01/10-02/16"))+geom_line(aes(x=lag,y=male16_p/2,colour="01/10-02/16",linetype="P-value"))+
  geom_point(aes(x=lag,y=male09_p/2,shape="P-value",color="01/10-02/09"))+geom_line(aes(x=lag,y=male09_p/2,colour="01/10-02/09",linetype="P-value"))+
  labs(x="Lag",y="R Square",title="Coefficients of lag model under interaction of average distance(male)")+
  scale_x_continuous(limits = c(0,30),breaks = c(0,5,10,15,20,25,30),expand = c(0,1))+
  scale_colour_manual(name="class",values=cols)+
  scale_shape_manual(name="class",values = shapes)+
  scale_linetype_manual(name="class",values=linetypes)+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())



female_with_average<-ggplot(total_with)+
  geom_point(aes(x=lag,y=female29_r,shape="R Square",colour="01/10-02/29"))+geom_line(aes(x=lag,y=female29_r,colour="01/10-02/29",linetype="R Square"))+
  geom_point(aes(x=lag,y=female26_r,shape="R Square",colour="01/10-02/26"))+geom_line(aes(x=lag,y=female26_r,colour="01/10-02/26",linetype="R Square"))+
  geom_point(aes(x=lag,y=female16_r,shape="R Square",colour="01/10-02/16"))+geom_line(aes(x=lag,y=female16_r,colour="01/10-02/16",linetype="R Square"))+
  geom_point(aes(x=lag,y=female09_r,shape="R Square",colour="01/10-02/09"))+geom_line(aes(x=lag,y=female09_r,colour="01/10-02/09",linetype="R Square"))+
  scale_y_continuous(limits = c(0,1),breaks = c(0.25,0.5,0.75,1),expand = c(0,0),sec.axis = sec_axis(~.*2,name="P-value",breaks = c(0.5,1,1.5,2)))+
  geom_point(aes(x=lag,y=female29_p/2,shape="P-value",color="01/10-02/29"))+geom_line(aes(x=lag,y=female29_p/2,colour="01/10-02/29",linetype="P-value"))+
  geom_point(aes(x=lag,y=female26_p/2,shape="P-value",color="01/10-02/26"))+geom_line(aes(x=lag,y=female26_p/2,colour="01/10-02/26",linetype="P-value"))+
  geom_point(aes(x=lag,y=female16_p/2,shape="P-value",color="01/10-02/16"))+geom_line(aes(x=lag,y=female16_p/2,colour="01/10-02/16",linetype="P-value"))+
  geom_point(aes(x=lag,y=female09_p/2,shape="P-value",color="01/10-02/09"))+geom_line(aes(x=lag,y=female09_p/2,colour="01/10-02/09",linetype="P-value"))+
  labs(x="Lag",y="R Square",title="Coefficients of lag model under interaction of average distance(female)")+
  scale_x_continuous(limits = c(0,30),breaks = c(0,5,10,15,20,25,30),expand = c(0,1))+
  scale_colour_manual(name="class",values=cols)+
  scale_shape_manual(name="class",values = shapes)+
  scale_linetype_manual(name="class",values=linetypes)+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())

plot_grid(male_with_average,female_with_average,ncol=1)




#2






#3
library(dplyr)
date4<-window(date00, start = as.Date('2020-01-10'),end = as.Date('2020-02-29'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date4[,7+i]~mv*mii,data=date4)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date4[,7+i]~mv+mii,data=date4)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date4[,38+i]~fv*fii,data=date4)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date4[,38+i]~fv+fii,data=date4)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total3_with29<-as.matrix(summary_m$r)
total3_with29<-cbind(total3_with29,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total3_with29)<-c("male29_r","male29_p","female29_r","female29_p")

date4<-window(date00, start = as.Date('2020-01-10'),end = as.Date('2020-02-26'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date4[,7+i]~mv*mii,data=date4)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date4[,7+i]~mv+mii,data=date4)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date4[,38+i]~fv*fii,data=date4)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date4[,38+i]~fv+fii,data=date4)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total3_with26<-as.matrix(summary_m$r)
total3_with26<-cbind(total3_with26,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total3_with26)<-c("male26_r","male26_p","female26_r","female26_p")

date4<-window(date00, start = as.Date('2020-01-10'),end = as.Date('2020-02-16'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date4[,7+i]~mv*mii,data=date4)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date4[,7+i]~mv+mii,data=date4)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date4[,38+i]~fv*fii,data=date4)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date4[,38+i]~fv+fii,data=date4)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total3_with16<-as.matrix(summary_m$r)
total3_with16<-cbind(total3_with16,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total3_with16)<-c("male16_r","male16_p","female16_r","female16_p")

date4<-window(date00, start = as.Date('2020-01-10'),end = as.Date('2020-02-09'))
summary_m<-data.frame()
summary_f<-data.frame()
summary1<-data.frame()
summary2<-data.frame()
for(i in 0:30){
  fit_m<-lm(date4[,7+i]~mv*mii,data=date4)
  coef_m<-data.frame(r=summary(fit_m)$r.squared,adj_r=summary(fit_m)$adj.r.squared,Pmv=round(coef(summary(fit_m))[2,4],3),pma=round(coef(summary(fit_m))[3,4],3),pmvma=round(coef(summary(fit_m))[4,4],3))
  summary_m<-rbind(summary_m,coef_m)
  fit1<-lm(date4[,7+i]~mv+mii,data=date4)
  coef1<-data.frame(r=summary(fit1)$r.squared,adj_r=summary(fit1)$adj.r.squared,Pmv=round(coef(summary(fit1))[2,4],3),pma=round(coef(summary(fit1))[3,4],3))
  summary1<-rbind(summary1,coef1)
  fit_f<-lm(date4[,38+i]~fv*fii,data=date4)
  coef_f<-data.frame(r=summary(fit_f)$r.squared,adj_r=summary(fit_f)$adj.r.squared,Pfv=round(coef(summary(fit_f))[2,4],3),pfa=round(coef(summary(fit_f))[3,4],3),pfvfa=round(coef(summary(fit_f))[4,4],3))
  summary_f<-rbind(summary_f,coef_f)
  fit2<-lm(date4[,38+i]~fv+fii,data=date4)
  coef2<-data.frame(r=summary(fit2)$r.squared,adj_r=summary(fit2)$adj.r.squared,Pfv=round(coef(summary(fit2))[2,4],3),pfa=round(coef(summary(fit2))[3,4],3))
  summary2<-rbind(summary2,coef2)
}
total3_with09<-as.matrix(summary_m$r)
total3_with09<-cbind(total3_with09,summary_m$pmvma,summary_f$r,summary_f$pfvfa)
colnames(total3_with09)<-c("male09_r","male09_p","female09_r","female09_p")

total3_with29<-as.data.frame(total3_with29)
total3_with26<-as.data.frame(total3_with26)
total3_with16<-as.data.frame(total3_with16)
total3_with09<-as.data.frame(total3_with09)
total3_with<-as.data.frame(matrix(numeric(0),nrow = 31))
total3_with<-cbind(total3_with,total3_with09,total3_with16,total3_with26,total3_with29,lag)

#r2 and pvalue plot_with interaction
cols=c('01/10-02/09'="darkblue", '01/10-02/16'="red", '01/10-02/26'="orange",'01/10-02/29'="darkgreen")
shapes=c('R Square'=16,'P-value'=15)
linetypes=c('R Square'="solid",'P-value'="dashed")
male_with_intercity<-ggplot(total3_with)+
  geom_point(aes(x=lag,y=male29_r,shape="R Square",colour="01/10-02/29"))+geom_line(aes(x=lag,y=male29_r,colour="01/10-02/29",linetype="R Square"))+
  geom_point(aes(x=lag,y=male26_r,shape="R Square",colour="01/10-02/26"))+geom_line(aes(x=lag,y=male26_r,colour="01/10-02/26",linetype="R Square"))+
  geom_point(aes(x=lag,y=male16_r,shape="R Square",colour="01/10-02/16"))+geom_line(aes(x=lag,y=male16_r,colour="01/10-02/16",linetype="R Square"))+
  geom_point(aes(x=lag,y=male09_r,shape="R Square",colour="01/10-02/09"))+geom_line(aes(x=lag,y=male09_r,colour="01/10-02/09",linetype="R Square"))+
  scale_y_continuous(limits = c(0,1),breaks = c(0.25,0.5,0.75,1),expand = c(0,0),sec.axis = sec_axis(~.*2,name="P-value",breaks = c(0.5,1,1.5,2)))+
  geom_point(aes(x=lag,y=male29_p/2,shape="P-value",color="01/10-02/29"))+geom_line(aes(x=lag,y=male29_p/2,colour="01/10-02/29",linetype="P-value"))+
  geom_point(aes(x=lag,y=male26_p/2,shape="P-value",color="01/10-02/26"))+geom_line(aes(x=lag,y=male26_p/2,colour="01/10-02/26",linetype="P-value"))+
  geom_point(aes(x=lag,y=male16_p/2,shape="P-value",color="01/10-02/16"))+geom_line(aes(x=lag,y=male16_p/2,colour="01/10-02/16",linetype="P-value"))+
  geom_point(aes(x=lag,y=male09_p/2,shape="P-value",color="01/10-02/09"))+geom_line(aes(x=lag,y=male09_p/2,colour="01/10-02/09",linetype="P-value"))+
  labs(x="Lag",y="R Square",title="Coefficients of lag model under interaction of inter-city distance(male)")+
  scale_x_continuous(limits = c(0,30),breaks = c(0,5,10,15,20,25,30),expand = c(0,1))+
  scale_colour_manual(name="class",values=cols)+
  scale_shape_manual(name="class",values = shapes)+
  scale_linetype_manual(name="class",values=linetypes)+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())

female_with_intercity<-ggplot(total3_with)+
  geom_point(aes(x=lag,y=female29_r,shape="R Square",colour="01/10-02/29"))+geom_line(aes(x=lag,y=female29_r,colour="01/10-02/29",linetype="R Square"))+
  geom_point(aes(x=lag,y=female26_r,shape="R Square",colour="01/10-02/26"))+geom_line(aes(x=lag,y=female26_r,colour="01/10-02/26",linetype="R Square"))+
  geom_point(aes(x=lag,y=female16_r,shape="R Square",colour="01/10-02/16"))+geom_line(aes(x=lag,y=female16_r,colour="01/10-02/16",linetype="R Square"))+
  geom_point(aes(x=lag,y=female09_r,shape="R Square",colour="01/10-02/09"))+geom_line(aes(x=lag,y=female09_r,colour="01/10-02/09",linetype="R Square"))+
  scale_y_continuous(limits = c(0,1),breaks = c(0.25,0.5,0.75,1),expand = c(0,0),sec.axis = sec_axis(~.*2,name="P-value",breaks = c(0.5,1,1.5,2)))+
  geom_point(aes(x=lag,y=female29_p/2,shape="P-value",color="01/10-02/29"))+geom_line(aes(x=lag,y=female29_p/2,colour="01/10-02/29",linetype="P-value"))+
  geom_point(aes(x=lag,y=female26_p/2,shape="P-value",color="01/10-02/26"))+geom_line(aes(x=lag,y=female26_p/2,colour="01/10-02/26",linetype="P-value"))+
  geom_point(aes(x=lag,y=female16_p/2,shape="P-value",color="01/10-02/16"))+geom_line(aes(x=lag,y=female16_p/2,colour="01/10-02/16",linetype="P-value"))+
  geom_point(aes(x=lag,y=female09_p/2,shape="P-value",color="01/10-02/09"))+geom_line(aes(x=lag,y=female09_p/2,colour="01/10-02/09",linetype="P-value"))+
  labs(x="Lag",y="R Square",title="Coefficients of lag model under interaction of inter-city distance(female)")+
  scale_x_continuous(limits = c(0,30),breaks = c(0,5,10,15,20,25,30),expand = c(0,1))+
  scale_colour_manual(name="class",values=cols)+
  scale_shape_manual(name="class",values = shapes)+
  scale_linetype_manual(name="class",values=linetypes)+
  theme_classic()+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())

plot_grid(male_with_intercity,female_with_intercity,ncol=1)





plot1<-plot(effect("mv:ma",fit_m,,list(ma=c(5000,6000,7000,8000))),multiline=TRUE,title="male")  
plot2<-plot(effect("fv:fa",fit_f,,list(fa=c(4000,5000,6000,7000))),multiline=TRUE,title="female")
plot_grid(plot1,plot2,ncol=2,byrow=T)



plot1<-plot(effect("mv:ma",fit_m),multiline=TRUE,title="male")  
plot2<-plot(effect("fv:fa",fit_f),multiline=TRUE,title="female")
plot_grid(plot1,plot2,ncol=2,byrow=T)
########################## Part I: Impact of mobility reduction on COVID-19 transmission #########################
library(mgcv)
library(nlme)
col=c("#e41a1c","#737373","#006d2c","#984ea3","#ff7f00","#4292c6","#a65628","#ce1256","#084081",
      "#67001f","#d6604d","#4393c3","#8073ac","#2d004b","#b35806","#8c510a","#80cdc1","#01665e",
      "#003c30","#d9f0d3","#c51b7d","#fdbb84","#d7301f","#08306b","#993404","#525252")
col11<-col[1:11]

windows(width=70,height=120)
par(mfrow = c(5,1))

# plot 1: mobilty volume in  CN 9 CITIES
pop<-c(161.2205,160.0748,55.2959,46.7320,34.6933,82.3922,48.3647,269.9939,408.1051)  # users for each city *10k 

par(mar = c(1, 5, 2, 2))
modgam_dg<-gam(m$m2020_dg/pop[1]~s(m$day),data = m)  # gam model
plot(m$day,m$m2020_dg/pop[1],pch=19,col=col[1],xlab="Date",ylab="Mobility Volume (/10000)",ylim = c(0,35000),
     cex=0.1,xaxt="n", yaxt="n",yaxs='i',bty="l",  # raw data dot
     panel.first = rect(24, -1e6, 31, 1e6, col='FloralWhite', border=NA), # add shadow
     mgp = c(4, 1, 0))  
axis(side=1,lwd=0.005,col = '#525252', at=c(10,15,20,25,30,35,40,45,50,55,60), labels = c("Jan 10","Jan 15","Jan 20","Jan 25","Jan 30","Feb 4","Feb 9","Feb 14","Feb 19","Feb 24","Feb 29"))
axis(side=2,lwd=0.005,col = '#525252', at=seq(0,35000,by=5000), las=1)
lines(m$day,fitted(modgam_dg),col=col[1],lwd=0.15) # fitted gam line

modgam_fs<-gam(m$m2020_fs/pop[2]~s(m$day),data = m)
lines(m$day,m$m2020_fs /pop[2],pch=19,type = "p",col=col[2],cex=0.3)
lines(m$day,fitted(modgam_fs),col=col[2],lwd=0.15) 
modgam_hz<-gam(m$m2020_hz/pop[3]~s(m$day),data = m)
lines(m$day,m$m2020_hz/pop[3],pch=19,type = "p",col=col[3],cex=0.3)
lines(m$day,fitted(modgam_hz),col=col[3],lwd=0.15)
modgam_jm<-gam(m$m2020_jm/pop[4]~s(m$day),data = m)
lines(m$day,m$m2020_jm/pop[4],pch=19,type = "p",col=col[4],cex=0.3)
lines(m$day,fitted(modgam_jm),col=col[4],lwd=0.15) 
modgam_zq<-gam(m$m2020_zq/pop[5]~s(m$day),data = m)
lines(m$day,m$m2020_zq/pop[5],pch=19,type = "p",col=col[5],cex=0.3)
lines(m$day,fitted(modgam_zq),col=col[5],lwd=0.15)
modgam_zs<-gam(m$m2020_zs/pop[6]~s(m$day),data = m)
lines(m$day,m$m2020_zs/pop[6],pch=19,type = "p",col=col[6],cex=0.3)
lines(m$day,fitted(modgam_zs),col=col[6],lwd=0.15) 
modgam_zh<-gam(m$m2020_zh/pop[7]~s(m$day),data = m)
lines(m$day,m$m2020_zh/pop[7],pch=19,type = "p",col=col[7],cex=0.3)
lines(m$day,fitted(modgam_zh),col=col[7],lwd=0.15) 
modgam_sz<-gam(m$m2020_sz/pop[8]~s(m$day),data = m)
lines(m$day,m$m2020_sz/pop[8],pch=19,type = "p",col=col[8],cex=0.3)
lines(m$day,fitted(modgam_sz),col=col[8],lwd=0.15) 
modgam_gz<-gam(m$m2020_gz/pop[9]~s(m$day),data = m)
lines(m$day,m$m2020_gz/pop[9],pch=19,type = "p",col=col[9],cex=0.3)
lines(m$day,fitted(modgam_gz),col=col[9],lwd=0.15) 

abline(v = c(24,31,33,40),lty = 2,col="#969696")

# add text of intervention
mtext("Guangdong \nLevel 1 response", side=3, line=-1.5,col = "grey20", at =20, cex =0.5 )
mtext("Spring\n Festivel", side=3, line=-1.5, at =27.5, col = "grey20",cex =0.5 )
mtext("Festivel\n extended", side=3, line=-1.5, at =32, col = "grey20",cex =0.5 )
mtext("Workplace\n closed", side=3, line=-1.5, at =37,col = "grey20", cex =0.5 )
mtext("Resume work\n by steps", side=3, line=-1.5, at =43, col = "grey20",cex =0.5 )

plot.new()   
######## plot 2 : all growth rate
# 3-day average moving for all
m$cases_aver3<-0  
for(i in 3:length(m$day)) {
  m$cases_aver3[i]<-(m$cases[i]+m$cases[i-1]+m$cases[i-2])/3
}

# 7-day average moving for all
m$cases_aver7<-0  
for(i in 7:length(m$day)) {
  m$cases_aver7[i]<-mean(m$cases[i:(i-6)])
}

m$gr<-m$cases_aver3/m$cases_aver7

library(ggplot2)
vp <- viewport(height = unit(1,"npc"), width=unit(1, "npc"), 
               just = c("left","top"),
               y = 1, x = 0)

m$day<-as.numeric(m$day)
p<-ggplot(m) + 
  geom_rect(aes(xmin=24, xmax=31, ymin=0, ymax=Inf),fill="FloralWhite",alpha=1)+ # add shadow for intervention
  # add vertical line
  geom_vline(xintercept = 24, linetype="dashed",  color = "#969696", size=0.3)+  # add vertical line
  geom_vline(xintercept = 31, linetype="dashed",  color = "#969696", size=0.3)+
  geom_vline(xintercept = 33, linetype="dashed",  color = "#969696", size=0.3)+
  geom_vline(xintercept = 40, linetype="dashed",  color = "#969696", size=0.3)+
  #add growth rate
  geom_point(aes(x = day, y = gr ), size=0.05, color = "#08519c", fill ="#08519c", group =2)+    #add raw data points
  geom_smooth(aes(x = day, y = gr ),method = "gam",se=T,fill = "#bdd7e7",size=0.1,show.legend = FALSE,col="#08519c")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5, face = "italic"),
        axis.text = element_text(angle = 0, hjust = 0.5,color = "black",size = 6.5),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line(),
        axis.line = element_line(colour = 'black', size = 0.3),
        plot.margin = unit(c(0,0.7,0,0.2), "cm"),
        axis.title=element_text(size=8.3),
        axis.ticks.length=unit(.18, "cm"))+
  labs(x="", y="GR")+
  scale_x_continuous(breaks=seq(19,60,4),
                     labels = c("Jan 19","Jan 23","Jan 27","Jan 31","Feb 4","Feb 8","Feb 12","Feb 16","Feb 20","Feb 24","c"),
                     limits=c(17,52.5),expand=c(0,0))+
  scale_y_continuous(breaks=seq(0,3,0.5),limits=c(0,3),expand = expand_scale(mult = c(0, 0), 
                                                                             add = c(0,0)))

print(p,vp = vp)    # add ggplot to the window


##########plot 3:  rolling correaltion between mobility volume and cases
library(ggplot2)
library(gridBase)
library(grid)
plot.new()              ## 
vps <- baseViewports()
pushViewport(vps$figure) ##   
vp1 <-plotViewport(c(1.40,0.5,3,1.6)) ## create new vp with margins
vp <- viewport(height = unit(5,"npc"), width=unit(0.55, "npc"), 
               just = c("left","top"),
               y = -1.5, x =0.005)

library(ggplot2)
require(roll)
result <- roll_cor(x=m$m2020,y=m$gr,width =window)  
cor<-data.frame(day=c(1:10),cor=result)
p3<-ggplot(cor, aes(x=day, y=cor))+
  geom_line(size=0.4,color="#045a8d")+
  geom_point(size=0.9,color="#252525")+
  scale_x_continuous(breaks= seq(1,11, by = 2),
                     labels = c("Jan 24","Jan 26","Jan 28","Jan 30","Feb 2","c"))+
  scale_y_continuous(breaks= seq(0,1, by = 0.2), limits =c(0,1), expand = c(0,0))+
  labs(x="Date", y="Correlation")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5, face = "italic"),
        axis.text = element_text(angle = 0, hjust = 0.5,size =7,colour = "black" ),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        axis.line = element_line(colour = 'black', size = 0.3),
        axis.title=element_text(size=8),
        plot.margin = unit(c(0.5,1.1,0,0.2), "cm"))+
  annotate(geom="text", x=5, y=0.6, size=2.5,label="Static correlation ", color="#000000")+
  annotate(geom="text", x=6, y=1, size=2.5,label="Dynamic correlation ", color="#000000")+
  geom_hline(yintercept = 0.708, linetype="dashed",  color = "#67000d", size=0.3)  # add static correlation line

print(p3,vp = vp)    # add ggplot to the window

################### plot 4:  daily incidence vs Moblity rate (after the splitting date of Feb 3£¬linear relathionship)
library(ggplot2)
library(gridBase)
library(grid)
plot.new()              ## 
vps <- baseViewports()
pushViewport(vps$figure) ##   
vp1 <-plotViewport(c(1.40,0.5,3,1.6)) ## create new vp with margins
vp <- viewport(height = unit(25,"npc"), width=unit(0.45, "npc"), 
               just = c("left","top"),
               y = -7.8, x =0.5)

n=2
d<-data.frame(mr=log(m$m2020),cases=m$gr)
p4<-ggplot(d) + 
  geom_point(aes(x = mr, y = cases ), size = 0.8, color = "#08519c", fill = "#969696", group =2)+    #add raw data points
  geom_smooth(aes(x = mr, y = cases ),method = "glm",se=T,fill = "#fcbba1",size=0.05,show.legend = FALSE,col="#67000d")+   #
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5, face = "italic"),
        axis.text = element_text(angle = 0, hjust = 0.5,size =7,colour = "black" ),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        axis.line = element_line(colour = 'black', size = 0.3),
        axis.title=element_text(size=8),
        plot.margin = unit(c(0.5,0,0,0), "cm"))+
  scale_x_continuous(breaks=seq(15.5,20,0.3))+
  scale_y_continuous(breaks=seq(0.3,2.8,by=0.5), limits=c(0.3,2.8), expand = expand_scale(mult = c(0.0, 0), 
                                                                                          add = c(0,0)))+
  labs(x="Mobility volume (log)", y="GR")

print(p4,vp = vp)    # add ggplot to the window

### get 95% ci for slope ###
mod<-glm(m$gr~log(m$m2020),data=m)
confint(mod)  
summary(mod)
require(rsq)
rsq(mod)
confint(mod)*log(0.9)


############### Part II: modifying effect of age and sex ###############################
########### calculat mobility ratio and growth ratio ####################
# mobility ratio for sex and age
m$mr_m<-m$male2020/mean_male_mobility
m$mr_f<-m$female2020/mean_female_mobility
range(m$mr_f)
range(m$mr_m)

m$mr_18<-m$age18_2020/mean18
m$mr_29<-m$age29_2020/mean29
m$mr_39<-m$age39_2020/mean39
m$mr_49<-m$age49_2020/mean49
m$mr_59<-m$age59_2020/mean59
m$mr_60<-m$age60_2020/mean60

# growth rate for all, sex and age
m$cases_aver3<-0  
for(i in 3:length(m$day)) {
  m$cases_aver3[i]<-(m$cases[i]+m$cases[i-1]+m$cases[i-2])/3
}

# 7-day average moving 
m$cases_aver7<-0  
for(i in 7:length(m$day)) {
  m$cases_aver7[i]<-mean(m$cases[i:(i-6)])
}

m$gr<-m$cases_aver3/m$cases_aver7

# 3-day average moving 
m$male_cases_aver3<-0  
for(i in 3:length(m$day)) {
  m$male_cases_aver3[i]<-(m$male_cases[i]+m$male_cases[i-1]+m$male_cases[i-2])/3
}

m$female_cases_aver3<-0  
for(i in 3:length(m$day)) {
  m$female_cases_aver3[i]<-(m$female_cases[i]+m$female_cases[i-1]+m$female_cases[i-2])/3
}

# 7-day average moving 
m$male_cases_aver7<-0  
for(i in 7:length(m$day)) {
  m$male_cases_aver7[i]<-mean(m$male_cases[i:(i-6)])
}

m$female_cases_aver7<-0  
for(i in 7:length(m$day)) {
  m$female_cases_aver7[i]<-mean(m$female_cases[i:(i-6)])
}

m$gr_f<-m$female_cases_aver3/m$female_cases_aver7
m$gr_m<-m$male_cases_aver3/m$male_cases_aver7


# age groups cases
m$cases_18_aver3<-0 
m$cases_29_aver3<-0 
m$cases_39_aver3<-0 
m$cases_49_aver3<-0 
m$cases_59_aver3<-0 
m$cases_60_aver3<-0 
for(i in 3:length(m$day)) {
  m$cases_18_aver3[i]<-(m$cases_18[i]+m$cases_18[i-1]+m$cases_18[i-2])/3
  m$cases_29_aver3[i]<-(m$cases_29[i]+m$cases_29[i-1]+m$cases_29[i-2])/3
  m$cases_39_aver3[i]<-(m$cases_39[i]+m$cases_39[i-1]+m$cases_39[i-2])/3
  m$cases_49_aver3[i]<-(m$cases_49[i]+m$cases_49[i-1]+m$cases_49[i-2])/3
  m$cases_59_aver3[i]<-(m$cases_59[i]+m$cases_59[i-1]+m$cases_59[i-2])/3
  m$cases_60_aver3[i]<-(m$cases_60[i]+m$cases_60[i-1]+m$cases_60[i-2])/3
}

# age groups cases
m$cases_18_aver7<-0 
m$cases_29_aver7<-0 
m$cases_39_aver7<-0 
m$cases_49_aver7<-0 
m$cases_59_aver7<-0 
m$cases_60_aver7<-0 
for(i in 7:length(m$day)) {
  m$cases_18_aver7[i]<-mean(m$cases_18[i:(i-6)])
  m$cases_29_aver7[i]<-mean(m$cases_29[i:(i-6)])
  m$cases_39_aver7[i]<-mean(m$cases_39[i:(i-6)])
  m$cases_49_aver7[i]<-mean(m$cases_49[i:(i-6)])
  m$cases_59_aver7[i]<-mean(m$cases_59[i:(i-6)])
  m$cases_60_aver7[i]<-mean(m$cases_60[i:(i-6)])
}

m$gr_18<-0
m$gr_29<-0 
m$gr_39<-0 
m$gr_49<-0 
m$gr_59<-0
m$gr_60<-0
m$gr_18<-m$cases_18_aver3/m$cases_18_aver7 
m$gr_29<-m$cases_29_aver3/m$cases_29_aver7 
m$gr_39<-m$cases_39_aver3/m$cases_39_aver7 
m$gr_49<-m$cases_49_aver3/m$cases_49_aver7 
m$gr_59<-m$cases_59_aver3/m$cases_59_aver7 
m$gr_60<-m$cases_60_aver3/m$cases_60_aver7 


########### col and window ##########
col=c("#377eb8","#e41a1c","#00441b","#525252","#3f007d","#a63603")  # À¶£¬ºì£¬ÂÌ£¬»Ò,×Ï£¬³È
shadow=c("#9ecae1","#fcbba1","#ccece6","#bdbdbd","#bcbddc","#fdd0a2")

require(mgcv)

windows(width=75,height=50)
par(mfrow = c(3,4))

################## plot 1: sex-specific mobility volume / million population
# male 1749.3 *10k, female 1725.81*10k (2018 goverment data)
mod_male<-gam(m$male2020/1749.3~s(m$day,bs="cr", k=15),knots=list(m$day),data = m,method="REML")
mod_female<-gam(m$female2020/1725.81~s(m$day,bs="cr", k=15),knots=list(m$day),data = m,method="REML")

plot(m$day,m$male2020/1749.3,pch=19,col=col[1],ylim=c(-500,12500),xlab="Date",ylab="", xaxt="n",yaxt="n",yaxs='i',
     cex=0.25,bty="l",cex.lab=1.2)   # dots
axis(side=1,at=seq(1,61,by=10), labels = c("Jan 1","Jan 11","Jan 21","Jan 31","Feb 10","Feb 20","Feb 30"))
axis(side=2, at=seq(0,12500,by=2500),cex.axis=1)
mtext("Mobility volume/ 10000 population", side=2, line=2.5, cex=0.75)
lines(m$day[1:60],fitted(mod_male),col=col[1],lwd=1) # fitted lines
lines(m$day[1:60],m$female2020[1:60]/1725.81,pch=19,type = "p",col=col[2],cex=0.25)
lines(m$day[1:60],fitted(mod_female),col=col[2],lwd=1) 
abline(v = c(24,31,33,40),lty = 2,col="#969696")

################## plot 2: male and female GR
mod_male<-gam(m$gr_m~s(m$day,bs="cr", k=15),knots=list(m$day),data = m,method="REML")
mod_female<-gam(m$gr_f~s(m$day,bs="cr", k=15),knots=list(m$day),data = m,method="REML")

plot(m$day,m$gr_m,pch=20,col=col[1],ylim=c(0,3),xlab="Date",ylab="Sex-specific growth rate",xaxt="n",yaxt="n",yaxs='i',
     cex=0.25,bty="l",cex.lab=1.2)   # dots
axis(side=1, at=seq(20,45,by=5),labels = c("Jan 20","Jan 25","Jan 30","Feb 4","Feb 9","Feb 14"))
axis(side=2, at=seq(0,3,by=0.5), las=1)
lines(m$day,fitted(mod_male),col=col[1]) # fitted lines

lines(m$day,m$gr_f,pch=20,type = "p",col=col[2],cex=0.25)
lines(m$day,fitted(mod_female),col=col[2]) 
abline(v = c(24,31,33,40),lty = 2,col="#969696")

legend(-10, 3.5, legend=c("Male","Female"), xpd=NA,
       col=col[1:2], pch=c(19,19),lty=c(1,1), cex=1, box.lty=0,ncol=2)

################## plot 3:  regression for moblity and sex-specific GR #######################
library(ggplot2)
library(gridBase)
library(grid)
plot.new()              ## 
vps <- baseViewports()
pushViewport(vps$figure) ##   
vp1 <-plotViewport(c(1.45,0.5,3,1.6)) ## create new vp with margins
vp <- viewport(height = unit(0.85,"npc"), width=unit(1.04, "npc"), 
               just = c("left","top"),
               y = 0.93, x = 0.01)

require(ggplot2)

# plot for regression: y: sex-specific GR x: sex-mobility
require(ggplot2)
p3<-ggplot(d) + 
  geom_point(aes(x = m, y = c_m ), size=0.01, color = col[1], fill =col[1], group =2)+    #add raw data points
  geom_smooth(aes(x = m, y = c_m ),method = "glm",se=T,fill = "#bdd7e7",size=0.05,show.legend = FALSE,col=col[1])+ 
  geom_point(aes(x = f, y = c_f ), size=0.01, color = col[2], fill = col[2], group =2)+    #add raw data points
  geom_smooth(aes(x = f, y = c_f ),method = "glm",se=T,fill = "#fcae91",size=0.05,show.legend = FALSE,col=col[2])+ 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5, face = "italic"),
        axis.text = element_text(angle = 0, hjust = 0.5,color = "black",size = 7.5),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line(),
        axis.ticks.length=unit(.18, "cm"),
        axis.title=element_text(size=10),
        plot.margin = unit(c(0.8,1,0,0.2), "cm"))+
  labs(x="Mobility volume (log)", y="Sex-specific growth rate")+
  scale_x_continuous(breaks=seq(14,17.0,0.5),limits =c(14,16.6))+
  scale_y_continuous(breaks=seq(0.25,2.75,0.5),limits=c(0.25,2.75),expand = expand_scale(mult = c(0, 0), 
                                                                                         add = c(0,0)))+
  annotate(geom="text", x=15.8, y=2.5, size=2.8,label="All P values for slopes <0.05", color="#000000")

print(p3,vp = vp)    # add ggplot to the window

######################  plot 4: x:sex-specific mobility  y: all cases GR ##########################
library(ggplot2)
library(gridBase)
library(grid)
plot.new()              ## 
vps <- baseViewports()
pushViewport(vps$figure) ##   
vp1 <-plotViewport(c(1.45,0.5,3,1.6)) ## create new vp with margins
vp <- viewport(height = unit(2.5,"npc"), width=unit(4.1, "npc"), 
               just = c("left","top"),
               y = 0.75, x = 1)

p4<-ggplot(d) + 
  geom_point(aes(x = m, y = c ), size=0.01, color = col[1], fill =col[1], group =2)+    #add raw data points
  geom_smooth(aes(x = m, y = c ),method = "glm",se=T,fill = "#bdd7e7",size=0.05,show.legend = FALSE,col=col[1])+ 
  geom_point(aes(x = f, y = c ), size=0.01, color = col[2], fill = col[2], group =2)+    #add raw data points
  geom_smooth(aes(x = f, y = c ),method = "glm",se=T,fill = "#fcae91",size=0.05,show.legend = FALSE,col=col[2])+ 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5, face = "italic"),
        axis.text = element_text(angle = 0, hjust = 0.5,color = "black",size = 7.5),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line(),
        axis.ticks.length=unit(.18, "cm"),
        axis.title=element_text(size=10),
        plot.margin = unit(c(0.8,1,0,0.2), "cm"))+
  labs(x="Mobility volume (log)", y="All growth rate")+
  scale_x_continuous(breaks=seq(14,17.0,0.5),limits =c(14,16.6))+
  scale_y_continuous(breaks=seq(0.25,2.75,0.5),limits=c(0.25,2.75),expand = expand_scale(mult = c(0, 0), 
                                                                                         add = c(0,0)))+
  annotate(geom="text", x=15.8, y=2.5, size=2.8,label="All P values for slopes <0.05", color="#000000")

print(p4,vp = vp)    # add ggplot to the window

##=========================================================##
######### age-specific mobilty and cases , correaltion#######
##=========================================================##
####### plot 5: age-specific mobility #############
pop<-c(49.6104,	514.7723,	338.9312,	187.8897,	84.2587,	32.2438)

mod<-gam(m[,23]/pop[1]~s(m$day,bs="cr", k=15),knots=list(m$day),data = m,method="REML")
plot(m$day,m[,23]/pop[1],pch=19,col=col[1],ylim=c(-500,25000),xlab="Date",ylab="Mobility volume/ 10000 population",cex=0.25,
     xaxt="n",bty="l",yaxs='i',cex.lab=1.1)   # dots
axis(side=1,at=seq(1,61,by=10), labels = c("Jan 1","Jan 11","Jan 21","Jan 31","Feb 10","Feb 20","Feb 30"))
lines(m$day,fitted(mod),col=col[1],lwd=0.5) # fitted lines
abline(v = c(24,31,33,40),lty = 2,col="#969696")

# for other 5 age groups
for(i in 1:5) {
  lines(m$day,m[,(23+i*2)]/pop[1+i],pch=19,type = "p",col=col[i+1],cex=0.25)
  mod<-gam(m[,(23+i*2)]/pop[1+i]~s(m$day,bs="cr", k=15),knots=list(m$day[1:60]),data = m,method="REML")  
  lines(m$day,fitted(mod),col=col[i+1],lwd=0.5) 
}

################## plot 6 : age-specific GR ######################
plot(m$day,m$gr_18,pch=19,col=col[1],xlim=c(20,45),ylim=c(-0.01,3),xlab="Date",ylab="Age-specific growth rate",cex=0.25,
     bty="l",yaxs='i',cex.lab=1.2,yaxt="n",
     xaxt="n")   # dots
axis(side=1, at=seq(20,45,by=5),labels = c("Jan 20","Jan 25","Jan 30","Feb 4","Feb 9","Feb 14"))
axis(side=2, at=seq(0,3,by=0.5), las=1)
lines(m$day,fitted(mod18),col=col[1],lwd=0.5) # fitted lines
abline(v = c(24,31,33,40),lty = 2,col="#969696")

# age 29
lines(m$day,m$gr_29,pch=20,type = "p",col=col[2],cex=0.25)
lines(m$day,fitted(mod29),col=col[2],lwd=0.5) 
# age 39
lines(m$day,m$gr_39,pch=20,type = "p",col=col[3],cex=0.25)
lines(m$day,fitted(mod39),col=col[3],lwd=0.5)
# age 49
lines(m$day,m$gr_49,pch=20,type = "p",col=col[4],cex=0.25)
lines(m$day,fitted(mod49),col=col[4],lwd=0.5)
# age 59
lines(m$day,m$gr_59,pch=20,type = "p",col=col[5],cex=0.25)
lines(m$day,fitted(mod59),col=col[5],lwd=0.5)
# age 60
lines(m$day,m$gr_60,pch=20,type = "p",col=col[6],cex=0.25)
lines(m$day,fitted(mod60),col=col[6],lwd=0.5)


legend(-15, 3.9, legend=c("<18 years","18-29 years","30-39 years","40-49 years","50-59 years",">60 years"), xpd=NA,
       col=col, pch=c(19,19,19,19,19,19),lty=c(1,1,1,1,1,1), cex=1, box.lty=0,ncol=6)


################## plot 7: gam model for age-specific gr and log (mobility)#########################
library(ggplot2)
library(gridBase)
library(grid)
plot.new()             
vps <- baseViewports()
pushViewport(vps$figure) ##  
vp1 <-plotViewport(c(0,0,10,10)) ## create new vp with margins
vp <- viewport(height = unit(7.5,"npc"), width=unit(16.5, "npc"), 
               just = c("left","top"),
               y = -7.7, x = -13.8)

#### regression slope  ####
col=c("#377eb8","#e41a1c","#00441b","#525252","#3f007d","#a63603")  # À¶£¬ºì£¬ÂÌ£¬»Ò,×Ï£¬³È
shadow=c("#9ecae1","#fcbba1","#ccece6","#bdbdbd","#bcbddc","#fdd0a2")

require(ggplot2)
p7<-ggplot() + 
  geom_point(data=d18,aes(x=m, y = c), size=0.01, color = col[1], fill = col[1], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d18,aes(x=m, y = c),method = "glm",se=T,fill = shadow[1],size=0.05,show.legend = FALSE,col=col[1])+ 
  geom_point(data=d29,aes(x=m, y = c),size=0.01, color = col[2], fill = col[2], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d29,aes(x=m, y = c),method = "glm",se=T,fill = shadow[2],size=0.05,show.legend = FALSE,col=col[2])+
  geom_point(data=d39,aes(x=m, y = c),size=0.01, color = col[3], fill = col[2], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d39,aes(x=m, y = c),method = "glm",se=T,fill = shadow[3],size=0.05,show.legend = FALSE,col=col[3])+
  geom_point(data=d49,aes(x=m, y = c),size=0.01, color = col[4], fill = col[2], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d49,aes(x=m, y = c),method = "glm",se=T,fill = shadow[4],size=0.05,show.legend = FALSE,col=col[4])+
  geom_point(data=d59,aes(x=m, y = c),size=0.01, color = col[5], fill = col[2], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d59,aes(x=m, y = c),method = "glm",se=T,fill = shadow[5],size=0.05,show.legend = FALSE,col=col[5])+
  geom_point(data=d60,aes(x=m, y = c),size=0.01, color = col[6], fill = col[2], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d60,aes(x=m, y = c),method = "glm",se=T,fill = shadow[6],size=0.05,show.legend = FALSE,col=col[6])+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5, face = "italic"),
        axis.text = element_text(angle = 0, hjust = 0.5,color = "black",size = 7.5),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line(),
        axis.title=element_text(size=10),
        axis.ticks.length=unit(.18, "cm"),
        plot.margin = unit(c(0.8,1,0,0.2), "cm"))+
  labs(x="Mobility (log)", y="Age-specific growth rate")+
  scale_x_continuous(breaks=seq(11.5,16.0,0.5),limits =c(11.5,16.0))+
  scale_y_continuous(breaks=seq(0.25,2.75,0.5),limits=c(0.25,2.75),expand = expand_scale(mult = c(0, 0),add = c(0,0)))+
  annotate(geom="text", x=14.8, y=2.7, size=2.8,label="All P values for slopes <0.05", color="#000000")

print(p7,vp = vp)    # add ggplot to the window

################## plot 8: gam model for all gr and log (mobilty )#########################
library(ggplot2)
library(gridBase)
library(grid)
plot.new()              
vps <- baseViewports()
pushViewport(vps$figure) ##   
vp1 <-plotViewport(c(0,0,10,10)) ## create new vp with margins
vp <- viewport(height = unit(22.22,"npc"), width=unit(66, "npc"), 
               just = c("left","top"),
               y = -24.45, x =5)

require(ggplot2)
p8<-ggplot() + 
  geom_point(data=d18,aes(x=m, y = c), size=0.01, color = col[1], fill = col[1], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d18,aes(x=m, y = c),method = "glm",se=T,fill = shadow[1],size=0.05,show.legend = FALSE,col=col[1])+ 
  geom_point(data=d29,aes(x=m, y = c),size=0.01, color = col[2], fill = col[2], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d29,aes(x=m, y = c),method = "glm",se=T,fill = shadow[2],size=0.05,show.legend = FALSE,col=col[2])+
  geom_point(data=d39,aes(x=m, y = c),size=0.01, color = col[3], fill = col[2], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d39,aes(x=m, y = c),method = "glm",se=T,fill = shadow[3],size=0.05,show.legend = FALSE,col=col[3])+
  geom_point(data=d49,aes(x=m, y = c),size=0.01, color = col[4], fill = col[2], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d49,aes(x=m, y = c),method = "glm",se=T,fill = shadow[4],size=0.05,show.legend = FALSE,col=col[4])+
  geom_point(data=d59,aes(x=m, y = c),size=0.01, color = col[5], fill = col[2], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d59,aes(x=m, y = c),method = "glm",se=T,fill = shadow[5],size=0.05,show.legend = FALSE,col=col[5])+
  geom_point(data=d60,aes(x=m, y = c),size=0.01, color = col[6], fill = col[2], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(data=d60,aes(x=m, y = c),method = "glm",se=T,fill = shadow[6],size=0.05,show.legend = FALSE,col=col[6])+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5, face = "italic"),
        axis.text = element_text(angle = 0, hjust = 0.5,color = "black",size = 7.5),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line(),
        axis.title=element_text(size=10),
        axis.ticks.length=unit(.18, "cm"),
        plot.margin = unit(c(0.8,1,0,0.2), "cm"))+
  labs(x="Mobility (log)", y="All growth rate")+
  scale_x_continuous(breaks=seq(11.5,16.0,0.5),limits =c(11.5,16.0))+
  scale_y_continuous(breaks=seq(0.25,2.75,0.5),limits=c(0.25,2.75),expand = expand_scale(mult = c(0, 0),add = c(0,0)))+
  annotate(geom="text", x=14.8, y=2.7, size=2.8,label="All P values for slopes <0.05", color="#000000")


print(p8,vp = vp)    # add ggplot to the window


################ Part III: modify effect of destinations  ######################
windows(width=70,height=29)
par(mfrow = c(2,4))
plot.new()
plot.new()
plot.new()
plot.new()

col=c("#a63603","#377eb8","#3f007d","#e41a1c","#00441b","#525252")  # 
shadow=c("#fdd0a2","#9ecae1","#bcbddc","#fcbba1","#ccece6","#bdbdbd")

################## plot 1: destination-specific mobility volume / 10k population
require(mgcv)
mod<-gam(m[,47]/269.9939~s(m$day,bs="cr", k=15),knots=list(m$day),data = m,method="REML")
plot(m$day,m[,47]/269.9939,pch=19,col=col[1],ylim=c(-400,6000),xlab="Date",ylab="Mobility volume/ 10000 population",cex=0.3,
     xaxt="n",yaxt="n",bty="l",yaxs='i',cex.lab=1)  # dots
axis(side=1, at=seq(5,55,by=9), labels = c("Jan 5","Jan 14","Jan 23","Feb 1","Feb 10","Feb 19"),cex.axis=1) 
axis(side=2, las=1,cex.axis=1)
lines(m$day,fitted(mod),col=col[1],lwd=1) # fitted lines

# for other 5 groups
for(i in 1:5) {
  lines(m$day,m[,(47+i)]/269.9939,pch=19,type = "p",col=col[i+1],cex=0.3)
  mod<-gam(m[,(47+i)]/269.9939~s(m$day,bs="cr", k=15),knots=list(m$day),data = m,method="REML")  
  lines(m$day,fitted(mod),col=col[i+1],lwd=1) 
}

abline(v = c(24,31,33,40),lty = 2,col="#969696")

mtext("Guangdong \nLevel 1 response", side=3, line=-1.5,col = "grey20", at =18, cex =0.5 )
mtext("Spring\n Festivel", side=3, line=-1.5, at =27, col = "grey20",cex =0.5 )
mtext("Festivel\n extended", side=3, line=-3.25, at =34, col = "grey20",cex =0.5 )
mtext("Workplace\n closed", side=3, line=-1.5, at =37,col = "grey20", cex =0.5 )
mtext("Resume work\n by steps", side=3, line=-1.5, at =45.5, col = "grey20",cex =0.5 )


legend(13, 7400, legend=c("Work","School","Recreation","Shopping","Traffic","Other"), xpd=NA,
       col=col, pch=c(19,19,19,19,19,19),lty=c(1,1,1,1,1,1), cex=0.7, box.lty=0,ncol=6)


########## plot 2 : regression : y: gr x: log (destination-specific mobility)  ############
################plot 2: glm for destination-specific mobility volume and R(t) in Shenzhen

## R(t) in shenzhen
# sz cases 
case<-read.csv("E:/work/¿ÆÑÐ/ÐÂ¹Ú²¡¶¾·ÎÑ×2020/·ÖÎö/szcases3.16.csv")
# symptoms onset table
d<-table(case$origin,case$onset,useNA = c("no"))
onset<-colSums(d)
#Rt 
library(R0)
mGT <- generation.time("gamma", c(2.6,1)) 
TD <- est.R0.TD(onset, mGT, begin=10, end=40, nsim=1000) 
# get R(t)
TD$R
# confidence interval
TD$conf.int
# plot R(T) with confidence interval
plot(TD$R,pch=19,type="o",col="white",ylim=c(-0.3,6),xlab="Date",
     ylab="Instantaneous reproduction number R(t) ",  cex=1.0,xaxt="n", yaxt="n",yaxs='i',bty="l")
axis(side=1, at=seq(1,31,by=6),labels = c("Jan 10","Jan 16","Jan 22","Jan 28","Feb 3","Feb 9"))
axis(side=2,at=seq(0,6,by=1), las=1)
#abline(v = c(24,31,33,40),lty = 2,col="#969696")
polygon(c(1:31, rev(1:31)), c(TD$conf.int[,1],rev(TD$conf.int[,2])), col ="#c6dbef",border = NA )
lines(TD$R,col="#023858")
abline(h=1,lty = 2,col="#969696")


# plot glm model for mobility and Rt
library(ggplot2)
library(gridBase)
library(grid)
plot.new()              ## 
vps <- baseViewports()
pushViewport(vps$figure) ##   
vp1 <-plotViewport(c(0,0,10,10)) ## create new vp with margins
vp <- viewport(height = unit(0.95,"npc"), width=unit(1, "npc"), 
               just = c("left","top"),
               y = 0.99, x = 0.002)
p<-ggplot(d3) + 
  geom_point(aes(x = work , y = cases), size = 0.25, color = col[1], fill = col[1], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(aes(x = work , y = cases ),method = "glm",se=T,fill = shadow[1],size=0.08,show.legend = FALSE,col=col[1])+ 
  geom_point(aes(x =  school , y =cases ), size = 0.25, color = col[2], fill = col[2], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(aes(x =  school , y = cases ),method = "glm",se=T,fill = shadow[2],size=0.08,show.legend = FALSE,col=col[2])+ 
  geom_point(aes(x = leisure , y = cases), size = 0.25, color = col[3], fill = col[3], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(aes(x =leisure , y = cases ),method = "glm",se=T,fill = shadow[3],size=0.08,show.legend = FALSE,col=col[3])+ 
  geom_point(aes(x =shopping , y = cases ), size = 0.25, color = col[4], fill = col[4], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(aes(x = shopping , y = cases ),method = "glm",se=T,fill = shadow[4],size=0.08,show.legend = FALSE,col=col[4])+ 
  geom_point(aes(x = traffic , y = cases), size = 0.25, color = col[5], fill = col[5], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(aes(x = traffic , y = cases ),method = "glm",se=T,fill = shadow[5],size=0.08,show.legend = FALSE,col=col[5])+ 
  geom_point(aes(x = other , y = cases ), size = 0.25, color = col[6], fill = col[6], group =2,show.legend = FALSE)+    #add raw data points
  geom_smooth(aes(x = other , y = cases ),method = "glm",se=T,fill = shadow[6],size=0.08,show.legend = FALSE,col=col[6])+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5, face = "italic"),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line(),
        axis.ticks.length=unit(.15, "cm"),
        axis.text.x=element_text(size=6.5,colour = "black",vjust=-0.5),
        axis.text.y=element_text(size=6.5,colour = "black",vjust=0.5),
        axis.title.x=element_text(size=8,colour = "black",vjust=-4),
        axis.title.y=element_text(size=8,colour = "black",vjust=4),
        plot.margin = unit(c(1,1,0.5,0.3), "cm"))+
  labs(x="Mobility (log)", y="R(t)")+
  scale_x_continuous(breaks=seq(11.2,14.2,by=0.5),limits = c(11.2,14.2),labels =c ("11.0","11.5","12.0","12.5","13.0","13.5","14.0"))+
  scale_y_continuous(breaks=seq(0.5,2.5,by=0.25),limits = c(0.5,2.5),expand = expand_scale(mult = c(0, 0),add = c(0,0)))

#print(p,vp = vp)    # add ggplot to the window


############# part IV: modifying effect of distances #################


#install.packages("mgcv")
library(mgcv)
library(nlme)
col=c("#e41a1c","#737373","#006d2c","#984ea3","#ff7f00","#4292c6","#a65628","#ce1256","#084081",
      "#67001f","#d6604d","#4393c3","#8073ac","#2d004b","#b35806","#8c510a","#80cdc1","#01665e",
      "#003c30","#d9f0d3","#c51b7d","#fdbb84","#d7301f","#08306b","#993404","#525252")
col11<-col[1:11]

windows(width=45,height=40)
par(mfrow = c(3,2))

# plot 1: mobilty volume_ratio and distance_ratio in CN 
modgam_m<-gam(m$m2020/normal_m~s(m$day),data = m)  # gam model
plot(m$day,m$m2020/normal_m,pch=15,col="#004529",ylim=c(0,1.5),xlim=c(11,60),xlab="Date",ylab="Mobility Pattern Ratio",
cex=0.6,yaxs='i',xaxt="n", yaxt="n",bty="l") # raw data dot
axis(side=1, at=seq(0,60,by=10),labels = c("Jan 1","Jan 11","Jan 21","Jan 31","Feb 10","Feb 20","Mar 1"))
axis(side=2, at=seq(0,1.5,by=0.25), 
     labels =  c("0.00","0.25","0.50","0.75","1.00","1.25","1.50"),las=1)
lines(m$day,fitted(modgam_m),col="#004529") # fitted gam line


modgam_r<-gam(m$r2020/normal_r~s(m$day),data = m)
lines(m$day,m$r2020/normal_r,pch=19,type = "p",col="#fe9929",cex=0.6)
lines(m$day,fitted(modgam_r),col="#fe9929") 

legend(20.5,1.85, legend="Observed", xpd=NA,
       col="#fe9929", pch=19, cex=1, box.lty=0,ncol=2)
legend(28.5,1.85, legend=c("Modelled"), xpd=NA,
       col="#fe9929", lty=1, cex=1, box.lty=0,ncol=2)
legend(20.5,1.7, legend="Observed", xpd=NA,
       col="#004529", pch=15, cex=1, box.lty=0,ncol=2)
legend(28.5,1.7, legend=c("Modelled"), xpd=NA,
       col="#004529", lty=1, cex=1, box.lty=0,ncol=2)

### plot 2: mobilty volume_ratio and distance_ratio in US 
data_us$day<-1:length(data_us$Date)
data_us$m_ratio<-data_us$m/normal_m_us  
data_us$r_ratio<-data_us$r/normal_r_us

modgam_m<-gam(data_us$m_ratio~s(data_us$day),data = data_us)  # gam model
plot(data_us$day,data_us$m_ratio,pch=15,col="#023858",ylim=c(0,1.5),xlab="Date",ylab="Mobility Pattern Ratio",
     cex=0.6,xaxt="n", yaxt="n",yaxs='i',bty="l") # raw data dot
axis(side=1, at=seq(22,95,by=14), labels = c("Feb 22","Mar 7","Mar 21","Apr 4","Apr 18","May 2"))
axis(side=2, at=seq(0,1.5,by=0.25), 
     labels =  c("0.00","0.25","0.50","0.75","1.00","1.25","1.50"),las=1)
lines(data_us$day,fitted(modgam_m),col="#023858") # fitted gam line

modgam_r<-gam(data_us$r_ratio~s(data_us$day),data = m)
lines(data_us$day,data_us$r_ratio,pch=19,type = "p",col="#fe9929",cex=0.6)
lines(data_us$day,fitted(modgam_r),col="#fe9929") 

legend(32.5,1.85, legend="Observed", xpd=NA,
       col="#fe9929", pch=19, cex=1, box.lty=0,ncol=2)
legend(44,1.85, legend=c("Modelled"), xpd=NA,
       col="#fe9929", lty=1, cex=1, box.lty=0,ncol=2)

legend(32.5,1.7, legend="Observed", xpd=NA,
       col="#004529", pch=15, cex=1, box.lty=0,ncol=2)
legend(44,1.7, legend=c("Modelled"), xpd=NA,
       col="#004529", lty=1, cex=1, box.lty=0,ncol=2)



###############  plot 3&4£º R(t)R ratio as the ratio between the daily R 
# and the R from the first day of the calcualtion  ###################
R$day<-1:length(R$Date)
par(mar = c(6,4, 2, 2))
plot(R$CN,pch=19,type="o",col="white",ylim=c(0,3),xlab="Date",
     ylab="Instantaneous reproduction number R(t) ",  cex=1.0,xaxt="n", yaxt="n",yaxs='i',bty="l")
axis(side=1, at=seq(0,60,by=10),labels = c("Jan 1","Jan 11","Jan 21","Jan 31","Feb 10","Feb 20","Mar 1"))
axis(side=2,at=seq(0,3,by=0.5), 
     labels =  c("0.0","0.5","1.0","1.5","2.0","2.5","3.0"),las=1)
abline(h=1,lty = 2,col="#969696")
abline(h=0.5,lty = 2,col="#969696")
abline(h=2.0,lty = 2,col="#969696")
abline(v = c(24,31,33,40),lty = 2,col="#969696")
polygon(c(R$day[1:60], rev(R$day[1:60])), c(R$cn_lower_90[1:60],rev(R$cn_upper_90[1:60])), col ="#a6bddb",border = NA )
lines(R$CN[1:60],col="#023858")
### plot 4 : R(t) in US
par(mar = c(6,4, 2, 2))
plot(R$US,pch=19,col="white",ylim=c(0,3),xlab="Date",
     ylab="Instantaneous reproduction number R(t) ",cex=1.0,xaxt="n", yaxt="n",yaxs='i',bty="l")
axis(side=1,  at=seq(1,99,by=14), labels = c("Feb 22","Mar 7","Mar 21","Apr 4","Apr 18","May 2","May 16","May 30"))
axis(side=2,at=seq(0,3,by=0.5), 
     labels =  c("0.0","0.5","1.0","1.5","2.0","2.5","3.0"),las=1)
abline(h=1,lty = 2,col="#969696")
abline(h=0.5,lty = 2,col="#969696")
abline(h=2.0,lty = 2,col="#969696")
lines(R$US,col="#023858")


############===================================############
############ plot 5 $ 6 : interacction for CN AND US
############===================================############
######### interaction: R(t)_ratio=m_ratio*r_ratio   CN  ######### 
library(ggplot2)
library(gridBase)
library(grid)
plot.new()              ## 
vps <- baseViewports()
pushViewport(vps$figure) ##   
vp1 <-plotViewport(c(1.45,0.5,3,1.6)) ## create new vp with margins 
vp <- viewport(height = unit(1.04,"npc"), width=unit(0.96, "npc"), 
               just = c("left","top"),
               y = 1.04, x =0.015)

m_ratio_cn<-m$m2020/normal_m_cn
r_ratio_cn<-m$r2020/normal_m_r
CN_ratio<-R$CN/normal_R_cn

ccf1<-ccf(m_ratio_cn,CN_ratio,lag.max=30,plot=F)  
ccf2<-ccf(r_ratio_cn,CN_ratio,lag.max=30,plot=F)

require(jtools)
require(ggplot2)
require(interactions)
d<-data.frame(r=r_ratio_cn,
              m=m_ratio_cn,
              cases=CN_ratio)
require(psych)
mod_ratio_cn<-glm(cases~m*r,data=d)   # glm model
summ(mod_ratio_cn) 
require(rsq)
rsq(mod_ratio_cn)  
p_cn<-interact_plot(mod_ratio_cn, pred = m, modx = r, modx.values = c(0.9,1.0,1.1),interval=TRUE,line.thickness=0.4,colors = c("#045a8d","#004529","#fc4e2a"),
                    x.label = "Mobility Volume Ratio ", y.label = "R(t)_Ratio",
                    legend.main = "Mobility Distance Ratio")+
  theme_apa()+
  scale_x_continuous(breaks=seq(0.3,1.0,0.1),limits = c(0.3,1.0), expand = c(0.04,0))+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8),limits = c(0,1.8),
                     labels = c("0.0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8"), expand = c(0,0))+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5, face = "italic"),
        axis.text.x = element_text(angle = 0, hjust = 0.5,colour = "black",size=8),
        axis.text.y = element_text(angle = 0, hjust = 0.5,colour = "black",size=8),
        axis.ticks.length=unit(.15, "cm"),
        axis.title.x =element_text(size=8),
        axis.title.y =element_text(size=8),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(size = 10),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line(),
        legend.position="top",
        plot.margin = unit(c(0,0.2,0,0.1), "cm"))+
  annotate(geom="text", x=0.44, y=1.64, size=3,label="Interaction coefficient = 4.96", color="#000000")+
  annotate(geom="text", x=0.32, y=1.47, size=3,label="P", fontface = 'italic',color="#000000")+
  annotate(geom="text", x=0.39, y=1.47, size=3,label="value < 0.01", color="#000000")+
  annotate(geom="text", x=0.775, y=0.235, size=3,label="Model", color="#000000")+
  annotate(geom="text", x=0.85, y=0.25, size=3,label=paste("italic(R)^2 ==",0.73), parse=TRUE,color="#000000")+
  xlab("Mobility Volumes Ratio ") + ylab("R(t) Ratio")

print(p_cn,vp = vp)    # add ggplot to the window

################## PLOT 6: INTERACTION IN US #############
library(ggplot2)
library(gridBase)
library(grid)
plot.new()              ## 
vps <- baseViewports()
pushViewport(vps$figure) ##   
vp1 <-plotViewport(c(1.45,0.5,3,1.6)) ## create new vp with margins
vp <- viewport(height = unit(3.4,"npc"), width=unit(2, "npc"), 
               just = c("left","top"),
               y = 3.4, x = 1.01)

#########  Part II:  interaction: R(t)_ratio=m_ratio*r_ratio  US ######### 
m_ratio<-data_us$m/normal_m_us
r_ratio<-data_us$r/normal_r_us
US_ratio<-R$US/normal_R_us

ccf1<-ccf(m_ratio,US_ratio,lag.max=30,plot=F)  
ccf2<-ccf(r_ratio,US_ratio,lag.max=30,plot=F)

require(jtools)
require(ggplot2)
require(interactions)
d<-data.frame(r=r_ratio,
              m=m_ratio,
              cases=US_ratio)
require(psych)
mod_ratio<-glm(cases~m*r,data=d)   # glm model
summ(mod_ratio)  
require(rsq)
rsq(mod_ratio)  
mod_ratio_2<-glm(cases~m+r,data=d) 
# anova for validation of interaction
anova(mod_ratio,mod_ratio_2,test = "Chisq")

p_us<-interact_plot(mod_ratio, pred = m, modx = r, modx.values = c(0.80,0.90,1.0),interval=TRUE,line.thickness=0.4,vary.lty=TRUE,
                    x.label = "Mobility Ratio ", y.label = "R(t)_Ratio",colors = c("#045a8d","#004529","#fc4e2a"),
                    legend.main = "Mobility Distance Ratio")+
  theme_apa()+
  scale_x_continuous(breaks=seq(0.6,1.0,by=0.1),limits = c(0.6,1.0), expand = c(0.03,0))+
  scale_y_continuous(breaks=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),limits = c(0.3,1.0), expand = c(0,0))+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5, face = "italic"),
        axis.text.x = element_text(angle = 0, hjust = 0.5,colour = "black",size=8),
        axis.text.y = element_text(angle = 0, hjust = 0.5,colour = "black",size=8),
        axis.ticks.length=unit(.15, "cm"),
        axis.title.x =element_text(size=8),
        axis.title.y =element_text(size=8),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(size = 10),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line(),
        legend.position="top",
        plot.margin = unit(c(0.8,1,0,0.2), "cm"))+
  annotate(geom="text", x=0.69, y=0.95, size=3,label="Interaction coefficient = 3.78 ", color="#000000")+
  annotate(geom="text", x=0.615, y=0.88, size=3,label="P", fontface = 'italic',color="#000000")+
  annotate(geom="text", x=0.657, y=0.88, size=3,label="value < 0.01", color="#000000")+
  annotate(geom="text", x=0.890, y=0.388, size=3,label="Model", color="#000000")+
  annotate(geom="text", x=0.935, y=0.394, size=3,label=paste("italic(R)^2 ==",0.82), parse=TRUE,color="#000000")+
  xlab("Mobility Volumes Ratio ") + ylab("R(t) Ratio")

print(p_us,vp = vp)    # add ggplot to the window

mtext("A", side=3, line=47, at =-1.35, cex =1,font=2)
mtext("B", side=3, line=47, at =-0.09, cex =1,font=2)
mtext("C", side=3, line=25, at =-1.35, cex =1,font=2)
mtext("D", side=3, line=25, at =-0.09, cex =1,font=2)
mtext("E", side=3, line=0.5, at =-1.35, cex =1,font=2)
mtext("F", side=3, line=0.5, at =-0.085, cex =1,font=2)

mtext("diatance ratio", side=3, line=47.7, at =-0.62, cex =0.695)
mtext("volume ratio", side=3, line=46.20, at =-0.63, cex =0.695)
mtext("diatance ratio", side=3, line=47.7, at =0.6, cex =0.695)
mtext("volume ratio", side=3, line=46.20, at =0.59, cex =0.695)





























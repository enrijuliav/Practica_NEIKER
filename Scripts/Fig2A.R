##Fig.2A

# Installing packages
if(!require("ggplot2")) install.packages("ggplot2")
library("ggplot2")
if(!require("forcats")) install.packages("forcats")
library(forcats)
if(!require("cowplot")) install.packages("cowplot")
library(cowplot)

df<-read.csv("Processed_data/MD_figures/gas_fig2aaa.csv")
df
df$supp <- as.factor(df$supp)
df$supp <- fct_inorder(df$supp)

p1<-ggplot(df,aes(x=dose, y=len, colour=supp,shape=supp))+
  geom_errorbar(aes(ymin=len-se, ymax=len+se),colour="black", width=.2,size=0.5)+
  geom_line(size=0.5) +
  geom_point(size=6,aes(fill=supp),pch=21,color='black')+
  scale_color_manual(values = c('#000000','#000000','#000000','#000000','#000000'))+
  scale_fill_manual(values = c('#ffffff','#D5D5D5','#D7E4FC','#8DB1FB','#6393FB'))+
  scale_y_continuous(limits = c(0.01,0.025),breaks = c(0.012,0.024))+
  scale_x_continuous(breaks=0:15*3)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.justification=c(0.85,0.1),
        legend.position=c(0.2,0.6))+
  labs(title = "", y="N₂O emissions (µg N·g⁻¹ soil)", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  facet_grid(~ group1, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7))
p1
p2<-ggplot(df,aes(x=dose, y=lenHN, colour=supp,shape=supp))+
  geom_errorbar(aes(ymin=lenHN-seHN, ymax=lenHN+seHN),colour="black", width=.35,size=0.5)+
  geom_line(size=0.5) +
  geom_point(size=6,aes(fill=supp),pch=21,color='black')+
  scale_color_manual(values = c('#000000','#000000','#000000','#000000','#000000'))+
  scale_fill_manual(values = c('#ffffff','#D5D5D5','#D7E4FC','#8DB1FB','#6393FB'))+
  scale_y_continuous(limits = c(0.5,15),breaks = c(0,5,10,15)) +
  scale_x_continuous(breaks=0:15*3)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.justification=c(0.85,0.1),
        legend.position="none")+
  labs(title = "", y="", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  facet_grid(~ group2, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7))
p2
p3<-ggplot(df,aes(x=dose, y=lenGD, colour=supp,shape=supp))+
  geom_errorbar(aes(ymin=lenGD-seGD, ymax=lenGD+seGD),colour="black", width=.35,size=0.5)+
  geom_line(size=0.5) +
  geom_point(size=6,aes(fill=supp),pch=21,color='black')+
  scale_color_manual(values = c('#000000','#000000','#000000','#000000','#000000'))+
  scale_fill_manual(values = c('#ffffff','#D5D5D5','#D7E4FC','#8DB1FB','#6393FB'))+
  scale_y_continuous(limits = c(0.005,0.2),breaks = c(0,0.1,0.2)) +
  scale_x_continuous(breaks=0:15*3)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.justification=c(0.85,0.1),
        legend.position="none")+
  labs(title = "", y="", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  facet_grid(~ group3, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7))
p3


plot_grid(p1,p2,p3,ncol=3,nrow=1)

# Define a reusable function to load the data from the paper.
# This data has the name of the figure before the colnames,
# which complicates the loading process
setwd("~/Documentos/GitHub/Practica_NEIKER")

library("ggplot2")
library(forcats)
library(cowplot)

load3A <- function(csv) {
  # Load data
  data <- read.csv2(csv)
  # Change colnames
  colnames(data) <- data[1,]
  data <- data[-1,]
  data$`Soil type` <- as.factor(data$`Soil type`)
  data$Treatment <- as.factor(data$Treatment)
  data$Time <- as.numeric(gsub("day","",data$Time))
  data$`N2O emission` <- as.numeric(gsub(",",".",data$`N2O emission`))
  return(data[-c(1,5)])
}

rawdata <- load3A("Raw_data/MD3A_rawdata.csv")
# Select the variables for the analysis: Soil type and metabolites
data <- as.data.frame(cbind("Time","Soil type","Treatment","N2O", "se"))

for (time in unique(rawdata$Time)) {
  for (soil in unique(rawdata$`Soil type`)) {
    for (treat in unique(rawdata$Treatment)) {
      temp <- rawdata[rawdata["Time"]==time & rawdata["Soil type"]==soil & rawdata["Treatment"]==treat,]
      data <- rbind(data, c(time, soil, treat, mean(temp[,4]), sd(temp[,4])))
    }
    
  }
  
}

colnames(data) <- data[1,]
data <- data[-1,]
data$N2O <- as.numeric(data$N2O)
data$se <- as.numeric(data$se)
data$Time <- as.numeric(data$Time)

p1<-ggplot(data[data$`Soil type`=="Black soil",],aes(x=Time, y=N2O, colour=Treatment,shape=Treatment))+
    geom_errorbar(aes(ymin=N2O-se, ymax=N2O+se),colour="black", width=.2,size=0.5)+
   geom_line(size=0.5) +
  geom_point(size=6,aes(fill=Treatment),pch=21,color='black')+
  scale_color_manual(values = c('#d7f0d0','#83cb9b','#1ba858'))+
  scale_fill_manual(values = c('#d7f0d0','#83cb9b','#1ba858'))+
  scale_y_continuous(limits = c(0,26),breaks = c(0:5*5))+
  scale_x_continuous(breaks=0:5)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.justification=c(0.85,0.1),
        legend.position=c(0.4,0.8))+
  labs(title = "Black soil", y="N2O emissions", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",size=0.7))
p1
p2<-ggplot(data[data$`Soil type`=="Red soil",],aes(x=Time, y=N2O, colour=Treatment,shape=Treatment))+
  geom_errorbar(aes(ymin=N2O-se, ymax=N2O+se),colour="black", width=.2,size=0.5)+
  geom_line(size=0.5) +
  geom_point(size=6,aes(fill=Treatment),pch=21,color='black')+
  scale_color_manual(values = c('#d7f0d0','#83cb9b','#1ba858'))+
  scale_fill_manual(values = c('#d7f0d0','#83cb9b','#1ba858'))+
  scale_y_continuous(limits = c(0,26),breaks = c(0:5*5))+
  scale_x_continuous(breaks=0:5)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.justification=c(0.85,0.1),
        legend.position=c(0.4,0.8))+
  labs(title = "Red soil", y="N2O emissions", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",size=0.7))
p2
p3<-ggplot(data[data$`Soil type`=="Yellow soil",],aes(x=Time, y=N2O, colour=Treatment,shape=Treatment))+
  geom_errorbar(aes(ymin=N2O-se, ymax=N2O+se),colour="black", width=.2,size=0.5)+
  geom_line(size=0.5) +
  geom_point(size=6,aes(fill=Treatment),pch=21,color='black')+
  scale_color_manual(values = c('#d7f0d0','#83cb9b','#1ba858'))+
  scale_fill_manual(values = c('#d7f0d0','#83cb9b','#1ba858'))+
  scale_y_continuous(limits = c(0,26),breaks = c(0:5*5))+
  scale_x_continuous(breaks=0:5)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        legend.justification=c(0.85,0.1),
        legend.position=c(0.4,0.8))+
  labs(title = "Yellow soil", y="N2O emissions", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",size=0.7))
p3

plot_grid(p1,p2,p3,ncol=3,nrow=1)

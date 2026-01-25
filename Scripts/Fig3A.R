# Define a reusable function to load the data from the paper.
# This data has the name of the figure before the colnames,
# which complicates the loading process
setwd("~/Documentos/GitHub/Practica_NEIKER")

library(ggplot2)
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


#######################################################################
#-----------------------------Estadística-----------------------------#
#######################################################################


# Select the variables for the analysis: Soil type and metabolites
soiltypes <- levels(rawdata$`Soil type`)

# Initialize an object that will have all the stat results
stats <- rep(NA, 0)
pvalues <- rep(NA, 0)

# Cycle through every soil type and metabolite
for (soil in soiltypes) {
  # Select only the data of our interest
  example <- rawdata[rawdata["Soil type"]==soil & rawdata["Time"]==5,
                     c("Treatment", "N2O emission")]
  # ANOVA test
  result <- aov(example[["N2O emission"]] ~ Treatment, data = example)
  # Save the general ANOVA pvalue
  pvalues <- append(pvalues,summary(result)[[1]]["Pr(>F)"][1,])
  # Extract the values for every posible combination of treatments in one set of conditions
  temp <- as.data.frame(TukeyHSD(result)[[1]])
  colnames(temp) <- sub("", paste(soil, "N2O emission."), colnames(temp))
  temp<- round(temp, digits = 3)
  stats <- append(stats, temp[4])
}

padj <- round(p.adjust(pvalues, method = "fdr"), digits = 3)
padj < 0.05

stats <- as.data.frame(stats)
stats <- rbind(stats, padj)
rownames(stats) <- c(rownames(temp), "p_adj")

write.csv(stats, file = "Processed_data/stats3A")


#######################################################################
#-----------------------------Preparación-----------------------------#
#######################################################################


# Select the variables for the analysis: Soil type and metabolites
data <- as.data.frame(cbind("Time","Soil type","Treatment","N2O", "se"))

for (time in unique(rawdata$Time)) {
  for (soil in unique(rawdata$`Soil type`)) {
    for (treat in unique(rawdata$Treatment)) {
      temp <- rawdata[rawdata["Time"]==time & rawdata["Soil type"]==soil & rawdata["Treatment"]==treat,]
      data <- rbind(data, c(time, soil, treat, mean(temp[,4]), sd(temp[,4])/sqrt(3)))
    }
  }
}

colnames(data) <- data[1,]
data <- data[-1,]
data$N2O <- as.numeric(data$N2O)
data$se <- as.numeric(data$se)
data$Time <- as.numeric(data$Time)
data$Treatment <- factor(data$Treatment, c("CT", "2×109 cell · g-1", "1010 cell · g-1")) 


####################################################################
#-----------------------------Graficas-----------------------------#
####################################################################


p1<-ggplot(data[data$`Soil type`=="Black soil",],aes(x=Time, y=N2O, colour=Treatment,shape=Treatment))+
    geom_errorbar(aes(ymin=N2O-se, ymax=N2O+se),colour="black", width=.2,size=0.5)+
   geom_line(size=0.5) +
  geom_point(size=6,aes(fill=Treatment),pch=21,color='black')+
  scale_color_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  scale_fill_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  scale_y_continuous(limits = c(0,26),breaks = c(0:5*5))+
  scale_x_continuous(breaks=0:5)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank())+
  theme(legend.position = "none")+
  labs(y="N₂O emissions (µg N·g⁻¹ soil)", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",size=0.7)) +
  facet_grid(~ `Soil type`, scale="free",space="free_y")
  
p1
p2<-ggplot(data[data$`Soil type`=="Red soil",],aes(x=Time, y=N2O, colour=Treatment,shape=Treatment))+
  geom_errorbar(aes(ymin=N2O-se, ymax=N2O+se),colour="black", width=.2,size=0.5)+
  geom_line(size=0.5) +
  geom_point(size=6,aes(fill=Treatment),pch=21,color='black')+
  scale_color_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  scale_fill_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  scale_y_continuous(limits = c(0,32),breaks = c(0:6*5))+
  scale_x_continuous(breaks=0:5)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank())+
  theme(legend.direction = "horizontal", legend.position = "top")+
  labs(y="", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",size=0.7)) +
  facet_grid(~ `Soil type`, scale="free",space="free_y")
p2
p3<-ggplot(data[data$`Soil type`=="Yellow soil",],aes(x=Time, y=N2O, colour=Treatment,shape=Treatment))+
  geom_errorbar(aes(ymin=N2O-se, ymax=N2O+se),colour="black", width=.2,size=0.5)+
  geom_line(size=0.5) +
  geom_point(size=6,aes(fill=Treatment),pch=21,color='black')+
  scale_color_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  scale_fill_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  scale_y_continuous(limits = c(0,15),breaks = c(0:3*5))+
  scale_x_continuous(breaks=0:5)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank())+
  theme(legend.position = "none")+
  labs(y="", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",size=0.7))+
  facet_grid(~ `Soil type`, scale="free",space="free_y")
p3

ggarrange(p1,p2,p3,ncol=3,nrow=1, common.legend = TRUE, legend="top")
 
# Save with 1600*600 resolution

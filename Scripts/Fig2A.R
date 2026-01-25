setwd("~/Documentos/GitHub/Practica_NEIKER")

library(ggplot2)
library(forcats)
library(cowplot)
library(ggplot2)
library(ggbreak)
library(ggpubr)

load2A <- function(csv) {
  # Load data
  data <- read.csv(csv)
  # Change colnames
  colnames(data) <- data[1,]
  data <- data[-1,]
  data$`Soil type` <- as.factor(data$`Soil type`)
  data$Treatment <- as.factor(data$Treatment)
  data$Time <- as.numeric(gsub("day","",data$Time))
  data$Replicate <- as.numeric(data$Replicate)
  data$`N2O emission` <- as.numeric(gsub(",",".", data$`N2O emission`))

  return(data)
}

rawdata <- load2A("Raw_data/MD2A_rawdata.csv")


#######################################################################
#-----------------------------Estadística-----------------------------#
#######################################################################


# Select the variables for the analysis: Soil type and metabolites
soiltypes <- unique(rawdata[["Soil type"]])

# Initialize an object that will have all the stat results
stats <- rep(NA, 0)
pvalues <- rep(NA, 0)

# Cycle through every soil type and metabolite
for (soil in soiltypes) {
  # Select only the data of our interest
  example <- rawdata[rawdata["Soil type"]==soil & rawdata["Time"]==15,
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

write.csv(stats, file = "Processed_data/stats2A")


#######################################################################
#-----------------------------Preparación-----------------------------#
#######################################################################


graph <- rawdata[c("Time", "Soil type", "Treatment", "N2O emission")]
N2O <- rep(NA, 0)

for (day in unique(graph$Time)) {
  for (treat in c("CT", "CT_N", "0.01% CH4 _N", "0.1% CH4 _N", "1% CH4 _N")) {
    mask <- graph$Time==day & graph$Treatment == treat
    temp <- cbind(treat, day,
      mean(graph[mask & graph$`Soil type`=="Black soil", "N2O emission"]),
      sd(graph[mask & graph$`Soil type`=="Black soil", "N2O emission"])/sqrt(3), "Black soil",
      mean(graph[mask & graph$`Soil type`=="Red soil", "N2O emission"]),
      sd(graph[mask & graph$`Soil type`=="Red soil", "N2O emission"])/sqrt(3), "Red soil",
      mean(graph[mask & graph$`Soil type`=="Yellow soil", "N2O emission"]),
      sd(graph[mask & graph$`Soil type`=="Yellow soil", "N2O emission"])/sqrt(3), "Yellow soil")
    N2O <- rbind(N2O, temp)
  }
}
N2O <- as.data.frame(N2O)
colnames(N2O) <- c("Treatment", "Time", "N2O_B", "se_B", "Black","N2O_R", "se_R","Red","N2O_Y", "se_Y","Yellow")

N2O$Treatment <- factor(N2O$Treatment, c("CT", "CT_N", "0.01% CH4 _N", "0.1% CH4 _N", "1% CH4 _N"))
N2O$Time <- as.numeric(N2O$Time)
N2O$N2O_B <- as.numeric(N2O$N2O_B)
N2O$se_B <- as.numeric(N2O$se_B)
N2O$N2O_R <- as.numeric(N2O$N2O_R)
N2O$se_R <- as.numeric(N2O$se_R)
N2O$N2O_Y <- as.numeric(N2O$N2O_Y)
N2O$se_Y <- as.numeric(N2O$se_Y)

####################################################################
#-----------------------------Graficas-----------------------------#
####################################################################


p1<-ggplot(N2O,aes(x=Time, y=N2O_B, colour=Treatment,shape=Treatment))+
  geom_errorbar(aes(ymin=N2O_B-se_B, ymax=N2O_B+se_B),colour="black", width=.2,size=0.5)+
  geom_line(size=0.5) +
  geom_point(size=6,aes(fill=Treatment),pch=21,color='black')+
  scale_color_manual(values = c('#000000','#000000','#000000','#000000','#000000'))+
  scale_fill_manual(values = c('#ffffff','#D5D5D5','#D7E4FC','#8DB1FB','#6393FB'))+
  scale_y_continuous(limits = c(0.01,0.025),breaks = c(0.012,0.024))+
  scale_x_continuous(breaks=0:15*3)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank())+
  labs(title = "", y="N₂O emissions (µg N·g⁻¹ soil)", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  facet_grid(~ Black, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7))
p1
p2<-ggplot(N2O,aes(x=Time, y=N2O_R, colour=Treatment,shape=Treatment))+
  geom_errorbar(aes(ymin=N2O_R-se_R, ymax=N2O_R+se_R),colour="black", width=.35,size=0.5)+
  geom_line(size=0.5) +
  geom_point(size=6,aes(fill=Treatment),pch=21,color='black')+
  scale_color_manual(values = c('#000000','#000000','#000000','#000000','#000000'))+
  scale_fill_manual(values = c('#ffffff','#D5D5D5','#D7E4FC','#8DB1FB','#6393FB'))+
  scale_y_continuous(limits = c(0.5,15),breaks = c(0,5,10,15)) +
  scale_x_continuous(breaks=0:15*3)+
  theme_bw()+
  theme(legend.direction = "horizontal", legend.position = "top")+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank())+
  labs(title = "", y="", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  facet_grid(~ Red, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7))
p2
p3<-ggplot(N2O,aes(x=Time, y=N2O_Y, colour=Treatment,shape=Treatment))+
  geom_errorbar(aes(ymin=N2O_Y-se_Y, ymax=N2O_Y+se_Y),colour="black", width=.35,size=0.5)+
  geom_line(size=0.5) +
  geom_point(size=6,aes(fill=Treatment),pch=21,color='black')+
  scale_color_manual(values = c('#000000','#000000','#000000','#000000','#000000'))+
  scale_fill_manual(values = c('#ffffff','#D5D5D5','#D7E4FC','#8DB1FB','#6393FB'))+
  scale_y_continuous(limits = c(0.005,0.2),breaks = c(0,0.1,0.2)) +
  scale_x_continuous(breaks=0:15*3)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank())+
  labs(title = "", y="", x = "Time (d)")+
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68))+
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68))+
  theme(axis.title = element_text(size = 18, colour = "black"))+
  facet_grid(~ Yellow, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7))
p3


ggarrange(p1,p2,p3,ncol=3,nrow=1, common.legend = TRUE, legend="top")


# Save with 1200*600 resolution


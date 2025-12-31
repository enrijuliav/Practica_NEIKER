setwd("~/Documentos/GitHub/Practica_NEIKER")

library(ggplot2)
library(forcats)
library(cowplot)
library(ggplot2)
library(ggbreak)
library(ggpubr)

load1B <- function(csv) {
  # Load data
  data <- read.csv(csv)
  # Change colnames
  colnames(data) <- data[1,]
  data <- data[-1,]
  data$`Soil type` <- as.factor(data$`Soil type`)
  data$Treatment <- as.factor(data$Treatment)
  data$Time <- as.numeric(gsub("day","",data$Time))
  data$Replicate <- as.numeric(data$Replicate)
  data$nirS <- as.numeric(gsub(",",".",data$nirS))
  data$nirK <- as.numeric(gsub(",",".",data$nirK))
  data$`log(nirS)` <- log10(data$nirS)
  data$`log(nirK)` <- log10(data$nirK)
  return(data)
}

rawdata <- load3B("Raw_data/MD1B_rawdata.csv")

# Select the variables for the analysis: Soil type and metabolites
soiltypes <- unique(rawdata[["Soil type"]])
genes <- grep("^log", colnames(rawdata), value = T)

# Initialize an object that will have all the stat results
stats <- rep(NA, 0)
pvalues <- rep(NA, 0)

# Cycle through every soil type and metabolite
for (soil in soiltypes) {
  for (gene in genes) {
    # Select only the data of our interest
    example <- rawdata[rawdata["Soil type"]==soil, c("Treatment",gene)]
    # ANOVA test
    result <- aov(example[[gene]] ~ Treatment, data = example)
    # Save the general ANOVA pvalue
    pvalues <- append(pvalues,summary(result)[[1]]["Pr(>F)"][1,])
    # Extract the values for every posible combination of treatments in one set of conditions
    temp <- as.data.frame(TukeyHSD(result)[[1]])
    colnames(temp) <- sub("", paste(soil, gene), colnames(temp))
    temp<- round(temp, digits = 3)
    stats <- append(stats, temp[4])
  }
}

padj <- round(p.adjust(pvalues, method = "fdr"), digits = 3)
padj < 0.05

stats <- as.data.frame(stats)
stats <- rbind(stats, padj)
rownames(stats) <- c(rownames(temp), "p_adj")

write.csv(stats, file = "Processed_data/stats2B")


#######################################################################
#-----------------------------Preparación-----------------------------#
#######################################################################


graph <- rawdata[c("Soil type", "Treatment", "log(nirS)", "log(nirK)")]
nirs <- cbind(rep("nirS", length(graph[graph$`Soil type`=="Black soil",1])),
              graph[graph$`Soil type`=="Black soil",c(2,3,1)],
              graph[graph$`Soil type`=="Red soil",c(3,1)],
              graph[graph$`Soil type`=="Yellow soil",c(3,1)])
colnames(nirs) <- c("group","Treatment", "genes_B","Black","genes_R","Red","genes_Y","Yellow")

nirk <- cbind(rep("nirK", length(graph[graph$`Soil type`=="Black soil",1])),
              graph[graph$`Soil type`=="Black soil",c(2,4,1)],
              graph[graph$`Soil type`=="Red soil",c(4,1)],
              graph[graph$`Soil type`=="Yellow soil",c(4,1)])
colnames(nirk) <- c("group","Treatment","genes_B","Black","genes_R","Red","genes_Y","Yellow")

plot <- rbind(nirs,nirk)
plot$group <- as.factor(plot$group)
plot$Treatment <- factor(plot$Treatment, c("CT", "CT_N", "0.01% CH4 _N", "0.1% CH4 _N", "1% CH4 _N"))


####################################################################
#-----------------------------Graficas-----------------------------#
####################################################################


bA<-ggplot(plot, aes(fill=Treatment, y=genes_B, x=group))+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black")+
  theme_classic(base_size = 12)+
  theme(panel.border=element_rect(fill='transparent',linewidth=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  geom_vline(aes(xintercept=1.5),linetype=2,cex=2)+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1))+
  theme(legend.direction = "horizontal", legend.position = "none")+
  labs(title = "", y="Gene copies·g⁻¹soil (RNA, log₁₀)", x = "")+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  scale_fill_manual(values = c('#ffffff','#dae1ff','#9faaff','#5a71ff','#2a2aff')) +
  facet_grid(~ Black, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7))+
  geom_jitter(data = plot, aes(x = group, y = genes_B,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F) +
  ylim(0, 8)

bA

bB<-ggplot(plot, aes(fill=Treatment, y=genes_R, x=group))+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black")+
  theme_classic(base_size = 12)+
  theme(panel.border=element_rect(fill='transparent',linewidth=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  geom_vline(aes(xintercept=1.5),linetype=2,cex=2)+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1))+
  theme(legend.direction = "horizontal", legend.position = "top")+
  labs(title = "", y="", x = "")+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  scale_fill_manual(values = c('#ffffff','#dae1ff','#9faaff','#5a71ff','#2a2aff')) +
  facet_grid(~ Red, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7))+
  geom_jitter(data = plot, aes(x = group, y = genes_R,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F) +
  ylim(0, 8)

bB

bC<-ggplot(plot, aes(fill=Treatment, y=genes_Y, x=group))+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black")+
  theme_classic(base_size = 12)+
  theme(panel.border=element_rect(fill='transparent',linewidth=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  geom_vline(aes(xintercept=1.5),linetype=2,cex=2)+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1))+
  theme(legend.direction = "horizontal", legend.position = "none")+
  labs(title = "", y="", x = "")+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  scale_fill_manual(values = c('#ffffff','#dae1ff','#9faaff','#5a71ff','#2a2aff')) +
  facet_grid(~ Yellow, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7))+
  geom_jitter(data = plot, aes(x = group, y = genes_Y,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F) +
  ylim(0, 8)

bC

ggarrange(bA,bB,bC,ncol=3,nrow=1, common.legend = TRUE, legend="top")

# Save with 1600*600 resolution

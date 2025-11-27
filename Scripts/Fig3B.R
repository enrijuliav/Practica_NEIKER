# Define a reusable function to load the data from the paper.
# This data has the name of the figure before the colnames,
# which complicates the loading process
setwd("~/Documentos/GitHub/Practica_NEIKER")

library(ggplot2)
library(forcats)
library(cowplot)
library(ggplot2)
library(ggbreak)

load3B <- function(csv) {
  # Load data
  data <- read.csv2(csv)
  # Change colnames
  colnames(data) <- data[1,]
  data <- data[-1,]
  data$`Soil type` <- as.factor(data$`Soil type`)
  data$Treatment <- as.factor(data$Treatment)
  data$Time <- as.numeric(gsub("day","",data$Time))
  data$Replicate <- as.numeric(data$Replicate)
  data$`log(nirS)` <- as.numeric(gsub(",",".",data$`log(nirS)`))
  data$`log(nirK)` <- as.numeric(gsub(",",".",data$`log(nirK)`))
  return(data)
}

rawdata <- load3B("Raw_data/MD3B_rawdata.csv")

# Select the variables for the analysis: Soil type and metabolites
# Select the variables for the analysis: Soil type and metabolites
soiltypes <- unique(rawdata[["Soil type"]])
genes <- colnames(rawdata)[c(6,8)]

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
    stats <- append(stats,TukeyHSD(result)[1])
  }
}

# Transform the vector into a matrix and change the row and column names
# for legibiliy and accessibility
stats <- matrix(data = stats, nrow = length(soiltypes), ncol = length(genes), byrow = TRUE)
pvalues <- matrix(data = pvalues, nrow = length(soiltypes), ncol = length(genes), byrow = TRUE)
colnames(stats) <- genes
rownames(stats) <- soiltypes
colnames(pvalues) <- genes
rownames(pvalues) <- soiltypes

padj <- round(p.adjust(pvalues, method = "fdr"), digits = 3)
padj < 0.05



# A <- read.csv("genes_fig2bbb.csv",header = T)
# A$condictions <- as.factor(A$condictions)
# A$condictions <- fct_inorder(A$condictions)
# A$group <- as.factor(A$group)
# A$group <- fct_inorder(A$group)

graph <- rawdata[-c(1,2,6,8)]
nirs <- cbind(rep("nirS", length(graph[graph$`Soil type`=="Black soil",1])),
              graph[graph$`Soil type`=="Black soil",c(2,3,4,1)],
              graph[graph$`Soil type`=="Red soil",c(4,1)],
              graph[graph$`Soil type`=="Yellow soil",c(4,1)])
colnames(nirs) <- c("group","Treatment", "Replicate", "genes_B","Black","genes_R","Red","genes_Y","Yellow")

nirk <- cbind(rep("nirK", length(graph[graph$`Soil type`=="Black soil",1])),
              graph[graph$`Soil type`=="Black soil",c(2,3,5,1)],
              graph[graph$`Soil type`=="Red soil",c(5,1)],
              graph[graph$`Soil type`=="Yellow soil",c(5,1)])
colnames(nirk) <- c("group","Treatment", "Replicate","genes_B","Black","genes_R","Red","genes_Y","Yellow")

plot <- rbind(nirs,nirk)
plot$group <- as.factor(plot$group)

bA<-ggplot(plot, aes(fill=Treatment, y=genes_B, x=group))+
    geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black")+
    theme_classic(base_size = 12)+
    theme(panel.border=element_rect(fill='transparent',linewidth=0.5),
          text = element_text(family = "C",size = 18, colour = "black"))+
    geom_vline(aes(xintercept=as.numeric(as.factor(group))+0.5),linetype=2,cex=2)+
    geom_rect(aes(xmin=as.numeric(as.factor(group))+0.5,xmax=Inf,ymin=(-Inf),ymax=Inf),
             fill='white',color='white')+
    # geom_vline(xintercept =plot$Treatment,linetype=2,cex=2)+
    geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black")+
    stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
                 width = 0.2,position = position_dodge(1))+
    scale_y_continuous(limits = c(0,8))+
  # theme(legend.direction = "horizontal", legend.position = "top")+
  labs(title = "", y="Gene copies", x = "")+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  # scale_fill_manual(values = c('#ffffff','#dae1ff','#9faaff','#5a71ff','#2a2aff'))+
  facet_grid(~ Black, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",
                                        size=0.7))+
  geom_jitter(data = plot, aes(x = group, y = genes_B,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F)
bA

bB<-ggplot(plot, aes(fill=Treatment, y=genes_R, x=group))+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black")+
  theme_classic(base_size = 12)+
  theme(panel.border=element_rect(fill='transparent',linewidth=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  geom_vline(aes(xintercept=as.numeric(as.factor(group))+0.5),linetype=2,cex=2)+
  geom_rect(aes(xmin=as.numeric(as.factor(group))+0.5,xmax=Inf,ymin=(-Inf),ymax=Inf),
            fill='white',color='white')+
  # geom_vline(xintercept =plot$Treatment,linetype=2,cex=2)+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black")+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1))+
  scale_y_continuous(limits = c(0,8))+
  # theme(legend.direction = "horizontal", legend.position = "top")+
  labs(title = "", y="Gene copies", x = "")+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  # scale_fill_manual(values = c('#ffffff','#dae1ff','#9faaff','#5a71ff','#2a2aff'))+
  facet_grid(~ Red, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",
                                        size=0.7))+
  geom_jitter(data = plot, aes(x = group, y = genes_R,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F)
bB

bC<-ggplot(plot, aes(fill=Treatment, y=genes_Y, x=group))+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black")+
  theme_classic(base_size = 12)+
  theme(panel.border=element_rect(fill='transparent',linewidth=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  geom_vline(aes(xintercept=as.numeric(as.factor(group))+0.5),linetype=2,cex=2)+
  geom_rect(aes(xmin=as.numeric(as.factor(group))+0.5,xmax=Inf,ymin=(-Inf),ymax=Inf),
            fill='white',color='white')+
  # geom_vline(xintercept =plot$Treatment,linetype=2,cex=2)+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black")+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1))+
  scale_y_continuous(limits = c(0,8))+
  # theme(legend.direction = "horizontal", legend.position = "top")+
  labs(title = "", y="Gene copies", x = "")+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  # scale_fill_manual(values = c('#ffffff','#dae1ff','#9faaff','#5a71ff','#2a2aff'))+
  facet_grid(~ Yellow, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",
                                        size=0.7))+
  geom_jitter(data = plot, aes(x = group, y = genes_Y,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F)
bC

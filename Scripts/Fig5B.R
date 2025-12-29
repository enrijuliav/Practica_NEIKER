setwd("~/Documentos/GitHub/Practica_NEIKER")

library(ggplot2)
library(forcats)
library(cowplot)
library(ggplot2)
library(ggbreak)

load5B <- function(csv) {
  # Load data
  data <- read.csv2(csv)
  # Change colnames
  colnames(data) <- data[1,]
  data <- data[-1,]
  # Change data types
  data$`Soil type` <- factor(data$`Soil type`)
  data$Treatment <- factor(data$Treatment, c("CT", "13CH4", "13CH4 _N"))
  for (name in grep("ate", colnames(data), value = T)) {
    data[name] <- as.numeric(sub(",",".",data[[name]]))
  }
  # Choose our columns
  data <- data[c("Soil type", "Treatment", "Acetate", "Propionate", "Butyrate", "Lactate", "Malate", "Succinate")]
  return(data)
}

# Load the data
rawdata <- load5B("Raw_data/MD5B_rawdata.csv")


# Select the variables for the analysis: Soil type and metabolites
soiltypes <- unique(rawdata[["Soil type"]])
metabolites <- grep("ate", colnames(rawdata), value = T)

# Initialize an object that will have all the stat results
stats <- rep(NA, 0)
pvalues <- rep(NA, 0)

# Cycle through every soil type and metabolite
for (soil in soiltypes) {
  for (metabolite in metabolites) {
    # Select only the data of our interest
    example <- rawdata[rawdata["Soil type"]==soil, c("Treatment",metabolite)]
    # ANOVA test
    result <- aov(example[[metabolite]] ~ Treatment, data = example)
    # Save the general ANOVA pvalue
    pvalues <- append(pvalues,round(summary(result)[[1]]["Pr(>F)"][1,], 3))
    # Extract the values for every posible combination of treatments in one set of conditions
    temp <- as.data.frame(round(TukeyHSD(result)[[1]], 3))
    colnames(temp) <- sub("", paste(soil, metabolite), colnames(temp))
    temp <- round(temp, digits = 3)
    stats <- append(stats, temp[4])
  }
}

padj <- round(p.adjust(pvalues, method = "fdr"), digits = 3)
padj < 0.05

stats <- as.data.frame(stats)
stats <- rbind(stats, pvalues, padj)
rownames(stats) <- c(rownames(temp), "p_val", "p_adj")

write.csv(stats, file = "Processed_data/stats5B")


# # Like this, we have replicated the results of the paper.
# # However there are values that are not the same. We could try to see if they
# # discarded some outliers:
# boxplot.stats(rawdata[rawdata["Soil type"] == "Black soil","Acetate"])$out
# boxplot.stats(rawdata[rawdata["Soil type"] == "Black soil","Butyrate"])$out
# boxplot.stats(rawdata[rawdata["Soil type"] == "Black soil","Succinate"])$out
# boxplot.stats(rawdata[rawdata["Soil type"] == "Red soil","Succinate"])$out
# boxplot.stats(rawdata[rawdata["Soil type"] == "Yellow soil","Butyrate"])$out
# 
# boxplot.stats(rawdata[rawdata["Soil type"] == "Yellow soil","Malate"])$out
# rawdata[rawdata == 49.37] <- NA
# example <- rawdata[rawdata["Soil type"]=="Yellow soil", c("Treatment","Malate")]
# summary(aov(example[["Malate"]] ~ Treatment, data = example))
# TukeyHSD(aov(example[["Malate"]] ~ Treatment, data = example))
# 
# boxplot.stats(rawdata[rawdata["Soil type"] == "Yellow soil","Succinate"])$out
# # Analyzing the data we can see that almost no outlier is found. In the case an
# # outlier is found, discarding it does not help with the results.
# 
# # Besides, I would say they were not careful enough with their statystical analysis.
# # Mainly, because of the number of combinations (6*3=18)
# # and the established limit (pvalue = 0.05), there is a high probability at least
# # 1 of the significant result is just a false positive.
# # In this case, I would apply a simple FDR correction (which is not as restrictive
# # as Bonferroni, and would not be useful, but it takes into consideration multiple
# # assays).
# padj <- round(p.adjust(pvalues, method = "fdr"), digits = 3)
# padj < 0.05


# Now we will roughly represent Figure 5B, with the significance we calculated.

##Fig.2B
library(ggplot2)
library(forcats)
library(ggbreak)

rawdata$grid <- rep("Acetate", nrow(rawdata))

FA <- ggplot(rawdata, aes(fill=Treatment, y = Acetate, x = `Soil type`)) + 
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black") +
  theme_classic(base_size = 12) +
  theme(panel.border=element_rect(fill='transparent',size=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  scale_fill_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  geom_vline(aes(xintercept=as.numeric(as.factor(`Soil type`))+0.5),linetype=2,cex=2)+
  geom_rect(aes(xmin=3.48,xmax=Inf,ymin=(-Inf),ymax=Inf),
            fill='white',color='white')+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  labs(x="",y="") +
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1)) +
  facet_grid(~ grid, scale="free",space="free_y") +
  geom_jitter(data = rawdata, aes(x = `Soil type`, y = Acetate ,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F)
  
FA

################################################################3

rawdata$grid <- rep("Propionate", nrow(rawdata))

FB <- ggplot(rawdata, aes(fill=Treatment, y = Propionate, x = `Soil type`)) + 
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black") +
  theme_classic(base_size = 12) +
  theme(panel.border=element_rect(fill='transparent',size=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  scale_fill_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  geom_vline(aes(xintercept=as.numeric(as.factor(`Soil type`))+0.5),linetype=2,cex=2)+
  geom_rect(aes(xmin=3.48,xmax=Inf,ymin=(-Inf),ymax=Inf),
            fill='white',color='white')+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  theme(legend.position = "none")+
  labs(x="",y="") +
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1)) +
  facet_grid(~ grid, scale="free",space="free_y") +
  geom_jitter(data = rawdata, aes(x = `Soil type`, y = Propionate ,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F)

FB

################################################################3

rawdata$grid <- rep("Butyrate", nrow(rawdata))

FC <- ggplot(rawdata, aes(fill=Treatment, y = Butyrate, x = `Soil type`)) + 
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black") +
  theme_classic(base_size = 12) +
  theme(panel.border=element_rect(fill='transparent',size=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  scale_fill_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  geom_vline(aes(xintercept=as.numeric(as.factor(`Soil type`))+0.5),linetype=2,cex=2)+
  geom_rect(aes(xmin=3.48,xmax=Inf,ymin=(-Inf),ymax=Inf),
            fill='white',color='white')+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  theme(legend.position = "none")+
  labs(x="",y="") +
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1)) +
  facet_grid(~ grid, scale="free",space="free_y") +
  geom_jitter(data = rawdata, aes(x = `Soil type`, y = Butyrate ,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F)

FC

################################################################3

rawdata$grid <- rep("Lactate", nrow(rawdata))

FD <- ggplot(rawdata, aes(fill=Treatment, y = Lactate, x = `Soil type`)) + 
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black") +
  theme_classic(base_size = 12) +
  theme(panel.border=element_rect(fill='transparent',size=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  scale_fill_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  geom_vline(aes(xintercept=as.numeric(as.factor(`Soil type`))+0.5),linetype=2,cex=2)+
  geom_rect(aes(xmin=3.48,xmax=Inf,ymin=(-Inf),ymax=Inf),
            fill='white',color='white')+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  theme(legend.position = "none")+
  labs(x="",y="") +
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1)) +
  facet_grid(~ grid, scale="free",space="free_y") +
  geom_jitter(data = rawdata, aes(x = `Soil type`, y = Lactate ,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F)

FD

################################################################3

rawdata$grid <- rep("Malate", nrow(rawdata))

FE <- ggplot(rawdata, aes(fill=Treatment, y = Malate, x = `Soil type`)) + 
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black") +
  theme_classic(base_size = 12) +
  theme(panel.border=element_rect(fill='transparent',size=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  scale_fill_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  geom_vline(aes(xintercept=as.numeric(as.factor(`Soil type`))+0.5),linetype=2,cex=2)+
  geom_rect(aes(xmin=3.48,xmax=Inf,ymin=(-Inf),ymax=Inf),
            fill='white',color='white')+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  theme(legend.position = "none")+
  labs(x="",y="") +
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1)) +
  facet_grid(~ grid, scale="free",space="free_y") +
  geom_jitter(data = rawdata, aes(x = `Soil type`, y = Malate ,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F)

FE

################################################################3

rawdata$grid <- rep("Succinate", nrow(rawdata))

FF <- ggplot(rawdata, aes(fill=Treatment, y = Succinate, x = `Soil type`)) + 
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black") +
  theme_classic(base_size = 12) +
  theme(panel.border=element_rect(fill='transparent',size=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  scale_fill_manual(values = c('#dae1ff','#9faaff','#2a2aff'))+
  geom_vline(aes(xintercept=as.numeric(as.factor(`Soil type`))+0.5),linetype=2,cex=2)+
  geom_rect(aes(xmin=3.48,xmax=Inf,ymin=(-Inf),ymax=Inf),
            fill='white',color='white')+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  theme(legend.position = "none")+
  labs(x="",y="") +
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1)) +
  facet_grid(~ grid, scale="free",space="free_y") +
  geom_jitter(data = rawdata, aes(x = `Soil type`, y = Succinate ,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F)

FF

FG <- ggarrange(FA, FB, FC, FD, FE, FF, ncol=3, nrow=2, common.legend = TRUE, legend="top")

annotate_figure(FG,
                bottom = text_grob("Soil type", size = 20),
                left = text_grob("¹³C-labeled fraction (%)", rot = 90, size = 20))

# Save with 1600*600 resolution

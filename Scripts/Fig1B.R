setwd("~/Documentos/GitHub/Practica_NEIKER")

library(ggplot2)
library(forcats)
library(cowplot)
library(ggplot2)
library(ggbreak)
library(ggpubr)

load1B <- function(csv) {
  # Load data
  data <- read.csv2(csv)
  # Change colnames
  data <- data[c("MBC","DR", "MOA","nirK.gene.abundance", "nirS.gene.abundance", "pmoA.gene.abundance")]
  data$MBC <- as.numeric(gsub(",",".", gsub("\\.","", data$MBC)))
  data$nirK.gene.abundance <- as.numeric(gsub(",",".", gsub("\\.","", data$nirK.gene.abundance)))
  data$nirS.gene.abundance <- as.numeric(gsub(",",".", gsub("\\.","", data$nirS.gene.abundance)))
  data$pmoA.gene.abundance <- as.numeric(gsub(",",".", gsub("\\.","", data$pmoA.gene.abundance)))
  return(data)
}

rawdata <- load1B("Raw_data/MD1B_rawdata.csv")


#######################################################################
#-----------------------------Estadística-----------------------------#
#######################################################################


# # Initialize an object that will have all the stat results
# stats <- rep(NA, 0)
# pvalues <- rep(NA, 0)
# 
# # Cycle through every soil type and metabolite
# for (soil in soiltypes) {
#   for (gene in genes) {
#     # Select only the data of our interest
#     example <- rawdata[rawdata["Soil type"] == soil, c("Treatment", gene)]
#     # ANOVA test
#     result <- aov(example[[gene]] ~ Treatment, data = example)
#     # Save the general ANOVA pvalue
#     pvalues <- append(pvalues, summary(result)[[1]]["Pr(>F)"][1, ])
#     # Extract the values for every posible combination of treatments in one set of conditions
#     temp <- as.data.frame(TukeyHSD(result)[[1]])
#     colnames(temp) <- sub("", paste(soil, gene), colnames(temp))
#     temp <- round(temp, digits = 3)
#     stats <- append(stats, temp[4])
#   }
# }
# 
# padj <- round(p.adjust(pvalues, method = "fdr"), digits = 3)
# padj < 0.05
# 
# stats <- as.data.frame(stats)
# stats <- rbind(stats, padj)
# rownames(stats) <- c(rownames(temp), "p_adj")
# 
# write.csv(stats, file = "Processed_data/stats2B")


#######################################################################
#-----------------------------Preparación-----------------------------#
#######################################################################


# Calculate de natural logarithm of the gas/genes divided by the biomass
gases <- log(rawdata[c("DR","MOA")]/rawdata$MBC)
genes <- log(rawdata[c("nirK.gene.abundance", "nirS.gene.abundance", "pmoA.gene.abundance")]/rawdata$MBC)

# Reformatting
genesK <- cbind(rep("nirK", nrow(genes)),genes[c("nirK.gene.abundance", "pmoA.gene.abundance")])
colnames(genesK) <- c("group", "gene", "pmoA")
genesS <- cbind(rep("nirS", nrow(genes)),genes[c("nirS.gene.abundance", "pmoA.gene.abundance")])
colnames(genesS) <- c("group", "gene", "pmoA")

genes <- rbind(genesK, genesS)

nirKmask <- genes$group=="nirK"
nirSmask <- genes$group=="nirS"


####################################################################
#-----------------------------Graficas-----------------------------#
####################################################################


p1<-ggplot(gases,aes(x=MOA, y=DR, fill = T))+
  geom_point(size=4,pch=21)+
  scale_color_manual(values = c('#000000'))+
  scale_fill_manual(values = c('#6393FB'))+
  scale_y_continuous(limits = c(-8,0),breaks = c(-8,-4,0))+
  scale_x_continuous(limits = c(-8,-1),breaks = c(-8,-6,-4,-2))+
  theme_bw() +
  theme(legend.position = "none") +
  geom_smooth(method='lm', aes(colour = "black")) +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()) +
  labs(title = "", y="DR (Ln-transformed,\nnmol ¹⁵N h⁻· µg⁻¹ biomass-C)", x = "MOA (Ln-transformed,\nµg CH₄ h⁻· µg⁻¹·biomass-C)") +
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68)) +
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68)) +
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7)) +
  annotate("label", x=-7.8, y=-0.5, label=paste("R²=",round(summary(lm(gases$DR ~ gases$MOA))[["r.squared"]], 2)))
p1




p2<-ggplot(genes,aes(x=pmoA, y=gene, fill = group)) +
  geom_point(size=4,pch=21) +
  scale_color_manual(values = c('#000000', "#000000")) +
  scale_y_continuous(limits = c(4,17.5),breaks = c(4, 8, 12, 16)) +
  scale_x_continuous(limits = c(8.5, 15),breaks = c(9, 11, 13, 15)) +
  theme_bw() +
  theme(legend.position = c(0.055, 0.88), legend.title=element_blank()) +
  geom_smooth(method='lm', color = c("black")) +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()) +
  labs(title = "", y = "Gene copies·µg⁻¹ biomass-C \n(Ln-transformed)", x = "pmoA gene copies·µg⁻¹ biomass-C\n(Ln-transformed)") +
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.ticks.x = element_line(colour = "black", size = 0.68)) +
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68)) +
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7)) +
  annotate("label", x=9.4, y=17, label=paste("nirK R²=",round(summary(lm(genes$gene[nirKmask] ~ genes$pmoA[nirKmask]))[["r.squared"]], 2))) +
  annotate("label", x=9.4, y=16, label=paste("nirS R²=",round(summary(lm(genes$gene[nirSmask] ~ genes$pmoA[nirSmask]))[["r.squared"]], 2))) 
p2

ggarrange(p1,p2,ncol=1,nrow=2)

# Save with 900*1100 resolution
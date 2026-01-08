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


#######################################################################
#-----------------------------Estadística-----------------------------#
#######################################################################


pval_gases <- summary(lm(DR ~ MOA, data = gases))[["coefficients"]][8]
pval_genesK <- summary(lm(gene ~ pmoA, data = genes[nirKmask,]))[["coefficients"]][8]
pval_genesS <- summary(lm(gene ~ pmoA, data = genes[nirSmask,]))[["coefficients"]][8]

pval_symbol <- function(pval) {
  # Returns the corresponding symbol depending on the pval
  return(ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "")))
}

symbol_gases <- pval_symbol(pval_gases)
symbol_genesK <- pval_symbol(pval_genesK)
symbol_genesS <- pval_symbol(pval_genesS)


stats <- data.frame(regression = c("MOA vs DR", "nirK vs pmoA", "nirS vs pmoA"),
                    pval = c(pval_gases, pval_genesK, pval_genesS))

write.csv(stats, file = "Processed_data/stats1B")


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
  annotate("label", x=-7.8, y=-0.5,
           label=paste("R²=",round(summary(lm(gases$DR ~ gases$MOA))[["r.squared"]], 2), symbol_gases))
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
  annotate("label", x=9.4, y=17,
           label=paste("nirK R²=",round(summary(lm(genes$gene[nirKmask] ~ genes$pmoA[nirKmask]))[["r.squared"]], 2), symbol_genesK)) +
  annotate("label", x=9.4, y=16,
           label=paste("nirS R²=",round(summary(lm(genes$gene[nirSmask] ~ genes$pmoA[nirSmask]))[["r.squared"]], 2), symbol_genesS)) 
p2

ggarrange(p1,p2,ncol=1,nrow=2)

# Save with 900*1100 resolution
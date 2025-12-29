setwd("~/Documentos/GitHub/Practica_NEIKER")

library(ggplot2)
library(forcats)
library(cowplot)
library(ggplot2)
library(ggbreak)

load3C <- function(csv) {
  # Load data
  data <- read.csv2(csv)
  # Change colnames
  colnames(data) <- data[1,]
  data <- data[-1,]
  data$`Soil type` <- as.factor(data$`Soil type`)
  data$Treatment <- as.factor(data$Treatment)
  data$gene <- as_factor(data$gene)
  data$denitrifiers <- sub("otu", "OTU", data$denitrifiers)
  data$`LDA score (log10)` <- as.numeric(gsub(",",".",data$`LDA score (log10)`))
  data$p <- as.numeric(gsub(",",".",data$p))
  colnames(data) <- c("Soiltype", "Treatment", "gene","denitrifiers", "LDA", "p")
  data <- data[data$Treatment!="CT", c("Soiltype", "gene","denitrifiers", "LDA", "p")]
  return(data)
}

rawdata <- load3C("Raw_data/MD3C_rawdata.csv")


###################################################################
#-----------------------------Gráfica-----------------------------#
###################################################################


ggplot(data=rawdata[rawdata$Soiltype == "Black soil",], aes(x = LDA, y = denitrifiers, fill = gene)) + geom_col()

ggplot(data=rawdata[rawdata$Soiltype == "Red soil",], aes(x = LDA, y = denitrifiers, fill = gene)) + geom_col()

ggplot(data=rawdata[rawdata$Soiltype == "Yellow soil",], aes(x = LDA, y = denitrifiers, fill = gene)) + geom_col()

setwd("~/Documentos/GitHub/Practica_NEIKER")

library(ggplot2)
library(forcats)
library(cowplot)
library(ggplot2)
library(ggbreak)
library(ggpubr)

load4A <- function(csv) {
  # Load data
  data <- read.csv(csv)
  
  # Change colnames
  colnames(data) <- data[1,]
  data <- data[-1,-1]
  
  data$`Soil type` <- as_factor(data$`Soil type`)
  data$Treatment <- as_factor(data$Treatment)
  data$Fraction <- as.numeric(data$Fraction)
  data$`Buoyant density` <- as.numeric(gsub(",",".",data$`Buoyant density`))
  data$`pmoA gene copies` <- as.numeric(gsub(",",".",data$`pmoA gene copies`))
  data$`nirS gene copies` <- as.numeric(gsub(",",".",data$`nirS gene copies`))
  data$`nirK gene copies` <- as.numeric(gsub(",",".",data$`nirK gene copies`))
  return(data)
}

rawdata <- load4A("Raw_data/MD4A_rawdata.csv")


#######################################################################
#-----------------------------Preparación-----------------------------#
#######################################################################

soils <- unique(rawdata$`Soil type`)
treatments <- unique(rawdata$Treatment)
fractions <- unique(rawdata$Fraction)

data <- as.data.frame(cbind("Soil.type","Treatment","Fraction", "Buoyant.density", "se.Buoyant.density", "pmoA", "se.pmoA", "nirS", "se.nirS", "nirK", "se.nirK"))
colnames(data) <- data[1,]

for (soil in soils) {
  soilmask <- rawdata$`Soil type`== soil
  for (treat in treatments) {
    treatmask <- rawdata$Treatment == treat
    for (frac in fractions) {
      fracmask <- rawdata$Fraction == frac
      mask <- soilmask & treatmask & fracmask
      data <- rbind(data,
                    c(soil, treat, frac,
                      round(mean(rawdata[mask,"Buoyant density"]), 3), round(sd(rawdata[mask,"Buoyant density"])/sqrt(3), 3),
                      round(mean(rawdata[mask,"pmoA gene copies"]), 4), round(sd(rawdata[mask,"pmoA gene copies"])/sqrt(3), 4),
                      round(mean(rawdata[mask,"nirS gene copies"]), 0), round(sd(rawdata[mask,"nirS gene copies"])/sqrt(3), 0),
                      round(mean(rawdata[mask,"nirK gene copies"]), 0), round(sd(rawdata[mask,"nirK gene copies"])/sqrt(3), 0)))
    }
  }
}

data <- data[-1,]

for (col in colnames(data)[-c(1:3)]) {
  data[col] <- as.numeric(data[[col]])
}


for (soil in soils) {
  soilmask <- data$Soil.type== soil
  for (treat in treatments) {
    mask <- data$Treatment == treat & soilmask
    for (col in c("pmoA", "nirS", "nirK")) {
      se_col <-  paste("se.", col, sep = "")
      data[mask, se_col] <- data[mask, se_col]/sum(data[mask, col])
      data[mask, col] <- data[mask, col]/sum(data[mask, col])
    }
  }
}





####################################################################
#-----------------------------Graficas-----------------------------#
####################################################################


p1_1 <- ggplot(data[data$Soil.type=="Black soil",],
               aes(x = Buoyant.density, y = pmoA, group = Treatment, fill = Treatment, colour = Treatment)) +
  geom_line(size=0.5) +
  geom_errorbar(aes(ymin = pmoA - se.pmoA, ymax = pmoA + se.pmoA), width=.002, size = 0.5) +
  geom_errorbar(aes(xmin = Buoyant.density - se.Buoyant.density, xmax = Buoyant.density + se.Buoyant.density),
                width=.005, size = 0.5) +
  geom_point(size=4, pch=21, color = "black") +
  scale_color_manual(values = c('black','#1ba858')) +
  scale_fill_manual(values = c('white','#1ba858')) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()) +
  labs(title = "", y="Black soil", x = "Buoyant density (g·mL⁻¹)") +
  theme(axis.text.x = element_text(size = 18, colour = "black", angle = 270),
        axis.ticks.x = element_line(colour = "black", size = 0.68)) +
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68)) +
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7)) + scale_y_continuous(position="right") +
  coord_flip() + scale_x_reverse(limits = c(1.69,1.74),breaks = c(169:174/100)) 

p1_1


p1_2 <- ggplot(data[data$Soil.type=="Red soil",],
               aes(x = Buoyant.density, y = pmoA, group = Treatment, fill = Treatment, colour = Treatment)) +
  geom_line(size=0.5) +
  geom_errorbar(aes(ymin = pmoA - se.pmoA, ymax = pmoA + se.pmoA), width=.002, size = 0.5) +
  geom_errorbar(aes(xmin = Buoyant.density - se.Buoyant.density, xmax = Buoyant.density + se.Buoyant.density),
                width=.005, size = 0.5) +
  geom_point(size=4, pch=21, color = "black") +
  scale_color_manual(values = c('black','#1ba858')) +
  scale_fill_manual(values = c('white','#1ba858')) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()) +
  labs(title = "", y="Red soil", x = "") +
  theme(axis.text.x = element_text(size = 18, colour = "black", angle = 270),
        axis.ticks.x = element_line(colour = "black", size = 0.68)) +
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68)) +
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7)) + scale_y_continuous(position="right") +
  coord_flip() + scale_x_reverse(limits = c(1.69,1.74),breaks = c()) 

p1_2


p1_3 <- ggplot(data[data$Soil.type=="Yellow soil",],
               aes(x = Buoyant.density, y = pmoA, group = Treatment, fill = Treatment, colour = Treatment)) +
  geom_line(size=0.5) +
  geom_errorbar(aes(ymin = pmoA - se.pmoA, ymax = pmoA + se.pmoA), width=.002, size = 0.5) +
  geom_errorbar(aes(xmin = Buoyant.density - se.Buoyant.density, xmax = Buoyant.density + se.Buoyant.density),
                width=.005, size = 0.5) +
  geom_point(size=4, pch=21, color = "black") +
  scale_color_manual(values = c('black','#1ba858')) +
  scale_fill_manual(values = c('white','#1ba858')) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()) +
  labs(title = "", y="Yellow soil", x = "") +
  theme(axis.text.x = element_text(size = 18, colour = "black", angle = 270),
        axis.ticks.x = element_line(colour = "black", size = 0.68)) +
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68)) +
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7)) + scale_y_continuous(position="right") +
  coord_flip() + scale_x_reverse(limits = c(1.69,1.74),breaks = c()) 
p1_3


p1 <- ggarrange(p1_1,p1_2, p1_3,ncol=3,nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(p1, top = text_grob("pmoA", face = "bold", size = 20))

# Save with 800 * 800 resolution



p2_1 <- ggplot(data[data$Soil.type=="Black soil",],
               aes(x = Buoyant.density, y = nirS, group = Treatment, fill = Treatment, colour = Treatment)) +
  geom_line(size=0.5) +
  geom_errorbar(aes(ymin = nirS - se.nirS, ymax = nirS + se.nirS), width=.002, size = 0.5) +
  geom_errorbar(aes(xmin = Buoyant.density - se.Buoyant.density, xmax = Buoyant.density + se.Buoyant.density),
                width=.005, size = 0.5) +
  geom_point(size=4, pch=21, color = "black") +
  scale_color_manual(values = c('black','#6D91C8')) +
  scale_fill_manual(values = c('white','#6D91C8')) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()) +
  labs(title = "", y="Black soil", x = "") +
  theme(axis.text.x = element_text(size = 18, colour = "black", angle = 270),
        axis.ticks.x = element_line(colour = "black", size = 0.68)) +
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68)) +
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7)) + scale_y_continuous(position="right") +
  coord_flip() + scale_x_reverse(limits = c(1.69,1.74),breaks = c()) 

p2_1


p2_2 <- ggplot(data[data$Soil.type=="Red soil",],
               aes(x = Buoyant.density, y = nirS, group = Treatment, fill = Treatment, colour = Treatment)) +
  geom_line(size=0.5) +
  geom_errorbar(aes(ymin = nirS - se.nirS, ymax = nirS + se.nirS), width=.002, size = 0.5) +
  geom_errorbar(aes(xmin = Buoyant.density - se.Buoyant.density, xmax = Buoyant.density + se.Buoyant.density),
                width=.005, size = 0.5) +
  geom_point(size=4, pch=21, color = "black") +
  scale_color_manual(values = c('black','#6D91C8')) +
  scale_fill_manual(values = c('white','#6D91C8')) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()) +
  labs(title = "", y="Red soil", x = "") +
  theme(axis.text.x = element_text(size = 18, colour = "black", angle = 270),
        axis.ticks.x = element_line(colour = "black", size = 0.68)) +
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68)) +
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7)) + scale_y_continuous(position="right") +
  coord_flip() + scale_x_reverse(limits = c(1.69,1.74),breaks = c()) 

p2_2


p2_3 <- ggplot(data[data$Soil.type=="Yellow soil",],
               aes(x = Buoyant.density, y = nirS, group = Treatment, fill = Treatment, colour = Treatment)) +
  geom_line(size=0.5) +
  geom_errorbar(aes(ymin = nirS - se.nirS, ymax = nirS + se.nirS), width=.002, size = 0.5) +
  geom_errorbar(aes(xmin = Buoyant.density - se.Buoyant.density, xmax = Buoyant.density + se.Buoyant.density),
                width=.005, size = 0.5) +
  geom_point(size=4, pch=21, color = "black") +
  scale_color_manual(values = c('black','#6D91C8')) +
  scale_fill_manual(values = c('white','#6D91C8')) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()) +
  labs(title = "", y="Yellow soil", x = "") +
  theme(axis.text.x = element_text(size = 18, colour = "black", angle = 270),
        axis.ticks.x = element_line(colour = "black", size = 0.68)) +
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68)) +
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7)) + scale_y_continuous(position="right") +
  coord_flip() + scale_x_reverse(limits = c(1.69,1.74),breaks = c()) 

p2_3


p2 <- ggarrange(p2_1,p2_2, p2_3,ncol=3,nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(p2, top = text_grob("nirS", face = "bold", size = 20))

# Save with 800*800 resolution



p3_1 <- ggplot(data[data$Soil.type=="Black soil",],
               aes(x = Buoyant.density, y = nirK, group = Treatment, fill = Treatment, colour = Treatment)) +
  geom_line(size=0.5) +
  geom_errorbar(aes(ymin = nirK - se.nirK, ymax = nirK + se.nirK), width=.002, size = 0.5) +
  geom_errorbar(aes(xmin = Buoyant.density - se.Buoyant.density, xmax = Buoyant.density + se.Buoyant.density),
                width=.005, size = 0.5) +
  geom_point(size=4, pch=21, color = "black") +
  scale_color_manual(values = c('black','#E37369')) +
  scale_fill_manual(values = c('white','#E37369')) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()) +
  labs(title = "", y="Black soil", x = "") +
  theme(axis.text.x = element_text(size = 18, colour = "black", angle = 270),
        axis.ticks.x = element_line(colour = "black", size = 0.68)) +
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68)) +
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7)) + scale_y_continuous(position="right") +
  coord_flip() + scale_x_reverse(limits = c(1.69,1.74),breaks = c()) 

p3_1


p3_2 <- ggplot(data[data$Soil.type=="Red soil",],
               aes(x = Buoyant.density, y = nirK, group = Treatment, fill = Treatment, colour = Treatment)) +
  geom_line(size=0.5) +
  geom_errorbar(aes(ymin = nirK - se.nirK, ymax = nirK + se.nirK), width=.002, size = 0.5) +
  geom_errorbar(aes(xmin = Buoyant.density - se.Buoyant.density, xmax = Buoyant.density + se.Buoyant.density),
                width=.005, size = 0.5) +
  geom_point(size=4, pch=21, color = "black") +
  scale_color_manual(values = c('black','#E37369')) +
  scale_fill_manual(values = c('white','#E37369')) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()) +
  labs(title = "", y="Red soil", x = "") +
  theme(axis.text.x = element_text(size = 18, colour = "black", angle = 270),
        axis.ticks.x = element_line(colour = "black", size = 0.68)) +
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68)) +
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7)) + scale_y_continuous(position="right") +
  coord_flip() + scale_x_reverse(limits = c(1.69,1.74),breaks = c()) 

p3_2


p3_3 <- ggplot(data[data$Soil.type=="Yellow soil",],
               aes(x = Buoyant.density, y = nirK, group = Treatment, fill = Treatment, colour = Treatment)) +
  geom_line(size=0.5) +
  geom_errorbar(aes(ymin = nirK - se.nirK, ymax = nirK + se.nirK), width=.002, size = 0.5) +
  geom_errorbar(aes(xmin = Buoyant.density - se.Buoyant.density, xmax = Buoyant.density + se.Buoyant.density),
                width=.005, size = 0.5) +
  geom_point(size=4, pch=21, color = "black") +
  scale_color_manual(values = c('black','#E37369')) +
  scale_fill_manual(values = c('white','#E37369')) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()) +
  labs(title = "", y="Yellow soil", x = "") +
  theme(axis.text.x = element_text(size = 18, colour = "black", angle = 270),
        axis.ticks.x = element_line(colour = "black", size = 0.68)) +
  theme(axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.68)) +
  theme(axis.title = element_text(size = 18, colour = "black"))+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black", size=0.7)) + scale_y_continuous(position="right") +
  coord_flip() + scale_x_reverse(limits = c(1.69,1.74),breaks = c()) 

p3_3


p3 <- ggarrange(p3_1,p3_2, p3_3,ncol=3,nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(p3, top = text_grob("nirK", face = "bold", size = 20))

# Save with 800*800 resolution

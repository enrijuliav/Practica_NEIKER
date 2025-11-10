# Define a reusable function to load the data from the paper.
# This data has the name of the figure before the colnames,
# which complicates the loading process

loaddata <- function(csv) {
  # Load data
  data <- read.csv(csv, sep = ";")
  # Change colnames
  colnames(data) <- data[1,]
  data <- data[-1,]
  # Change decimal separator from , to .
  for (name in colnames(data)[-c(1:4)]) {
    data[name] <- as.numeric(sub(",",".",data[[name]]))
    }
  return(data)
}

# Load the data
rawdata <- loaddata("Raw_data/MD5B_rawdata.csv")

# Load packages

# Select the variables for the analysis: Soil type and metabolites
soiltype <- unique(rawdata[["Soil type"]])
metabolites <- colnames(rawdata)[-c(1:4)]

# Initialize an object that will have all the stat results
stats <- rep(NA, 0)
pvalues <- rep(NA, 0)

# Cycle through every soil type and metabolite
for (soil in soiltype) {
  for (metabolite in metabolites) {
    # Select only the data of our interest
    example <- rawdata[rawdata["Soil type"]==soil, c("Treatment",metabolite)]
    # ANOVA test
    result <- aov(example[[metabolite]] ~ Treatment, data = example)
    # Save the general ANOVA pvalue
    pvalues <- append(pvalues,summary(result)[[1]]["Pr(>F)"][1,])
    # Extract the values for every posible combination of treatments in one set of conditions
    stats <- append(stats,TukeyHSD(result)[1])
  }
}

# Transform the vector into a matrix and change the row and column names
# for legibiliy and accessibility
stats <- matrix(data = stats, nrow = length(soiltype), ncol = length(metabolites), byrow = TRUE)
pvalues <- matrix(data = pvalues, nrow = length(soiltype), ncol = length(metabolites), byrow = TRUE)
colnames(stats) <- metabolites
rownames(stats) <- soiltype
colnames(pvalues) <- metabolites
rownames(pvalues) <- soiltype

# Like this, we have replicated the results of the paper.
# However there are values that are not the same. We could try to see if they
# discarded some outliers:
boxplot.stats(rawdata[rawdata["Soil type"] == "Black soil","Acetate"])$out
boxplot.stats(rawdata[rawdata["Soil type"] == "Black soil","Butyrate"])$out
boxplot.stats(rawdata[rawdata["Soil type"] == "Black soil","Succinate"])$out
boxplot.stats(rawdata[rawdata["Soil type"] == "Red soil","Succinate"])$out
boxplot.stats(rawdata[rawdata["Soil type"] == "Yellow soil","Butyrate"])$out

boxplot.stats(rawdata[rawdata["Soil type"] == "Yellow soil","Malate"])$out
rawdata[rawdata == 49.37] <- NA
example <- rawdata[rawdata["Soil type"]=="Yellow soil", c("Treatment","Malate")]
summary(aov(example[["Malate"]] ~ Treatment, data = example))
TukeyHSD(aov(example[["Malate"]] ~ Treatment, data = example))

boxplot.stats(rawdata[rawdata["Soil type"] == "Yellow soil","Succinate"])$out
# Analyzing the data we can see that almost no outlier is found. In the case an
# outlier is found, discarding it does not help with the results.

# Besides, I would say they were not careful enough with their statystical analysis.
# Mainly, because of the number of combinations (6*3=18)
# and the established limit (pvalue = 0.05), there is a high probability at least
# 1 of the significant result is just a false positive.
# In this case, I would apply a simple FDR correction (which is not as restrictive
# as Bonferroni, and would not be useful, but it takes into consideration multiple
# assays).
padj <- round(p.adjust(pvalues, method = "fdr"), digits = 3)
padj < 0.05


# Now we will roughly represent Figure 5B, with the significance we calculated.

##Fig.2B
library(ggplot2)
library(forcats)
library(ggbreak)
rawdata$`Soil type`<-factor(rawdata$`Soil type`)
rawdata$Treatment <- factor(rawdata$Treatment)
rawdata$Replicate <- factor(rawdata$Replicate)
prueba <- rawdata[,c(2,3,5)]


FA <- ggplot(rawdata, aes(fill=Treatment, y = Succinate, x = `Soil type`)) + 
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black",size=0.5) +
  theme_classic(base_size = 12) +
  theme(panel.border=element_rect(fill='transparent',size=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  geom_vline(aes(xintercept=as.numeric(as.factor(`Soil type`))+0.5),linetype=2,cex=2)+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1))
FA




A$condictions <- as.factor(A$condictions)
A$condictions <- fct_inorder(A$condictions)
A$group <- as.factor(A$group)
A$group <- fct_inorder(A$group)
bA<-ggplot(A, aes(fill=condictions, y=values, x=group))+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black",size=0.5)+
  theme_classic(base_size = 12)+
  theme(panel.border=element_rect(fill='transparent',size=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  geom_vline(aes(xintercept=as.numeric(as.factor(group))+0.5),linetype=2,cex=2)+
  geom_rect(aes(xmin=as.numeric(as.factor(group))+0.5,xmax=Inf,ymin=(-Inf),ymax=Inf),
            fill='white',color='white')+
  geom_vline(xintercept =A$condictions,linetype=2,cex=2)+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black",size=0.5)+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1))+
  scale_y_continuous(limits = c(0,8))+
  theme(legend.direction = "horizontal", legend.position = "top")+
  labs(title = "", y="Gene copies", x = "")+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  scale_fill_manual(values = c('#ffffff','#dae1ff','#9faaff','#5a71ff','#2a2aff'))+
  facet_grid(~ group1, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",
                                        size=0.7))+
  geom_jitter(data = A, aes(x = group, y = values,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 12345),
              size = 2, alpha = 0.8,show.legend = F)
bA

bB<-ggplot(A, aes(fill=condictions, y=valuesHN, x=group))+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black",size=0.5)+
  theme_classic(base_size = 12)+
  theme(panel.border=element_rect(fill='transparent',size=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  geom_vline(aes(xintercept=as.numeric(as.factor(group))+0.5),linetype=2,cex=2)+
  geom_rect(aes(xmin=as.numeric(as.factor(group))+0.5,
                xmax=Inf, ymin=(-Inf),ymax=Inf),
            fill='white',color='white')+
  geom_vline(xintercept =A$condictions,linetype=2,cex=2)+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black",size=0.5)+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1))+
  theme(legend.direction = "horizontal", legend.position = "top")+
  labs(title = "", y="Gene copies", x = "")+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  scale_fill_manual(values = c('#ffffff','#dae1ff','#9faaff','#5a71ff','#2a2aff'))+
  scale_y_continuous(limits = c(0,8))+
  facet_grid(~ group2, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",
                                        size=0.7))+
  geom_jitter(data = A, aes(x = group, y = valuesHN,color="black"),
              shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 123),
              size = 2, alpha = 0.8,show.legend = F)
bB
bC<-ggplot(A, aes(fill=condictions, y=valuesGD, x=group))+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black",size=0.5)+
  theme_classic(base_size = 12)+
  theme(panel.border=element_rect(fill='transparent',size=0.5),
        text = element_text(family = "C",size = 18, colour = "black"))+
  geom_vline(aes(xintercept=as.numeric(as.factor(group))+0.5),linetype=2,cex=2)+
  geom_rect(aes(xmin=as.numeric(as.factor(group))+0.5,
                xmax=Inf, ymin=(-Inf),ymax=Inf),
            fill='white',color='white')+
  geom_vline(xintercept =A$condictions,linetype=2,cex=2)+
  geom_bar(position=position_dodge(1),stat="summary",width=0.7,colour = "black",size=0.5)+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1))+
  theme(legend.direction = "horizontal", legend.position = "top")+
  labs(title = "", y="Gene copies", x = "")+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  scale_fill_manual(values = c('#ffffff','#dae1ff','#9faaff','#5a71ff','#2a2aff'))+
  scale_y_continuous(limits = c(0,8))+
  facet_grid(~ group3, scale="free",space="free_y")+
  theme(strip.text = element_text(size = 18),
        strip.background = element_rect(fill="#d9d9d9", colour="black",size=0.7))+
  geom_jitter(data = A, aes(x = group, y = valuesGD,color="black"),shape=21,color='black',
              position = position_jitterdodge(jitter.height=0.1,
                                              jitter.width = 0.2,
                                              dodge.width = 1,
                                              seed = 123),
              size = 2, alpha = 0.8,show.legend = F)
bC
(bA)|(bB)|(bC)




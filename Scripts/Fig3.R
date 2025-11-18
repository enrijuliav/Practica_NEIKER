# Define a reusable function to load the data from the paper.
# This data has the name of the figure before the colnames,
# which complicates the loading process
setwd("~/Documentos/GitHub/Practica_NEIKER")

load3A <- function(csv) {
  # Load data
  data <- read.csv2(csv)
  # Change colnames
  colnames(data) <- data[1,]
  data <- data[-1,]
  data$`Soil type` <- as.factor(data$`Soil type`)
  data$Treatment <- as.factor(data$Treatment)
  data$Replicate <- as.factor(data$Replicate)
  data$Time <- as.numeric(gsub("day","",data$Time))
  data$`N2O emission` <- as.numeric(gsub(",",".",data$`N2O emission`))
  return(data)
}

data <- load3A("Raw_data/MD3A_rawdata.csv")


for (soil in unique(data$`Soil type`)) {
  for (metabolite in metabolites) {
    # Select only the data of our interest
    example <- rawdata[rawdata["Soil type"]==soil, c("Treatment",metabolite)]
    # ANOVA test
    result <- aov(example[[metabolite]] ~ Treatment, data = example)
    # Save the general ANOVA pvalue
    pvalues <- append(pvalues,summary(result)[[1]]["Pr(>F)"][1,])
    # Extract the values for every possible combination of treatments in one set of conditions
    stats <- append(stats,TukeyHSD(result)[1])
  }
}
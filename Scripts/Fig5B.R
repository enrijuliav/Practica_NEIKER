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
    # Print the general ANOVA result and significance
    print(summary(result))
    pvalues <- append(pvalues,summary(result)[[1]]["Pr(>F)"][1,])
    # Extract the values for every posible combination of treatments
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

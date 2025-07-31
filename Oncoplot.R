# Install required packages
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
if (!requireNamespace("maftools", quietly = TRUE)) {
  BiocManager::install("maftools")
}
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

# Load packages
library(maftools)
library(readxl)

# Load MAF and clinical annotation from Excel
maf_data <- read_excel("path/to/your/file.xlsx", sheet = "mut")
clin_data <- read_excel("path/to/your/file.xlsx", sheet = "clin")

# Convert to MAF object
object <- read.maf(maf = maf_data, clinicalData = clin_data)

# Plot oncoplot (top 20 mutated genes by default) with default colors
oncoplot(maf = maf_object, top = 20)

# Set mutation colors
my_colors <- c(
  "Frame_Shift_Del" = "#e41a1c",
  "Frame_Shift_Ins" = "#377eb8",
  "Missense_Mutation" = "#4daf4a",
  "Nonsense_Mutation" = "#984ea3",
  "Nonstop_Mutation" = "#ff7f00",
  "In_Frame_Del" = "#ffff33",
  "In_Frame_Ins" = "#a65628",
  "Splice_Site" = "#f781bf",
  "Translation_Start_Site" = "#999999",
  "Multi_Hit" = "#66c2a5"  
)

# Plot oncoplot (top 20 mutated genes by default) with defined colors
oncoplot(maf = maf_object, top = 20, colors = my_colors)


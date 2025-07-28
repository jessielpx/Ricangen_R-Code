# Install and load all the packages needed:
install.packages("survival")
install.packages("survminer")
install.packages("readxl")
library(survival)
library(survminer)
library(readxl)

# Load data: Clinical data including gene mutation for each sample
tumors <- read_excel('PATH_TO_FOLDER/input_data.xlsx', sheet = 'input_data_clin')
# Exclude metastatic samples if needed
## There is a column "LocalisedRCC", 1 = Localised tumor, 0 = metastatic tumor
tumors <- tumors[tumors$LocalisedRCC == "1", ]

# Set the stratifier
## For Example, comparing survival between SETD2+/-
stratifier <- tumors$SETD2

# Applying filters
## Based on VHL status
tumors <- tumors[tumors$VHL == "0", ]
## Based on chromosomal event
tumors <- tumors[tumors$Chr3p_loss == "1", ]
## Based on tumor stage
tumors <- tumors[tumors$Stage == "III", ]
## Exlude specific samples
exclude_ids <- c("K2140265", "LRV228", "RS114736")
tumors <- tumors[!tumors$PatientID %in% exclude_ids, ]

# Define DFS censored at 5 years
## The maximum time for survival analysis
max_time <- 5
## Capture DFS time by 5 years
tumors$DFSYears_5yr <- pmin(tumors$DFSYears, max_time)
## Mark events beyond 5 years as censored
tumors$DFSEvent_5yr <- ifelse(tumors$DFSYears > max_time, 0, tumors$DFSEvent)

# Define the survival object
tumorSurv_5yr <- Surv(time = tumors$DFSYears_5yr, event = tumors$DFSEvent_5yr)

# Fit the Kaplan-Meier survival model
tumorKM_5yr <- survfit(tumorSurv_5yr ~ stratifier, data = tumors, type = "kaplan-meier")

# Log-rank test
logrank_test_5yr <- survdiff(tumorSurv_5yr ~ stratifier, data = tumors)

# Calculate p-value for the log-rank test
logrank_pval_5yr <- 1 - pchisq(logrank_test_5yr$chisq, df = length(logrank_test_5yr$n) - 1)
logrank_pval_5yr <- if (logrank_pval_5yr > 0.99999) {
  "p > 0.99999"
} else {
  paste("p =", signif(logrank_pval_5yr, 3))
}

# Optional: Pairwise p values (Bonferroni-adjusted) for multiple curves
pairwise_results <- pairwise_survdiff(
  Surv(DFSYears_5yr, DFSEvent_5yr) ~ stratifier,
  data = tumors,
  p.adjust.method = "bonferroni"
)
pairwise_results$p.value



# Generate curves
res <- ggsurvplot(
  tumorKM_5yr,
  conf.int = FALSE,
  pval = logrank_pval_5yr,
  pval.size = 4.5,
  pval.coord = c(3.7, 0.99),
  risk.table = TRUE,     # Risk table
  legend.labs = c("Group1","Group2"),     # It will automatically extract factors in alphabetic order
  legend = c(0.17, 0.3),     
  break.time.by = 1,
  legend.title = "",
  censor.shape = "",     # "+", "*", "|"...
  censor.size = 12,
  palette = c("#E63946","#457B9D"),
  xlab = "Time (years)",
  ylab = "DFS",
  font.x = c(14, "plain","black"),
  font.y = c(14, "plain","black"),
  font.tickslab = c(14, "plain","black"),
  font.legend = c(12, "plain","black"),
  font.risk.table = c(14, "plain","black"),  # This never works :-(
  risk.table.y.text = TRUE,
  risk.table.y.text.col = TRUE,
  risk.table.y.text.size = 14,
  risk.table.title = "Number at risk",
  ggtheme = theme_classic(base_size = 14) +
    theme(
      legend.key.size = unit(0, "lines"),
      legend.spacing.x = unit(0, "cm"),
      legend.spacing.y = unit(0, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.background = element_blank(),
      legend.key = element_blank()
    )
)

# Display and check
res

# Set the risk table
res$table <- res$table +
  theme(
    axis.text.x = element_text(size = 14, color = "black")
  )
res$table$layers[[1]]$aes_params$size <- 4

# Use patchwork to aligne the curves and risk table
library(patchwork)
aligned_plot <- res$plot / res$table + plot_layout(heights = c(6, 1))

print(aligned_plot)


# Colors for use

"#9B30FD", "#2E8B57"
"#457B9D", "#E63946"
"#f6a600", "#44AA99", "dodgerblue", "firebrick2"
"purple", "olivedrab3", "steelblue1", "azure4","gold","red","blue"





## Install required packages
install.packages("survival")
install.packages("survminer")
install.packages("forestmodel")
install.packages("readxl")

## Load libraries
library(survival)
library(survminer)
library(forestmodel)
library(readxl)

## Load clinical data and filter localized RCC only
tumors <- read_excel('PATH_TO_FOLDER/input_data.xlsx', sheet = 'input_data_clin')
tumors <- tumors[tumors$LocalisedRCC == "1", ]

## Optional filtering: by VHL status or other criteria
tumors <- tumors[tumors$VHL == "1", ]
tumors <- tumors[tumors$Stage == "III", ]

## Factor leveling (the first one will be reference), and set new labels
# Four levels:
tumors$Variate <- factor(tumors$Variate, levels = c("Group1", "Group2", "Group3", "Group4"), labels = c("Group1", "Group2", "Group3", "Group4"))
# Two levels:
tumors$Variate <- factor(tumors$Variate, levels = c("Group1", "Group2"))
# Key covariates
tumors$Sex <- factor(tumors$Sex, levels = c("Female", "Male"))
tumors$Age_raw <- as.numeric(tumors$Age_raw)
tumors$Stage <- factor(tumors$Stage, levels = c("I", "II", "III"))
tumors$VHLCategory <- factor(tumors$VHLCategory, levels = c("VHLmut+≤2", "VHLmut+>2"))

## Cap DFS time at 5 years and adjust event status accordingly
max_time <- 5
tumors$DFSYears_5yr <- pmin(tumors$DFSYears, max_time)
tumors$DFSEvent_5yr <- ifelse(tumors$DFSYears > max_time, 0, tumors$DFSEvent)

## Create survival object
surv_object <- Surv(time = tumors$DFSYears_5yr, event = tumors$DFSEvent_5yr)

## Fit Cox Proportional Hazards Model
cox_model <- coxph(surv_object ~ Variate + Stage + VHLCategory + Age_raw + Sex, data = tumors)
# Fit stratified model (e.g., by Stage)
cox_model <- coxph(surv_object ~ Variate + VHLCategory + Age_raw + Sex + strata(Stage), data = tumors)

## View model summary
summary(cox_model)

## Generate forest plot
forest_model(cox_model, recalculate_width = TRUE)

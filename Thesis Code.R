install.packages("haven")
install.packages("survey")
install.packages("srvyr")
install.packages("labelled")
install.packages("sandwich")
install.packages("lmtest")
install.packages("ggrepel")
install.packages("extrafont")

library(haven)
library(dplyr)
library(labelled)
library(survey)
library(srvyr)
library(sandwich)
library(lmtest)
library(tidyr)
library(car)
library(ggplot2)
library(ggrepel)
library(extrafont)
library(broom)
library(readr)


################################################  INITIATION  ################################################


#SET WORKING DIRECTORY

setwd()


#LOAD DATA WITH SELECTED VARIABLES

data <- read_dta("2021.dta") %>%
  select(
    v395, v384a, v384b, v384c, s815d,
    v106, v190, v502, v025,
    v467b, v467c, v467d,
    v005, v001, v022, v106, v130
  )


#RECODE AND FILTER

data <- data %>%
  mutate(
    
    #Creation of weight for survey design
    weight = v005 / 1000000,
    
    #Recoding
    v106 = factor(v106, levels = 0:3, labels = c("NoEd", "Primary", "Secondary", "Higher")),
    v190 = factor(v190, levels = 1:5, labels = c("Poorest", "Poor", "Middle", "Richer", "Richest")),
    v502 = factor(v502, levels = 0:2, labels = c("NeverinUnion", "CurrentlyinUnion", "FormerlyinUnion")),
    v025 = factor(v025, levels = c(1, 2), labels = c("Urban", "Rural")),
    v467b = factor(v467b, levels = c(0, 1, 2), labels = c("NoProblem", "BigProblem", "NotBigProblem")),
    v467c = factor(v467c, levels = c(0, 1, 2), labels = c("NoProblem", "BigProblem", "NotBigProblem")),
    v467d = factor(v467d, levels = c(0, 1, 2), labels = c("NoProblem", "BigProblem", "NotBigProblem")),
    v130 = factor(ifelse(v130 == 5, 0, 1), levels = c(0, 1), labels = c("NoReligion", "Religion"))
    
  ) %>%
  
  #Filter out missing rows
  filter(
    !is.na(v395), !is.na(v384a), !is.na(v106), !is.na(v190),
    !is.na(v025), !is.na(v502), !is.na(v467b), !is.na(v467c), !is.na(v467d),
  )


#DEFINE SURVEY DESIGN

dhs_design <- svydesign(
  ids = ~v001,
  strata = ~v022,
  weights = ~weight,
  data = data,
  nest = TRUE
)

################################################  DESCRIPTIVE STATISTICS  ################################################


options(survey.lonely.psu = "adjust")

descriptives <- svymean(~v395 + v384a + v384b + v384c + s815d+
                          v106 + v190 + v025 + v502 + v467b + v467c + v467d + v130,
                        design = dhs_design, na.rm = TRUE)


#MAKE A DATA FRAME

descriptive_table <- as.data.frame(descriptives)
descriptive_table$Variable <- rownames(descriptive_table)
descriptive_table <- descriptive_table %>%
  select(Variable, Mean = mean, SE = SE) %>%
  mutate(Mean = round(Mean, 4), SE = round(SE, 4))


#VIEW

print(descriptive_table)


#EXPORT

write_csv(descriptive_table, "Descriptive Stats.csv")

################################################  COMPARISON WITH CLEAN SAMPLE VS. ORIGINAL SAMPLE  ################################################


#MAKE A ORIGINAL SAMPLE

full_data <- read_dta("2021.dta") %>%
  select(
    v395, v384a, v384b, v384c, s815d,
    v106, v190, v502, v025,
    v467b, v467c, v467d,
    v005, v001, v022, v106,v130
  )


#RECODE 

full_data <- full_data %>%
  mutate(
    #Creation of weight for survey design
    weight = v005 / 1000000,
    
    #Recoding
    v106 = factor(v106, levels = 0:3, labels = c("NoEd", "Primary", "Secondary", "Higher")),
    v190 = factor(v190, levels = 1:5, labels = c("Poorest", "Poor", "Middle", "Richer", "Richest")),
    v502 = factor(v502, levels = 0:2, labels = c("NeverinUnion", "CurrentlyinUnion", "FormerlyinUnion")),
    v025 = factor(v025, levels = c(1, 2), labels = c("Urban", "Rural")),
    v467b = factor(v467b, levels = c(0, 1, 2), labels = c("NoProblem", "BigProblem", "NotBigProblem")),
    v467c = factor(v467c, levels = c(0, 1, 2), labels = c("NoProblem", "BigProblem", "NotBigProblem")),
    v467d = factor(v467d, levels = c(0, 1, 2), labels = c("NoProblem", "BigProblem", "NotBigProblem")),
    v130 = factor(ifelse(v130 == 5, 0, 1), levels = c(0, 1), labels = c("NoReligion", "Religion"))
    
  )


#DEFINE VARIABLES TO COMPARE

vars_to_compare <- ~
  v395 + v384a + v384b + v384c + s815d+
  v106 + v190 + v025 + v502 + v467b + v467c + v467d + v130


#CLEAN THE DATA FOR 

clean_data <- full_data %>%
  filter(complete.cases(v395, v384a, v384b, v384c, s815d, v106, v190, v502, v025, v467b, v467c, v467d,v130)) %>%
  mutate(across(where(is.factor), ~droplevels(.)))  # Reset factor levels


#CREATE SURVEY DESIGNS

design_full <- svydesign(ids = ~v001, weights = ~weight, data = full_data, nest = TRUE)
design_clean <- svydesign(ids = ~v001, weights = ~weight, data = clean_data, nest = TRUE)


#COMPUTE MEANS

means_full <- svymean(vars_to_compare, design_full) %>% as.data.frame() %>%
  mutate(Variable = rownames(.), Mean_Full = mean) %>%
  select(Variable, Mean_Full)

means_clean <- svymean(vars_to_compare, design_clean) %>% as.data.frame() %>%
  mutate(Variable = rownames(.), Mean_Clean = mean) %>%
  select(Variable, Mean_Clean)


#MAKE COMPARISON TABLE

comparison <- full_join(means_full, means_clean, by = "Variable") %>%
  mutate(Difference = Mean_Clean - Mean_Full) %>%
  mutate(across(c(Mean_Full, Mean_Clean, Difference), round, 4))


#EXPORT

write_csv(comparison, "comparison.csv", na = "NA")

################################################  MISSING VALUES  ################################################


#MAKE NEW DATAFRAME

missing_data <- read_dta("2021.dta")


#COMPUTE MISSING NUMBER

missing_summary <- data.frame(
  Variable = c("v384d", "v107", "v632","v395"),
  Missing = sapply(missing_data[c("v384d", "v107", "v632","v395")], function(x) sum(is.na(x))),
  Total = nrow(missing_data)
)


#COMPUTE MISSING PERCENTAGE

missing_summary$PercentMissing <- round((missing_summary$Missing / missing_summary$Total) * 100, 2)


#EXPORT

write_csv(missing_summary, "Missing_Variables.csv")

################################################  ASSUMPTIONS  ################################################


#CREATE FULL MODEL

first_model <- svyglm(
  v395 ~ v384a + v384b + v384c + s815d +  
    v106 + v190 + v025 + v502 + v467b + v467c + v467d + v130 + 
    v384a:v106 + v384a:v190 + v384a:v025 + v384a:v502 + v384a:v467b + v384a:v467c + v384a:v467d + v384a:v130 +
    v384b:v106 + v384b:v190 + v384b:v025 + v384b:v502 + v384b:v467b + v384b:v467c + v384b:v467d + v384b:v130 +
    v384c:v106 + v384c:v190 + v384c:v025 + v384c:v502 + v384c:v467b + v384c:v467c + v384c:v467d + v384c:v130 +
    s815d:v106 + s815d:v190 + s815d:v025 + s815d:v502 + s815d:v467b + s815d:v467c + s815d:v467d + s815d:v130 ,
  design = dhs_design,
  family = quasibinomial()
)


#MAKE DATAFRAME

model_results1 <- tidy(first_model) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ))


#EXPORT

write_csv(model_results1, "First Model.csv")


#TRANFORMATION INTO GLM MODEL FOR ASSUMPTIONS

glm_model <- glm(
  v395 ~ v384a + v384b + v384c + s815d + 
    v106 + v190 + v025 + v502 + v467b + v467c + v467d + v130 + 
    v384a:v106 + v384a:v190 + v384a:v025 + v384a:v502 + v384a:v467b + v384a:v467c + v384a:v467d + v384a:v130 +
    v384b:v106 + v384b:v190 + v384b:v025 + v384b:v502 + v384b:v467b + v384b:v467c + v384b:v467d + v384b:v130 +
    v384c:v106 + v384c:v190 + v384c:v025 + v384c:v502 + v384c:v467b + v384c:v467c + v384c:v467d + v384c:v130 +
    s815d:v106 + s815d:v190 + s815d:v025 + s815d:v502 + s815d:v467b + s815d:v467c + s815d:v467d + s815d:v130 ,
  data = data,
  family = quasibinomial()
)


#-----------------------------GVIF-------------------------------


#FIRST VIF TEST

gvif_matrix <- vif(glm_model)
gvif_df <- as.data.frame(gvif_matrix)
gvif_df$Variable <- rownames(gvif_matrix)
gvif_df <- gvif_df[, c("Variable", "GVIF", "Df", "GVIF^(1/(2*Df))")]


#EXPORT

write_csv(gvif_df, "VIF Model.csv")

#---------------------------- TRANSFORMATION - REMOVAL WEALTH AND EDUCTAION ---------------------------- 


#REMOVE WEALTH AND EDUCATION FROM GLM

glm_model2 <- glm(
  v395 ~ v384a + v384b + v384c + s815d + 
     v025 + v502 + v467b + v467c + v467d + v130 +
    v384a:v025 + v384a:v502 + v384a:v467b + v384a:v467c + v384a:v467d +v384a:v130 +
   v384b:v025 + v384b:v502 + v384b:v467b + v384b:v467c + v384b:v467d +v384b:v130 +
     v384c:v025 + v384c:v502 + v384c:v467b + v384c:v467c + v384c:v467d +v384c:v130 +
     s815d:v025 + s815d:v502 + s815d:v467b + s815d:v467c + s815d:v467d + s815d:v130,
  data = data,
  family = quasibinomial()
)


#---------------------------- VIF TEST 2 ---------------------------- 


#SECOND VIF TEST

gvif2_matrix <- vif(glm_model2)
gvif2_df <- as.data.frame(gvif2_matrix)
gvif2_df$Variable <- rownames(gvif2_matrix)
gvif2_df <- gvif2_df[, c("Variable", "GVIF", "Df", "GVIF^(1/(2*Df))")]


#EXPORT

write_csv(gvif2_df, "VIF Model 2.csv")

#---------------------------- INFLUENCE PLOT ---------------------------- 


#MAKE INFLUENCE DATA

influence_data <- glm_model2 %>%
  augment() %>%
  mutate(
    hatvalue = .hat,
    studres = rstudent(glm_model2),
    cooks.distance = .cooksd,
    ID = row_number()
  )


#COMPUTE LEVERAGE

mean_leverage <- mean(influence_data$hatvalue)
high_leverage_cutoff <- 2 * mean_leverage


#CREATE PLOT

ggplot(influence_data, aes(x = hatvalue, y = studres)) +
  geom_point(aes(size = cooks.distance, colour = cooks.distance), alpha = 0.5) +
  scale_size(range = c(1, 10)) +
  scale_colour_gradient(low = "#73A8D8", high = "#1F3B73") +
    geom_vline(xintercept = c(mean_leverage, high_leverage_cutoff), linetype = "dashed") +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
    geom_text_repel(
    data = subset(influence_data, cooks.distance > quantile(cooks.distance, 0.997)),
    aes(label = ID),
    size = 3,
    box.padding = 0.3,
    max.overlaps = 20,
    family = "Times New Roman"
  ) +
    theme_minimal(base_family = "Times New Roman") +
  labs(
    x = "Hat-Values",
    y = "Studentised Residuals",
    size = "Cook's Distance",
    colour = "Cook's Distance"
  )


#EXPORT

ggsave("influence_plot.png", width = 7, height = 4, units = "in", dpi = 300)

#---------------------------- HISTOGRAM OF FITTED PROBABILITIES ---------------------------- 


#COMPUTE FITTED VALUES

fitted_vals <- fitted(glm_model2)


#EXPORT 

png(filename = "Fitted_Values_Histogram.png", width = 1200, height = 800, res = 150)
par(family = "Times")


#CREATE HISTOGRAM

hist(fitted_vals, 
     breaks = 30, 
     col = "#1F3B73", 
     border = "white", 
     main = "",
     xlab = "Predicted Probability",
     ylab = "Frequency",
     cex.lab = 1.2, 
     cex.axis = 1.2, 
     cex.main = 1.4)
dev.off()

#---------------------------- EVENTS PER VARIABLE ---------------------------- 


#COUNT EVENTS AND PREDICTORS

n_events <- sum(data$v395 == 1, na.rm = TRUE)
n_predictors <- (length(coef(glm_model2))-1)


#COMPUTE EVENTS PER VARIABLE

epv <- n_events / n_predictors
print(paste("EPV =", round(epv, 2)))


################################################  FINAL MODEL  ################################################


#FINAL MODEL WITHOUT WEALTH AND EDUCATION

final_model <- svyglm(
  v395 ~ v384a + v384b + v384c + s815d +  
     v025 + v502 + v467b + v467c + v467d + v130 + 
    v384a:v025 + v384a:v502 + v384a:v467b + v384a:v467c + v384a:v467d +v384a:v130 +
    v384b:v025 + v384b:v502 + v384b:v467b + v384b:v467c + v384b:v467d +v384b:v130 +
    v384c:v025 + v384c:v502 + v384c:v467b + v384c:v467c + v384c:v467d +v384c:v130 +
    s815d:v025 + s815d:v502 + s815d:v467b + s815d:v467c + s815d:v467d + s815d:v130,
  design = dhs_design,
  family = quasibinomial()
)


#MAKE DATAFRAME

model_results <- tidy(final_model) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ))


#EXPORT

write_csv(model_results, "Final Model.csv")


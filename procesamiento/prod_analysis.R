# 0. Identification ---------------------------------------------------

# Title: Data preparation for research paper on Attributions about wealth, perceived greed, and punishment toward women and men in wealth
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsible: Researcher

# Executive Summary: This script contains the code to data preparation for Contact and Classism
# Date: July 23, 2025

# 1. Packages  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjmisc, 
               sjPlot,
               here,
               lavaan,
               effectsize,
               sjlabelled,
               data.table,
               psych,
               rstatix,
               gtools,
               summarytools,
               kableExtra,
               jmv,
               semTools)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(here("input/data/proc/df_study3.RData"))

glimpse(df_study3)

# Asegurar factores
df_study3$sex <- factor(df_study3$sex)
df_study3$income_quintile <- factor(df_study3$income_quintile)
df_study3$country <- factor(df_study3$country)

# Crear dummies excluyendo la primera categoría de cada uno
sex_dummies <- model.matrix(~ sex, data = df_study3)[, -1]
income_dummies <- model.matrix(~ income_quintile, data = df_study3)[, -1]
country_dummies <- model.matrix(~ country, data = df_study3)[, -1]

# Añadir al dataframe
df_study3 <- cbind(df_study3, sex_dummies, income_dummies, country_dummies) %>% as_tibble()

sex_vars <- colnames(sex_dummies)
income_vars <- colnames(income_dummies)
country_vars <- colnames(country_dummies)

# 3. Analysis --------------------------------------------------------------

#### 3.1 Descriptive ####

#### 3.1.1 All countries ####

df_study3 %>% 
  select(external_i, internal_i, greedy_i, punish_i) %>% 
  summarytools::dfSummary()

M <- df_study3 %>% 
  select(external_i, internal_i, greedy_i, punish_i, condi_gender) %>% 
  remove_all_labels() %>% 
  mutate_all(~as.numeric(.)) 

descrip_men <- M %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                 na.deletion = "pairwise", 
                 corr.method = "pearson", 
                 triangle = "lower")

descrip_women <- M %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                   na.deletion = "pairwise", 
                   corr.method = "pearson", 
                   triangle = "upper")


#### 3.1.2 Argentina ####

M_arg <- df_study3 %>% 
  filter(country == "Argentina") %>% 
  select(external_i, internal_i, greedy_i, punish_i, condi_gender) %>% 
  remove_all_labels() %>% 
  mutate_all(~as.numeric(.)) 

descrip_men_arg <- M_arg %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M_arg %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                   na.deletion = "pairwise", 
                   corr.method = "pearson", 
                   triangle = "lower")

descrip_women_arg <- M_arg %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M_arg %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                   na.deletion = "pairwise", 
                   corr.method = "pearson", 
                   triangle = "upper")


#### 3.1.2 Chile ####

M_cl <- df_study3 %>% 
  filter(country == "Chile") %>% 
  select(external_i, internal_i, greedy_i, punish_i, condi_gender) %>% 
  remove_all_labels() %>% 
  mutate_all(~as.numeric(.)) 

descrip_men_cl <- M_cl %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M_cl %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                   na.deletion = "pairwise", 
                   corr.method = "pearson", 
                   triangle = "lower")

descrip_women_cl <- M_cl %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M_cl %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                   na.deletion = "pairwise", 
                   corr.method = "pearson", 
                   triangle = "upper")


#### 3.1.3 Colombia ####

M_col <- df_study3 %>% 
  filter(country == "Colombia") %>% 
  select(external_i, internal_i, greedy_i, punish_i, condi_gender) %>% 
  remove_all_labels() %>% 
  mutate_all(~as.numeric(.)) 

descrip_men_col <- M_col %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M_col %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                   na.deletion = "pairwise", 
                   corr.method = "pearson", 
                   triangle = "lower")

descrip_women_col <- M_col %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M_col %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                   na.deletion = "pairwise", 
                   corr.method = "pearson", 
                   triangle = "upper")

#### 3.1.3 Mexico ####

M_mex <- df_study3 %>% 
  filter(country == "Mexico") %>% 
  select(external_i, internal_i, greedy_i, punish_i, condi_gender) %>% 
  remove_all_labels() %>% 
  mutate_all(~as.numeric(.)) 

descrip_men_mex <- M_mex %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M_mex %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                   na.deletion = "pairwise", 
                   corr.method = "pearson", 
                   triangle = "lower")

descrip_women_mex <- M_mex %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M_mex %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                   na.deletion = "pairwise", 
                   corr.method = "pearson", 
                   triangle = "upper")

#### 3.1.3 Spain ####

M_spa <- df_study3 %>% 
  filter(country == "Spain") %>% 
  select(external_i, internal_i, greedy_i, punish_i, condi_gender) %>% 
  remove_all_labels() %>% 
  mutate_all(~as.numeric(.)) 

descrip_men_spa <- M_spa %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M_spa %>% 
  filter(condi_gender == 0) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                   na.deletion = "pairwise", 
                   corr.method = "pearson", 
                   triangle = "lower")

descrip_women_spa <- M_spa %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  psych::describe() %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

M_spa %>% 
  filter(condi_gender == 1) %>% 
  select(-condi_gender) %>% 
  sjPlot::tab_corr(., digits = 2,
                   na.deletion = "pairwise", 
                   corr.method = "pearson", 
                   triangle = "upper")


#### 3.2 Reliability ####

##### 3.2.1 External attributions ####

alpha_by_country_external <- df_study3 %>%
  select(country, condi_gender, ex_we_1, ex_we_2) %>%
  group_by(country, condi_gender) %>%
  group_modify(~{
    data_items <- select(.x, ex_we_1, ex_we_2)
    alpha_result <- psych::alpha(data_items)
    tibble(alpha = alpha_result$total$raw_alpha, 
           std.alpha = alpha_result$total$std.alpha)
  })


alpha_general_external <- df_study3 %>%
  select(condi_gender, ex_we_1, ex_we_2) %>%
  group_by(condi_gender) %>%
  group_modify(~{
    data_items <- select(.x, ex_we_1, ex_we_2)
    alpha_result <- psych::alpha(data_items)
    tibble(alpha = alpha_result$total$raw_alpha, 
           std.alpha = alpha_result$total$std.alpha)
  })


##### 3.2.2 Internal attributions ####

alpha_by_country_internal <- df_study3 %>%
  select(country, condi_gender, in_we_1, in_we_2) %>%
  group_by(country, condi_gender) %>%
  group_modify(~{
    data_items <- select(.x, in_we_1, in_we_2)
    alpha_result <- psych::alpha(data_items)
    tibble(alpha = alpha_result$total$raw_alpha, 
           std.alpha = alpha_result$total$std.alpha)
  })


alpha_general_internal <- df_study3 %>%
  select(condi_gender, in_we_1, in_we_2) %>%
  group_by(condi_gender) %>%
  group_modify(~{
    data_items <- select(.x, in_we_1, in_we_2)
    alpha_result <- psych::alpha(data_items)
    tibble(alpha = alpha_result$total$raw_alpha, 
           std.alpha = alpha_result$total$std.alpha)
  })


##### 3.2.3 Greedy ####

alpha_by_country_greedy <- df_study3 %>%
  select(country, condi_gender, greedy_1, greedy_2, greedy_3) %>%
  group_by(country, condi_gender) %>%
  group_modify(~{
    data_items <- select(.x, greedy_1, greedy_2, greedy_3)
    alpha_result <- psych::alpha(data_items)
    tibble(alpha = alpha_result$total$raw_alpha, 
           std.alpha = alpha_result$total$std.alpha)
  })


alpha_general_greedy <- df_study3 %>%
  select(condi_gender, greedy_1, greedy_2, greedy_3) %>%
  group_by(condi_gender) %>%
  group_modify(~{
    data_items <- select(.x, greedy_1, greedy_2, greedy_3)
    alpha_result <- psych::alpha(data_items)
    tibble(alpha = alpha_result$total$raw_alpha, 
           std.alpha = alpha_result$total$std.alpha)
  })

##### 3.2.4 Punishment ####

alpha_by_country_punish <- df_study3 %>%
  select(country, condi_gender, punish_1, punish_2, punish_3) %>%
  group_by(country, condi_gender) %>%
  group_modify(~{
    data_items <- select(.x, punish_1, punish_2, punish_3)
    alpha_result <- psych::alpha(data_items)
    tibble(alpha = alpha_result$total$raw_alpha, 
           std.alpha = alpha_result$total$std.alpha)
  })


alpha_general_punish <- df_study3 %>%
  select(condi_gender, punish_1, punish_2, punish_3) %>%
  group_by(condi_gender) %>%
  group_modify(~{
    data_items <- select(.x, punish_1, punish_2, punish_3)
    alpha_result <- psych::alpha(data_items)
    tibble(alpha = alpha_result$total$raw_alpha, 
           std.alpha = alpha_result$total$std.alpha)
  })


# CFA models ----

#### CFA All countries #### 
model_cfa <- '
  # Factores latentes
  greedy_f =~ greedy_1 + greedy_2 + greedy_3
  punish_f =~ punish_1 + punish_2 + punish_3
'
# Estimación 

subset(df_study3, subset = condi_gender == 0,
       select = c(greedy_1, greedy_2, greedy_3, 
                  punish_1, punish_2, punish_3)) %>% 
  as_numeric() %>% 
  mardia(na.rm = TRUE, plot=TRUE)


subset(df_study3, subset = condi_gender == 1,
       select = c(greedy_1, greedy_2, greedy_3, 
                  punish_1, punish_2, punish_3)) %>% 
  as_numeric() %>% 
  mardia(na.rm = TRUE, plot=TRUE)

fit_cfa_man <- lavaan::cfa(model_cfa, 
               data = subset(df_study3, condi_gender == 0), 
               estimator = "MLR")

fit_cfa_women <- lavaan::cfa(model_cfa, 
                           data = subset(df_study3, condi_gender == 1),
                           estimator = "MLR")

summary(fit_cfa_man, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(fit_cfa_women, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


## By country ##

paises <- c("Argentina", "Chile", "Colombia", "Mexico", "Spain")

# man
run_cfa_country_man <- function(pais) {
  message("\n##### CFA ", pais, " #####")
  
  
  fit_cfa_man <- lavaan::cfa(model_cfa, 
                             data = subset(df_study3, condi_gender == 0), 
                             estimator = "MLR")
  
  print(summary(fit_cfa_man, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
  
  fit_meas <- fitmeasures(fit_cfa_man, c("chisq", "pvalue", "df", "cfi", "tli", 
                                         "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
  
  return(list(
    country = pais,
    fit = fit_cfa_man,
    fit_meas = fit_meas
  ))
}

resultados_cfa_man <- map(paises, run_cfa_country_man)

names(resultados_cfa_man) <- paises

# women
run_cfa_country_women <- function(pais) {
  message("\n##### CFA ", pais, " #####")
  
  
  fit_cfa_man <- lavaan::cfa(model_cfa, 
                             data = subset(df_study3, condi_gender == 1), 
                             estimator = "MLR")
  
  print(summary(fit_cfa_man, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
  
  fit_meas <- fitmeasures(fit_cfa_man, c("chisq", "pvalue", "df", "cfi", "tli", 
                                         "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
  
  return(list(
    country = pais,
    fit = fit_cfa_man,
    fit_meas = fit_meas
  ))
}

resultados_cfa_women <- map(paises, run_cfa_country_women)

names(resultados_cfa_women) <- paises


# SEM models ----

#### SEM All countries #### 

# Definir modelo 

model_base <- ('
  # Factores latentes
  greedy_f =~ greedy_1 + greedy_2 + greedy_3
  punish_f =~ punish_1 + punish_2 + punish_3

  # Efectos directos
  greedy_f ~ a1*external_i + a2*internal_i
  punish_f ~ b1*external_i + b2*internal_i

  punish_f ~ m1*greedy_f

  # Efectos indirectos
  ind_punish_external := a1 * m1
  ind_punish_internal := a2 * m1
')

controls <- c("age", "edu_dic", "ses", "po", "hig_ide", "mid_ide", "low_ide",
              sex_vars, income_vars, country_vars)

control_formula <- paste(controls, collapse = " + ")

control_block <- paste0(
  "greedy_f ~ ", control_formula, "\n",
  "punish_f ~ ", control_formula, "\n"
)

model <- paste(model_base, control_block)

# Estimación 

df_man <- subset(df_study3, subset = condi_gender == 0)
df_women <- subset(df_study3, subset = condi_gender == 1)

fit_sem_man <- lavaan::sem(model, data = df_man, estimator = "MLR")
fit_sem_women <- lavaan::sem(model, data = df_women, estimator = "MLR")

summary(fit_sem_man, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(fit_sem_women, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Monte Carlo confidence intervals 
monteCarloCI(fit_sem_man, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)
monteCarloCI(fit_sem_women, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)


# Efectos directos, indirectos y totales
parameterEstimates(fit_sem_man, standardized = TRUE) %>%
  dplyr::filter(op %in% c("~", ":="))


## Man

# Todos los efectos
est_man <- parameterEstimates(fit_sem_man, standardized = TRUE)

# Efectos directos (regresiones)
direct <- est_man %>%
  filter(op == "~", lhs %in% c("punish_f"), 
         rhs %in% c("external_i", "internal_i")) %>%
  select(lhs, rhs, est.std = std.all, p.direct = pvalue)

# Efectos indirectos (los que tú definiste con :=)
indirect <- est_man %>%
  filter(op == ":=", grepl("ind_", lhs)) %>%
  mutate(
    outcome = case_when(
      grepl("punish", lhs) ~ "punish_f"
    ),
    predictor = case_when(
      grepl("external", lhs) ~ "external_i",
      grepl("internal", lhs) ~ "internal_i"
    )
  ) %>%
  select(lhs, outcome, predictor, est.indirect = est, p.indirect = pvalue, std.indirect = std.all, se, ci.lower, ci.upper)

# Unir por outcome y predictor
effects_total_man <- left_join(direct,
                           indirect,
                           by = c("lhs" = "outcome", "rhs" = "predictor")) %>%
  mutate(total = est.std + std.indirect)


## Women

# Todos los efectos
est_women <- parameterEstimates(fit_sem_women, standardized = TRUE)

# Efectos directos (regresiones)
direct <- est_women %>%
  filter(op == "~", lhs %in% c("punish_f"), 
         rhs %in% c("external_i", "internal_i")) %>%
  select(lhs, rhs, est.std = std.all, p.direct = pvalue)

# Efectos indirectos (los que tú definiste con :=)
indirect <- est_women %>%
  filter(op == ":=", grepl("ind_", lhs)) %>%
  mutate(
    outcome = case_when(
      grepl("punish", lhs) ~ "punish_f"
    ),
    predictor = case_when(
      grepl("external", lhs) ~ "external_i",
      grepl("internal", lhs) ~ "internal_i"
    )
  ) %>%
  select(lhs, outcome, predictor, est.indirect = est, p.indirect = pvalue, std.indirect = std.all, se, ci.lower, ci.upper)

# Unir por outcome y predictor
effects_total_women <- left_join(direct,
                               indirect,
                               by = c("lhs" = "outcome", "rhs" = "predictor")) %>%
  mutate(total = est.std + std.indirect)

#### SEM for each country ####
paises <- c("Argentina", "Chile", "Colombia", "Mexico", "Spain")

controls1 <- c("age", "edu_dic", "ses", "po", "hig_ide", "mid_ide", "low_ide",
               sex_vars, income_vars)

control_formula1 <- paste(controls1, collapse = " + ")

control_block1 <- paste0(
  "greedy_f ~ ", control_formula1, "\n",
  "greedy_f ~ ", control_formula1, "\n"
)

model1 <- paste(model_base, control_block1)

## Man


model_sin_sexOther <- gsub("sexOther \\+ ", "", model1)
model_sin_sexOther <- gsub(" \\+ sexOther", "", model_sin_sexOther)
model_sin_sexOther <- gsub("sexOther", "", model_sin_sexOther)  # Por si quedó solo

modelos_por_pais <- list(
  "Argentina" = model_sin_sexOther,
  "Colombia"  = model_sin_sexOther,
  "Mexico"    = model_sin_sexOther,
  "Chile"     = model1,
  "Spain"     = model1
)

run_sem_country_man <- function(pais) {
  message("\n##### SEM ", pais, " #####")
  
  modelo_pais <- modelos_por_pais[[pais]]
  data_pais <- subset(df_man, country == pais)
  
  fit <- sem(modelo_pais, data = data_pais, estimator = "MLR")
  
  print(summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
  
  fit_meas <- fitmeasures(fit, c("chisq", "pvalue", "df", "cfi", "tli", 
                                 "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
  
  monteCarloCI(fit, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)
  
  params <- parameterEstimates(fit, standardized = TRUE) %>%
    filter(op %in% c("~", ":="))
  
  return(list(
    country = pais,
    fit = fit,
    fit_meas = fit_meas,
    parameters = params
  ))
}

resultados_sem_man <- map(paises, run_sem_country_man)

names(resultados_sem_man) <- paises


## Women

modelos_por_pais <- list(
  "Argentina" = model_sin_sexOther,
  "Colombia"  = model_sin_sexOther,
  "Mexico"    = model_sin_sexOther,
  "Chile"     = model1,
  "Spain"     = model_sin_sexOther
)

run_sem_country_women <- function(pais) {
  message("\n##### SEM ", pais, " #####")
  
  modelo_pais <- modelos_por_pais[[pais]]
  data_pais <- subset(df_women, country == pais)
  
  fit <- sem(modelo_pais, data = data_pais, estimator = "MLR")
  
  print(summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
  
  fit_meas <- fitmeasures(fit, c("chisq", "pvalue", "df", "cfi", "tli", 
                                 "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
  
  monteCarloCI(fit, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)
  
  params <- parameterEstimates(fit, standardized = TRUE) %>%
    filter(op %in% c("~", ":="))
  
  return(list(
    country = pais,
    fit = fit,
    fit_meas = fit_meas,
    parameters = params
  ))
}

resultados_sem_women <- map(paises, run_sem_country_women)
names(resultados_sem_women) <- paises

# 4. Save and export  ----------------------------------------------------------------

save(df_study3,
     fit_cfa_man,fit_cfa_women,
     resultados_cfa_man, resultados_cfa_women,
     fit_sem_man,fit_sem_women,
     est_man, effects_total_man,
     est_women, effects_total_women,
     resultados_sem_man, resultados_sem_women,
     file = here("input/data/proc/all_models.RData"))

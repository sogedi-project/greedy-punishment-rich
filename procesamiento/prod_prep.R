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
               sjlabelled,
               naniar,
               data.table,
               psych,
               rstatix)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(url("https://github.com/sogedi-project/sogedi-data/raw/refs/heads/main/output/data/db_proc.RData"))

glimpse(db_proc)

# 3. Processing --------------------------------------------------------------

db_proc_lab <- db_proc

db_proc <- sjlabelled::remove_all_labels(db_proc)

db_proc <- sjlabelled::copy_labels(df_new = db_proc, df_origin = db_proc_lab)

# 3.1 Select ----

db_proc <- db_proc %>% 
  dplyr::select(ID, natio_recoded, age, sex, edu, ses, inc, n_perso, currency, 
                condi_class, condi_gender, ex_we_1, ex_we_2, in_we_1, in_we_2,
                greedy_1, greedy_2, greedy_3, punish_1, punish_2, punish_3,
                hig_ide, mid_ide, low_ide, po) %>% 
  as_tibble()

# 3.2 Filter ----

db_proc <- db_proc %>% 
  filter(natio_recoded %in% c(1, 3, 4, 9, 13) & age >= 18 & condi_class == 1)


# 3.3 Recode and transform ----

# country
frq(db_proc$natio_recoded)

db_proc$natio_recoded <- car::recode(db_proc$natio_recoded, 
                                     recodes = c("1 = 'Argentina';
                                                 3 = 'Chile';
                                                 4 = 'Colombia';
                                                 9 = 'Spain';
                                                 13 = 'Mexico'"), 
                                     as.factor = T)


# sex
frq(db_proc$sex)

db_proc$sex <- car::recode(db_proc$sex, recodes = c("2 = 'Male'; 1 = 'Female'; 0 = 'Other'"),
                                     as.factor = T)


# age
frq(db_proc$age)

db_proc$age <- sjlabelled::set_na(db_proc$age, na = c(999))

# ses
frq(db_proc$ses) #ok

# education
frq(db_proc$edu)

db_proc$edu_dic <- if_else(db_proc$edu > 4, "Universitary education or more", "Less than Universitary")
db_proc$edu_dic <- factor(db_proc$edu_dic, levels = c("Less than Universitary", "Universitary education or more"))

frq(db_proc$edu_dic)

# income
frq(db_proc$currency)
frq(db_proc$inc)
frq(db_proc$n_perso)

db_proc$n_perso <- if_else(db_proc$n_perso > 60, NA, db_proc$n_perso)

db_proc <- db_proc %>% 
  mutate(exchange_rates = case_when(natio_recoded == "Argentina" ~ 915.161,
                                    natio_recoded == "Chile" ~ 944.457,
                                    natio_recoded == "Colombia" ~ 4307.27,
                                    natio_recoded == "Mexico" ~ 20.06,
                                    natio_recoded == "Spain" ~ 0.924))

db_proc$equiv_income_local <- db_proc$inc / (1 + 0.5 * (db_proc$n_perso - 1))

db_proc$equiv_income_usd <- (db_proc$equiv_income_local / db_proc$exchange_rates)

db_proc <- db_proc %>% 
  group_by(natio_recoded) %>% 
  mutate(income_quintile = ntile(-desc(equiv_income_usd), n = 5)) %>% 
  ungroup()

db_proc$income_quintile <- factor(db_proc$income_quintile,
         levels = c(1, 2, 3, 4, 5),
         labels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5'))

frq(db_proc$income_quintile)


# class identity
db_proc %>% 
  select(hig_ide, mid_ide, low_ide) %>% 
  frq() # ok


# political orientarion
frq(db_proc$po)

# external attributions about wealth

db_proc %>% 
  select(ex_we_1, ex_we_2, natio_recoded) %>% 
  group_by(natio_recoded) %>% 
  frq() # ok

# internal attributions about wealth

db_proc %>% 
  select(in_we_1, in_we_2, natio_recoded) %>% 
  group_by(natio_recoded) %>% 
  frq() # ok


## greedy

db_proc %>% 
  select(greedy_1, greedy_2, greedy_3, natio_recoded) %>% 
  group_by(natio_recoded) %>% 
  frq() # ok

# punitive attitudes toward richs

db_proc %>% 
  select(punish_1, punish_2, punish_3, natio_recoded) %>% 
  group_by(natio_recoded) %>% 
  frq()

# reliability (all countries)

## external attributions
db_proc %>% 
  select(ex_we_1, ex_we_2) %>% 
  mutate_all(~as.numeric(.)) %>% 
  psych::alpha() # 0.72

## internal attributions
db_proc %>% 
  select(in_we_1, in_we_2) %>% 
  mutate_all(~as.numeric(.)) %>% 
  psych::alpha() # 0.78

## greedy
db_proc %>% 
  select(greedy_1, greedy_2, greedy_3) %>% 
  mutate_all(~as.numeric(.)) %>% 
  psych::alpha() # 0.74

## punitive attitudes
db_proc %>% 
  select(punish_1, punish_2, punish_3) %>% 
  mutate_all(~as.numeric(.)) %>% 
  psych::alpha() # 0.73

# compute
db_proc$internal_i <- rowMeans(db_proc[, c("ex_we_1", "ex_we_2")], na.rm = TRUE)

db_proc$external_i <- rowMeans(db_proc[, c("in_we_1", "in_we_2")], na.rm = TRUE)

db_proc$greedy_i <- rowMeans(db_proc[, c("greedy_1", "greedy_2", "greedy_3")], na.rm = TRUE)

db_proc$punish_i <- rowMeans(db_proc[, c("punish_1", "punish_2", "punish_3")], na.rm = TRUE)

# 3.4 Missings values ----

colSums(is.na(db_proc))

n_miss(db_proc)

prop_miss(db_proc)*100

miss_var_summary(db_proc)

miss_var_table(db_proc)

vis_miss(db_proc) + theme(axis.text.x = element_text(angle=80))

db_proc <- na.omit(db_proc)

# 3.5 Labels ----

db_proc$natio_recoded <- sjlabelled::set_label(db_proc$natio_recoded, "Country")
db_proc$age <- sjlabelled::set_label(db_proc$age, "Age")
db_proc$sex <- sjlabelled::set_label(db_proc$sex, "Gender")
db_proc$edu <- sjlabelled::set_label(db_proc$edu, "Educational level")
db_proc$edu_dic <- sjlabelled::set_label(db_proc$edu_dic, "Universitary education")
db_proc$ses <- sjlabelled::set_label(db_proc$ses, "Subjective social status")
db_proc$income_quintile <- sjlabelled::set_label(db_proc$income_quintile, "Household equivalent income quintile per capita")
db_proc$po <- sjlabelled::set_label(db_proc$po, "Political identification")
db_proc$hig_ide <- sjlabelled::set_label(db_proc$hig_ide, "High SES identification")
db_proc$mid_ide <- sjlabelled::set_label(db_proc$mid_ide, "Middle SES identification")
db_proc$low_ide <- sjlabelled::set_label(db_proc$low_ide, "Low SES identification")

db_proc$ex_we_1 <- sjlabelled::set_label(db_proc$ex_we_1, "Having the right contacts and connections")
db_proc$ex_we_2 <- sjlabelled::set_label(db_proc$ex_we_2, "Having better opportunities by being born into a wealthy family")
db_proc$external_i <- sjlabelled::set_label(db_proc$external_i, "External attributions about wealth")

db_proc$in_we_1 <- sjlabelled::set_label(db_proc$in_we_1, "Personal ambition and motivation for self-improvement")
db_proc$in_we_2 <- sjlabelled::set_label(db_proc$in_we_2, "Working hard and being persistent")
db_proc$internal_i <- sjlabelled::set_label(db_proc$internal_i, "Internal attributions about wealth")

db_proc$greedy_1 <- sjlabelled::set_label(db_proc$greedy_1, "Rich women/men believe you can never have too much money")
db_proc$greedy_2 <- sjlabelled::set_label(db_proc$greedy_2, "Rich women/men are somewhat greedy and stingy")
db_proc$greedy_3 <- sjlabelled::set_label(db_proc$greedy_3, "Rich women/men prefer to spend their money on themselves rather than on others")
db_proc$greedy_i <- sjlabelled::set_label(db_proc$greedy_i, "Perceived greed")

db_proc$punish_1 <- sjlabelled::set_label(db_proc$punish_1, "Rich women/men who evade taxes should face harsher penalties than ordinary citizens")
db_proc$punish_2 <- sjlabelled::set_label(db_proc$punish_2, "Society should adopt stricter policies to prevent rich women/men from avoiding taxes")
db_proc$punish_3 <- sjlabelled::set_label(db_proc$punish_3, "Rich women/men should return any illegitimate earnings obtained through tax fraud or financial misconduct")
db_proc$punish_i <- sjlabelled::set_label(db_proc$punish_i, "Punitive attitudes toward the rich")

# 4. Save and export  ----------------------------------------------------------------

df_study3 <- db_proc %>% 
  select(ID, country = natio_recoded, age, sex, ses, edu, edu_dic, 
         income_quintile, equiv_income_usd, po, hig_ide, mid_ide, low_ide,
         condi_gender, 
         ex_we_1, ex_we_2, external_i, in_we_1, in_we_2, internal_i,
         greedy_1, greedy_2, greedy_3, greedy_i, 
         punish_1, punish_2, punish_3, punish_i
         )

save(df_study3, file = here("input/data/proc/df_study3.RData"))

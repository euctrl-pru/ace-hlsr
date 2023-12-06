## libraries
library(tidyverse)
library("data.table")
library(dplyr)
library(readxl)
library(knitr)
library(kableExtra)
library(htmltools)
library(janitor)
library(here)

## data source
source(here("data_source.R"))

## import data
data_raw  <-  read_xlsx(
  paste0(data_folder, data_file),
  # here("data",data_file),
  sheet = "F_Revenue",
  range = cell_limits(c(9, 1), c(NA, 4))) %>%
  as_tibble() %>% 
  rename(TYPE = 'Data', TOTAL = 'Grand Total') %>% 
  mutate(across(TYPE, str_replace, 'Sum of ', '')) %>% 
  filter( TYPE != "REVE_DELEGATION")

#sum rows
data_temp <- data_raw %>% filter(TYPE != "REVE_REVENUE") %>% 
  adorn_totals("row") %>% 
  mutate(TYPE = case_when(TYPE == 'Total' ~ 'REVE_REVENUE', TRUE ~ TYPE))

data_temp <- data_temp %>% select(-TYPE)
rownames(data_temp) <- data_raw$TYPE

new_reve_other <- t(colSums(rbind (data_temp['REVE_OTHER',], data_temp['REVE_EXCEPTIONAL',]), na.rm=TRUE))
data_temp['REVE_OTHER',] <- new_reve_other
data_temp$TYPE <- data_raw$TYPE

#totals for percentages
ert_reve_total <- data_temp[nrow(data_temp),'ERT'] %>% pull()
trm_reve_total <- data_temp[nrow(data_temp),'TRM'] %>% pull()

# prepare data for table
data_table <- data_temp %>% 
  mutate_at(vars(-TYPE), ~ as.numeric(.) /1000000) %>% 
  filter(TYPE != 'REVE_DELEGATION', TYPE != 'REVE_EXCEPTIONAL', TYPE != 'REVE_DELEGATION') %>% 
  mutate(
    ERT_F = if_else(
      TYPE == 'REVE_AIRPORT','n.a.',
      as.character(if_else(ERT<1.5,
                           format(round(ERT,1), nsmall =1 ,big.mark= " "),
                           format(round(ERT,0), nsmall =0 ,big.mark= " "))
      )),
    TRM_F = as.character(if_else(TRM<1.5,
                                 format(round(TRM,1), nsmall =1 ,big.mark= " "),
                                 format(round(TRM,0), nsmall =0 ,big.mark= " "))
    ),
    ERT_PERC = if_else(
      TYPE == 'REVE_AIRPORT','n.a.',
      as.character(paste0(round(ERT/ert_reve_total*100000000,
                                if_else(ERT/ert_reve_total*100000000 <0.5,2,1)),
                          "%"))
    ),
    TRM_PERC = as.character(paste0(round(TRM/trm_reve_total*100000000,
                                         if_else(TRM/trm_reve_total*100000000 <0.5,2,1)),
                                   "%")),
  ) %>% 
  mutate(LABEL = case_when(
    TYPE == 'REVE_CHARGE' ~ "Income from charges",
    TYPE == 'REVE_AIRPORT' ~ "Income from airport operators",
    TYPE == 'REVE_MILITARY' ~ "Income from the military",
    TYPE == 'REVE_EXEMPT_FLT' ~ "Income in respect of exempted flights",
    TYPE == 'REVE_DOMESTIC' ~ "Income from domestic goverment", 
    TYPE == 'REVE_FINANCIAL' ~ "Financial income",
    TYPE == 'REVE_OTHER' ~ "Other income (incl. exceptional revenue item)",
    TYPE == 'REVE_REVENUE' ~ ""
  )
  )

# Italicizing the 'Remark' column

data_table <- data_table %>%
  mutate(
    # Remark = cell_spec(Remark, "latex", bold = TRUE),
    ERT_F = cell_spec(ERT_F, "latex", align = "r"),
    ERT_PERC = cell_spec(ERT_PERC, "latex", align = "r"),
    TRM_PERC = cell_spec(TRM_PERC, "latex", align = "r"),
    TRM_F = cell_spec(TRM_F, "latex", align = "r"),
    LABEL = cell_spec(LABEL, "latex", align = "c")
  ) %>% 
  select(ERT_F,ERT_PERC, LABEL, TRM_PERC, TRM_F) %>% 
  rename('En-route'= ERT_F, 
         '%'= ERT_PERC, 
         'Gate-togate revenues (\u20AC M)'= LABEL, 
         ' %' =  TRM_PERC,
         'Terminal' = TRM_F)


##plot table
kable(data_table, escape = F, booktabs=T, format = "markdown") %>%
  kable_styling(latex_options="striped") 
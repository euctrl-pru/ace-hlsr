## libraries
library(tidyverse)
library("data.table")
library(dplyr)
library(readxl)
library(gt)
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
  )%>% 
  select(ERT_F,ERT_PERC, LABEL, TRM_PERC, TRM_F) %>% 
  rename('En-route'= ERT_F, 
         '%'= ERT_PERC, 
         'Gate-togate revenues (\u20AC M)'= LABEL, 
         ' %' =  TRM_PERC,
         'Terminal' = TRM_F)


##plot table
table_reve <- gt(
  data_table) %>% 
  tab_options(
    table.width = '100%',
    column_labels.background.color = '#538DD5',
    column_labels.vlines.color = "#EAEAEA", # don't know why this doesn't work
    column_labels.vlines.width = '1px',
    column_labels.border.top.color = "#EAEAEA",
    column_labels.border.top.width = '1px',
    table.border.top.width = '1px',
    table.border.bottom.width = '1px',
    data_row.padding = '3px') %>% 
  cols_align(
    align = "right",
    columns = c(1:2, 4:5)
  ) %>% 
  cols_align(
    align = "center",
    columns = 3
  ) %>% 
  tab_style(
    style = cell_borders(sides = c("right", "left"), color = "#EAEAEA", style = "solid", weight = px(1)),
    locations = cells_body()
  ) %>% 
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "white", style = "solid", weight = px(1)),
    locations = cells_body()
  ) %>% 
  tab_style(
    style = list(cell_fill(color = "#C0C0C0")),
    locations = cells_body(columns = everything(), rows = nrow(data_table))
  ) %>% 
  tab_style(
    style = list(cell_text(size = '0.95rem')),
    locations = cells_body(columns = everything(), rows = everything())
  )

table_reve

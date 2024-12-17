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
cost_data  <-  read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder,data_file ),
  sheet = "F_Cost breakdown",
  range = cell_limits(c(9, 1), c(NA, 4))) %>%
  as_tibble() %>% 
  rename(COST_TYPE = 'Data', COST = 'Grand Total') %>% 
  mutate(across(COST_TYPE, str_replace, 'Sum of ', ''))


atco_data  <-  read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder,data_file ),
  sheet = "F_Cost breakdown",
  range = cell_limits(c(9, 12), c(NA, 15))) %>%
  as_tibble() %>% 
  select(COST_TYPE, ERT, TRM, COST)

# add row with totals
cost_data <- cost_data %>% adorn_totals("row")


#totals for percentages
ert_cost_total <- cost_data[nrow(cost_data),'ERT'] %>% pull() 
trm_cost_total <- cost_data[nrow(cost_data),'TRM'] %>% pull()
g2g_cost_total <- cost_data[nrow(cost_data),'COST'] %>% pull()

## prepare data for COST pie

c_bdown_data <- rbind(cost_data, atco_data)

c_bdown_data <- c_bdown_data %>% 
  mutate_at(vars(-COST_TYPE), ~ . /1000000) %>% 
  mutate( COST_TYPE = factor(COST_TYPE, levels = c(
    'COST_STAFF', 
    'COST_ATCO_OPS', 
    'OTHER_STAFF_COST',
    'COST_OPERAT',
    'COST_DEPRECIATION',
    'COST_CAPITAL',
    'COST_EXCEPTIONAL',
    'Total')
  )
  ) %>% 
  arrange(COST_TYPE)


# prepare data for table
data_table <- c_bdown_data %>% 
  mutate(
    ERT_F = format(round(ERT,0), nsmall =0 ,big.mark= " "),
    TRM_F = format(round(TRM,0), nsmall =0 ,big.mark= " "),
    G2G_F = format(round(COST,0), nsmall =0 ,big.mark= " "),
    ERT_PERC = case_when(
      COST_TYPE == 'COST_ATCO_OPS' ~ 'n.a.',
      COST_TYPE == 'OTHER_STAFF_COST' ~ 'n.a.',
      TRUE ~  paste0(format(round(ERT/ert_cost_total*100000000,1), nsmall =1 ,big.mark= " "), "%")
    ),
    TRM_PERC = case_when(
      COST_TYPE == 'COST_ATCO_OPS' ~ 'n.a.',
      COST_TYPE == 'OTHER_STAFF_COST' ~ 'n.a.',
      TRUE ~  paste0(format(round(TRM/trm_cost_total*100000000,1), nsmall =1 ,big.mark= " "), "%")
    ),
    G2G_PERC = case_when(
      COST_TYPE == 'COST_ATCO_OPS' ~ 'n.a.',
      COST_TYPE == 'OTHER_STAFF_COST' ~ 'n.a.',
      TRUE ~  paste0(format(round(COST/g2g_cost_total*100000000,1), nsmall =1 ,big.mark= " "), "%")
    )
  ) %>% 
  mutate(LABEL = case_when(
    COST_TYPE == 'COST_STAFF' ~ "Staff costs",
    COST_TYPE == 'COST_ATCO_OPS' ~ paste0("ATCOs in OPS employment costs"),
    COST_TYPE == 'OTHER_STAFF_COST' ~ paste0("Other staff employment costs", "    "),
    COST_TYPE == 'COST_OPERAT' ~ "Non-staff operating costs",
    COST_TYPE == 'COST_DEPRECIATION' ~ "Depreciation costs",
    COST_TYPE == 'COST_CAPITAL' ~ "Cost of capital",
    COST_TYPE == 'COST_EXCEPTIONAL' ~ "Exceptional items",
    COST_TYPE == 'Total' ~ "Total ATM/CNS provision costs"
  )
  )%>%
  select(LABEL, ERT_F,ERT_PERC, TRM_F, TRM_PERC, G2G_F, G2G_PERC) %>% 
  rename (" " = LABEL, 
          "€ M" = ERT_F, 
          "%" =  ERT_PERC, 
          " € M" = TRM_F, 
          " %" = TRM_PERC, 
          "  € M" = G2G_F, 
          "  %" = G2G_PERC)


##plot table
t <- gt(
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
  tab_spanner(label = "En-route", columns = c(2:3)) %>% 
  tab_spanner(label = "Terminal", columns = c(4:5)) %>% 
  tab_spanner(label = "Gate-to-gate", columns = c(6:7)) %>% 
  cols_align(
    align = "right",
    columns = c(2:7)
  ) %>% 
  cols_align(
    align = "left",
    columns = 1
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
    style = list(cell_text(style = "italic",
                           align = 'right')),
    locations = cells_body(columns = everything(), rows = c(2:3))
  ) %>% 
  tab_style(
    style = list(cell_text(size = '0.95rem')),
    locations = cells_body(columns = everything(), rows = everything())
  ) %>% 
  cols_width(
    1 ~ pct(40) 
  )

t

## libraries
library(tidyr)
library("data.table")
library(dplyr)
library(readxl)
library(gt)
# library(webshot2)
library(htmltools)
library(here)

## data source
source(here("data_source.R"))

## import ANSP table
ansp  <- read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder, data_file),
  sheet = "Status",
  range = cell_limits(c(8, 1), c(NA, 3))) %>%
  as_tibble() %>% 
  filter(ANSP_NAME != 'UkSATSE')%>%
  replace(is.na(.), 0)

#parameters passed via qmd
if (exists("checkmark") == FALSE) {checkmark = "\u2714\ufe0f"}

## add checkmark if submission reviewed
ansp_checkmark <- ansp %>% 
  mutate(ANSP_NAME = paste0(ANSP_NAME, 
                            if_else(status == 1, paste0(" ", checkmark) ,""),
                            "\n(", country, ")")) %>% 
  select(-status,-country)

## split column 
# from https://stackoverflow.com/questions/61961687/unstacking-data-frame-by-columns-in-r
ansp_split <-ansp_checkmark %>% 
  mutate(mygroup = ceiling(row_number()/8)) %>%  
  mutate(rn = rowid(mygroup)) %>% 
  pivot_wider(names_from = c(mygroup), values_from = c(ANSP_NAME))%>%
  select(-rn) %>% 
  mutate_all(~ replace_na(., "")) %>% 
  rename(" " = 1, "  " = 2, "   " = 3, "    " = 4, "     " = 5) # for the pdf


##plot table
table_ansp_sub <- gt(
  ansp_split) %>% 
  tab_options(
    table.width = '100%',
    column_labels.hidden = TRUE, # for some reason this doesn't work in the pdf.
    # column_labels.font.size = '1px',
    # column_labels.padding = '0px',
    # column_labels.border.top.color = 'white',
    table.font.size = '12px',
    footnotes.border.bottom.style = 'hidden',
    footnotes.padding = if_else(checkmark=='√', '15px', '5px')) %>%  #tried this for the pdf but doesnt work
  tab_style(
    style = cell_borders(
      # style = 'hidden',
      sides = c("top", "bottom"),
      color = "white"
    ),
    locations = cells_body()
  ) %>% 
  tab_footnote(
    paste0(checkmark, " ", "Data submission has been reviewed"),
    locations = NULL,
    placement = c("left")
  )

table_ansp_sub

# gtsave(table_ansp_sub, 'table_ansp_sub.png') #doesn't work because cannot open chrome

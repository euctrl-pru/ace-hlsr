## libraries
library(tidyr)
library("data.table")
library(dplyr)
library(readxl)
library(reactable)
library(htmltools)
library(here)

## data source
source(here("data_source.R"))

## import ANSP table
ansp  <- read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder, data_file),
  sheet = "Status",
  range = cell_limits(c(8, 1), c(NA, 2))) %>%
  as_tibble() %>% 
  filter(ANSP_NAME != 'UkSATSE')%>%
  replace(is.na(.), 0)

## add checkmark if submission reviewed
ansp_checkmark <- ansp %>% 
  mutate(ANSP_NAME = paste0(ANSP_NAME, if_else(status == 1, " \u2714\ufe0f",""))) %>% 
  select(-status)

## split column 
# from https://stackoverflow.com/questions/61961687/unstacking-data-frame-by-columns-in-r
ansp_split <-ansp_checkmark %>% 
  mutate(mygroup = ceiling(row_number()/8)) %>%  
  mutate(rn = rowid(mygroup)) %>% 
  pivot_wider(names_from = c(mygroup), values_from = c(ANSP_NAME))%>%
  select(-rn)

## find out number of rows.. we need it later for table formatting
ansp_rows <- nrow(ansp_split)

##plot table
table_ansp_sub <- reactable(
  ansp_split,
  bordered = FALSE,
  # defaultPageSize = 43,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(style=list("font-size" = "12px", "white-space"= "wrap"),
                         headerStyle = list(display = "none"),
                         # footerStyle = list(fontWeight = "bold", display = "block"),
                         html = TRUE
  ),
  columns = list(
    '1' = colDef(minWidth = 20),
    '2' = colDef(minWidth = 18),
    '3' = colDef(minWidth = 18),
    '4' = colDef(minWidth = 26),
    '5' = colDef(minWidth = 18)
  ),
  borderless = TRUE
  ,
  rowStyle = JS(paste0("function(rowInfo) {
    if (rowInfo.index == 0) {
      return { borderTop: '2px solid #3F557A' }
    } else if (rowInfo.index == ",ansp_rows-1,") {
      return { borderBottom: '2px solid #3F557A' }
    } else {
      return { borderTop: '1px solid transparent' }
    }
  }")
  )
)

table_ansp_sub

## libraries
library(dplyr)
library(tidyverse)
library(readxl)
library(plotly)
library(htmltools)
library(magick)
library(here)
library(webshot)
library(htmlwidgets)
# library(RSelenium)
library(here)
# library(ggrepel)

## data source
source(here("data_source.R"))

## import data
pie_staff_data_all  <-  read_xlsx(
                                  paste0(data_folder, data_file),
                                  # here("data", data_file ),
                                  sheet = "F_Staff",
                                  range = cell_limits(c(10, 1), c(NA, 3))) %>%
  as_tibble() %>% 
  rename(STAF_TYPE = 'Data', STAF = 'Total') %>% 
  mutate(across(STAF_TYPE, str_replace, 'Sum of ', ''))

## prepare data for pie
pie_staff_data <- pie_staff_data_all %>% 
  filter(YEAR_DATA == year_report) %>% 
  select(-YEAR_DATA) %>% 
  filter(grepl('STAF_', STAF_TYPE)) %>% 
  filter(!grepl('COST_', STAF_TYPE))

## labels and colurs

pie_staff_data <- pie_staff_data %>% 
  mutate(LABEL = case_when(
    STAF_TYPE == 'STAF_ATCO' ~ "ATCOs in OPS",
    STAF_TYPE == 'STAF_ATCO_OTHER' ~ "ATCOs on other duties",
    STAF_TYPE == 'STAF_AB_INITIO' ~ "Ab-initio trainees",
    STAF_TYPE == 'STAF_TRAINEE' ~ "On-the-job trainees",
    STAF_TYPE == 'STAF_ATC_ASSISTANT' ~ "ATC assistants",
    STAF_TYPE == 'STAF_OPS_SUPPORT' ~ "OPS-Support",
    STAF_TYPE == 'STAF_TECH_OPERAT' ~ "Technical support\nfor maintenance",
    STAF_TYPE == 'STAF_TECH_PLANNING' ~ "Technical support\nfor planning & development",
    STAF_TYPE == 'STAF_ADMIN' ~ "Admin.",
    STAF_TYPE == 'STAF_ANCILLARY' ~ "Ancillary services",
    STAF_TYPE == 'STAF_OTHER' ~ "Other"
  ),
  MYCOLOR = case_when(
    STAF_TYPE == 'STAF_ATCO' ~ "#003366",
    STAF_TYPE == 'STAF_ATCO_OTHER' ~ "#D4E9FB",
    STAF_TYPE == 'STAF_AB_INITIO' ~ "#A9D3F7",
    STAF_TYPE == 'STAF_TRAINEE' ~ "#7FBCF2",
    STAF_TYPE == 'STAF_ATC_ASSISTANT' ~ "#EDF6A0",
    STAF_TYPE == 'STAF_OPS_SUPPORT' ~ "#D9DDB3",
    STAF_TYPE == 'STAF_TECH_OPERAT' ~ "#C6CC8D",
    STAF_TYPE == 'STAF_TECH_PLANNING' ~ "#D1E616",
    STAF_TYPE == 'STAF_ADMIN' ~ "#8B9A0E",
    STAF_TYPE == 'STAF_ANCILLARY' ~ "#747A37",
    STAF_TYPE == 'STAF_OTHER' ~ "#4D5225"
  )
  ) %>% 
  mutate(LABEL =  paste0(LABEL,
                         "\n",
                         format(round(STAF/sum(STAF)*100,1), nsmall =1 ,big.mark= " "),
                         "%")
  ) %>% 
  mutate(
  STAF_TYPE = factor(STAF_TYPE, levels = c(
    'STAF_ATCO',
    'STAF_OTHER',                  
    'STAF_ANCILLARY',
    'STAF_ADMIN',
    'STAF_TECH_PLANNING', 
    'STAF_TECH_OPERAT',
    'STAF_OPS_SUPPORT',
    'STAF_ATC_ASSISTANT',
    'STAF_TRAINEE',
    'STAF_AB_INITIO',
    'STAF_ATCO_OTHER'
    ))) %>% 
  arrange(STAF_TYPE)

# parameters for initial slice
atcos_ops <- pie_staff_data %>% select(STAF_TYPE,STAF) %>% 
  filter(STAF_TYPE == 'STAF_ATCO') %>% pull() 

total_staf <- pie_staff_data %>%
  summarise(sum(STAF))%>% pull()

start_point_staf <- 90-atcos_ops/total_staf*180

domain_staff_x0 <- 0
domain_staff_x1 <- 0.45

mytext = 10

# plot piechart
pie_staff <- function(mytext){
  pie_staff_data %>% 
  plot_ly(
    labels = ~LABEL, values = ~STAF, type = 'pie',
    hoverinfo = "none",
    sort = FALSE,
    textinfo='label',
    textfont = list(size = mytext),
    marker = list(colors = ~MYCOLOR),
    # line = list(color = '#FFFFFF', width = 1)),
    domain = list(x = c(domain_staff_x0, domain_staff_x1), y = c(0, 1)),  #domain controls the position in the subplot
    automargin = TRUE,
    pull = c(0.05,0,0,0,0,0,0,0,0,0,0),
    rotation = start_point_staf
  ) %>% 
  layout(
    showlegend = FALSE
  ) %>% 
  config(
    responsive = FALSE,
    displaylogo = FALSE,
    displayModeBar = F
  )
}
## prepare data for pie

pie_atco_data <- pie_staff_data_all %>% 
  filter(YEAR_DATA == year_report)%>% 
  filter(STAF_TYPE == "ACC_ATCO_NB" | STAF_TYPE == "APP_ATCO_NB")

## labels and colurs

pie_atco_data <- pie_atco_data %>% 
  mutate(LABEL = case_when(
    STAF_TYPE == 'ACC_ATCO_NB' ~ "ACC ATCOs in OPS",
    STAF_TYPE == 'APP_ATCO_NB' ~ "APPs + TWRs\nATCOs in OPS"
  ),
  MYCOLOR = case_when(
    STAF_TYPE == 'ACC_ATCO_NB' ~ "#125AA2",
    STAF_TYPE == 'APP_ATCO_NB' ~ "#2787E7"
  )
  )

domain_atco_x0 <-0.57
domain_atco_x1 <-0.88

# plot piechart
pie_atco <- function(mytext) {
  pie_atco_data %>% 
  plot_ly(
    labels = ~LABEL, values = ~STAF, type = 'pie',
    hoverinfo = "none",
    textinfo='label+percent',
    textfont = list(size = mytext),
    insidetextorientation='horizontal',
    marker = list(colors = ~MYCOLOR),
    domain = list(x = c(domain_atco_x0, domain_atco_x1), y = c(0, 1)),
    automargin = TRUE,
    rotation = 90
  ) %>% 
  layout(
    showlegend = FALSE
  ) %>% 
  config(
    responsive = FALSE,
    displaylogo = FALSE,
    displayModeBar = F
  )
}
  
# finally we don't use the lines. Too difficult to control in the html page
lines <- list (
  list(
    type = "line",
    line = list(color = "black", dash = "dash"),
    x0 = 0.34, 
    y0= 0.5+sin(atcos_ops/total_staf*pi)/2,
    x1 = 0.62, y1 = 0.91,
    xref = "paper",
    yref = "paper"
  ),
  list(
    type = "line",
    line = list(color = "black", dash = "dash"),
    x0 = 0.34, y0= 0.5-sin(atcos_ops/total_staf*pi)/2,
    x1 = 0.62, y1 = 0.09,
    xref = "paper",
    yref = "paper"
  ))

image_folder <- here("images")
# arrow_right <- image_read(paste0(image_folder,"/long_right_arrow.svg"))
 
myimages <- list(
  list(
    source = base64enc::dataURI(file = paste0(image_folder,"/long_right_arrow.png")),
    # source =raster2uri(as.raster(arrow_right)), #https://plotly-r.com/embedding-images.html
       x = (domain_staff_x1+domain_atco_x0)/2, y = 0.55,
       sizex = 0.15, sizey = 0.15,
       xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "center"
  )
)




if (knitr::is_latex_output()) {
  fig_pdf <- subplot(pie_staff(30), pie_atco(30)) %>%
    layout(
      height = 900, width = 1984,
      images = myimages)
  
  fig_pdf
  
} else {
  fig <- subplot(pie_staff(10), pie_atco(10)) %>%
   layout(images = myimages)


  fig
}


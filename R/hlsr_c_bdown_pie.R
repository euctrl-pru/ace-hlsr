## libraries
library(dplyr)
library(tidyverse)
library(readxl)
library(plotly)
library(htmltools)
library(magick)
library(here)
library(webshot)

## data source
source(here("data_source.R"))

## import data
pie_cost_data  <- read_xlsx(
                            # paste0(data_folder, data_file),
                              paste0(data_folder,data_file),
                             sheet = "F_Cost breakdown",
                             range = cell_limits(c(9, 1), c(NA, 4))) %>%
  as_tibble() %>% 
  rename(COST_TYPE = 'Data', COST = 'Grand Total') %>% 
  mutate(across(COST_TYPE, str_replace, 'Sum of ', ''))

## prepare data for COST pie

pie_cost_data <- pie_cost_data %>% 
  select(COST_TYPE, COST)  %>% 
  mutate_at(vars(-COST_TYPE), ~ . /1000000) 

## labels and colurs

pie_cost_data <- pie_cost_data %>% 
  mutate(LABEL = case_when(
    COST_TYPE == 'COST_STAFF' ~ "Staff costs",
    COST_TYPE == 'COST_OPERAT' ~ "Non-staff\noperating costs",
    COST_TYPE == 'COST_DEPRECIATION' ~ "Depreciation\ncosts",
    COST_TYPE == 'COST_CAPITAL' ~ "Cost of capital",
    COST_TYPE == 'COST_EXCEPTIONAL' ~ "Exceptional\nitems",
  ),
  MYCOLOR = case_when(
    COST_TYPE == 'COST_STAFF' ~ "#003366",
    COST_TYPE == 'COST_OPERAT' ~ "#78B4F0",
    COST_TYPE == 'COST_DEPRECIATION' ~ "#9AA349",
    COST_TYPE == 'COST_CAPITAL' ~ "#E1F060",
    COST_TYPE == 'COST_EXCEPTIONAL' ~ "#E0584F",
  )
  )

# total for percentages
total_cost <- pie_cost_data %>%
  summarise(sum(COST))%>% pull()

# labels with percentages
pie_cost_data <- pie_cost_data %>% 
  mutate(COST_PER = COST/total_cost*100) %>% 
  mutate(LABEL =  paste0(LABEL,
                         "\n",
                         format(round(COST_PER,1), nsmall =1 ,big.mark= " "),
                         "%")
  )

# parameters for initial slice
staff_cost <- pie_cost_data %>% select(COST_TYPE,COST) %>% 
  filter(COST_TYPE == 'COST_STAFF') %>% pull() 

start_point_cost <- 90-staff_cost/total_cost*180
domain_cost_x1 <- 0.45

# plot piechart
pie_cost <- function(myfont, mytext) {
pie_cost_data %>% 
  plot_ly(
    labels = ~LABEL, values = ~COST, type = 'pie',
    hoverinfo = "none",
    # textinfo='label',
    texttemplate = mytext,
    textfont = list(size = myfont),
    marker = list(colors = ~MYCOLOR),
    # line = list(color = '#FFFFFF', width = 1)),
    domain = list(x = c(0, domain_cost_x1), y = c(0, 1)),
    automargin = TRUE,
    rotation = start_point_cost
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
  
## import data
pie_atco_data  <-   read_xlsx(
                              # paste0(data_folder, data_file),
                              paste0(data_folder,data_file ),
                              sheet = "F_Cost breakdown",
                              range = cell_limits(c(9, 12), c(NA, 13))) %>%
                              as_tibble() 

## prepare data for COST pie

pie_atco_data <- pie_atco_data %>% 
  select(COST_TYPE, COST)  %>% 
  mutate_at(vars(-COST_TYPE), ~ . /1000000) 

## labels and colurs

pie_atco_data <- pie_atco_data %>% 
  mutate(LABEL = case_when(
    COST_TYPE == 'COST_ATCO_OPS' ~ "ATCOs in OPS\nemployment costs",
    COST_TYPE == 'OTHER_STAFF_COST' ~ "Other staff\nemployment costs"
  ),
  MYCOLOR = case_when(
    COST_TYPE == 'COST_ATCO_OPS' ~ "#E0584F",
    COST_TYPE == 'OTHER_STAFF_COST' ~ "#003366"
  )
  )

# plot piechart

domain_atco_x0 <- 0.6
domain_atco_x1 <- 0.95

pie_atco <- function(myfont, mytext){
  pie_atco_data %>% 
  plot_ly(
    labels = ~LABEL, values = ~COST, type = 'pie',
    hoverinfo = "none",
    # textinfo='label+percent',
    texttemplate = mytext,
    textfont = list(size = myfont),
    insidetextorientation='horizontal',
    marker = list(colors = ~MYCOLOR),
    # line = list(color = '#FFFFFF', width = 1)),
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
    x0 = 0.27, y0= 1,
    x1 = (domain_atco_x1+domain_atco_x0)/2, y1 = 0.93,
    xref = "paper",
    yref = "paper"
  ),
  list(
    type = "line",
    line = list(color = "black", dash = "dash"),
    x0 = 0.27, y0= 0.0,
    x1 = (domain_atco_x1+domain_atco_x0)/2, y1 = 0.07,
    xref = "paper",
    yref = "paper"
  ))

myannotations <- function(myfont, myvertical) {
list(
  x = 0.45,
  y = myvertical,
  text = paste0("<b>", 
                "Total ATM/CNS provision cost: ", 
                "\u20AC ",
                format(round(total_cost,0), big.mark = " "),
                " M</b>"),
  xref = "paper",
  yref = "paper",
  xanchor = "center",
  showarrow = FALSE,
  font = list(color = "black",
              size = myfont)
)
}

image_folder <- here("images")
# arrow_right <- image_read(paste0(image_folder,"/long_right_arrow.svg")) #magick
# rsvg_svg(paste0(image_folder,"/long_right_arrow.svg"), 
         # paste0(image_folder,"/long_right_arrow-cairo.svg")) # needed only once
# arrow_right <- readPicture(paste0(image_folder,"/long_right_arrow-cairo.svg")) #grImport2
# grid.picture(arrow_right)


myimages <- list(
  list(
    source = base64enc::dataURI(file = paste0(image_folder,"/long_right_arrow.png")),
     # source =raster2uri(as.raster(arrow_right)), #https://plotly-r.com/embedding-images.html
       x = (domain_cost_x1 + domain_atco_x0)/2, y = 0.55,
       sizex = 0.15, sizey = 0.15,
       xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "center"
  )
)

fig <- subplot(pie_cost(10, '%{label}'), pie_atco(10, '%{label}</br>%{percent}')) %>%
layout(annotations = myannotations(12, -0.15), images = myimages)

# fig <- subplot(pie_cost, pie_atco)

fig

fig_pdf <- subplot(pie_cost(14, '<b>%{label}</b>'), pie_atco(14, '<b>%{label}</br>%{percent}</b>')) %>%
  layout(annotations = myannotations(18, 0), images = myimages)

# export to image
# the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS

fig_dir <- 'figures/'

invisible(export(fig_pdf, paste0(fig_dir,"figure-2-3-hlsr_c_bdown_pie.png")))


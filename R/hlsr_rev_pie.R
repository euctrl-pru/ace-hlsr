
## libraries
library(dplyr)
library(tidyverse)
library("data.table")
library(readxl)
library(plotly)
library(htmltools)
library(magick)
library(here)
library(webshot)
# library(ggrepel)
## data source
source(here("data_source.R"))

## import data
pie_data  <-  read_xlsx(
  paste0(data_folder, data_file),
  # here("data", data_file ),
  sheet = "F_Revenue",
                        range = cell_limits(c(9, 1), c(NA, 4))) %>%
  as_tibble() %>% 
  rename(TYPE = 'Data', TOTAL = 'Grand Total') %>% 
  mutate(across(TYPE, str_replace, 'Sum of ', '')
  )

## prepare data for central pie

pie_er_trm_data <- pie_data %>% 
  filter(TYPE != "REVE_REVENUE", TYPE != "REVE_DELEGATION") %>%
  select(ERT, TRM) %>% 
  mutate_all(~ . /1000000) %>% 
  summarise(ERT = sum(ERT), TRM = sum(TRM)) %>% 
  pivot_longer(c(ERT,TRM),names_to = "TYPE", values_to = "REVE") %>% 
  mutate(LABELS = case_when(
    TYPE == 'ERT' ~ paste0("En-route"),
    TYPE == 'TRM' ~ paste0("Terminal")
  )
  )

# totals for percentages
trm_reve <- pie_er_trm_data %>%
  filter(TYPE == 'TRM') %>% 
  select(REVE) %>% pull() 

ert_reve <- pie_er_trm_data %>%
  filter(TYPE == 'ERT') %>% 
  select(REVE) %>% pull()

total_reve <- trm_reve+ert_reve

# parameters for initial slice
start_point_er_trm <- 90+trm_reve/total_reve*180
er_trm_domain_x0 <- 0.35
er_trm_domain_x1 <- 0.65

# plot piechart

pie_er_trm <- function(mytext){
  
  pie_er_trm_data %>% 
  plot_ly(
    labels = ~LABELS, 
    values = ~REVE, 
    type = 'pie',
    hoverinfo = "none",
    textinfo='label+percent',
    textfont = list(size = mytext),
    marker = list(colors = c("#003366", "#E0584F"),
                  line = list(color = '#FFFFFF', width = 1)),
    rotation = start_point_er_trm,
    pull = c(0,0.05),
    domain = list(x = c(er_trm_domain_x0, er_trm_domain_x1), y = c(0, 1))
    
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
  
  
#transpose
pie_data_t <- transpose(pie_data)
colnames(pie_data_t) <- pie_data$TYPE
rownames(pie_data_t) <- colnames(pie_data)

## process and sum other reve
pie_data_temp <- pie_data_t %>% filter(row_number()!=1)  %>% 
  mutate_all( ~ as.numeric(.) /1000000) %>% 
  mutate(REVE_OTHER = REVE_OTHER + REVE_EXCEPTIONAL) %>%
  select( -REVE_REVENUE, -REVE_DELEGATION, -REVE_EXCEPTIONAL) %>% 
  select(REVE_CHARGE, REVE_OTHER, REVE_FINANCIAL, REVE_DOMESTIC, REVE_EXEMPT_FLT,
         REVE_MILITARY, REVE_AIRPORT) 

# transpose back
pie_data_pivot <- transpose(pie_data_temp)
colnames(pie_data_pivot) <- rownames(pie_data_temp)
pie_data_pivot$CONCEPT <- colnames(pie_data_temp)

# labels, label positions and colours
pie_data_pivot <- pie_data_pivot  %>%  
  mutate(ERT_PERC = ERT/ert_reve,
         TRM_PERC = TRM/trm_reve,
         TOTAL_PERC = TOTAL/total_reve) %>% 
  mutate(LABEL = case_when(
    CONCEPT == 'REVE_CHARGE' ~ "Charges",
    CONCEPT == 'REVE_AIRPORT' ~ "\t\nAirport\noperators",
    CONCEPT == 'REVE_MILITARY' ~ "\t\nMilitary",
    CONCEPT == 'REVE_EXEMPT_FLT' ~ "\t\nExempted\nflights",
    CONCEPT == 'REVE_DOMESTIC' ~ "\t\nDomestic\ngovernment", 
    CONCEPT == 'REVE_FINANCIAL' ~ "\t\nFinancial",
    CONCEPT == 'REVE_OTHER' ~ "\t\nOther income"
  ),
  MYCOLOR_TRM = case_when(
    CONCEPT == 'REVE_CHARGE' ~ "#811D17",
    CONCEPT == 'REVE_AIRPORT' ~ "#C12C22",
    CONCEPT == 'REVE_MILITARY' ~ "#C64D45",
    CONCEPT == 'REVE_EXEMPT_FLT' ~ "#EC9B95",
    CONCEPT == 'REVE_DOMESTIC' ~ "#F3BCB9", 
    CONCEPT == 'REVE_FINANCIAL' ~ "#F9DEDC",
    CONCEPT == 'REVE_OTHER' ~ "#D9D9D9"
  ),
  MYCOLOR_ER = case_when(
    CONCEPT == 'REVE_CHARGE' ~ "#125AA2",
    CONCEPT == 'REVE_AIRPORT' ~ "#C12C22",
    CONCEPT == 'REVE_MILITARY' ~ "#6A9FD5",
    CONCEPT == 'REVE_EXEMPT_FLT' ~ "#0A85FF",
    CONCEPT == 'REVE_DOMESTIC' ~ "#5CADFF", 
    CONCEPT == 'REVE_FINANCIAL' ~ "#ADD6FF",
    CONCEPT == 'REVE_OTHER' ~ "#D9D9D9"
  )
  )
#filter ERT data and redo label for pie
pie_er_data <- pie_data_pivot %>% 
  select(ERT, CONCEPT, LABEL,MYCOLOR_ER, ERT_PERC) %>% 
  mutate(LABEL = paste0(LABEL,
                        "\n",
                        round(ERT_PERC*100,if_else(ERT_PERC*100 <0.5,2,1)),
                        "%"))

# parameters for initial slice
er_charge_reve <- pie_er_data %>%
  filter(CONCEPT == 'REVE_CHARGE') %>%
  select(ERT) %>% pull()

start_point_er <- 270+(ert_reve-er_charge_reve)/ert_reve*180
er_domain_x0 <- 0
er_domain_x1 <- 0.27

# plot piechart
pie_er <- function(mytext){
  pie_er_data %>% 
  filter(CONCEPT != "REVE_AIRPORT") %>% 
  plot_ly(
    labels = ~LABEL, values = ~ERT, type = 'pie',
    hoverinfo = "none",
    textinfo='label',
    textfont = list(size = mytext),
    insidetextorientation='horizontal',
    marker = list(colors = ~MYCOLOR_ER),
    rotation = start_point_er,
    sort = FALSE,
    automargin = TRUE,
    domain = list(x = c(er_domain_x0, er_domain_x1), y = c(0, 1))
  ) %>% 
  layout(
    margin = list(l = 0),
    showlegend = FALSE
  ) %>% 
  config(
    responsive = FALSE,
    displaylogo = FALSE,
    displayModeBar = F
  )
}
  
## prepare data for terminal pie

#filter data for pie
pie_trm_data <- pie_data_pivot %>% 
  select(TRM, CONCEPT, LABEL,MYCOLOR_TRM, TRM_PERC) %>% 
  mutate(LABEL = paste0(LABEL,
                        "\n",
                        round(TRM_PERC*100,if_else(TRM_PERC*100 <0.5,2,1)),
                        "%"))


# parameters for initial slice
trm_charge_reve <- pie_trm_data %>%
  filter(CONCEPT == 'REVE_CHARGE') %>%
  select(TRM) %>% pull()

start_point_trm <- 45+(trm_reve-trm_charge_reve)/trm_reve*180
trm_domain_x0 <- 0.73
trm_domain_x1 <- 1

# plot piechart
pie_trm <- function(mytext){
  pie_trm_data %>% 
  plot_ly(
    labels = ~LABEL, values = ~TRM, type = 'pie',
    hoverinfo = "none",
    textinfo='label',
    textfont = list(size = mytext),
    insidetextorientation='horizontal',
    marker = list(colors = ~MYCOLOR_TRM),
    rotation = start_point_trm,
    automargin = TRUE,
    sort = FALSE,
    domain = list(x = c(trm_domain_x0, trm_domain_x1), y = c(0, 1))
  ) %>% 
  layout(
    margin = list(r = 0, l =0),
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
    x0 = 0.61, y0= 0.78,
    x1 = 0.81, y1 = 0.87,
    xref = "paper",
    yref = "paper"
  ),
  list(
    type = "line",
    line = list(color = "black", dash = "dash"),
    x0 = 0.61, y0= 0.22,
    x1 = 0.81, y1 = 0.13,
    xref = "paper",
    yref = "paper"
  ),
  list(
    type = "line",
    line = list(color = "black", dash = "dash"),
    x0 = 0.21, y0= 0.85,
    x1 = 0.45, y1 = 0.92,
    xref = "paper",
    yref = "paper"
  ),
  list(
    type = "line",
    line = list(color = "black", dash = "dash"),
    x0 = 0.21, y0= 0.15,
    x1 = 0.45, y1 = 0.08,
    xref = "paper",
    yref = "paper"
  )  )


myannotations <- function(mytext, myvert){
  list(
  list(
    x = 0.15,
    y = myvert,
    text = paste0("<b>", 
                  "Total en-route\nrevenue: ", 
                  "\u20AC ",
                  format(round(ert_reve,0), big.mark = " "),
                  " M</b>"),
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    showarrow = FALSE,
    font = list(color = "black",
                size=mytext)
  ),
  list(
    x = 0.5,
    y = myvert,
    text = paste0("<b>", 
                  "Gate-to-gate\nrevenue: ", 
                  "\u20AC ",
                  format(round(total_reve,0), big.mark = " "),
                  " M</b>"),
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    showarrow = FALSE,
    font = list(color = "black",
                size=mytext)
  ),
  list(
    x = 0.85,
    y = myvert,
    text = paste0("<b>", 
                  "Total terminal\nrevenue: ", 
                  "\u20AC ",
                  format(round(trm_reve,0), big.mark = " "),
                  " M</b>"),
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    showarrow = FALSE,
    font = list(color = "black",
                size=mytext)
  )
)
}
  
image_folder <- here("images")
# arrow_left <- image_read(paste0(image_folder,"/long_left_arrow.svg"))
# arrow_right <- image_read(paste0(image_folder,"/long_right_arrow.svg"))

myimages <- list(
  list(
      source = base64enc::dataURI(file = paste0(image_folder,"/long_right_arrow.png")),
    # source =raster2uri(as.raster(arrow_left)), #https://plotly-r.com/embedding-images.html
       x = (er_domain_x1+er_trm_domain_x0)/2, y = 0.55,
       sizex = 0.18, sizey = 0.18,
       xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "center"
  ),
  list(
    source = base64enc::dataURI(file = paste0(image_folder,"/long_left_arrow.png")),
    # source =raster2uri(as.raster(arrow_right)), #https://plotly-r.com/embedding-images.html
       x = (er_trm_domain_x1+trm_domain_x0)/2, y = 0.55,
       sizex = 0.18, sizey = 0.18,
       xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "center"
  )
)


fig <- subplot(pie_er(10), pie_er_trm(10), pie_trm(10)) %>%
  layout(annotations = myannotations(11, -0.3), images = myimages)


fig


fig_pdf <-  subplot(pie_er(14), pie_er_trm(14), pie_trm(14)) %>%
  layout(annotations = myannotations(18, -0.05), images = myimages)


# export to image
# the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS

fig_dir <- 'figures/'

invisible(export(fig_pdf, paste0(fig_dir,"figure-2-1-hlsr_rev_pie.png")))
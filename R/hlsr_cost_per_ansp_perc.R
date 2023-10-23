## libraries
library(dplyr)
library(stringr)
library(readxl)
library(plotly)
library(here)
## data source
source(here("data_source.R"))

## import data
data_raw <- read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder,data_file ),
  sheet = "F_Costs",
  range = cell_limits(c(7, 1), c(NA, 3))) %>%
  as_tibble() %>% 
  rename(COST=3)

## process data for plot
data_plot <- data_raw %>% 
  group_by(ANSP_NAME) %>% arrange(YEAR_DATA) %>% 
  mutate(YOY = COST/lag(COST)-1) %>% 
  filter(YEAR_DATA == max(YEAR_DATA)) %>% 
  arrange(desc(COST))  
  

#calculate range for x axis and max year
# cost_max <- round((max(data_plot$COST/10^6)+500)/100,0)*100
perc_max <- max(abs(data_plot$YOY)) + 0.4
year_max <- max(data_plot$YEAR_DATA)

# draw cost variation plot
p2 <- data_plot %>% 
  plot_ly(
    # width = 500, 
    # height = 750,
    x = ~ YOY,
    y = ~ factor(ANSP_NAME, levels = rev(data_plot$ANSP_NAME)),
    marker = list(color =('#95B3D7')),
    text = ~ if_else(YOY>=0, paste0("+",format(round(YOY*100,1), nsmall =1), "%"),
                     paste0(format(round(YOY*100,1), nsmall = 1), "%")),
    textangle = 0,
    textposition = "outside",
    # insidetextanchor =  "start",
    textfont = list(color = '#88A9D2'),
    cliponaxis = FALSE,
    type = "bar",
    orientation = 'h',
    hoverinfo = "none",
    showlegend = F
  ) %>% 
  layout(
    title = list(
      text = paste0("Changes ",year_max-1,"-",year_max," (in %)"), 
      font = list(color = '#747a7f', size = 12),
      y = 0.02),
    xaxis = list(
      title = "",
      # fixedrange = TRUE,
      range = c(-perc_max, perc_max),
      # automargin = T,
      # tickvals = c(),
      # tickformat=",.0%",
      showticklabels = FALSE,
      # autotick = T, 
      zeroline = T, showline = T, mirror = T,
      linecolor='#BFBFBF',
      zerolinecolor='#BFBFBF',
      # domain=c(0.6,1),
      showgrid = F
    ),
    yaxis = list(
      title = "",
      linewidth=1, linecolor='#BFBFBF',
      # titlefont   = list(size = 13),
      fixedrange = TRUE,
      autotick = F,
      showticklabels = FALSE,
      gridcolor = '#BFBFBF',
      tickson="boundaries",
      # domain=c(0,1),
      zeroline = F, showline = F, showgrid = T
    ),
    bargap = 0.4,
    autosize = T,
    margin = list(l=0, r=0),
    plot_bgcolor = 'white',
    uniformtext=list(minsize=10, mode='show')
  ) %>%
  config(responsive = FALSE,
         displaylogo = FALSE,
         displayModeBar = F)

p2


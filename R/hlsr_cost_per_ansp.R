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
  paste0(data_folder, data_file),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "F_Costs",
  range = cell_limits(c(7, 1), c(NA, 3))) %>%
  as_tibble() %>% 
  rename(COST=3)

## process data for plot
data_plot <- data_raw %>% 
  mutate(across('ANSP_NAME', str_replace, 'Continental', 'Cont.')) %>% 
  group_by(ANSP_NAME) %>% arrange(YEAR_DATA) %>% 
  mutate(YOY = COST/lag(COST)-1) %>% 
  filter(YEAR_DATA == max(YEAR_DATA)) %>% 
  arrange(desc(COST))  
  

#calculate range for x axis and max year
cost_max <- round((max(data_plot$COST/10^6)+500)/100,0)*100
# perc_max <- max(abs(data_plot$YOY)) + 0.4
year_max <- max(data_plot$YEAR_DATA)

# draw costs plot
p1 <- data_plot %>% 
  plot_ly(
    # width = 500, 
    # height = 750,
    x = ~ COST/10^6,
    y = ~ paste0(ANSP_NAME, "  "),
    marker = list(color =('#4F81BD')),
    text = ~ format(round(COST/10^6,0), big.mark = " "),
    textangle = 0,
    textposition = "outside",
    # insidetextanchor =  "start",
    textfont = list(color = 'black'),
    cliponaxis = FALSE,
    type = "bar",
    orientation = 'h',
    hoverinfo = "none",
    showlegend = F
  ) %>% 
  layout(
    title = list(
      text = paste0("Total ATM/CNS provision costs in ", year_max ," (M€",year_max,")"), 
      font = list(color = '#747a7f', size = 12),
      y = 0),
    xaxis = list(
      title = "",
      # fixedrange = TRUE,
      range = c(0, cost_max),
      # automargin = T,
      # tickvals = c(),
      tickangle = 0,
      autotick = T, zeroline = F,
      # domain=c(0,0.6),
      showgrid = F
    ),
    yaxis = list(
      title = "",
      linewidth=1, linecolor='#BFBFBF',
      # titlefont   = list(size = 13),
      fixedrange = TRUE,
      ticks = 'outside',
      tickson="boundaries",
      tickcolor='#BFBFBF', ticklen=3,
      # tickformat=",.0%", 
      categoryorder = "total ascending",
      # domain=c(0,1),
      zeroline = F, showline = T, showgrid = F
    ),
    autosize = T,
    bargap = 0.4,
    plot_bgcolor = '#DCE6F2',
    uniformtext=list(minsize=10, mode='show')
  ) %>%
  config(responsive = FALSE,
         displaylogo = FALSE,
         displayModeBar = F)

p1

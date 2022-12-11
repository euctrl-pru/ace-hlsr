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
  range = cell_limits(c(7, 6), c(NA, 7))) %>%
  as_tibble() %>% 
  rename(COST=2)

## process data for plot
data_plot <- data_raw %>% 
  mutate(COST = COST/10^9) %>% 
  arrange(YEAR_DATA) %>% 
  filter(YEAR_DATA >= year_report-5, YEAR_DATA <= year_report) %>% 
  mutate(YOY = COST/lag(COST)-1)

#calculate min and max for y axis
cost_max <- round((max(data_plot$COST)+0.5)*2,0)/2
cost_min <- round((min(data_plot$COST)-0.5)*2,0)/2


##vertical line function
vline <- function(x = 0, y=0) {
  list(
    type = "line",
    y0 = cost_min,
    y1 = y,
    yref = "y",
    x0 = x,
    x1 = x,
    # xref = "paper",
    line = list(color = '#808080', dash="dot", width = 1)
  )
}



# draw costs plot
p1 <- data_plot %>% 
  plot_ly(
    # width = 500, 
    # height = 750,
    x = ~ YEAR_DATA,
    y = ~ COST,
    type = 'scatter', mode = 'lines', 
    line = list(width = 4), 
    hoverinfo = "none",
    showlegend = F
  ) %>% 
  add_trace(
    inherit = FALSE,
    x = ~ YEAR_DATA,
    y = ~ COST+0.07,
    text = ~ if_else(YOY>=0, paste0("+",round(YOY*100,1), "%"),
                     paste0(round(YOY*100,1), "%")),
    textangle = 0,
    textposition = "top center",
    # insidetextanchor =  "start",
    textfont = list(color = 'black'),
    cliponaxis = FALSE,
    mode = 'text',
    hoverinfo = "none",
    showlegend = F
  ) %>% 
  layout(
    xaxis = list(
      title = "",
      # fixedrange = TRUE,
      # range = c(0, cost_max),
      # automargin = T,
      # tickvals = c(),
      autotick = F, zeroline = F, showline = T,
      # domain=c(0,0.6),
      showgrid = F
    ),
    yaxis = list(
      title = paste0("Billions €", year_report),
      linewidth=1, linecolor='black',
      # titlefont   = list(size = 13),
      fixedrange = TRUE,
      ticks = 'outside',
      range = c(cost_min,cost_max),
      # tickson="boundaries",
      # tickcolor='#BFBFBF', ticklen=3,
      tickformat=".1f",
      # categoryorder = "total ascending",
      # domain=c(0,1),
      zeroline = F, showline = T, showgrid = F
    ),
    shapes = list(vline(year_report-5, pull(data_plot[1,'COST'])),
                  vline(year_report-4, pull(data_plot[2,'COST'])),
                  vline(year_report-3, pull(data_plot[3,'COST'])),
                  vline(year_report-2, pull(data_plot[4,'COST'])),
                  vline(year_report-1, pull(data_plot[5,'COST'])),
                  vline(year_report, pull(data_plot[6,'COST']))
                  ),
    autosize = T,
    # plot_bgcolor = '#DCE6F2',
    uniformtext=list(minsize=10, mode='show')
  ) %>%
  config(responsive = FALSE,
         displaylogo = FALSE,
         displayModeBar = F)


p1



## libraries
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(plotly)
library(here)
library(webshot)
library(janitor)
library(magick)
## data source
source(here("data_source.R"))

## import data
data_raw <- read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder,data_file ),
  sheet = "F_Revenue_evo",
  range = cell_limits(c(9, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()


## process data for plot
data_plot <- data_raw %>% 
  arrange(year_data) %>% 
  filter(year_data >= year_report-5, year_data <= year_report) %>% 
  mutate(
    grand_total = reve_revenue - reve_delegation,
    value = grand_total/10^9,
    dif_py = grand_total / lag(grand_total, 1) -1,
    text_label = if_else(is.na(dif_py) == TRUE, NA_character_,
                         paste0('<b>',if_else(dif_py >= 0,"+", ""), format(round(dif_py * 1000, 0)/10, nsmall = 1, trim = TRUE), "%</b>"))
    ) 

#calculate min and max for y axis
value_max <- ceiling((max(data_plot$value))/2)*2 
# cost_min <- round((min(data_plot$COST)-0.5)*2,0)/2

# draw costs plot
p <- function(myfont, mywidth, myheight) {
  data_plot %>% 
  plot_ly(
    width = mywidth,
    height = myheight,
    x = ~ year_data,
    y = ~ value,
    type = "bar",
    marker = list(color = '#E0584F'),
    text = ~ text_label,
    name = "Gate-to-gate ANS revenues",
    textangle = 0,
    textposition = "outside",
    textfont = list(color = 'black', size= if_else(myfont <20, myfont, myfont+2)),
    hovertemplate = paste('%{y:.1f} B€2023'),
    showlegend = F
  ) %>% 
  layout(
    bargap = 0.4,
    legend = list(orientation = 'h',
                  font = list(size = myfont),
                  y = -0.15,
                  x = 0.5,
                  xanchor = "center",
                  bgcolor = 'transparent'),
    xaxis = list(
      title = "",
      # ticks = 'outside',
      range = c(min(data_plot$year_data) - 0.5, max(data_plot$year_data) + 0.5),
      # tick0 = min(data_plot$year_data),
      tickfont = list(size = myfont),
      # fixedrange = TRUE,
      # automargin = T,
      # tickvals = c(),
      autotick = F, zeroline = F, showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = paste0("Billion €2023"),
      linewidth = 1, linecolor='black',
      titlefont   = list(size = myfont),
      fixedrange = TRUE,
      tickfont = list(size = myfont),
      ticks = 'outside',
      dtick = 2,
      range = c(0, value_max + 0.1), #so the last line is plotted
      showgrid = TRUE,
      # tickson="boundaries",
      # tickcolor='#BFBFBF', ticklen=3,
      tickformat=".0f",
      # categoryorder = "total ascending",
      zeroline = F, showline = T, showgrid = F
    ),
   autosize = T,
    hovermode = "x unified",
    uniformtext=list(minsize=myfont, mode='show')
  ) %>%
  config(responsive = FALSE,
         displaylogo = FALSE,
         displayModeBar = F)

}

p(12, NULL, NULL)




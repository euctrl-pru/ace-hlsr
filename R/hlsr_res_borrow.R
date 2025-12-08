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
  sheet = "F_CapResBor",
  range = cell_limits(c(9, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()


## process data for plot
data_plot <- data_raw %>% 
  arrange(year_data) %>% 
  filter(year_data >= year_report-5, year_data <= year_report) %>% 
  mutate(
    borrow = (liab_longt_borrow + liab_curr_borrow) / 10^9,
    value = liab_capital_reserve/10^9,
    dif_py_1 = value / lag(value, 1) -1,
    dif_py_2 = borrow / lag(borrow, 1) -1,
    text_label_1 = if_else(is.na(dif_py_1) == TRUE, NA_character_,
                         paste0('<i>',if_else(dif_py_1 >= 0,"+", ""), round(dif_py_1 * 100, 0), "%</i>")),
    text_label_2 = if_else(is.na(dif_py_2) == TRUE, NA_character_,
                           paste0('<i>',if_else(dif_py_2 >= 0,"+", ""), round(dif_py_2 * 100, 0), "%</i>"))
    
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
    marker = list(color = '#003366'),
    text = ~ text_label_1,
    name = "Capital & reserves",
    textangle = -90,
    textposition = "outside",
    textfont = list(color = '#003366', size= if_else(myfont <20, myfont, myfont+2)),
    hovertemplate = paste('%{y:.1f} B€', year_report),
    showlegend = T
  ) %>% 
    add_trace(
      y = ~ borrow,
      marker = list(color = '#2990EA'),
      name = "Borrowings",
      text = ~ text_label_2,
      textangle = -90,
      textposition = "outside",
      textfont = list(color = '#2990EA', size= if_else(myfont <20, myfont, myfont+2)),
      showlegend = T  
    ) %>% 
  layout(
    barmode = 'stak',
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
      title = paste0("Billion €", year_report),
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




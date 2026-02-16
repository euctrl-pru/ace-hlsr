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
  sheet = "F_Capex",
  range = cell_limits(c(9, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()


## process data for plot
data_plot <- data_raw %>% 
  arrange(year_data) %>% 
  filter(year_data >= year_report-5, year_data <= year_report) %>% 
  mutate(
    value = total/10^9,
    dif_py = total / lag(total, 1) -1,
    text_label = if_else(is.na(dif_py) == TRUE, NA_character_,
                         paste0('<i>',if_else(dif_py >= 0,"+", ""), round(dif_py * 100, 0), "%</i>"))
    ) 

#calculate min and max for y axis
value_max <- ceiling((max(data_plot$value))*10)/10
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
    marker = list(color = '#9AA349'),
    text = ~ text_label,
    name = "Capex",
    textangle = 0,
    textposition = "outside",
    textfont = list(color = '#003366', size= if_else(myfont <20, myfont, myfont+2)),
    hovertemplate = paste('%{y:.1f} B€', year_report),
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
      title = paste0("Capital expenditures (billion €", year_report, ")"),
      linewidth = 1, linecolor='black',
      titlefont   = list(size = myfont),
      fixedrange = TRUE,
      tickfont = list(size = myfont),
      ticks = 'outside',
      dtick = 0.3,
      range = c(0, value_max + 0.21), #so the last line is plotted
      showgrid = TRUE,
      # tickson="boundaries",
      # tickcolor='#BFBFBF', ticklen=3,
      tickformat=".1f",
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


if (knitr::is_latex_output()) {
  p(13, NULL, NULL)
  
  } else {
  p(12, NULL, NULL)
  
}


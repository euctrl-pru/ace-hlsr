## libraries
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(plotly)
library(here)
library(webshot)
library(magick)
## data source
source(here("data_source.R"))

## import data
data_raw <- read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder,data_file ),
  sheet = "current_ratio",
  range = cell_limits(c(5, 2), c(NA, NA))) %>%
  as_tibble() %>% 
  pivot_longer(!year, names_to = "year_data", values_to = "value") %>% 
  rename (type = 1)


## process data for plot
data_plot <- data_raw %>% 
  arrange(year_data) %>% 
  filter(year_data >= year_report-5, year_data <= year_report) %>% 
  mutate_at(c(2), ~as.numeric(.)) %>% 
  mutate(value_text = case_when(type == "3rd quartile" ~ value + 0.15,
                                .default = value - 0.15))

#calculate min and max for y axis
value_max <- ceiling((max(data_plot$value)/2))*2
# cost_min <- round((min(data_plot$COST)-0.5)*2,0)/2

# draw costs plot
p <- function(myfont, mywidth, myheight) {
  data_plot %>% 
  plot_ly(
    width = mywidth,
    height = myheight,
    x = ~ year_data,
    y = ~ value,
    type = 'scatter', mode = 'lines', 
    color = ~ factor(type, levels = c("ANSP average",
                                      "1st quartile",
                                      "3rd quartile")
    ),
    colors = c('#003366', '#E0584F', '#9AA349'),
    line = list(width = 4), 
    hovertemplate = paste('%{y:.2f}'),
    showlegend = T
  ) %>% 
  add_trace(
    inherit = FALSE,
    x = ~ year_data,
    y = ~ value_text,
    text = ~ format(round(value, 2), nsmall=2),
    textangle = 0,
    textposition = ~ if_else(type == '3rd quartile', "top center", "bottom center"),
    # insidetextanchor =  "start",
    textfont = list(color = 'black', size = myfont +1),
    cliponaxis = FALSE,
    mode = 'text',
    hoverinfo = "none",
    showlegend = F
  ) %>%
  layout(
    legend = list(orientation = 'h',
                  font = list(size = myfont),
                  y = -0.15,
                  x = 0.5,
                  xanchor = "center",
                  bgcolor = 'transparent'),
    xaxis = list(
      title = "",
      ticks = 'outside',
      range = c(min(data_plot$year_data) - 0.5, max(data_plot$year_data) + 0.5),
      tick0 = min(data_plot$year_data),
      tickfont = list(size = myfont),
      # fixedrange = TRUE,
      # automargin = T,
      # tickvals = c(),
      autotick = F, zeroline = F, showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = paste0("Current assets / current liabilities ratio", "\n&nbsp;"),
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


# export to image
# the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS

fig_dir <- 'figures/'
image_name <- "figure-5-1-hlsr_current_ratio.png"

invisible(export(p(28, 1320, 700), paste0(fig_dir, image_name)))
# invisible(figure <- image_read(paste0(fig_dir,image_name)))
# invisible(cropped <- image_crop(figure, "754x520"))
# invisible(image_write(cropped, paste0(fig_dir,image_name)))

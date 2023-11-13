## libraries
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
  pivot_longer(!year, names_to = "ratio", values_to = "count")


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
    height = 230,
    x = ~ YEAR_DATA,
    y = ~ COST,
    type = 'scatter', mode = 'lines', 
    line = list(width = 4, color = "#003366"), 
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
      linewidth = 1, linecolor='black',
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


# export to image
# the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS

fig_dir <- 'figures/'

invisible(export(p1, paste0(fig_dir,"figure-2-5-1-hlsr_eco_ce.png")))
invisible(figure <- image_read(paste0(fig_dir,"figure-2-5-1-hlsr_eco_ce.png")))
invisible(cropped <- image_crop(figure, "0x230"))
invisible(image_write(cropped, paste0(fig_dir,"figure-2-5-1-hlsr_eco_ce.png")))

## libraries
library(dplyr)
library(stringr)
library(readxl)
library(plotly)
library(here)
library(magick)
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

# import ANSP countries
ansp  <- read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder, data_file),
  sheet = "Status",
  range = cell_limits(c(8, 1), c(NA, 3))) %>%
  as_tibble() %>% 
  replace(is.na(.), 0) %>% 
  select(-status)  

data_plot = merge(x=data_plot, y=ansp, by="ANSP_NAME")

data_plot <- data_plot %>% 
  mutate_at("ANSP_NAME", ~ str_replace (.,'Continental', 'Cont.')) 
  
#calculate range for x axis and max year
cost_max <- round((max(data_plot$COST/10^6)+500)/100,0)*100
# perc_max <- max(abs(data_plot$YOY)) + 0.4
year_max <- max(data_plot$YEAR_DATA)

# draw costs plot
p1 <- function(myfont, mywidth, myheight, vmargin, myautosize){
  data_plot %>% 
  plot_ly(
    width = mywidth,
    height = myheight,
    x = ~ COST/10^6,
    y = ~ paste0(ANSP_NAME, "  "),
    marker = list(color =('#003366')),
    text = ~ format(round(COST/10^6,0), big.mark = " "),
    textangle = 0,
    textposition = "outside",
    # insidetextanchor =  "start",
    textfont = list(color = 'black', size = myfont + 1),
    cliponaxis = FALSE,
    type = "bar",
    orientation = 'h',
    hoverinfo = "none",
    showlegend = F
  ) %>% 
    add_trace(
      type = 'bar',
      marker = list(color =('transparent')),
      text = ~ paste0(ANSP_NAME, " (", country, "): ", format(round(COST/10^6,0), big.mark = " ")),
      textfont = list(color = 'transparent', size = 1),
      name = "",
      hovertemplate = paste('%{text}<extra></extra>'), #extra stuff is to remove the name of the trace
      # hoverinfo = "none",
      showlegend = F
    ) %>%
    layout(
    title = list(
      text = paste0("Total ATM/CNS provision costs in ", year_max ," (M€",year_max,")"), 
      font = list(color = '#747a7f', size = myfont),
      y = 0.02),
    xaxis = list(
      title = "",
      # fixedrange = TRUE,
      range = c(0, cost_max),
      # automargin = T,
      # tickvals = c(),
      # tickangle = 0,
      # autotick = T, 
      zeroline = F,
      # domain=c(0,0.6),
      showticklabels = FALSE,
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
      tickfont = list(size = myfont),
      categoryorder = "total ascending",
      # domain=c(0,1),
      zeroline = F, showline = T, showgrid = F
    ),
    autosize = myautosize,
    margin = list(b = vmargin),
    bargap = 0.4,
    barmode = 'overlay',
    plot_bgcolor = '#DCE6F2',
    uniformtext=list(minsize=myfont, mode='show')
  ) %>%
  config(responsive = FALSE,
         displaylogo = FALSE,
         displayModeBar = F)
}
  


if (knitr::is_latex_output()) {
  p1(12, 490, 600, 30, 'T')
  
  } else{
  p1(12, NULL, NULL, 30, 'T')
  
}

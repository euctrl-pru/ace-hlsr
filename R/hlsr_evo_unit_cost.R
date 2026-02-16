
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

ace_graph_data <- read_xlsx(
                            paste0(data_folder, data_file),
                            # here("data", data_file ),
                            sheet = "F_Unit cost",
                            range = cell_limits(c(18, 2), c(24, NA))) %>%
  as_tibble() %>% 
  mutate_all(as.numeric)%>%
  rename(year_data=1)

p <- function(myfont, mywidth, myheight) {
  ace_graph_data %>%
  plot_ly(
    width = mywidth,
    height = myheight,
    x = ~ year_data,
    y = ~ costs_per_cph,
    yaxis = "y1",
    marker = list(color =('#78B4F0')),
    text = ~ paste("  <b>",round(costs_per_cph,0),"</b>"),
    textangle = -90,
    textposition = "inside",
    insidetextanchor =  "start",
    textfont = list(color = '#FFFFFF', size = myfont + 1),
    type = "bar",
    hoverinfo = "none",
    showlegend = F
  )%>%
  add_trace(
    inherit = FALSE,
    x = ~ year_data,
    y = ~ costs_per_cph+20,
    yaxis = "y1",
    # colors = c('#4F81BD'),
    mode = 'text',
    text =  if_else(is.na(ace_graph_data$costs_per_cph_change_perc) == TRUE, NA_character_,
      paste0("<b>",
                  if_else(ace_graph_data$costs_per_cph_change_perc >0, "+", ""),
                  format(round(ace_graph_data$costs_per_cph_change_perc*100,1), 1), "%","</b>")
      ),
    textfont = list(color = 'black', size = myfont),
    type = 'scatter',  mode = 'lines',
    hoverinfo = "none",
    showlegend = F
    
  )%>%
  add_trace(
    x = ~ year_data,
    y = ~ round(index_costs,1), 
    inherit = FALSE,
    yaxis = "y2",
    type = 'scatter',  mode = 'lines', name = 'ATM/CNS provision costs',
    line = list(color = "#003366"),
    hovertemplate = paste('<b>ATM/CNS costs index</b>: <br>%{y}',
                          "<extra></extra>",
                          sep = "")
  )%>%
  add_trace(
    x = ~ year_data,
    y = ~ round(index_cph,1), 
    inherit = FALSE,
    yaxis = "y2",
    type = 'scatter',  mode = 'lines', name = 'Composite flight-hours',
    line = list(color = "#E1F060"),
    hoverlabel=list(bgcolor="#E1F060",font=list(color='black')),
    hovertemplate = paste('<b>Composite flight-hours index</b>: <br>%{y}',
                          "<extra></extra>",
                          sep = "")
  )%>%
  layout(
    autosize = T,
    xaxis = list(
      title = "",
      fixedrange = TRUE,
      tickfont = list(size = myfont),
      # automargin = T,
      # tickvals = 2014:2019,
      autotick = F,
      showgrid = F
    ),
    
    yaxis = list(
      title = paste("\U20AC","per composite flight-hour"),
      titlefont   = list(size = myfont + 1),
      fixedrange = TRUE,
      tickfont = list(size = myfont),
      # tickformat=",.0%", ticks = 'outside',
      zeroline = T, showline = F, showgrid = T
    ),
    yaxis2 = list (
      overlaying = "y",
      side = "right",
      title = paste ("Index of costs and traffic", "<br>","(", min(ace_graph_data$year_data), " = 100)",sep = ""),
      titlefont = list(size = myfont +1 ),
      tickfont = list(size = myfont),
      range = list(20, 10+round(max(ace_graph_data$index_costs, ace_graph_data$index_cph)/10)*10),
      automargin = T,
      showgrid = F
    ),
    bargap = 0.4,
    legend = list(orientation = 'h', 
                  font = list(size = myfont),
                  xanchor = "left", 
                  x = -0.05, 
                  y = -0.12),
    # hovermode = "x unified",
    uniformtext=list(minsize=myfont, mode='show')
  ) %>%
  config(displaylogo = FALSE, modeBarButtons = list(list("toImage"))
  )
}


if (knitr::is_latex_output()) {
  p(12, 500, 300)
  
}else{
  
  p(12, NULL, 330)
  
}
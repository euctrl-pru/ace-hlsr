
## libraries
library(dplyr)
library(stringr)
library(readxl)
library(plotly)
library(stringr)
library(tidyr)
library(here)
## data source
source(here("data_source.R"))

# import data
data_raw <- read_xlsx(
            paste0(data_folder, data_file),
              # here("data","hlsr2021_data.xlsx"),
            sheet = "F_Support",
                      range = cell_limits(c(7, 1), c(NA, NA))) %>%
  as_tibble() 


# prepare data for main plot
data_prep <- data_raw  %>% 
  mutate(QUART1 = quantile(`Support costs per composite flight-hour`, 0.25),
         QUART3 = quantile(`Support costs per composite flight-hour`, 0.75),
         LABELS = format(round(`Support costs per composite flight-hour`,0), big.mark = " ")
  ) %>% 
  mutate(LABELS = if_else(`Support costs per composite flight-hour` >=1000, LABELS,
                          str_sub(LABELS, start= -nchar(LABELS)+1)) #to avoid the leading space created by format
  ) 

data_plot <- data_prep %>% 
  select(-c(`Support costs`, `Composite flight-hours`)) %>% 
  rename(`Employment costs (excl. ATCOs in OPS) per composite flight-hour` = 
           `Non ATCO employement cost per composite flight-hour`,
         `Non-staff operating costs per composite flight-hour` = 
           `Non-staff operating costs (excluding exceptional cost) per composite flight-hours`,
         `Exceptional costs per composite flight-hour` = 
           `Exceptional Cost per composite flight hour`) %>% 
  pivot_longer(!c(ANSP_NAME, LABELS, QUART1, QUART3, `Support costs per composite flight-hour`), names_to = "TYPE", values_to = "VALUE" )

# help table for labels and additional traces
data_help <- data_prep %>% 
  select(ANSP_NAME, LABELS, QUART1, QUART3, `Support costs per composite flight-hour`)

#prepare data for inset
data_inset <- data_plot %>% 
  filter(ANSP_NAME == 'DSNA'| ANSP_NAME == 'ENAIRE' | ANSP_NAME == 'DFS' |
           ANSP_NAME == 'ENAV' | ANSP_NAME == 'NATS (Continental)') %>% 
  mutate(ANSP_NAME = case_when(
    ANSP_NAME == 'NATS (Continental)' ~ 'NATS\n(Continental)',
    TRUE ~ ANSP_NAME)) 

# help table for labels and additional traces
data_help_inset <- data_help %>% 
  filter(ANSP_NAME == 'DSNA'| ANSP_NAME == 'ENAIRE' | ANSP_NAME == 'DFS' |
           ANSP_NAME == 'ENAV' | ANSP_NAME == 'NATS (Continental)') %>% 
  mutate(ANSP_NAME = case_when(
    ANSP_NAME == 'NATS (Continental)' ~ 'NATS\n(Continental)',
    TRUE ~ ANSP_NAME))%>% 
  mutate(LAB_COORD = 0)


# system average for annotation
sys_avg <- data_raw %>% summarise(sum(`Support costs`)/sum(`Composite flight-hours`)) %>% pull()

# plot

plot_all <- data_plot %>%
  plot_ly(
    x = ~ ANSP_NAME,
    y = ~ VALUE,
    yaxis = "y1",
    color = ~ factor(TYPE, levels = c("Employment costs (excl. ATCOs in OPS) per composite flight-hour",
                                      "Non-staff operating costs per composite flight-hour",
                                      "Capital-related costs per composite flight-hour",
                                      "Exceptional costs per composite flight-hour")
    ),
    colors = c('#000080', '#FF9900', '#C3D69B', '#993366'),
    # text = ~ LABELS,
    # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
    # textangle = -90,
    # textposition = "outside", cliponaxis = FALSE,
    # insidetextanchor =  "start",
    # textfont = list(color = 'black', size = 7),
    type = "bar", 
    hoverinfo = "none",
    showlegend = T
  ) %>%
  add_trace( data = data_help,
             inherit = FALSE,
             # marker = list(color =('transparent')),
             x = ~ ANSP_NAME,
             y = ~ `Support costs per composite flight-hour`,
             yaxis = "y1",
             mode = 'text',
             text = ~ LABELS,
             textfont = list(color = 'black', size = 7),
             # textangle = 0,
             textposition = "top center", cliponaxis = FALSE,
             type = 'scatter',  mode = 'lines',
             hoverinfo = "none",
             showlegend = F
  ) %>% 
  add_trace(data = data_help,
            inherit = FALSE,
            x = ~ ANSP_NAME,
            y = ~ QUART1,
            yaxis = "y1",
            # colors = c('#4F81BD'),
            type = 'scatter',  mode = 'lines',
            line = list(color = '#003366', width = 2, dash = 'dash'),
            opacity = 0.5,
            hoverinfo = "none",
            showlegend = F
  ) %>%
  add_trace(data = data_help,
            inherit = FALSE,
            x = ~ ANSP_NAME,
            y = ~ QUART3,
            yaxis = "y1",
            # color = c('#333399'),
            type = 'scatter',  mode = 'lines',
            line = list(color = '#003366', width = 2, dash = 'dash'),
            opacity = 0.5,
            hoverinfo = "none",
            showlegend = F
  ) %>% 
  config( responsive = FALSE,
          displaylogo = FALSE,
          displayModeBar = F
          # modeBarButtons = list(list("toImage"))
  )

plot_inset <- data_inset %>%
  plot_ly(
    x = ~ ANSP_NAME,
    y = ~ VALUE,
    yaxis = "y1",
    color = ~ factor(TYPE, levels = c("Employment costs (excl. ATCOs in OPS) per composite flight-hour",
                                      "Non-staff operating costs per composite flight-hour",
                                      "Capital-related costs per composite flight-hour",
                                      "Exceptional costs per composite flight-hour")
    ),
    colors = c('#000080', '#FF9900', '#C3D69B', '#993366'),
    # marker = list(color =('#8989FF')),
    text = " ", # for some reason I need to keep this to avoid labels in prev plot to autosize
    # cliponaxis = FALSE,
    # textangle = -90,
    # textposition = "inside",
    # insidetextanchor =  "start",
    # textfont = list(color = 'black', size = 9),
    type = "bar",
    hoverinfo = "none",
    showlegend = F
  ) %>% 
  add_trace(data = data_help_inset,
            inherit = FALSE,
            # marker = list(color =('transparent')),
            x = ~ ANSP_NAME,
            y = ~ `Support costs per composite flight-hour`,
            yaxis = "y1",
            mode = 'text',
            text = ~ LABELS,
            textfont = list(color = 'black', size = 10),
            # textangle = 0,
            textposition = "top center", cliponaxis = FALSE,
            type = 'scatter',  mode = 'lines',
            hoverinfo = "none",
            showlegend = F
  ) %>% 
  add_annotations (data = data_help_inset,
                   text = ~ ANSP_NAME,
                   x = ~ ANSP_NAME,
                   y = ~ LAB_COORD,      
                   showarrow = F,
                   xref = "x",
                   yref = "y",
                   yanchor = "top",
                   xanchor = "center",
                   align = "right",
                   textangle = -90,
                   font = list(color = 'black', size = 9)
  ) %>% 
  add_trace(data = data_help_inset,
            inherit = FALSE,
            x = ~ ANSP_NAME,
            y = ~ QUART1,
            yaxis = "y1",
            # colors = c('#4F81BD'),
            type = 'scatter',  mode = 'lines',
            line = list(color = '#003366', width = 2, dash = 'dash'),
            opacity = 0.5,
            hoverinfo = "none",
            showlegend = F
  ) %>%
  add_trace(data = data_help_inset,
            inherit = FALSE,
            x = ~ ANSP_NAME,
            y = ~ QUART3,
            yaxis = "y1",
            # color = c('#333399'),
            type = 'scatter',  mode = 'lines',
            line = list(color = '#003366', width = 2, dash = 'dash'),
            opacity = 0.5,
            hoverinfo = "none",
            showlegend = F
  ) %>%
  config(responsive = FALSE,
         displaylogo = FALSE,
         displayModeBar = F
         # modeBarButtons = list(list("toImage"))
  )

myannotations <- list(
  x = 0.1,
  y = 0.93,
  text = paste0("<b>", 
                "European system average: ", 
                "\u20AC ",
                format(round(sys_avg,0), big.mark = " "),
                "</b>"),
  xref = "paper",
  yref = "paper",
  xanchor = "left",
  showarrow = FALSE,
  font = list(color = "#1F497D",
              size=13)
)

# this is ugly but it's the only way i found for space as thousand sep for the y axis
# https://stackoverflow.com/questions/64024937/how-to-change-thousands-separator-for-blank-in-r-plotly

ticklabels1 <- seq(from=0, to=round(max(data_plot$`Support costs per composite flight-hour`+200)), by=200)
ticktexts1 <- c(0,format(ticklabels1[-1], big.mark = " "))

ticklabels2 <- seq(from=0, to=round(max(data_inset$`Support costs per composite flight-hour`+200)), by=200)
ticktexts2 <- c(0,format(ticklabels2[-1], big.mark = " "))

fig <- subplot(plot_all, plot_inset) %>% 
  layout( autosize = T, 
          uniformtext=list(minsize=8, mode='show'), #this is important so it does not autofit fonts
          bargap = 0.45,
          barmode = 'stack',
          title = list(text = "", font = list(color = "black", size = 14)),
          font = list(family = "Helvetica"),
          legend = list(orientation = 'h',
                        traceorder = 'reversed', #for some reason this does not work
                        font = list(size = 9),
                        y = -0.55,
                        x = -0.05,
                        bgcolor = 'transparent'),
          xaxis = list(title = "",
                       tickangle=270,
                       tickfont = list(size=11),
                       autotick = F,
                       # tick0=0.25,
                       fixedrange = TRUE,
                       showgrid = F,
                       categoryorder = "total descending",
                       domain=c(0,1)),
          yaxis = list(title = paste("\U20AC","per composite flight-hour"),
                       titlefont   = list(size = 12),
                       tickfont = list(size=11),
                       # dtick = 200,
                       tickvals = ticklabels1,
                       ticktext = ticktexts1,
                       # automargin = FALSE,
                       # margin = list(l=100),
                       fixedrange = TRUE,
                       linewidth=1, linecolor='transparent',  mirror = T,
                       # range = list(0, 200+round(max(data_plot$VALUE/1000), 1)*1000),
                       zeroline = T, showline = T, showgrid = F,
                       domain=c(0,1)),
          xaxis2 = list(title = "",
                        showticklabels = FALSE,
                        # tickangle=270,
                        # tickfont = list(size=10),
                        autotick = F,
                        fixedrange = TRUE,
                        showgrid = F,
                        categoryorder = "total descending",
                        domain=c(0.65,1)),
          yaxis2 = list(title = "",
                        # titlefont   = list(size = 13),
                        tickfont = list(size=10),
                        # dtick = 200,
                        tickvals = ticklabels2,
                        ticktext = ticktexts2,
                        fixedrange = TRUE,
                        # range = list(0, 200+round(max(data_inset$VALUE/1000), 1)*1000),
                        zeroline = T, showline = F, showgrid = F,
                        domain=c(0.45,1)),
          annotations = myannotations
  )

fig

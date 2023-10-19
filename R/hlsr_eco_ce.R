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
                      # paste0(data_folder, data_file),
                        here("data",data_file ),
                      sheet = "F_Eco CE",
                      range = cell_limits(c(7, 1), c(NA, 3))) %>% mutate_at(c(2,3), ~replace_na(.,0)) %>% 
  as_tibble() 

data_raw_extra <-  read_xlsx(
                            # paste0(data_folder, data_file),
                            here("data", data_file ),
                            sheet = "F_Eco CE",
                            range = cell_limits(c(7, 5), c(NA, 7))) %>%
  as_tibble() %>% mutate_at(c(2,3), ~replace_na(.,0)) %>% 
  mutate(FIN_CE = COST_CONTROLLABLE/COMPOSITE_FLIGHTHOUR) 

cost_delay <-  read_xlsx(
                        # paste0(data_folder, data_file),
                        here("data", data_file ),
                        sheet = "F_Eco CE",
                        range = cell_limits(c(7, 10), c(8, 10))) %>%
  as_tibble() %>% pull()

data_merged = merge(x=data_raw, y=data_raw_extra, by="ANSP_NAME")

data_calc <- data_merged %>% 
  mutate(DELAY_ERT_CPH = TDM_ERT_ALL_REASON*cost_delay/COMPOSITE_FLIGHTHOUR,
         DELAY_ARP_CPH = TDM_ARP_ALL_REASON*cost_delay/COMPOSITE_FLIGHTHOUR,
         ECO_CE = FIN_CE+DELAY_ERT_CPH+DELAY_ARP_CPH) %>% 
  select(ANSP_NAME, FIN_CE, DELAY_ERT_CPH, DELAY_ARP_CPH, ECO_CE)

# prepare data for main plot
data_prep <- data_calc  %>% 
  mutate(QUART1 = quantile(ECO_CE, 0.25),
         QUART3 = quantile(ECO_CE, 0.75),
         LABELS = format(round(ECO_CE,0), big.mark = " ")
  ) %>% 
  mutate(LABELS = if_else(ECO_CE >=1000, LABELS,
                          str_sub(LABELS, start= -nchar(LABELS)+1)) #to avoid the leading space created by format
  ) 

data_plot <- data_prep %>% 
  rename(`Financial gate-to-gate cost-effectiveness` = FIN_CE,
         `Unit cost of en-route ATFM delays` = DELAY_ERT_CPH,
         `Unit cost of airport ATFM delays` = DELAY_ARP_CPH) %>% 
  pivot_longer(!c(ANSP_NAME, LABELS, QUART1, QUART3, ECO_CE), names_to = "TYPE", values_to = "VALUE" )

# help table for labels and additional traces
data_help <- data_prep %>% 
  select(ANSP_NAME, LABELS, QUART1, QUART3, ECO_CE)

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
sys_avg_fce <- data_raw_extra %>% summarise(sum(COST_CONTROLLABLE)/sum(COMPOSITE_FLIGHTHOUR)) %>% pull()
sys_avg_ece <- data_merged %>% 
  summarise(sum(COST_CONTROLLABLE+TDM_ERT_ALL_REASON*cost_delay+TDM_ARP_ALL_REASON*cost_delay)/sum(COMPOSITE_FLIGHTHOUR)) %>% pull()

# plot

plot_all <- data_plot %>%
  plot_ly(
    x = ~ ANSP_NAME,
    y = ~ VALUE,
    yaxis = "y1",
    color = ~ factor(TYPE, levels = c("Financial gate-to-gate cost-effectiveness",
                                      "Unit cost of en-route ATFM delays",
                                      "Unit cost of airport ATFM delays")
    ),
    colors = c('#9999FF', '#FF0000', '#EEE800'),
    # text = ~ LABELS,
    # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
    # textangle = -90,
    # textposition = "outside", cliponaxis = FALSE,
    # insidetextanchor =  "start",
    # textfont = list(color = 'black', size = 7),
    type = "bar", 
    hovertemplate = paste('%{y:.0f}'),
    # hoverinfo = "none",
    showlegend = T
  ) %>%
  add_trace( data = data_help,
             inherit = FALSE,
             marker = list(color =('transparent')),
             x = ~ ANSP_NAME,
             y = ~ ECO_CE,
             yaxis = "y1",
             mode = 'text',
             text = ~ LABELS,
             textfont = list(color = 'black', size = 8),
             # textangle = 0,
             name = 'Total',
             textposition = "top center", cliponaxis = FALSE,
             type = 'scatter',  mode = 'lines',
             hovertemplate = paste('%{y:.0f}'),
             # hoverinfo = "none",
             showlegend = F
  ) %>% 
  add_trace(data = data_help,
            inherit = FALSE,
            x = ~ ANSP_NAME,
            y = ~ QUART1,
            yaxis = "y1",
            # colors = c('#4F81BD'),
            type = 'scatter',  mode = 'lines',
            line = list(color = '#333399', width = 2, dash = 'dash'),
            opacity = 0.3,
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
            line = list(color = '#333399', width = 2, dash = 'dash'),
            opacity = 0.3,
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
    color = ~ factor(TYPE, levels = c("Financial gate-to-gate cost-effectiveness",
                                      "Unit cost of en-route ATFM delays",
                                      "Unit cost of airport ATFM delays")
    ),
    colors = c('#9999FF', '#FF0000', '#EEE800'),
    # marker = list(color =('#8989FF')),
    text = " ", # for some reason I need to keep this to avoid labels in prev plot to autosize
    # cliponaxis = FALSE,
    # textangle = -90,
    # textposition = "inside",
    # insidetextanchor =  "start",
    # textfont = list(color = 'black', size = 9),
    type = "bar",
    hovertemplate = paste('%{y:.0f}'),
    # hoverinfo = "none",
    showlegend = F
  ) %>% 
  add_trace(data = data_help_inset,
            inherit = FALSE,
            marker = list(color =('transparent')),
            x = ~ ANSP_NAME,
            y = ~ ECO_CE,
            yaxis = "y1",
            mode = 'text',
            text = ~ LABELS,
            textfont = list(color = 'black', size = 10),
            # textangle = 0,
            textposition = "top center", cliponaxis = FALSE,
            type = 'scatter',  mode = 'lines',
            name = 'Total',
            hovertemplate = paste('%{y:.0f}'),
            # hoverinfo = "none",
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
            line = list(color = '#333399', width = 2, dash = 'dash'),
            opacity = 0.3,
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
            line = list(color = '#333399', width = 2, dash = 'dash'),
            opacity = 0.3,
            hoverinfo = "none",
            showlegend = F
  ) %>%
  config(responsive = FALSE,
         displaylogo = FALSE,
         displayModeBar = F
         # modeBarButtons = list(list("toImage"))
  )

myannotations <- list(list(
  x = 0.12,
  y = 1.10,
  text = paste0("<b>", 
                "European system avg. for economic cost-effectiveness: ", 
                "\u20AC ",
                format(round(sys_avg_ece,0), big.mark = " "),
                "</b>"),
  xref = "paper",
  yref = "paper",
  xanchor = "left",
  showarrow = FALSE,
  font = list(color = "#993366",
              size=10)
),
list(
  x = 0.12,
  y = 1.05,
  text = paste0("<b>", 
                "European system avg. for financial cost-effectiveness: ", 
                "\u20AC ",
                format(round(sys_avg_fce,0), big.mark = " "),
                "</b>"),
  xref = "paper",
  yref = "paper",
  xanchor = "left",
  showarrow = FALSE,
  font = list(color = "#8989FF",
              size=10)
)
)

# this is ugly but it's the only way i found for space as thousand sep for the y axis
# https://stackoverflow.com/questions/64024937/how-to-change-thousands-separator-for-blank-in-r-plotly

ticklabels1 <- seq(from=0, to=round(max(data_plot$ECO_CE+200)), by=200)
ticktexts1 <- c(0,format(ticklabels1[-1], big.mark = " "))

ticklabels2 <- seq(from=0, to=round(max(data_inset$ECO_CE+200)), by=200)
ticktexts2 <- c(0,format(ticklabels2[-1], big.mark = " "))

fig <- subplot(plot_all, plot_inset) %>% 
  layout( autosize = T, 
          uniformtext = list(minsize=8, mode='show'), #this is important so it does not autofit fonts
          bargap = 0.45,
          barmode = 'stack',
          title = list(text = "", font = list(color = "black", size = 14)),
          font = list(family = "Helvetica"),
          hovermode = "x unified",
          hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
          legend = list(orientation = 'h',
                        traceorder = 'reversed', #for some reason this does not work
                        font = list(size = 10),
                        y = -0.55,
                        x = 0.0,
                        bgcolor = 'transparent'),
          xaxis = list(title = "",
                       tickangle = 270,
                       tickfont = list(size=11),
                       autotick = F,
                       # tick0=0.25,
                       fixedrange = TRUE,
                       showgrid = F,
                       categoryorder = "total descending",
                       domain=c(0,1)),
          yaxis = list(title = paste("\U20AC","per composite flight-hour"),
                       titlefont = list(size = 12),
                       tickfont = list(size = 11),
                       # dtick = 200,
                       tickvals = ticklabels1,
                       ticktext = ticktexts1,
                       # automargin = FALSE,
                       # margin = list(l=100),
                       fixedrange = TRUE,
                       hoverformat = '.0f',
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
                        domain=c(0.70,1)),
          yaxis2 = list(title = "",
                        # titlefont   = list(size = 13),
                        tickfont = list(size=10),
                        # dtick = 200,
                        tickvals = ticklabels2,
                        ticktext = ticktexts2,
                        hoverformat = '.0f',
                        fixedrange = TRUE,
                        # range = list(0, 200+round(max(data_inset$VALUE/1000), 1)*1000),
                        zeroline = T, showline = F, showgrid = F,
                        domain=c(0.45,0.95)),
          annotations = myannotations
  )

fig


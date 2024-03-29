
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
library(magick)

# import data
data_raw <- read_xlsx(
            paste0(data_folder, data_file),
              # here("data", data_file ),
            sheet = "F_Support",
                      range = cell_limits(c(7, 1), c(NA, NA))) %>%
  as_tibble() 

# import ANSP countries
ansp  <- read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder, data_file),
  sheet = "Status",
  range = cell_limits(c(8, 1), c(NA, 3))) %>%
  as_tibble() %>% 
  replace(is.na(.), 0) %>% 
  select(-status)

data_merged = merge(x=data_raw, y=ansp, by="ANSP_NAME")

# prepare data for main plot
data_prep <- data_merged  %>% 
  mutate(QUART1 = quantile(`Support costs per composite flight-hour`, 0.25),
         QUART3 = quantile(`Support costs per composite flight-hour`, 0.75),
         LABELS = format(round(`Support costs per composite flight-hour`,0), big.mark = " ")
  ) 

# %>% 
#   mutate(LABELS = if_else(`Support costs per composite flight-hour` >=1000, LABELS,
#                           str_sub(LABELS, start= -nchar(LABELS)+1)) #to avoid the leading space created by format
#   ) 

data_plot <- data_prep %>% 
  select(-c(`Support costs`, `Composite flight-hours`)) %>% 
  rename(`Employment costs (excl. ATCOs in OPS) per composite flight-hour` = 
           `Non ATCO employement cost per composite flight-hour`,
         `Non-staff operating costs per composite flight-hour` = 
           `Non-staff operating costs (excluding exceptional cost) per composite flight-hours`,
         `Exceptional costs per composite flight-hour` = 
           `Exceptional Cost per composite flight hour`) %>% 
  pivot_longer(!c(ANSP_NAME, country, LABELS, QUART1, QUART3, `Support costs per composite flight-hour`), names_to = "TYPE", values_to = "VALUE" )

# help table for labels and additional traces
data_help <- data_prep %>% 
  select(ANSP_NAME, country, LABELS, QUART1, QUART3, `Support costs per composite flight-hour`)

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

plot_all <- function(myfont, mywidth, myheight){
  data_plot %>%
  plot_ly(
    height = myheight,
    width = mywidth,
    x = ~ ANSP_NAME,
    y = ~ VALUE,
    yaxis = "y1",
    color = ~ factor(TYPE, levels = c("Employment costs (excl. ATCOs in OPS) per composite flight-hour",
                                      "Non-staff operating costs per composite flight-hour",
                                      "Capital-related costs per composite flight-hour",
                                      "Exceptional costs per composite flight-hour")
    ),
    colors = c('#003366', '#78B4F0', '#E1F060', '#E0584F'),
    # text = ~ LABELS,
    # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
    # textangle = -90,
    # textposition = "outside", cliponaxis = FALSE,
    # insidetextanchor =  "start",
    # textfont = list(color = 'black', size = 7),
    type = "bar", 
    hovertemplate = paste('%{y:.0f}'),
    showlegend = T
  ) %>%
  add_trace( data = data_help,
             inherit = FALSE,
             marker = list(color =('transparent')),
             x = ~ ANSP_NAME,
             y = ~ `Support costs per composite flight-hour`,
             yaxis = "y1",
             mode = 'text',
             text = ~ LABELS,
             textfont = list(color = 'black', size = myfont),
             # textangle = 0,
             textposition = "top center", cliponaxis = FALSE,
             type = 'scatter',  mode = 'lines',
             hoverinfo = 'none',
             # hovertemplate = paste('%{y:.0f}'),
             name = "Total",
             showlegend = F
  ) %>% 
    add_trace( data = data_help,
               inherit = FALSE,
               marker = list(color =('transparent')),
               x = ~ ANSP_NAME,
               y = ~ `Support costs per composite flight-hour`,
               yaxis = "y1",
               mode = 'text',
               text = ~ paste0(ANSP_NAME, " (", country, ")"),
               textfont = list(color = 'transparent', size = 1),
               # textangle = 0,
               # textposition = "top center", cliponaxis = FALSE,
               type = 'scatter',  mode = 'lines',
               hovertemplate = paste('%{text} %{y:.0f}<extra></extra>'),
               name = "",
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
}

plot_inset <- function(myfont){ 
  data_inset %>%
  plot_ly(
    x = ~ ANSP_NAME,
    y = ~ VALUE,
    yaxis = "y1",
    color = ~ factor(TYPE, levels = c("Employment costs (excl. ATCOs in OPS) per composite flight-hour",
                                      "Non-staff operating costs per composite flight-hour",
                                      "Capital-related costs per composite flight-hour",
                                      "Exceptional costs per composite flight-hour")
    ),
    colors = c('#003366', '#78B4F0', '#E1F060', '#E0584F'),
    # marker = list(color =('#8989FF')),
    text = " ", # for some reason I need to keep this to avoid labels in prev plot to autosize
    # cliponaxis = FALSE,
    # textangle = -90,
    # textposition = "inside",
    # insidetextanchor =  "start",
    # textfont = list(color = 'black', size = 9),
    type = "bar",
    hovertemplate = paste('%{y:.0f}'),
    showlegend = F
  ) %>% 
    add_trace( data = data_help_inset,
               inherit = FALSE,
               marker = list(color =('transparent')),
               x = ~ ANSP_NAME,
               y = ~ `Support costs per composite flight-hour`,
               yaxis = "y1",
               mode = 'text',
               text = ~ LABELS,
               textfont = list(color = 'black', size = myfont),
               # textangle = 0,
               textposition = "top center", cliponaxis = FALSE,
               type = 'scatter',  mode = 'lines',
               hoverinfo = 'none',
               # hovertemplate = paste('%{y:.0f}'),
               name = "Total",
               showlegend = F
    ) %>% 
    add_trace( data = data_help_inset,
               inherit = FALSE,
               marker = list(color =('transparent')),
               x = ~ ANSP_NAME,
               y = ~ `Support costs per composite flight-hour`,
               yaxis = "y1",
               mode = 'text',
               text = ~ paste0(ANSP_NAME, " (", country, ")"),
               textfont = list(color = 'transparent', size = 1),
               # textangle = 0,
               # textposition = "top center", cliponaxis = FALSE,
               type = 'scatter',  mode = 'lines',
               hovertemplate = paste('%{text} %{y:.0f}<extra></extra>'),
               name = "",
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
                   font = list(color = 'black', size = myfont)
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
}

myannotations <- function(myfont){
list(
  x = 0.12,
  y = 1.05,
  text = paste0("<b>", 
                "European system average: ", 
                "\u20AC ",
                format(round(sys_avg,0), big.mark = " "),
                "</b>"),
  xref = "paper",
  yref = "paper",
  xanchor = "left",
  showarrow = FALSE,
  font = list(color = "#003366",
              size = myfont)
)
}

# this is ugly but it's the only way i found for space as thousand sep for the y axis
# https://stackoverflow.com/questions/64024937/how-to-change-thousands-separator-for-blank-in-r-plotly

ticklabels1 <- seq(from=0, to=round(max(data_plot$`Support costs per composite flight-hour`+200)), by=200)
ticktexts1 <- c(0,format(ticklabels1[-1], big.mark = " "))

ticklabels2 <- seq(from=0, to=round(max(data_inset$`Support costs per composite flight-hour`+200)), by=200)
ticktexts2 <- c(0,format(ticklabels2[-1], big.mark = " "))

fig <- function(myfont, mywidth, myheight, vlegend, vdomain){ 
  subplot(plot_all(myfont, mywidth, myheight), plot_inset(myfont + 1)) %>% 
  layout( autosize = T, 
          uniformtext = list(minsize=myfont, mode='show'), #this is important so it does not autofit fonts
          bargap = 0.45,
          barmode = 'stack',
          title = list(text = "", font = list(color = "black", size = 14)),
          font = list(family = "Helvetica"),
          hovermode = "x unified",
          hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
          legend = list(orientation = 'h',
                        traceorder = 'reversed', #for some reason this does not work
                        font = list(size = if_else(myfont == 8, myfont + 1, myfont + 3)),
                        y = vlegend,
                        x = -0.05,
                        bgcolor = 'transparent'),
          xaxis = list(title = "",
                       tickangle=270,
                       tickfont = list(size = myfont + 3),
                       autotick = F,
                       # tick0=0.25,
                       fixedrange = TRUE,
                       showgrid = F,
                       categoryorder = "total descending",
                       domain=c(0,1)),
          yaxis = list(title = paste("\U20AC","per composite flight-hour"),
                       titlefont   = list(size = myfont + 4),
                       tickfont = list(size = myfont + 3),
                       # dtick = 200,
                       tickvals = ticklabels1,
                       ticktext = ticktexts1,
                       # automargin = FALSE,
                       # margin = list(l=100),
                       fixedrange = TRUE,
                       linewidth=1, linecolor='transparent',  mirror = T,
                       range = list(0, 200+round(max(data_plot$`Support costs per composite flight-hour`/1000), 1)*1000),
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
                        tickfont = list(size = myfont + 2),
                        # dtick = 200,
                        tickvals = ticklabels2,
                        ticktext = ticktexts2,
                        fixedrange = TRUE,
                        range = list(0, 200+round(max(data_inset$`Support costs per composite flight-hour`/1000), 1)*1000),
                        zeroline = T, showline = F, showgrid = F,
                        domain=c(vdomain - 0.4, vdomain)),
          annotations = myannotations(if_else(myfont <=10, myfont + 5, myfont + 13))
  )
}

fig(8, NULL, NULL, -0.85, 0.95)

# export to image
# the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS

fig_dir <- 'figures/'
fig_name <- "figure-4-7-hlsr_support.png"

invisible(export(fig(22, 1700, 900, -0.55, 1), paste0(fig_dir, fig_name)))
invisible(figure <- image_read(paste0(fig_dir,fig_name)))
invisible(cropped <- image_crop(figure, "0x900"))
invisible(image_write(cropped, paste0(fig_dir, fig_name)))



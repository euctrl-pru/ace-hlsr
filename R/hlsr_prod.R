
## libraries
library(dplyr)
library(stringr)
library(readxl)
library(plotly)
library(stringr)
library(here)
## data source
source(here("data_source.R"))
library(magick)

# import data
data_raw <-  read_xlsx(
                      paste0(data_folder, data_file),
                      # here("data", data_file ),
                      sheet = "F_Prod",
                      range = cell_limits(c(7, 1), c(NA, 4))) %>%
  as_tibble() %>% 
  rename(VALUE=2, HOURS =3, CFH =4)


# prepare data for main plot
data_plot <- data_raw  %>%
  select(ANSP_NAME, VALUE) %>% 
  mutate(QUART1 = quantile(VALUE, 0.25),
         QUART3 = quantile(VALUE, 0.75),
         LABELS = round(VALUE,2)
  ) 

#prepare data for inset
data_inset <- data_plot %>% 
  filter(ANSP_NAME == 'DSNA'| ANSP_NAME == 'ENAIRE' | ANSP_NAME == 'DFS' |
           ANSP_NAME == 'ENAV' | ANSP_NAME == 'NATS (Continental)') %>% 
  mutate(ANSP_NAME = case_when(
    ANSP_NAME == 'NATS (Continental)' ~ 'NATS\n(Continental)',
    TRUE ~ ANSP_NAME)) %>% 
  mutate(LAB_COORD = 0)

# system average for annotation
sys_avg <- data_raw %>% summarise(sum(HOURS)/sum(CFH)) %>% pull()

# plot

plot_all <- function(myfont){
  data_plot %>%
  plot_ly(
    height = '450px',
    x = ~ ANSP_NAME,
    y = ~ VALUE,
    yaxis = "y1",
    marker = list(color =('#003366')),
    text = ~ LABELS,
    # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
    # textangle = -90,
    textposition = "outside", cliponaxis = FALSE,
    # insidetextanchor =  "start",
    textfont = list(color = 'black', size = myfont),
    type = "bar",
    hoverinfo = "none",
    # domain = list(x = c(0, 1), y = c(0, 1)),
    showlegend = F
  ) %>% 
  add_trace(
    inherit = FALSE,
    x = ~ ANSP_NAME,
    y = ~ QUART1,
    yaxis = "y1",
    # colors = c('#4F81BD'),
    type = 'scatter',  mode = 'lines',
    line = list(color = '#000080', width = 2, dash = 'dash'),
    opacity = 0.5,
    hoverinfo = "none",
    showlegend = F
  ) %>%
  add_trace(
    inherit = FALSE,
    x = ~ ANSP_NAME,
    y = ~ QUART3,
    yaxis = "y1",
    # color = c('#333399'),
    type = 'scatter',  mode = 'lines',
    line = list(color = '#000080', width = 2, dash = 'dash'),
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
    marker = list(color =('#003366')),
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
  add_trace(
    inherit = FALSE,
    # marker = list(color =('transparent')),
    x = ~ ANSP_NAME,
    y = ~ VALUE+0.02,
    yaxis = "y1",
    mode = 'text',
    text = ~ LABELS,
    textfont = list(color = 'black', size = myfont + 1),
    # textangle = 0,
    textposition = "top center", cliponaxis = FALSE,
    type = 'scatter',  mode = 'lines',
    hoverinfo = "none",
    showlegend = F
  ) %>% 
  add_annotations (
    text = ~ ANSP_NAME,
    x = ~ ANSP_NAME,
    y = ~ LAB_COORD,      
    showarrow = F,
    xref = "x",
    yref = "y",
    yanchor = "bottom",
    xanchor = "center",
    align = "left",
    textangle = -90,
    font = list(color = 'white', size = myfont)
  ) %>% 
  add_trace(
    inherit = FALSE,
    x = ~ ANSP_NAME,
    y = ~ QUART1,
    yaxis = "y1",
    # colors = c('#4F81BD'),
    type = 'scatter',  mode = 'lines',
    line = list(color = '#000080', width = 2, dash = 'dash'),
    opacity = 0.5,
    hoverinfo = "none",
    showlegend = F
  ) %>%
  add_trace(
    inherit = FALSE,
    x = ~ ANSP_NAME,
    y = ~ QUART3,
    yaxis = "y1",
    # color = c('#333399'),
    type = 'scatter',  mode = 'lines',
    line = list(color = '#000080', width = 2, dash = 'dash'),
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
                # "\u20AC ",
                format(round(sys_avg,2), nsmall = 2),
                "</b>"),
  xref = "paper",
  yref = "paper",
  xanchor = "left",
  showarrow = FALSE,
  font = list(color = "#003366",
              size=myfont)
)
}

fig <- function(myfont, myinsetv) {
  subplot(plot_all(myfont), plot_inset(myfont + 1)) %>% 
  layout( autosize = T,
          uniformtext=list(minsize=8, mode='show'), #this is important so it does not autofit fonts
          bargap = 0.45,
          title = list(text = "", font = list(color = "black", size = 14)),
          font = list(family = "Helvetica"),
          xaxis = list(title = "",
                       tickangle=270,
                       tickfont = list(size = myfont + 3),
                       autotick = F,
                       # tick0=0.25,
                       # fixedrange = TRUE,
                       showgrid = F,
                       categoryorder = "total descending"
                       , domain=c(0,1)
                       ),
          yaxis = list(title = paste("Composite flight-hours per ATCO-hour"),
                       titlefont   = list(size = myfont + 4),
                       tickfont = list(size = myfont + 3),
                       dtick = 0.2,
                       tickformat = ".1f",
                       # tickvals = ticklabels1,
                       # ticktext = ticktexts1,
                       # automargin = FALSE,
                       # margin = list(l=100),
                       # fixedrange = TRUE,
                       linewidth=10, linecolor='transparent',  mirror = T,
                       range = list(0, 0.2+round(max(data_plot$VALUE), 1)),
                       zeroline = T, showline = T, showgrid = F
                       # , domain=c(0,1)
                       ),
          xaxis2 = list(title = "",
                        showticklabels = FALSE,
                        # tickangle=270,
                        # tickfont = list(size=10),
                        autotick = F,
                        # fixedrange = TRUE,
                        showgrid = F,
                        categoryorder = "total descending",
                        domain=c(0.65,1)),
          yaxis2 = list(title = "",
                        # titlefont   = list(size = 13),
                        tickfont = list(size = myfont + 2),
                        dtick = 0.2,
                        tickformat = ".1f",
                        # tickvals = ticklabels2,
                        # ticktext = ticktexts2,
                        # fixedrange = TRUE,
                        range = list(0, 0.2+round(max(data_inset$VALUE), 1)),
                        zeroline = T, showline = F, showgrid = F,
                        domain=c(myinsetv, myinsetv + 0.5)),
          annotations = myannotations(myfont + 5)
  )
}
  
fig(8, 0.45)

# export to image
# the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS

fig_dir <- 'figures/'

invisible(export(fig(10, 0.5), paste0(fig_dir,"figure-4-5-hlsr_prod.png")))
invisible(figure <- image_read(paste0(fig_dir,"figure-4-5-hlsr_prod.png")))
invisible(cropped <- image_crop(figure, "0x450"))
invisible(image_write(cropped, paste0(fig_dir,"figure-4-5-hlsr_prod.png")))

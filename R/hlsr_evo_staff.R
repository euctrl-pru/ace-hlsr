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
data_raw  <-  read_xlsx(
                        paste0(data_folder, data_file),
                        # here("data", data_file ),
                        sheet = "F_Staff",
                        range = cell_limits(c(9, 1), c(NA, 3))) %>%
              as_tibble() %>% 
              rename(STAF_TYPE = 'Data', STAF = 'Total') %>% 
              mutate(across(STAF_TYPE, str_replace, 'Sum of ', ''))

## process data for plot
data_prep <- data_raw %>% 
  filter(YEAR_DATA <= year_report, YEAR_DATA >=year_report-5) %>% 
  filter(grepl('STAF_', STAF_TYPE)) %>% 
  filter(!grepl('COST_', STAF_TYPE)) 

data_support <- data_prep %>% filter(STAF_TYPE != 'STAF_ATCO') %>% 
    group_by(YEAR_DATA) %>% summarise(STAF = sum(STAF)) %>% 
  mutate(STAF_TYPE = 'STAF_SUPPORT') %>%
  select(YEAR_DATA, STAF_TYPE, STAF)

data_ATCO <- data_prep %>% filter(STAF_TYPE == 'STAF_ATCO')

data_plot <- rbind (data_ATCO, data_support) %>% 
  mutate(LABEL = case_when(
                          STAF_TYPE == 'STAF_ATCO' ~ 'Number of ATCOs in OPS',
                          STAF_TYPE == 'STAF_SUPPORT' ~ 'Number of support staff'))

# draw costs plot
p1 <- data_plot %>% 
  plot_ly(
    # width = 500, 
    height = 230,
    x = ~ YEAR_DATA,
    y = ~ STAF/1000,
    color = ~ factor(LABEL, levels = c('Number of support staff', 'Number of ATCOs in OPS')),
    type = "bar",
    colors = c('#9AA349', '#003366'),
    text = ~ round(STAF/1000,0),
    textangle = 0,
    textposition = "inside",
    textfont = list(color = 'white'),
    insidetextanchor =  "middle",
    hoverinfo = "none",
    showlegend = TRUE
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
      title = paste0("Thousands of FTEs"),
      linewidth=1, linecolor='black',
      # titlefont   = list(size = 13),
      fixedrange = TRUE,
      ticks = 'outside',
      # range = c(cost_min,cost_max),
      # tickson="boundaries",
      # tickcolor='#BFBFBF', ticklen=3,
      # tickformat=".1f",
      # categoryorder = "total ascending",
      # domain=c(0,1),
      zeroline = F, showline = T, showgrid = F
    ),
    legend = list(orientation = 'h',
                  # font = list(size = 11),
                  y = -0.1,
                  x = 0.5,
                  xanchor = 'center',
                  bgcolor = 'transparent'),
    autosize = T,
    barmode = 'stack',
    bargap = 0.4,
    font = list(family = "Helvetica"),
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

invisible(export(p1, paste0(fig_dir,"figure-2-7-1-hlsr_evo_staff.png")))
invisible(figure <- image_read(paste0(fig_dir,"figure-2-7-1-hlsr_evo_staff.png")))
invisible(cropped <- image_crop(figure, "0x230"))
invisible(image_write(cropped, paste0(fig_dir,"figure-2-7-1-hlsr_evo_staff.png")))

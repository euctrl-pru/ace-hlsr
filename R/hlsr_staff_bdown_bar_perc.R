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
data_plot <- data_raw %>% 
  filter(grepl('STAF_', STAF_TYPE)) %>% 
  filter(!grepl('COST_', STAF_TYPE)) %>% 
  group_by(STAF_TYPE) %>% arrange(YEAR_DATA) %>% 
  mutate(YOY = STAF/lag(STAF)-1) %>% 
  ungroup() %>% 
  filter(YEAR_DATA == year_report) %>% 
  mutate(LABEL = case_when(
    STAF_TYPE == 'STAF_ATCO' ~ "ATCOs in OPS",
    STAF_TYPE == 'STAF_ATCO_OTHER' ~ "ATCOs on other duties",
    STAF_TYPE == 'STAF_AB_INITIO' ~ "Ab-initio trainees",
    STAF_TYPE == 'STAF_TRAINEE' ~ "On-the-job trainees",
    STAF_TYPE == 'STAF_ATC_ASSISTANT' ~ "ATC assistants",
    STAF_TYPE == 'STAF_OPS_SUPPORT' ~ "OPS-Support",
    STAF_TYPE == 'STAF_TECH_OPERAT' ~ "Technical support\nfor maintenance",
    STAF_TYPE == 'STAF_TECH_PLANNING' ~ "Technical support\nfor planning & development",
    STAF_TYPE == 'STAF_ADMIN' ~ "Admin.",
    STAF_TYPE == 'STAF_ANCILLARY' ~ "Ancillary services",
    STAF_TYPE == 'STAF_OTHER' ~ "Other"
      )
    ) %>% 
  mutate(LABEL = factor(LABEL, levels = c("Other",
                                          "Ancillary services",
                                          "Admin.",
                                          "Technical support\nfor planning & development",
                                          "Technical support\nfor maintenance",
                                          "OPS-Support",
                                          "ATC assistants",
                                          "On-the-job trainees",
                                          "Ab-initio trainees",
                                          "ATCOs on other duties",
                                          "ATCOs in OPS"
  ))) 

#calculate range for x axis 
xrange_max <- max(abs(data_plot$YOY)) + 0.4

# draw costs plot
p <- function(myfont, mywidth, myheight, vmargin, myautosize) {
  data_plot %>% 
    plot_ly(
      width = mywidth,
      height = myheight,
      x = ~ YOY,
      y = ~ LABEL,
      marker = list(color =('#2990EA')),
      text = ~ if_else(YOY>=0, paste0("+",format(round(YOY*100,1), nsmall=1), "%"),
                       paste0(format(round(YOY*100,1), nsmall=1), "%")),
      textangle = 0,
      textposition = "outside",
      # insidetextanchor =  "start",
      textfont = list(color = '#1782E3', size = myfont + 1),
      cliponaxis = FALSE,
      type = "bar",
      orientation = 'h',
      hoverinfo = "none",
      showlegend = F
    ) %>% 
      layout(
        title = list(
          text = paste0("Changes ",year_report-1,"-",year_report," (in %)"), 
          font = list(color = '#747a7f', size = myfont),
          y = 0.05),
        xaxis = list(
          title = "",
          # fixedrange = TRUE,
          range = c(-xrange_max, xrange_max),
          # automargin = T,
          # tickvals = c(),
          # tickformat=",.0%",
          showticklabels = FALSE,
          # autotick = T, 
          zeroline = T, showline = T, mirror = T,
          linecolor='#BFBFBF',
          zerolinecolor='#BFBFBF',
          # domain=c(0.6,1),
          showgrid = F
        ),
        yaxis = list(
          title = "",
          linewidth=1, linecolor='#BFBFBF',
          # titlefont   = list(size = 13),
          fixedrange = TRUE,
          autotick = F,
          showticklabels = FALSE,
          gridcolor = '#BFBFBF',
          tickson="boundaries",
          # domain=c(0,1),
          zeroline = F, showline = F, showgrid = T
        ),
        bargap = 0.4,
        autosize = myautosize,
        margin = list(b = vmargin, l=0, r=0),
        plot_bgcolor = 'white',
        uniformtext=list(minsize=10, mode='show')
      ) %>%
      config(responsive = FALSE,
             displaylogo = FALSE,
             displayModeBar = F)

}

p(12, NULL, NULL, 40, 'T')


# export to image
# the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS

fig_dir <- 'figures/'
image_name <- "figure-2-7-3-hlsr_staff_bdown_bar_perc.png"

invisible(export(p(26,500, 950, 100, 'F'), paste0(fig_dir,image_name)))
invisible(figure <- image_read(paste0(fig_dir,image_name)))
invisible(cropped <- image_crop(figure, "500"))
invisible(image_write(cropped, paste0(fig_dir,image_name)))



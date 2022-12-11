## libraries
library(dplyr)
library(stringr)
library(readxl)
library(plotly)
library(here)
## data source
source(here("data_source.R"))

## import data
data_raw  <-  read_xlsx(
                        paste0(data_folder, data_file),
                        # here("data","hlsr2021_data.xlsx"),
                        sheet = "F_Staff",
                        range = cell_limits(c(9, 1), c(NA, 3))) %>%
              as_tibble() %>% 
              rename(STAF_TYPE = 'Data', STAF = 'Total') %>% 
              mutate(across(STAF_TYPE, str_replace, 'Sum of ', ''))

## process data for plot
data_plot <- data_raw %>% 
  filter(YEAR_DATA == year_report) %>% 
  filter(grepl('STAF_', STAF_TYPE)) %>% 
  filter(!grepl('COST_', STAF_TYPE)) %>% 
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
  )%>% 
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
xrange_max <- round((max(data_plot$STAF)+5000)/1000,0)*1000

# draw costs plot
p1 <- data_plot %>% 
  plot_ly(
    x = ~ STAF,
    y = ~ LABEL,
    marker = list(color =('#4F81BD')),
    text = ~ format(round(STAF,0), big.mark = " "),
    textangle = 0,
    textposition = "outside",
    # insidetextanchor =  "start",
    textfont = list(color = 'black'),
    cliponaxis = FALSE,
    type = "bar",
    orientation = 'h',
    hoverinfo = "none",
    showlegend = F
  ) %>% 
  layout(
    title = list(
      text = paste0("Gate-to-gate ATM/CNS staff in ", year_report ," (in FTEs)"), 
      font = list(color = '#747a7f', size = 11),
      y = 0),
    xaxis = list(
      title = "",
      # fixedrange = TRUE,
      range = c(0, xrange_max),
      # automargin = T,
      # tickvals = c(),
      # tickangle = 0,
      # autotick = T, 
      showticklabels = FALSE,
      zeroline = F,
      # domain=c(0,0.6),
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
      # domain=c(0,1),
      zeroline = F, showline = T, showgrid = F
    ),
    autosize = T,
    bargap = 0.4,
    plot_bgcolor = '#DCE6F2',
    uniformtext=list(minsize=10, mode='show')
  ) %>%
  config(responsive = FALSE,
         displaylogo = FALSE,
         displayModeBar = F)


p1



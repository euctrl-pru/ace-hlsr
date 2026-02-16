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
data_raw_py  <-  read_xlsx(
                        paste0(data_folder, data_file),
                        # here("data", data_file ),
                        sheet = "F_CostCategories",
                        range = cell_limits(c(8, 1), c(NA, 7))) %>%
              as_tibble() %>% 
              janitor::clean_names() %>% 
  mutate(
    year_comparison = year_report-1
  )


data_raw_2019  <-  read_xlsx(
  paste0(data_folder, data_file),
  # here("data", data_file ),
  sheet = "F_CostCategories",
  range = cell_limits(c(8, 10), c(NA, 16))) %>%
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(
    year_comparison = 2019,
  )



## process data for plot
data_dif <- data_raw_py %>% 
  rbind(data_raw_2019) %>% 
  group_by(year_comparison) %>% 
  arrange(year_comparison, year_data) %>% 
  ungroup() %>% 
  mutate(
    staf_dif = if_else (year_comparison == 2019,
                        staf_cost - lag(staf_cost, year_report - 2019),
                        staf_cost - lag(staf_cost, 1)),
    non_staff_operating_dif = if_else (year_comparison == 2019,
                        non_staff_operating_costs_excluding_exceptional_cost - lag(non_staff_operating_costs_excluding_exceptional_cost, year_report - 2019),
                        non_staff_operating_costs_excluding_exceptional_cost - lag(non_staff_operating_costs_excluding_exceptional_cost, 1)),
    coc_dif = if_else (year_comparison == 2019,
                                cost_capital - lag(cost_capital, year_report - 2019),
                       cost_capital - lag(cost_capital, 1)),
    depreciation_dif = if_else (year_comparison == 2019,
                                cost_depreciation - lag(cost_depreciation, year_report - 2019),
                                cost_depreciation - lag(cost_depreciation, 1)),
    exceptional_dif = if_else (year_comparison == 2019,
                                cost_exceptional - lag(cost_exceptional, year_report - 2019),
                                cost_exceptional - lag(cost_exceptional, 1)),
    controllable_dif = if_else (year_comparison == 2019,
                                cost_controllable - lag(cost_controllable, year_report - 2019),
                                cost_controllable - lag(cost_controllable, 1)),

    staf_dif_perc = if_else (year_comparison == 2019,
                        staf_cost / lag(staf_cost, year_report - 2019) -1,
                        staf_cost / lag(staf_cost, 1) -1),
    non_staff_operating_dif_perc = if_else (year_comparison == 2019,
                                       non_staff_operating_costs_excluding_exceptional_cost / lag(non_staff_operating_costs_excluding_exceptional_cost, year_report - 2019) -1,
                                       non_staff_operating_costs_excluding_exceptional_cost / lag(non_staff_operating_costs_excluding_exceptional_cost, 1) -1),
    coc_dif_perc = if_else (year_comparison == 2019,
                       cost_capital / lag(cost_capital, year_report - 2019)-1,
                       cost_capital / lag(cost_capital, 1)-1),
    depreciation_dif_perc = if_else (year_comparison == 2019,
                                cost_depreciation / lag(cost_depreciation, year_report - 2019) -1,
                                cost_depreciation / lag(cost_depreciation, 1) -1),
    exceptional_dif_perc = if_else (year_comparison == 2019,
                               cost_exceptional / lag(cost_exceptional, year_report - 2019)-1,
                               cost_exceptional / lag(cost_exceptional, 1)-1),
    controllable_dif_perc = if_else (year_comparison == 2019,
                                cost_controllable / lag(cost_controllable, year_report - 2019) -1,
                                cost_controllable / lag(cost_controllable, 1) -1),
         NULL
  ) %>% 
  filter(year_data == year_report)  %>% 
  select(
    year_data,
    year_comparison,
    staf_dif,
    non_staff_operating_dif,
    depreciation_dif,
    coc_dif,
    exceptional_dif,
    controllable_dif,
    
    staf_dif_perc,
    non_staff_operating_dif_perc,
    depreciation_dif_perc,
    coc_dif_perc,
    exceptional_dif_perc,
    controllable_dif_perc
  ) %>% 
  pivot_longer(-c(year_data, year_comparison), 
               names_to = c("metric", "type"), # Separate column names into "metric" and "type"
               names_pattern = "^(.*?)(_perc)?$", # Match base metric and optional "_perc"
               values_to = "value"
  ) %>%
  mutate(
    type = if_else(type=="", "value", type) # Assign "value" to columns without "_perc"
  )


data_plot <- data_dif %>% 
  pivot_wider(id_cols = c(-year_data, year_comparison), 
              names_from = "type",
              values_from = "value") %>% 
  mutate(label = case_when(
    metric == 'staf_dif' ~ "Staff costs",
    metric == 'non_staff_operating_dif' ~ "Non-staff op. costs",
    metric == 'depreciation_dif' ~ "Depreciation costs",
    metric == 'coc_dif' ~ "Cost of capital",
    metric == 'exceptional_dif' ~ "Exceptional costs",
    metric == 'controllable_dif' ~ "Total",
  )
  )  %>% 
  mutate(label = factor(label, levels = c("Staff costs",
                                          "Non-staff op. costs",
                                          "Depreciation costs",
                                          "Cost of capital",
                                          "Exceptional costs",
                                          "Total"
                                          )
                        )
         ) %>% 
  janitor::clean_names()
  

data_plot_2019 <-data_plot %>% filter(year_comparison == 2019) %>% 
  mutate(empty = NA)
data_plot_py <-data_plot %>% filter(year_comparison == year_report-1)

#calculate range for x axis 
# xrange_max <- max(abs(data_plot$YOY), abs(data_plot$VS_2019)) +0.05

# draw costs plot
p <- function(myfont, mywidth, myheight, vmargin, myautosize) {
  plot_ly(
      data = data_plot_py,
      width = mywidth,
      height = myheight,
      x = ~ value/10^6 ,
      y = ~ label,
      marker = list(color =c('#003366', '#78B4F0', '#A0A954', '#E2F063', '#E0584F'
                             , '#7F7F7F'
                             ),
                    pattern = list(shape = "none")),
      name = paste0(year_report, " vs. ", stringr::str_sub(as.character(year_report-1), 3,4)),
      text = ~ paste0("<b><i>",if_else(perc>=0, "+", ""),
                      format(round(perc*100,1), nsmall=1, trim = TRUE), "%</i></b>"),
      textangle = 0,
      textposition = "outside",
      textfont = list(color = 'black', size = myfont),
      # cliponaxis = FALSE,
      type = "bar",
      orientation = 'h',
      hovertemplate = paste('%{x:.1f} M€'),
      showlegend = F
    ) %>% 
    add_trace(
      data = data_plot_2019,
      name = paste0(year_report, " vs. 19"),
      marker = list(color =c('#003366', '#78B4F0', '#A0A954', '#E2F063', '#E0584F'
                             , '#7F7F7F'),
                    pattern = list(shape = "\\")),
      text = ~ paste0("<i>",if_else(perc>=0, "+", ""),
                      format(round(perc*100,1), nsmall=1, trim = TRUE), "%</i>"),
      textfont = list(color = '#002A5F', size = myfont),
      cliponaxis = FALSE

    ) %>%
    add_trace(
      data = data_plot_2019,
      x = 0,
      y = ~ label,
      name = paste0(year_report, " vs. 19"),
      marker = list(color =c('black'),
                    pattern = list(shape = "none")),
      visible = 'legendonly',
      showlegend = TRUE
      
    ) %>%
    add_trace(
      data = data_plot_2019,
      x = 0,
      y = ~ label,
      name = paste0(year_report, " vs. ", stringr::str_sub(as.character(year_report-1), 3,4)),
      marker = list(color =c('black'),
                    pattern = list(shape = "\\")),
      visible = 'legendonly',
      showlegend = TRUE
      
    ) %>%
    layout(
        barmode = "group",
        xaxis = list(
          title = "€M",
          # fixedrange = TRUE,
          range = c(-610, 610),
          dtick = 300,
          # automargin = T,
          # tickvals = c(),
          # tickformat=",.0%",
          showticklabels = TRUE,
          gridcolor = '#BFBFBF',
          # autotick = T, 
          # mirror = T,
          linecolor='#BFBFBF',
          zerolinecolor='#BFBFBF',
          # domain=c(0.6,1),
          zeroline = F, showline = F, showgrid = T
        ),
        yaxis = list(
          title = "",
          linewidth=1, linecolor='#BFBFBF',
          # titlefont   = list(size = 13),
          autorange = "reversed",
          fixedrange = TRUE,
          autotick = F,
          showticklabels = TRUE,
          gridcolor = '#BFBFBF',
          tickson="boundaries",
          # domain=c(0,1),
          zeroline = F, showline = F, showgrid = F, mirror = T
        ),
        legend = list(
          x = 100, y = 0.5,
          traceorder = "normal",
          font = list(color = 'black')
        ),
        bargap = 0.3,
        hovermode = "y unified",
        autosize = myautosize,
        margin = list(b = vmargin, l=0, r=0),
        plot_bgcolor = 'white',
        uniformtext=list(minsize=10, mode='show')
      ) %>%
      config(responsive = FALSE,
             displaylogo = FALSE,
             displayModeBar = F) 
}



if (knitr::is_latex_output()) {
  p(12, 700, 350, 40, 'F')
  
} else{
  p(12, NULL, NULL, 40, 'T')
}



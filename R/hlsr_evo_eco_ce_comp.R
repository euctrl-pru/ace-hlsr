## libraries
library(dplyr)
library(stringr)
library(readxl)
library(plotly)
library(stringr)
library(tidyr)
library(data.table)
library(here)
## data source
source(here("data_source.R"))

# import data
data_raw <- read_xlsx(
                      # paste0(data_folder, data_file),
                      here("data","hlsr2021_data.xlsx"),
                      sheet = "E_EcoCostEff",
                      range = cell_limits(c(7, 1), c(NA, 3))) %>% mutate_at(c(2,3), ~replace_na(.,0)) %>% 
  as_tibble() %>% 
  rename(YEAR_DATA = Year)

data_raw_extra <-  read_xlsx(
                            # paste0(data_folder, data_file),
                            here("data","hlsr2021_data.xlsx"),
                            sheet = "E_EcoCostEff",
                            range = cell_limits(c(7, 5), c(NA, 7))) %>%
  as_tibble() %>% mutate_at(c(2,3), ~replace_na(.,0)) %>% 
  mutate(FIN_CE = COST_CONTROLLABLE/COMPOSITE_FLIGHTHOUR) 

cost_delay <-  read_xlsx(
                        # paste0(data_folder, data_file),
                        here("data","hlsr2021_data.xlsx"),
                        sheet = "E_EcoCostEff",
                        range = cell_limits(c(7, 9), c(NA, 10))) %>%
  as_tibble() %>% 
  rename (COST_DELAY = Total)

data_merged = merge(x=data_raw, y=data_raw_extra, by="YEAR_DATA")
data_merged = merge(x=data_merged, y=cost_delay, by="YEAR_DATA")

data_calc <- data_merged %>% 
  mutate(DELAY_CPH = (TDM_ERT_ALL_REASON*COST_DELAY+TDM_ARP_ALL_REASON)/COMPOSITE_FLIGHTHOUR,
         COST_EV = case_when(YEAR_DATA == min(YEAR_DATA) ~ 0,
                             TRUE ~ COST_CONTROLLABLE/lag(COST_CONTROLLABLE)-1),
         DELAY_CPH_EV = case_when(YEAR_DATA == min(YEAR_DATA) ~ 0,
                                  TRUE ~ DELAY_CPH/lag(DELAY_CPH)-1),
         CPH_EV = case_when(YEAR_DATA == min(YEAR_DATA) ~ 0,
                            TRUE ~ COMPOSITE_FLIGHTHOUR/lag(COMPOSITE_FLIGHTHOUR)-1),
  ) %>% 
  mutate(X_LABELS = paste(lag(YEAR_DATA),"-",substr(YEAR_DATA,3,4), sep = "")) %>%
  filter(YEAR_DATA > min(YEAR_DATA)+1) %>% 
  select(X_LABELS, COST_EV, CPH_EV, DELAY_CPH_EV) %>% 
  pivot_longer(cols = c("COST_EV","CPH_EV","DELAY_CPH_EV"), names_to = "TYPE", values_to ="VALUE")

#  data for plot - I had to split in two plots because I could not control the conditional label styling

plot_div <- 0.14

data_plot <- data_calc  %>% 
  mutate(PLOT1 = if_else(abs(VALUE) >  plot_div, 0, VALUE),
         PLOT2 = if_else(abs(VALUE) <=  plot_div, 0, VALUE),
         LABELS = if_else(abs(VALUE) < 0.0005, round(VALUE,4), round(VALUE,3)),
         LABELS = if_else(VALUE >=0, paste0("+", LABELS*100,"%"), paste0(LABELS*100,"%"))
  ) %>% 
  # mutate(TPOS = if_else(abs(VALUE) > 0.2, "end", "start")) %>%
  # mutate(TFONTCOLOR = if_else(TPOS == "inside" & TYPE != "CPH_EV", "white", "black")) %>%
  # mutate(TANGLE = if_else(abs(VALUE) > 0.2, -90, 0)) %>%
  mutate(TYPE = case_when(
    TYPE == "COST_EV" ~ "ATM/CNS provision costs",
    TYPE == "CPH_EV" ~ "Composite flight-hours",
    TYPE == "DELAY_CPH_EV" ~ "Unit costs of ATFM delays"
  )
  ) %>% 
  mutate(TYPE = factor(TYPE, 
                       levels = c("ATM/CNS provision costs",
                                  "Composite flight-hours",
                                  "Unit costs of ATFM delays"))
  )


##vertical line function
vline <- function(x = 0, color = "black") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    # xref = "paper",
    line = list(color = color, dash="dot", width = 1)
  )
}

# plot 
p <- plot_ly(
  data_plot,
  x = ~X_LABELS,
  y = ~PLOT1,
  color = ~TYPE,
  colors = c("#003366","#DEA900", "#993366"),
  text = ~ if_else(abs(VALUE) > plot_div, "", LABELS),
  textangle = -90,
  textposition = "outside",
  textfont = list(
    color = "black", size = 10),
  type = "bar",
  hoverinfo = "none"
) %>% 
  add_trace( inherit = FALSE,
             x = ~X_LABELS,
             y = ~PLOT2,
             color = ~TYPE,
             colors = c("#003366","#DEA900", "#993366"),
             xaxis= "x2",
             yaxis = "y2",
             text = ~ if_else(abs(VALUE) > plot_div, LABELS,"" ),
             textangle = -90,
             textposition = "inside",
             insidetextanchor = "start",
             textfont = list(
               color = "white", size = 10),
             type = "bar",
             showlegend = FALSE,
             hoverinfo = "none"
  ) %>%
  layout (plot_bgcolor = "transparent",
          font = list(family = "Helvetica"),
          legend = list(orientation = 'h',
                        font = list(size = 11),
                        y = -0.1,
                        x = 0.0,
                        bgcolor = 'transparent'),
          uniformtext=list(minsize=8, mode='show'), #this is important so it does not autofit fonts
          xaxis = list(
            title = "",
            showgrid = F
          ),
          yaxis = list(
            title = "",
            titlefont = list(size = 12),
            cliponaxis = FALSE,
            dtick = 0.05,
            tickformat=",.0%", ticks = 'outside',
            zeroline = T, showline = T, showgrid = F,
            range = list(-0.2,0.2)
          ),
          yaxis2 = list(overlaying = "y",
                        title = "",
                        cliponaxis = FALSE,
                        showticklabels = FALSE,
                        zeroline = T, showline = T, showgrid = F,
                        range = list(-0.2,0.2)
          ),
          xaxis2 = list(overlaying = "y", showticklabels = FALSE),
          shapes = list(vline(0.5), vline(1.5), vline(2.5), vline(3.5))
          
  ) %>% 
  config( responsive = FALSE,
          displaylogo = FALSE,
          displayModeBar = F
          # modeBarButtons = list(list("toImage"))
  )
p

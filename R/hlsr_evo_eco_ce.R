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
library(magick)

# import data
data_raw <- read_xlsx(
                      # paste0(data_folder, data_file),
                      paste0(data_folder,data_file ),
                      sheet = "E_EcoCostEff",
                      range = cell_limits(c(7, 1), c(NA, 3))) %>% mutate_at(c(2,3), ~replace_na(.,0)) %>% 
  as_tibble() %>% 
  rename(YEAR_DATA = Year)

data_raw_extra <-  read_xlsx(
                            # paste0(data_folder, data_file),
                            paste0(data_folder,data_file ),
                            sheet = "E_EcoCostEff",
                            range = cell_limits(c(7, 5), c(NA, 7))) %>%
  as_tibble() %>% mutate_at(c(2,3), ~replace_na(.,0)) %>% 
  mutate(FIN_CE = COST_CONTROLLABLE/COMPOSITE_FLIGHTHOUR) 

cost_delay <-  read_xlsx(
                          # paste0(data_folder, data_file),
                        paste0(data_folder,data_file ),
                        sheet = "E_EcoCostEff",
                          range = cell_limits(c(7, 9), c(NA, 10))) %>%
                          as_tibble() %>% 
                          rename (COST_DELAY = Total)

data_merged = merge(x=data_raw, y=data_raw_extra, by="YEAR_DATA")
data_merged = merge(x=data_merged, y=cost_delay, by="YEAR_DATA")

data_calc <- data_merged %>% 
  mutate(DELAY_ERT_CPH = TDM_ERT_ALL_REASON*COST_DELAY/COMPOSITE_FLIGHTHOUR,
         DELAY_ARP_CPH = TDM_ARP_ALL_REASON*COST_DELAY/COMPOSITE_FLIGHTHOUR,
         ECO_CE = FIN_CE+DELAY_ERT_CPH+DELAY_ARP_CPH,
         ECO_CE_EVO = case_when(YEAR_DATA == min(YEAR_DATA) ~ 0,
                                TRUE ~ ECO_CE/lag(ECO_CE)-1)
  ) %>% 
  select(YEAR_DATA, FIN_CE, DELAY_ERT_CPH, DELAY_ARP_CPH, ECO_CE, ECO_CE_EVO) 
# %>% 
  # filter(YEAR_DATA != min(YEAR_DATA))

# prepare data for plot
data_prep <- data_calc  %>% 
  mutate(LABELS = round(ECO_CE_EVO,3)
  ) %>% 
  mutate(LABELS = if_else(ECO_CE_EVO >=0, paste0("+", format(LABELS*100, nsmall=1),"%"), paste0(format(LABELS*100, nsmall=1),"%"))
  ) %>% 
  mutate(LABELS = case_when(YEAR_DATA == min(YEAR_DATA) ~ "",
                            .default = LABELS)
         )        # JC made me remove the first label

# calculate the joining lines
# https://stackoverflow.com/questions/58825957/connect-bars-with-lines-in-r-plotly
DF <- data_prep %>% select (FIN_CE,DELAY_ERT_CPH,DELAY_ARP_CPH, YEAR_DATA, LABELS) %>% 
  rename(`ATM/CNS provision costs per composite flight-hour` = FIN_CE,
         `Unit cost of en-route ATFM delays` = DELAY_ERT_CPH,
         `Unit cost of airport ATFM delays` = DELAY_ARP_CPH)

setDT(DF)

DT <- melt.data.table(DF, id.vars = c("YEAR_DATA", "LABELS"))
DT[, c("label_group", "cumsum_by_label") := .(.GRP, cumsum(value)), by = YEAR_DATA]

lineDT <- rbindlist(list(DT[, .(
  label_group = label_group - 0.3, # this value changes with the bargap
  cumsum_by_label = cumsum_by_label,
  variable = variable
)],
DT[, .(
  label_group = label_group + 0.3, # this value changes with the bargap
  cumsum_by_label = cumsum_by_label,
  variable = variable
)]))


# break down the lines in different sets to avoid flat segments
mylist <- sort(unique(lineDT$label_group))

lineDT1 <- lineDT %>% arrange(variable, label_group) %>%
  filter(label_group == mylist[2] | label_group == mylist[3])

lineDT2 <- lineDT %>% arrange(variable, label_group) %>%
  filter(label_group == mylist[4] | label_group == mylist[5])

lineDT3 <- lineDT %>% arrange(variable, label_group) %>%
  filter(label_group == mylist[6] | label_group == mylist[7])

lineDT4 <- lineDT %>% arrange(variable, label_group) %>%
  filter(label_group == mylist[8] | label_group == mylist[9])

lineDT5 <- lineDT %>% arrange(variable, label_group) %>%
  filter(label_group == mylist[10] | label_group == mylist[11])

# plot
p <- function(myfont, mywidth, myheight, myvoffset) { 
  plot_ly(
  DT,
  height = myheight,
  width = mywidth,
  x = ~ label_group,
  y = ~ value,
  color = ~ variable,
  type = "bar",
  colors = c('#78B4F0', '#E0584F', '#E1F060'),
  legendgroup =  ~ variable,
  hoverinfo = "none",
  showlegend = TRUE
) %>%
  layout(autosize = T, 
         barmode = 'stack',
         bargap = 0.4,
         font = list(family = "Helvetica"),
         legend = list(orientation = 'h',
                       traceorder = 'reversed', #for some reason this does not work
                       font = list(size = myfont + 3),
                       y = -0.1,
                       x = 0.0,
                       bgcolor = 'transparent'),
         uniformtext=list(minsize = myfont, mode='show'), #this is important so it does not autofit fonts
         xaxis = list(
           title = "",
           tickfont = list(size = myfont+3),
           ticktext = ~ YEAR_DATA,
           tickvals = ~ label_group,
           tickmode = "array"
         ),
         yaxis = list(
           title = paste0("\u20AC per composite flight-hour (", max(DT$YEAR_DATA)," prices)"),
           titlefont = list(size = myfont + 4),
           tickfont = list(size = myfont+3),
           showgrid = F)
  ) %>%
  add_annotations(
    data = DT[, .(maxval = max(cumsum_by_label),
                  label_group = unique(label_group),
                  LABELS = unique(LABELS)), by = YEAR_DATA],
    inherit = FALSE,
    text = ~ LABELS,
    xref = 'x',
    yref = 'y',
    y = ~ maxval + myvoffset,
    x = ~ label_group,
    showarrow = FALSE,
    yshift = 10,
    font = list(size=myfont + 3)
  ) %>%
  add_lines(
    data = lineDT1,
    inherit = FALSE,
    x = ~ c(label_group),
    y = ~ cumsum_by_label,
    color = ~ variable,
    line = list(color = 'black', width = 1, dash = "dot"),
    legendgroup =  ~ variable,
    showlegend = FALSE,
    hoverinfo = "none"
  )%>%
  add_lines(
    data = lineDT2,
    inherit = FALSE,
    x = ~ c(label_group),
    y = ~ cumsum_by_label,
    color = ~ variable,
    line = list(color = 'black', width = 1, dash = "dot"),
    legendgroup =  ~ variable,
    showlegend = FALSE,
    hoverinfo = "none"
  )%>%
  add_lines(
    data = lineDT3,
    inherit = FALSE,
    x = ~ c(label_group),
    y = ~ cumsum_by_label,
    color = ~ variable,
    line = list(color = 'black', width = 1, dash = "dot"),
    legendgroup =  ~ variable,
    showlegend = FALSE,
    hoverinfo = "none"
  )%>%
  add_lines(
    data = lineDT4,
    inherit = FALSE,
    x = ~ c(label_group),
    y = ~ cumsum_by_label,
    color = ~ variable,
    line = list(color = 'black', width = 1, dash = "dot"),
    legendgroup =  ~ variable,
    showlegend = FALSE,
    hoverinfo = "none"
  )%>%
  add_lines(
    data = lineDT5,
    inherit = FALSE,
    x = ~ c(label_group),
    y = ~ cumsum_by_label,
    color = ~ variable,
    line = list(color = 'black', width = 1, dash = "dot"),
    legendgroup =  ~ variable,
    showlegend = FALSE,
    hoverinfo = "none"
  ) %>% 
  config( responsive = FALSE,
          displaylogo = FALSE,
          displayModeBar = F
          # modeBarButtons = list(list("toImage"))
  )
}

p(8, NULL, NULL, 0)

# fig_dir <- 'figures/'
# fig_name <- "figure-3-1-1-hlsr_evo_eco_ce.png"
# 
# invisible(export(p(17, 600, 600, 10), paste0(fig_dir, fig_name)))
# invisible(figure <- image_read(paste0(fig_dir,fig_name)))
# invisible(cropped <- image_crop(figure, "600x600"))
# invisible(image_write(cropped, paste0(fig_dir, fig_name)))


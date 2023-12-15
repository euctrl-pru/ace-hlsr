## libraries
library(dplyr)
library(here)
library(readxl)
library(stringr)
library(tidyr)

source(here("data_source.R"))


# import data for 3-2
data_raw <- read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder,data_file ),
  sheet = "F_Eco CE",
  range = cell_limits(c(7, 1), c(NA, 3))) %>% mutate_at(c(2,3), ~replace_na(.,0)) %>% 
  as_tibble() 

data_raw_extra <-  read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder,data_file ),
  sheet = "F_Eco CE",
  range = cell_limits(c(7, 5), c(NA, 7))) %>%
  as_tibble() %>% mutate_at(c(2,3), ~replace_na(.,0)) %>% 
  mutate(FIN_CE = COST_CONTROLLABLE/COMPOSITE_FLIGHTHOUR) 

cost_delay <-  read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder,data_file ),
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
         QUART3 = quantile(ECO_CE, 0.75))
   
# extract quartiles for text
q1_3_2 <- round(max(data_prep$QUART1), digits = 0)
q3_3_2 <- round(max(data_prep$QUART3), digits = 0)

#-------------------------------
# import data for 4-4
data_raw <- read_xlsx(
  paste0(data_folder, data_file),
  # here("data", data_file ),
  sheet = "F_Fin CE",
  range = cell_limits(c(7, 1), c(NA, 4))) %>%
  as_tibble() %>% 
  rename(VALUE=2, COST =3, CFH =4) %>% 
  mutate(QUART1 = quantile(VALUE, 0.25),
         QUART3 = quantile(VALUE, 0.75)
  ) 

# extract quartiles for text
q1_4_4 <- round(max(data_raw$QUART1), digits = 0)
q3_4_4 <- round(max(data_raw$QUART3), digits = 0)

#-------------------------------
# import data for 4-5
data_raw <-  read_xlsx(
  paste0(data_folder, data_file),
  # here("data", data_file ),
  sheet = "F_Prod",
  range = cell_limits(c(7, 1), c(NA, 4))) %>%
  as_tibble() %>% 
  rename(VALUE=2, HOURS =3, CFH =4) %>%
  select(ANSP_NAME, VALUE) %>% 
  mutate(QUART1 = quantile(VALUE, 0.25),
         QUART3 = quantile(VALUE, 0.75)
  ) 

# extract quartiles for text
q1_4_5 <- round(max(data_raw$QUART1), digits = 2)
q3_4_5 <- round(max(data_raw$QUART3), digits = 2)

#-------------------------------
# import data for 4-6
data_raw <- read_xlsx(
  # paste0(data_folder, data_file),
  paste0(data_folder, data_file),
  sheet = "F_ATCO cost per h",
  range = cell_limits(c(7, 1), c(NA, 4))) %>%
  as_tibble() %>% 
  rename(VALUE=2, COST =3, HOUR =4) %>%
  select(ANSP_NAME, VALUE) %>% 
  mutate(QUART1 = quantile(VALUE, 0.25),
         QUART3 = quantile(VALUE, 0.75)
  ) 

# extract quartiles for text
q1_4_6 <- round(max(data_raw$QUART1), digits = 0)
q3_4_6 <- round(max(data_raw$QUART3), digits = 0)

#-------------------------------
# import data for 4-7
data_raw <- read_xlsx(
  paste0(data_folder, data_file),
  # here("data", data_file ),
  sheet = "F_Support",
  range = cell_limits(c(7, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  mutate(QUART1 = quantile(`Support costs per composite flight-hour`, 0.25),
         QUART3 = quantile(`Support costs per composite flight-hour`, 0.75)
  ) 

# extract quartiles for text
q1_4_7 <- round(max(data_raw$QUART1), digits = 0)
q3_4_7 <- round(max(data_raw$QUART3), digits = 0)
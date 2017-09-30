library(tidyverse)

unzip('inst/extdata/pfc_tra.zip', exdir = 'inst/extdata')

#datos análogos del aerogenerador 15
ae15 <- read_csv('inst/extdata/pfc_tra/ae15.csv')

#datos análogos del aerogenerador 19
ae19 <- read_csv('inst/extdata/pfc_tra/ae19.csv')

#datos análogos del aerogenerador 20
ae20 <- read_csv('inst/extdata/pfc_tra/ae20.csv')

#registro de eventos relacionados con fallo en el sistema de trasmisión de los aerogeneradores del parque
ae_tra <- read_csv('inst/extdata/pfc_tra/ae_tra.csv')

ae15_tra <- ae15 %>% left_join(ae_tra %>% filter(model == 'AE15')) %>%
  filter(!is.na(model))

#join datasets
aero <-
  (ae15 %>% mutate(aero = 'aero15')) %>%
  rbind((ae19 %>% mutate(aero = 'aero19'))) %>%
  rbind((ae20 %>% mutate(aero = 'aero20'))) %>%
  arrange(aero, date_time)

#select variables
aero <- aero %>%
  select (
    date_time,
    aero,
    power,
    bearing_temp,
    gen_1_speed,
    gen_bearing_temp,
    temp_oil_mult,
    temp_out_nacelle
  )

#no duplicated measures
aero %>% select(date_time, aero) %>% length == aero %>% select(date_time, aero) %>% unique %>% length

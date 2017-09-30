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

#power
aero %>%
  ggplot(aes(aero, power)) +
  geom_boxplot()

#bearing_temp
aero %>%
  ggplot(aes(aero, bearing_temp)) +
  geom_boxplot()

aero <- aero %>% mutate(bearing_temp = ifelse(bearing_temp < 0, NA, bearing_temp))

aero %>%
  mutate() %>%
  ggplot(aes(aero, bearing_temp)) +
  geom_boxplot()

#gen_1_speed
aero %>%
  ggplot(aes(aero, gen_1_speed)) +
  geom_boxplot()

#gen_bearing_temp
aero %>%
  ggplot(aes(aero, gen_bearing_temp)) +
  geom_boxplot()

aero <- aero %>% mutate(gen_bearing_temp = ifelse(gen_bearing_temp <= 0, NA, gen_bearing_temp))

aero %>%
  ggplot(aes(aero, gen_bearing_temp)) +
  geom_boxplot()

#temp_oil_mult
aero %>%
  ggplot(aes(aero, temp_oil_mult)) +
  geom_boxplot()

aero <- aero %>% mutate(temp_oil_mult = ifelse(temp_oil_mult <= 0, NA, temp_oil_mult))

aero %>%
  ggplot(aes(aero, temp_oil_mult)) +
  geom_boxplot()

#temp_out_nacelle
aero %>%
  ggplot(aes(aero, temp_out_nacelle)) +
  geom_boxplot()

aero <- aero %>% mutate(temp_out_nacelle = ifelse(temp_out_nacelle <= 0, NA, temp_out_nacelle))

aero %>%
  ggplot(aes(aero, temp_out_nacelle)) +
  geom_boxplot()

#missings
aggr_plot <- aggr(
  aero,
  col=c('navyblue','red'), 
  numbers=TRUE, 
  sortVars=TRUE, 
  labels=names(aero),
  cex.axis=.7,
  gap=0,
  ylab=c("Histogram of missing data","Pattern")
)

aero <- aero[complete.cases(aero),]

M <- cor(aero %>% select(-date_time, -aero))
corrplot(M, method="circle")

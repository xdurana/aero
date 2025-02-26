library(tidyverse)
library(corrplot)
library(VIM)

#power
aero %>%
  ggplot(aes(aero, power)) +
  geom_boxplot()

aero %>%
  ggplot(aes(date_time, power)) +
  geom_point() +
  geom_smooth(aes(date_time, power, color=aero)) +
  facet_grid(aero ~ .)

#bearing_temp
aero %>%
  ggplot(aes(aero, bearing_temp)) +
  geom_boxplot()

aero <- aero %>% mutate(bearing_temp = ifelse(bearing_temp <= 0, NA, bearing_temp))

aero %>%
  mutate() %>%
  ggplot(aes(aero, bearing_temp)) +
  geom_boxplot()

aero %>%
  ggplot(aes(date_time, bearing_temp)) +
  geom_point() +
  geom_smooth(aes(date_time, bearing_temp, color=aero)) +
  facet_grid(aero ~ .)

#gen_1_speed
aero %>%
  ggplot(aes(aero, gen_1_speed)) +
  geom_boxplot()

aero %>%
  ggplot(aes(date_time, gen_1_speed)) +
  geom_point() +
  geom_smooth(aes(date_time, gen_1_speed, color=aero)) +
  facet_grid(aero ~ .)

#gen_bearing_temp
aero %>%
  ggplot(aes(aero, gen_bearing_temp)) +
  geom_boxplot()

aero <- aero %>% mutate(gen_bearing_temp = ifelse(gen_bearing_temp <= 0, NA, gen_bearing_temp))

aero %>%
  ggplot(aes(aero, gen_bearing_temp)) +
  geom_boxplot()

aero %>%
  ggplot(aes(date_time, gen_bearing_temp)) +
  geom_point() +
  geom_smooth(aes(date_time, gen_bearing_temp, color=aero)) +
  facet_grid(aero ~ .)

#temp_oil_mult
aero %>%
  ggplot(aes(aero, temp_oil_mult)) +
  geom_boxplot()

aero <- aero %>% mutate(temp_oil_mult = ifelse(temp_oil_mult <= 0, NA, temp_oil_mult))

aero %>%
  ggplot(aes(aero, temp_oil_mult)) +
  geom_boxplot()

aero %>%
  ggplot(aes(date_time, temp_oil_mult)) +
  geom_point() +
  geom_smooth(aes(date_time, temp_oil_mult, color=aero)) +
  facet_grid(aero ~ .)

#temp_out_nacelle
aero %>%
  ggplot(aes(aero, temp_out_nacelle)) +
  geom_boxplot()

aero <- aero %>% mutate(temp_out_nacelle = ifelse(temp_out_nacelle <= 0, NA, temp_out_nacelle))

aero %>%
  ggplot(aes(aero, temp_out_nacelle)) +
  geom_boxplot()

aero %>%
  ggplot(aes(date_time, temp_out_nacelle)) +
  geom_point() +
  geom_smooth(aes(date_time, temp_out_nacelle, color=aero)) +
  facet_grid(aero ~ .)

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

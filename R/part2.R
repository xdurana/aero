library(tidyverse)
library(caret)

aero15 <- aero %>% filter(aero == 'aero15') %>% select(-date_time, -aero)
aero19 <- aero %>% filter(aero == 'aero19') %>% select(-date_time, -aero)
aero20 <- aero %>% filter(aero == 'aero20') %>% select(-date_time, -aero)
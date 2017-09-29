library(tidyverse)
library(VIM)
library(corrplot)

plot_feature <- function(ds, name) {
  par(mfrow=c(2,2))
  x <- ds %>% .[[name]]
  hist(x, breaks = 100)
  boxplot(x, col = rgb(0,0,1,0.5), main = sprintf("Boxplot of %s", name))
  qqnorm(x, main = "Normal QQ Plot - y")
  qqline(x, col = "red")
  #shapiro.test(ds[1:200,name])
}

clean_aero <- function(aero) {
  
  #registro de eventos relacionados con fallo en el sistema de trasmisión de los aerogeneradores del parque
  ae_tra <- read_csv('inst/extdata/pfc_tra/ae_tra.csv')
  
  #left join info status
  aero_sel <- aero %>% left_join(ae_tra %>% filter(model == 'aero'))
  
  #select features to analyze
  aero_sel <- aero_sel %>%
    select (
      date_time,
      power,
      bearing_temp,
      gen_1_speed,
      gen_bearing_temp,
      temp_oil_mult,
      temp_out_nacelle,
      code_desc,
      status
    )
  
  aero_sel <- aero %>%
    select (
      date_time,
      power,
      bearing_temp,
      gen_1_speed,
      gen_bearing_temp,
      temp_oil_mult,
      temp_out_nacelle
    )
  
  #no duplicated measures
  aero_sel$date_time %>% length == aero_sel$date_time %>% unique %>% length
  
  #summary
  summary(aero_sel)
  
  #power
  aero_sel %>% plot_feature('power')
  ggplot(aero_sel, aes(date_time, power)) + geom_line() + scale_x_datetime() + xlab("") + ylab("power")
  summary(aero_sel$power)
  
  #bearing_temp
  aero_sel %>% plot_feature('bearing_temp')
  aero_sel$bearing_temp[aero_sel$bearing_temp <= 0] <- NA
  aero_sel %>% plot_feature('bearing_temp')
  ggplot(aero_sel, aes(date_time, bearing_temp)) + geom_line() + scale_x_datetime() + xlab("") + ylab("bearing_temp")
  summary(aero_sel$bearing_temp)
  
  #gen_1_speed
  aero_sel %>% plot_feature('gen_1_speed')
  ggplot(aero_sel, aes(date_time, gen_1_speed)) + geom_line() + scale_x_datetime() + xlab("") + ylab("gen_1_speed")
  summary(aero_sel$gen_1_speed)
  
  #gen_bearing_temp
  aero_sel %>% plot_feature('gen_bearing_temp')
  aero_sel$gen_bearing_temp[aero_sel$gen_bearing_temp <= 0] <- NA
  aero_sel %>% plot_feature('gen_bearing_temp')
  ggplot(aero_sel, aes(date_time, gen_bearing_temp)) + geom_line() + scale_x_datetime() + xlab("") + ylab("gen_bearing_temp")
  summary(aero_sel$gen_bearing_temp)
  
  #temp_oil_mult
  aero_sel %>% plot_feature('temp_oil_mult')
  aero_sel$temp_oil_mult[aero_sel$temp_oil_mult <= 0] <- NA
  aero_sel %>% plot_feature('temp_oil_mult')
  ggplot(aero_sel, aes(date_time, temp_oil_mult)) + geom_line() + scale_x_datetime() + xlab("") + ylab("temp_oil_mult")
  summary(aero_sel$temp_oil_mult)
  
  #temp_out_nacelle
  aero_sel %>% plot_feature('temp_out_nacelle')
  aero_sel$temp_out_nacelle[aero_sel$temp_out_nacelle <= 0] <- NA
  aero_sel %>% plot_feature('temp_out_nacelle')
  ggplot(aero_sel, aes(date_time, temp_out_nacelle)) + geom_line() + scale_x_datetime() + xlab("") + ylab("temp_out_nacelle")
  
  aero_sel %>%
    arrange(
      date_time
    )
  
  aggr_plot <- aggr(
    aero_sel,
    col=c('navyblue','red'), 
    numbers=TRUE, 
    sortVars=TRUE, 
    labels=names(aero_sel),
    cex.axis=.7,
    gap=0,
    ylab=c("Histogram of missing data","Pattern")
  )
  
  aero_sel_complete <- aero_sel[complete.cases(aero_sel),] %>%
    arrange(date_time) %>%
    select(-date_time)
  
  M <- cor(aero_sel_complete)
  corrplot(M, method="circle")
  
  aero_sel_complete
}

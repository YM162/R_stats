# Carga de datos previa a los ejercicios.
load("coronavirus.Rdata")
install.packages("gganimate")
install.packages("gifski")
library(gifski)
library(gganimate)
library(ggplot2)
library(tidyverse)
library(janitor)
library(scales)
# Las fechas donde se han registrado los datos pueden sernos útiles
fechas = unique(coronavirus$date)
coronavirus.conf = coronavirus[coronavirus$type=="confirmed",]


#Necesito 5 data frames, uno de cada país, y luego unirlos
#Cada data frame tiene que tener las filas que le corresponden, confirmed, y modificado los cases por cumsum(cases)

corona.esp.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="Spain",select=c(cases, date, Country.Region))
corona.esp.cum <- corona.esp.cum %>%
  mutate(cases = cumsum(cases))
corona.it.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="Italy",select=c(cases, date, Country.Region))
corona.it.cum <- corona.it.cum %>%
  mutate(cases = cumsum(cases))
corona.us.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="US",select=c(cases, date, Country.Region))
corona.us.cum <- corona.us.cum %>%
  mutate(cases = cumsum(cases))
corona.an.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="Andorra",select=c(cases, date, Country.Region))
corona.an.cum <- corona.an.cum %>%
  mutate(cases = cumsum(cases))
corona.por.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="Portugal",select=c(cases, date, Country.Region))
corona.por.cum <- corona.por.cum %>%
  mutate(cases = cumsum(cases))
corona.kr.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="Korea, South",select=c(cases, date, Country.Region))
corona.kr.cum <- corona.kr.cum %>%
  mutate(cases = cumsum(cases))
total.acc <- rbind(corona.esp.cum,corona.it.cum,corona.us.cum,corona.an.cum,corona.por.cum,corona.kr.cum)








total.acc_smoother <- total.acc %>%
  group_by(Country.Region) %>%
  # Do somewhat rough interpolation for ranking
  # (Otherwise the ranking shifts unpleasantly fast.)
  complete(date = full_seq(date, 1)) %>%
  mutate(cases = spline(x = date, y = cases, xout = date)$y) %>%
  group_by(date) %>%
  mutate(rank = min_rank(-cases) * 1) %>%
  ungroup()
  
  # Then interpolate further to quarter years for fast number ticking.
  # Interpolate the ranks calculated earlier.
  group_by(Country.Region) %>%
  complete(date = full_seq(date, .5)) %>%
  mutate(cases = spline(x = date, y = cases, xout = date)$y) %>%
  # "approx" below for linear interpolation. "spline" has a bouncy effect.
  mutate(rank =      approx(x = date, y = cases,      xout = date)$y) %>%
  ungroup()  %>% 
  arrange(Country.Region,date)
  
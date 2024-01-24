install.packages("nycflights13")
install.packages("dplyr")
install.packages("knitr")

library(nycflights13)
library(dplyr)
library(knitr)

ua_delay <- filter(flights, carrier == "UA", year == 2013, month == 1, arr_delay >= 240)
ua_final <- select(ua_delay, flight, arr_delay)
kable(ua_final)
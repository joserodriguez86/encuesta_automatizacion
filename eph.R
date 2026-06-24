pacman::p_load(tidyverse, eph, sjmisc)

eph <- get_microdata(year = 2025, period = 2, type = "individual")


# Calculo de trabajadores en ciudades seleccionadas--------------

eph %>% 
  filter(ESTADO == 1) %>%
  frq(AGLOMERADO, weights = PONDERA)

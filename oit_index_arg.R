pacman::p_load(tidyverse, eph, gtsummary, survey)

# Bases -------------
oit_index <- read_delim("bases/oit index.txt", delim = ";")

eph <- get_microdata(year = 2025, period = 2, type = "individual")

load("bases/base_2021.RData")

base_picto <- haven::read_dta("bases/base2024_argentina.dta")


# Pegado de índice a bases -----------

oit_index <- oit_index %>%
  select(!`Occupation Name`) %>% 
  rename("ciuo" = "4-digit code") %>% 
  mutate(ciuo2 = str_sub(ciuo, 1, 2))

base_pirc <- base_pirc %>% 
  left_join(oit_index, by = c("CIUO_encuestado" = "ciuo"))

base_picto <- base_picto %>% 
  left_join(oit_index, by = c("CIUO" = "ciuo"))


# Procesamientos -------------
base2021 <- base_pirc %>%
  filter(M2.12 == 1) %>% 
  select(Mean, Exposure, POND2R_FIN_n) %>% 
  mutate(anio = 2021, .before = 1) %>% 
  rename(ponder = POND2R_FIN_n)

base2024 <- base_picto %>%
  filter(o9 == 1) %>%
  select(Mean, Exposure, pondera_sin_elevar) %>%
  mutate(anio = 2024, .before = 1) %>%
  rename(ponder = pondera_sin_elevar)
  
base_final <- bind_rows(base2021, base2024)

theme_gtsummary_language("es")

base_final %>%
  svydesign(data = ., ids = ~ 1, weights = ~ponder) %>%
  tbl_svysummary(include = c(Mean, Exposure),
                 label = c(Mean ~ "Promedio",
                           Exposure ~ "Categoría de riesgo"),
                 statistic = list(
                   all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} ({p}%)"),
                 digits = list(all_categorical() ~ c(0, 1),
                               all_continuous() ~ c(2, 1)),
                 missing = "no",
                 by = anio) %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = "salidas/tabla_descriptiva.docx")



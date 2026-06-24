#Librerías--------------

pacman::p_load(tidyverse, haven, summarytools, janitor, ggsci, GDAtools, ggsci, DT,
               VIM, gtsummary, ggridges, ggrepel, patchwork, ggtext)

#Bases-----------
indice_oit <-readxl::read_xlsx("bases/Final_Scores_ISCO08_Gmyrek_et_al_2025.xlsx")

indice_oit <- indice_oit %>% 
  select(ISCO_08, mean_score_2025, SD_2025, potential25) %>% 
  group_by(ISCO_08) %>% 
  summarise(mean = mean(mean_score_2025, na.rm = T),
            sd = mean(SD_2025, na.rm = T),
            potential = first(potential25)) %>% 
  rename(CIUO = ISCO_08) %>% 
  mutate(CIUO = as.integer(CIUO))

base <- read_sav("bases/Base OEI 20-11.sav")

base <- base %>% 
  left_join(indice_oit, by = "CIUO")


theme_set(theme_classic() +
            theme(plot.caption = element_text(size = 9)))

options(scipen = 999)


#Transformación variables---------
base <- base %>% 
  mutate(sex = factor(case_when(sex == 2 ~ "Varón",
                                sex == 1 | sex == 3 ~ "Mujer",
                                TRUE ~ NA_character_)),
         grupo_edad = factor(case_when(Edad < 30 ~ "19-29",
                                       Edad >= 30 & Edad < 40 ~ "30-39",
                                       Edad >= 40 & Edad < 66 ~ "40-65",
                                       Edad >= 66 ~ ">= 66"),
                             levels = c("19-29", "30-39", "40-65", ">= 66")),
         sitcony = factor(case_when(sitcony == 1 ~ "unido",
                                    sitcony == 2 ~ "casado",
                                    sitcony == 3 ~ "separado/a o divorciado/a",
                                    sitcony == 4 ~ "viudo/a",
                                    sitcony == 5 ~ "soltero/a",
                                    TRUE ~ NA_character_)),
         obs = factor(case_when(obs == 1 ~ "Obra social",
                                obs == 2 | obs == 4 ~ "Prepaga",
                                obs == 3 ~ "Hospital público o centros de salud",
                                TRUE ~ NA_character_)),
         pais = factor(case_when(pais == 1 ~ "En esta localidad",
                                 pais == 2 ~ "En otra localidad de esta provincia",
                                 pais == 3 ~ "En otra provincia",
                                 pais == 4 ~ "En un país limítrofe",
                                 pais == 5 ~ "En otro país",
                                 TRUE ~ NA_character_)),
         local = factor(case_when(local == 1 ~ "CABA",
                                  local == 2 ~ "Conurbano GBA",
                                  local == 3 ~ "Córdoba",
                                  local == 4 ~ "Mar del Plata",
                                  TRUE ~ NA_character_)),
         nivel_ed = factor(case_when(nivel == 1 | ((nivel == 3 | nivel == 4) & nivel2 == 0) ~ "Hasta sec. incom.",
                                     (nivel == 3 | nivel == 4) & nivel2 == 1 ~ "Hasta sec. com.",
                                     nivel == 5 & nivel2 == 0 ~ "Hasta terc. incom.",
                                     nivel == 5 & nivel2 == 1 ~ "Hasta terc. com.",
                                     nivel == 6 & nivel2 == 0 ~ "Universitario incom.",
                                     nivel == 6 & nivel2 == 1 ~ "Universitario com.",
                                     nivel == 7 ~ "Posgrado",
                                     TRUE ~ NA_character_),
                           levels = c("Hasta sec. incom.", "Hasta sec. com.", "Hasta terc. incom.", "Hasta terc. com.", "Universitario incom.", "Universitario com.", "Posgrado")),
         nivel_ed_2 = factor(case_when(nivel <= 4 | ((nivel == 5 | nivel == 6) & nivel2 == 0) ~ "Hasta sec. comp.",
                                       (nivel == 5 | nivel == 6) & nivel2 == 1 ~ "Superior comp.",
                                       nivel == 7 ~ "Superior comp."),
                             levels = c("Hasta sec. comp.",
                                        "Superior comp.")),
         tit = case_when(tit == 1 ~ "Ciencias sociales / humanidades",
                         tit == 2 ~ "Economía y administración",
                         tit == 3 ~ "Ciencias naturales",
                         tit == 4 ~ "Comunicación",
                         tit == 5 ~ "Arte",
                         tit == 6 ~ "Educación física",
                         tit == 7 ~ "Agrario / ambiente",
                         tit == 8 ~ "Técnica",
                         TRUE ~ NA_character_),
         tit2_c = factor(tit2_c),
         caes_f = factor(CAES_letra, labels = c("Agricultura, ganadería, caza, pesca",
                                                "Industria manufacturera",
                                                "Construcción",
                                                "Comercio",
                                                "Alojamiento y servicios de comidas",
                                                "Información y comunicación",
                                                "Actividades financieras y de seguros",
                                                "Actividades profesionales, científicas y técnicas",
                                                "Actividades administratias y servicios de apoyo",
                                                "Administración pública y defensa",
                                                "Enseñanza",
                                                "Salud humana y servicios sociales",
                                                "Otras actividades de servicio")),
         ramact = factor(ramact, labels = c("Software",
                                            "Metalmecánica",
                                            "Farmacéutica",
                                            "Alimentación",
                                            "Hotelería",
                                            "Textil",
                                            "Mueble")),
         CIUO1 = factor(case_when(CIUO >= 1000 & CIUO < 2000 ~ "Directores y gerentes",
                                  CIUO >= 2000 & CIUO < 3000 ~ "Profesionales científicos e intelectuales",
                                  CIUO >= 3000 & CIUO < 4000 ~ "Técnicos y profesionales de nivel medio",
                                  CIUO >= 4000 & CIUO < 5000 ~ "Personal de apoyo administrativo",
                                  CIUO >= 5000 & CIUO < 6000 ~ "Trabajadores de los servicios y comerciantes",
                                  CIUO >= 7000 & CIUO < 8000 ~ "Oficiales, operarios y artesanos de artes mecánicas y de otros oficios",
                                  CIUO >= 8000 & CIUO < 9000 ~ "Operadores de instalaciones y máquinas y ensambladores",
                                  CIUO >= 9000 & CIUO < 9990 ~ "Ocupaciones elementales",
                                  TRUE ~ NA_character_),
                        levels = c("Directores y gerentes",
                                   "Profesionales científicos e intelectuales",
                                   "Técnicos y profesionales de nivel medio",
                                   "Personal de apoyo administrativo",
                                   "Trabajadores de los servicios y comerciantes",
                                   "Oficiales, operarios y artesanos de artes mecánicas y de otros oficios",
                                   "Operadores de instalaciones y máquinas y ensambladores",
                                   "Ocupaciones elementales")),
         tamano = ifelse(trabajprin5 == -999, NA_character_, trabajprin5),
         tamano = factor(tamano, labels = c("Una sola persona",
                                            "2 a 5",
                                            "6 a 10",
                                            "11 a 50",
                                            "51 a 200",
                                            "Más de 200")),
         potential = factor(potential, levels = c("Not Exposed",
                                                  "Minimal Exposure",
                                                  "Exposed: Gradient 1",
                                                  "Exposed: Gradient 2",
                                                  "Exposed: Gradient 3",
                                                  "Exposed: Gradient 4"),
                            labels = c("No expuesto",
                                       "Exposición mínima",
                                       "Expuesto: Gradiente 1",
                                       "Expuesto: Gradiente 2",
                                       "Expuesto: Gradiente 3",
                                       "Expuesto: Gradiente 4")))
                                       

base <- base %>% 
  mutate(across(tarea1:tarea4, ~factor(., labels = c("Para nada", "Muy poco", "En alguna medida", "En gran medida", "En muy alta medida"))))

base <- base %>% 
  mutate(across(frecuencia1:tecno5, ~ na_if(., -999))) 

base <- base %>%
  mutate(across(frecuencia1:tecno5, ~factor(., levels = c(1, 2, 3, 4, 5),
                                            labels = c("Nunca",
                                                       "Menos de una vez al mes",
                                                       "Menos de una vez a la semana pero por lo menos una vez al mes",
                                                       "Por lo menos una vez a la semana pero no todos los días",
                                                       "Todos los días"))))


base <- base %>% 
  mutate(ia1 = factor(ia1, labels = c("IA-No", "IA-Sí")))


#Imputación ingresos
base <- base %>% 
  mutate(trabajprin15c = case_when(trabajprin15 == 1 ~ "No tuvo ingresos",
                                   trabajprin15 == 2 ~ "1 a 90000",
                                   trabajprin15 == 4 ~ "150001 a 200000",
                                   trabajprin15 == 6 ~ "270001 a 330000",
                                   trabajprin15 == 7 ~ "330001 a 410000",
                                   trabajprin15 == 8 ~ "410001 a 510000",
                                   trabajprin15 == 9 ~ "510001 a 660000",
                                   trabajprin15 == 10 ~ "660001 a 900000",
                                   trabajprin15 == 11 ~ "900001 a 1400000",
                                   trabajprin15 == 12 ~ "1400001 a 2500000",
                                   trabajprin15 == 13 ~ "2500001 a 5300000",
                                   trabajprin15 == -999 ~ "Ns/Nc"),
         rango_ingresos_ocup = case_when(trabajprin14 >= 1 & trabajprin14 <= 90000 ~ "1 a 90000",
                                         trabajprin14 >= 150001 & trabajprin14 <= 200000 ~ "150001 a 200000",
                                         trabajprin14 >= 270001 & trabajprin14 <= 330000 ~ "270001 a 330000",
                                         trabajprin14 >= 330001 & trabajprin14 <= 410000 ~ "330001 a 410000",
                                         trabajprin14 >= 410001 & trabajprin14 <= 510000 ~ "410001 a 510000",
                                         trabajprin14 >= 510001 & trabajprin14 <= 660000 ~ "510001 a 660000",
                                         trabajprin14 >= 660001 & trabajprin14 <= 900000 ~ "660001 a 900000",
                                         trabajprin14 >= 900001 & trabajprin14 <= 1400000 ~ "900001 a 1400000",
                                         trabajprin14 >= 1400001 & trabajprin14 <= 2500000 ~ "1400001 a 2500000",
                                         trabajprin14 >= 2500001 & trabajprin14 <= 5300000 ~ "2500001 a 5300000", 
                                         TRUE ~ NA_character_))

base$rango_ingresos_ocup <- ifelse(base$trabajprin14 == 0 | is.na(base$trabajprin14) |
                                     base$trabajprin14 > 8000000, 
                                   base$trabajprin15c,
                                   base$rango_ingresos_ocup)

tabla <- base %>% 
  select(trabajprin14, trabajprin15c, rango_ingresos_ocup)

base <- base %>% 
  mutate(ingresos = ifelse(trabajprin14 == 0 | is.na(trabajprin14) | trabajprin14 > 8000000, 
                           NA_real_, 
                           trabajprin14))

set.seed(971986)
#Imputo ingresos por rango
base <- hotdeck(data = base, variable = "ingresos", domain_var = "rango_ingresos_ocup") 

#Imputo ingresos que no tienen rango
base <- hotdeck(data = base, variable = "ingresos")


#IA------------
base %>%
  group_by(ia1, potential) %>% 
  tally() %>% 
  group_by(ia1) %>%
  mutate(porcentaje = n/sum(n)) %>%
  na.omit(potential) %>% 
  ggplot(aes(x = ia1, y = porcentaje, fill = potential)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(fill = "Potencial de IA OIT") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("salidas/ia1_potencial_oit.png", width = 7, height = 5, dpi = 300)


#ACM-----------------
base_mca <- base %>% 
  select(frecuencia4, frecuencia5, frecuencia6, frecuencia7, frecuencia8, frecuencia9, frecuencia10, frecuencia11, habili14, habili15, habili11, habili13, habili3, habili6, habili8, habili9, tecno4, tarea4, CIUO1, ramact, ia1, ia2_1:ia2_10, potential)

base_mca <- base_mca %>% 
  mutate(across(frecuencia4:tecno4, ~case_when(. == "Nunca" ~ "N",
                                               . == "Menos de una vez al mes" ~ "1",
                                               . == "Menos de una vez a la semana pero por lo menos una vez al mes" ~ "1-3",
                                               . == "Por lo menos una vez a la semana pero no todos los días" ~ "4-29",
                                               . == "Todos los días" ~ "30")),
         tarea4 = case_when(tarea4 == "Para nada" ~ "N",
                            tarea4 == "Muy poco" ~ "MP",
                            tarea4 == "En alguna medida" ~ "AM",
                            tarea4 == "En gran medida" ~ "GM",
                            tarea4 == "Muy alta medida" ~ "T"),
         
         across(ia2_1:ia2_10, ~case_when(. == 0 | is.na(.) ~ "No",
                                         . == 1 ~ "Si"))) %>% 
  mutate(across(ia2_1:ia2_10, as.factor)) %>%
  rename(cooperar = frecuencia4,
         compartir_info = frecuencia5,
         ensenar = frecuencia6,
         aconsejar = frecuencia7,
         planif_prop = frecuencia8,
         planif_otros = frecuencia9,
         org_tiempo = tarea4,
         influenciar = frecuencia10,
         negociar = frecuencia11,
         problema_simple = habili14,
         problema_complejo = habili15,
         calculo_medio = habili11,
         calculo_avanzado = habili13,
         lectura1 = habili3,
         lectura2 = habili6,
         escritura1 = habili8,
         escritura2 = habili9,
         leng_prog = tecno4,
         redactar = ia2_1,
         comprension = ia2_2,
         traduccion = ia2_3,
         reconocimiento = ia2_4,
         imagen_video = ia2_5,
         audio = ia2_6,
         chatbot = ia2_7,
         programacion = ia2_8,
         calculos = ia2_9,
         otros = ia2_10)



mca <- speMCA(base_mca[,1:18], ncp = 2)

mca <- flip.mca(mca, dim = 2)

tabla_inercia <- modif.rate(mca)$modif

tabla_inercia <- tabla_inercia %>% 
  mutate(mrate = round(mrate, 4),
         cum.mrate = round(cum.mrate, 4))

write.xlsx(tabla_inercia,
           file = "salidas/tabla_inercia_mca.xlsx",
           rowNames = TRUE) 


ggmca <- ggcloud_variables(mca, vlab = T, shapes = T, shapesize = 1.5, force = .2,
                           textsize = 2, legend = "none", col = "black") +
  labs(title = "Análisis de correspondencias múltiples", 
       subtitle = "Tareas y habilidades laborales. Variables activas",
       caption = "Fuente: elaboración propia en base a Encuesta de Automatización 2024") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        plot.caption = element_text(size = 9, hjust = 1)) +
  annotate("text", x = -.8, y = 2, label = "Presencia cuellos de botella", size = 4, hjust = .3, vjust = 0, colour = "red") +
  annotate("text", x = .7, y = 1, label = "Ausencia cuellos de botella", size = 4, hjust = .3, vjust = 0, colour = "red")

ggmca

cloud_ind <- ggcloud_indiv(mca, col = "lightgrey") +
  scale_color_d3() +
  labs(title = "Análisis de correspondencias múltiples", 
       subtitle = "Nube de individuos",
       caption = "Fuente: elaboración propia en base a Encuesta de Automatización 2024") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        plot.caption = element_text(size = 9, hjust = 1)) +
  guides(color = guide_legend(override.aes = list(size = 2)))

cloud_ind


ggmca <- ggcloud_variables(mca, vlab = T, shapes = T, shapesize = 1.5, force = .2,
                           textsize = 2, points = "best", legend = "none", col = "grey50") +
  labs(title = "Análisis de correspondencias múltiples", 
       subtitle = "Tareas y habilidades laborales. Variables suplementarias (Ocupaciones, rama de actividad \ny uso de IA)",
       caption = "Fuente: elaboración propia en base a Encuesta de Automatización 2024") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        plot.caption = element_text(size = 9, hjust = 1)) +
  scale_fill_ucscgb()

ggadd_supvars(ggmca, mca, vars = base_mca[, c("CIUO1", "ramact", "ia1", "potential")], vlab = F, force = 1, shapes = T, shapesize = 1.5)

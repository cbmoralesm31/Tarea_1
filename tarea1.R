## Tarea 1 Carolina Morales
library(tidyverse)
library(knitr)
library(kableExtra)

## Cargar dataset
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

## Cambiar sigla a nombre completo de cada país
Pais <- c("Australia", "Canada", "Alemania", "Reino Unido", "EEUU")
country <- c("au", "ca", "de", "gb", "us")
NombreFull <- data.frame(Pais, country)
ufo_sightings <- left_join(ufo_sightings, NombreFull)

## Armar data frame para recuento avistamientos por país
data1 <- ufo_sightings %>% filter(!is.na(country)) %>%  group_by(Pais) %>% summarise( n = n())
data1 <- as.data.frame(data1) 

## Crear columna de minutos, sacar los sin datos, filtrar por criterio de un dia
ufo <- ufo_sightings %>% mutate(minutos = (encounter_length/60)) %>% filter(!is.na(country), !is.na(minutos)) %>% filter(minutos < 1440)  %>% mutate(horas = (minutos/60))

## Crear columna year, a partir de fecha registrada no analizable
UFO <-  ufo %>% mutate(year = substr(x = ufo$date_documented, 5,10)) 
Year <- gsub("^.*?/","", UFO$year)
UFOS <- cbind(UFO, Year) %>% select(-"year")

## Información por País

data2 <- data1[with(data1, order(n)), ]
data2[1,1]

## grafico 1
barplot <- ggplot(data = data1, aes(Pais, n, fill= Pais)) + 
  geom_bar(stat = "identity") +
  labs(fill = "Pais", x= "Pais", y= "Número de observaciones") +
  scale_fill_discrete(name = "País", labels = c("Australia", "Canada", "Alemania", "Estados Unidos", "Reino Unido"))
barplot

## grafico 2
UFOS1 <- UFOS %>% group_by(Pais, Year) %>% summarise(n = n())

ggplot(UFOS1, aes(Year, n, color= Pais, group= Pais)) +
  geom_point()+ geom_line()+
  labs(fill = "Pais", x= "Año", y= "Numero de observaciones") +
  theme_grey()

## Tabla resumen

data4 <- data3 %>% spread(Pais, n) 
knitr::kable(data4, digits = 2, caption = "Tabla 1: Avistamientos de UFO entre los años 1998 y 2014 para los cinco paices en estudio") %>% 
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>% 
  footnote(general = "NA = no existen registros")
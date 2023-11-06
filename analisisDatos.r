# Lectura de datos
library(readr)
datos <- read_csv("GitHub/PosterManejodeDatos/Beneficiaros_de_becas_y_creditos_de_programas_de_acceso_a_la_educaci_n_superior_de_Antioquia_20231031.csv", 
                  col_types = cols(`FECHA DE NACIMIENTO` = col_datetime(format = "%m/%d/%Y %H:%M:%S %p")))
datos |> dim() # Dimensiones base de datos

view(datos)

summary(datos)

library(ggplot2)

# Unificar "estrato 2" con "ESTRATO 2"
datos |> 
  mutate(ESTRATO = replace(ESTRATO, which(ESTRATO == "Estrato 2"), "ESTRATO 2")) -> datos
datos |> 
  mutate(ESTRATO = replace(ESTRATO, which(ESTRATO == "Estrato 1"), "ESTRATO 1")) -> datos
datos |> 
  mutate(ESTRATO = replace(ESTRATO, which(ESTRATO == "Estrato 3"), "ESTRATO 3")) -> datos

library(dplyr)
library(lubridate)
datos |> 
  mutate(ano = year(`FECHA DE NACIMIENTO`),
         mes = month(`FECHA DE NACIMIENTO`),
         dia = day(`FECHA DE NACIMIENTO`)) -> datos


# Grafico circular de distribucion de genero
ggplot(datos, aes(x = "", fill = GÉNERO)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de género") +
  theme_void()

# Tipo de formación por género
ggplot(datos, aes(x = `TIPO DE FORMACIÓN`, fill = GÉNERO)) +
  geom_bar(position = "dodge") +
  labs(x = "Tipo de Formación", y = "", fill = "Género") +
  ggtitle("Tipo de formación por género") +
  theme_minimal()



# Distribución estrato
ggplot(datos, aes(x = "", fill = ESTRATO)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Estrato") +
  theme_void()

# Estrato de las victimas del conflicto armado
victimas_data <- datos[datos$'VICTIMA DEL CONFLICTO ARMADO' == 1, ]
ggplot(victimas_data, aes(x = "", fill = ESTRATO)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Estrato de las victimas del conflicto armado") +
  theme_void()
# Grupo etnico de las victimas del conflicto armado
ggplot(victimas_data, aes(x = "", fill = `GRUPO ETNICO`)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Grupo etnico de las victimas del conflicto armado") +
  theme_void()

# Tipo de beneficio por estrato
ggplot(datos, aes(x = `BENEFICIO OTORGADO`, fill = ESTRATO)) +
  geom_bar(position = "dodge") +
  labs(x = "Beneficio Otorgado", y = "", fill = "Estrato") +
  ggtitle("Tipo de Beneficio por Estrato") +
  theme_minimal()

#Tabla para ver mas detalladamente los estrato 4, estrato 5 y NA
tabla <- table(datos$ESTRATO, datos$`BENEFICIO OTORGADO`)
tabla

# Lectura de datos
library(readr)
datos <- read_csv("GitHub/PosterManejodeDatos/Beneficiaros_de_becas_y_creditos_de_programas_de_acceso_a_la_educaci_n_superior_de_Antioquia_20231031.csv", 
                  col_types = cols(`FECHA DE NACIMIENTO` = col_datetime(format = "%m/%d/%Y %H:%M:%S %p")))
datos |> dim() # Dimensiones base de datos

view(datos)

summary(datos)

library(ggplot2)

# Grafico circular de distribucion de genero en la asignacion de beneficios
ggplot(datos, aes(x = "", fill = GÉNERO)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de género") +
  theme_void()

# Unificar "estrato 2" con "ESTRATO 2"
datos |> 
  mutate(ESTRATO = replace(ESTRATO, which(ESTRATO == "Estrato 2"), "ESTRATO 2")) -> datos


victimas_data <- datos[datos$'VICTIMA DEL CONFLICTO ARMADO' == 1, ]

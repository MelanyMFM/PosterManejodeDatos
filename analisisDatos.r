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



# Filtrar los datos para las personas que fueron víctimas del conflicto armado (VICTIMA_CONFLICTO = "SI")
victimas_data <- datos %>%
  filter('VICTIMA DEL CONLFICTO ARMADO' == 1)

# Crear un gráfico de barras apiladas para mostrar la relación entre el municipio de nacimiento y el género de las víctimas
ggplot(victimas_data, aes(x = MUNICIPIO_DE_NACIMIENTO, fill = GENERO)) +
  geom_bar() +
  labs(title = "Municipio de Nacimiento y Género de Víctimas del Conflicto Armado") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

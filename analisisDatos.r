# Lectura de datos
library(readr)
datos <- read_csv("https://raw.githubusercontent.com/MelanyMFM/PosterManejodeDatos/main/Beneficiaros_de_becas_y_creditos_de_programas_de_acceso_a_la_educaci_n_super.csv", 
                  col_types = cols(CONVOCATORIA = col_date(format = "%Y"),`FECHA DE NACIMIENTO` = col_datetime(format = "%m/%d/%Y %H:%M:%S %p")))
datos |> dim() # Dimensiones base de datos

View(datos)

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


#Poner frecuencia en porcentajes

# Tipo de beneficio por estrato (correcta)
ggplot(datos, aes(x = ESTRATO, fill = `BENEFICIO OTORGADO`)) +
  geom_bar(position = "dodge") +
  labs(x = "Estrato", y = "", fill = "Tipo de beneficio") +
  ggtitle("Tipo de Beneficio por Estrato") +
  theme_minimal()


# Tipo de beneficio por estrato (columnas apiladas al 100%)

datos_filtrados <- datos %>%
  filter(!(ESTRATO %in% c("ESTRATO 4", "ESTRATO 5", "ND")))

colores_personalizados <- c("#66c2a5","#9aaf8d", "#cf9c76", "#DADAEB", "#9E9AC8", "#6A51A3")

# Crear el gráfico con los datos filtrados y la paleta de colores personalizada
ggplot(datos_filtrados, aes(x = ESTRATO, fill = `BENEFICIO OTORGADO`)) +
  geom_bar(position = "fill") +
  labs(x = "Estrato", y = "Porcentaje", fill = "Tipo de beneficio") +
  ggtitle("Tipo de Beneficio por Estrato") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_brewer()


# Tabla ¿?
tabla_frecuencias <- table(datos$GÉNERO, datos$`BENEFICIO OTORGADO`)
df_tabla <- as.data.frame.matrix(tabla_frecuencias)
print(df_tabla)

# Ejemplo de creación de tabla de frecuencias cruzadas relativas
tabla_frecuencias <- prop.table(table(datos$GÉNERO, datos$`BENEFICIO OTORGADO`), margin = 1)
df_tabla <- as.data.frame.matrix(tabla_frecuencias)
print(df_tabla)


# Crear el gráfico con los datos filtrados y la paleta de colores personalizada
ggplot(datos, aes(x = CONVOCATORIA, fill = `BENEFICIO OTORGADO`)) +
  geom_bar(position = "fill") +
  labs(x = "Estrato", y = "Porcentaje", fill = "Tipo de beneficio") +
  ggtitle("Tipo de Beneficio por Estrato") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_brewer()



ggplot(datos, aes(x =`VICTIMA DEL CONFLICTO ARMADO`, fill = `BENEFICIO OTORGADO`)) +
  geom_bar(position = "fill") +
  labs(x = "Estrato", y = "Porcentaje", fill = "Tipo de beneficio") +
  ggtitle("Tipo de Beneficio por Estrato") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_brewer()



ggplot(datos_filtrados, aes(x = `TIPO DE FORMACIÓN`, fill = `BENEFICIO OTORGADO`)) +
  geom_bar(position = "fill") +
  labs(x = "Tipo de Formación", y = "", fill = "Beneficio Otorgado") +
  ggtitle("Tipo de Beneficio por Formación") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_brewer()
# Tabla ¿?
tabla_frecuencias <- table(datos$`TIPO DE FORMACIÓN`, datos$`BENEFICIO OTORGADO`)
df_tabla <- as.data.frame.matrix(tabla_frecuencias)
print(df_tabla)
tabla_frecuencias <- prop.table(table(datos$`TIPO DE FORMACIÓN`, datos$`BENEFICIO OTORGADO`), margin = 1)
df_tabla <- as.data.frame.matrix(tabla_frecuencias)
print(df_tabla)
tecnoprof = datos[datos$`TIPO DE FORMACIÓN` == "TECNICA PROFESIONAL", ]
universitarios = datos[datos$`TIPO DE FORMACIÓN` == "UNIVERSITARIA",]
nprmalista = datos[datos$`TIPO DE FORMACIÓN` == "NORMALISTA",]
ggplot(nprmalista, aes(x = "", fill = GÉNERO)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Genero") +
  scale_fill_brewer()
ggplot(nprmalista, aes(x = "", fill = ESTRATO)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Estrato en Universitarios") +
  scale_fill_brewer()
count(nprmalista[nprmalista$GÉNERO == "FEMENINO", ])


ggplot(nprmalista, aes(x = "", fill = UNIVERSIDAD)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Genero") +
  scale_fill_brewer()



# Crear el gráfico con los datos filtrados y la paleta de colores personalizada
ggplot(datos, aes(x = `SUBREGIÓN DE RESIDENCIA`, fill = `BENEFICIO OTORGADO`)) +
  geom_bar(position = "fill") +
  labs(x = "Estrato", y = "Porcentaje", fill = "Tipo de beneficio") +
  ggtitle("Tipo de Beneficio por Estrato") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_brewer()
aburra <- datos[datos$`SUBREGIÓN DE RESIDENCIA` == "VALLE DE ABURRA",]
ggplot(aburra, aes(x = "", fill = ESTRATO)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Genero") +
  scale_fill_brewer()
ggplot(datos, aes(x = "", fill =`TIPO DE FORMACIÓN`)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Genero") +
  scale_fill_brewer()




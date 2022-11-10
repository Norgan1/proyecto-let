library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(gt)
data = read_csv("datos/datos-sin-procesar/imdb_top_1000.csv")


### Limpiamos la base de datos ###

# Eliminamos columnas sin utilizar.
data = data[c(-1,-4,-8,-9,-12,-13,-14)]

# Runtime es caracter, lo cambiamos a numérico. 
data$Runtime = as.numeric(strsplit(data$Runtime,split = " min"))

# Cambio de nombre de las columnas a español.
colnames(data) = c("Nombre_serie","Año_lanzamiento","Duracion","Genero","IMBD_Rating","Director",
                   "Actor_1", "Numero_de_votos", "Ganancias")

# Cambiamos "PG" por 1995, año de lanzamiento de la película. Hacemos numérica la variable.
data$Año_lanzamiento[data$Año_lanzamiento=="PG"] = 1995
data$Año_lanzamiento = as.numeric(data$Año_lanzamiento)

# Género 
a = strsplit(data$Genero,split = ",")
d = c()
for(i in 1:1000){
  d = append(d,a[[i]][1])
  
}

data$Genero = d # Ahora tenemos un género por película en vez de varios géneros


### Graficamos ###

# Esto crea ganancia media por décadas (NO TOMAMOS EN CUENTA 2020 YA QUE DA VALORES NA TODOS).
 data_decada_grossmean = data %>% 
  mutate(decada = floor(Año_lanzamiento/10)*10) %>% 
  group_by(decada) %>% 
  summarise(mean(Ganancias,na.rm = TRUE)) 

data_decada_grossmean = data_decada_grossmean[-11,]

# Creamos el gráfico y guardamos el tema personalizado
my_theme = theme(panel.background = element_rect(fill = "#121212"),
                 plot.background = element_rect(fill = "#121212"),
                 panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                 title = element_text(color = "white"), axis.line = element_line(color="white"),
                 axis.text = element_text(color = "white"), axis.ticks = element_blank())
 
ggplot(data_decada_grossmean,aes(decada,
                                 round(`mean(Ganancias, na.rm = TRUE)`),4)) +
  geom_bar(stat = "identity") + 
  geom_col(fill = "#f5c518")+
  geom_text(aes(label = label_number_si()(`mean(Ganancias, na.rm = TRUE)`)),
            vjust = -1,
            color = "white",
            fontface = "bold")+
  scale_x_continuous(breaks = seq(1920,2020,10))+
  scale_y_continuous(labels = label_number_si())+
  labs(title = "Ganancia media por década",
         subtitle = "(1920-2010)",
         x = NULL, y = "Ganancia media en millones de dólares")+
  my_theme

# Esto crea rating medio por décadas.
data_decada_rating = data %>% 
  mutate(decada = floor(Año_lanzamiento/10)*10) %>% 
  group_by(decada) %>% 
  summarise(mean(IMBD_Rating,na.rm = TRUE)) 

colnames(data_decada_rating) = c("decada","rating") # Cambio de nombres a las columnas.

# Creamos el gráfico.
ggplot(data_decada_rating,aes(x = decada ,y = rating ))+
  geom_line(color = "#f5c518", size = 1.5 )+
  geom_point(color = "white", size = 1.7)+
  geom_text(aes(label = round(rating,2)), vjust = -1.5, color = "white", fontface = "bold")+
  scale_x_continuous(breaks = seq(1920,2020,10))+
  ylim(7.3,8.5)+
  labs(title = "Rating medio por década", subtitle = "(1920 - 2020)",
       x = NULL, y = "Rating medio")+
  my_theme

# Esto crea duración media por décadas.
data_decada_duracion = data %>% 
  mutate(decada = floor(Año_lanzamiento/10)*10) %>% 
  group_by(decada) %>% 
  summarise(mean(Duracion,na.rm = TRUE))

colnames(data_decada_duracion) = c("decada","duracion") # Cambio de nombres a las columnas.

# Creamos el gráfico.
ggplot(data_decada_duracion,aes(x = decada ,y = duracion ))+
  geom_bar(stat = "identity")+
  geom_col(fill = "#f5c518")+
  geom_text(aes(label = round(duracion)), vjust = -1, color = "white", fontface = "bold")+
  scale_x_continuous(breaks = seq(1920,2020,10))+
  labs(title = "Duración media por década", subtitle = "(1920 - 2020)",
       x = NULL, y = "Duración media en minutos")+
  my_theme

# Esto crea número de votos medio por décadas

data_decada_votos = data |> 
  mutate(decada = floor(Año_lanzamiento/10)*10) |> 
  group_by(decada) |> 
  summarise(mean(Numero_de_votos,na.rm = TRUE))

colnames(data_decada_votos) = c("decada","numero_de_votos") # Cambio de nombres a las columnas

# Graficamos
ggplot(data_decada_votos,aes(x = decada ,y = numero_de_votos))+
  geom_bar(stat = "identity")+
  geom_col(fill = "#f5c518")+
  geom_text(aes(label = label_number_si()(numero_de_votos)), vjust = -0.5, color = "white", fontface = "bold")+
  scale_x_continuous(breaks = seq(1920,2020,10))+
  scale_y_continuous(labels = label_number_si())+
  labs(title = "Número de votos medio por década", subtitle = "(1920 - 2020)",
       x = NULL, y = "Número de votos medio")+
  my_theme


  






### Creación Tabla de Géneros más repetidos por década 

data_decada_genero = data %>% 
  mutate(decada = floor(Año_lanzamiento/10)*10) %>% 
  group_by(decada) %>% 
  count(Genero) |>
  slice_max(n) |> 
  gt() |>
  tab_header(title = "Géneros más repetidos por década", subtitle = "(1920 - 2020)") |> 
  cols_label(Genero = "Género", n = "Conteo") |>
  tab_footnote(footnote = "Notar que la poca cantidad de géneros contados en 2020 es debido
               a la pandemia que afectó a la producción de películas",
               locations = cells_body(columns = Genero,rows = 12:13))|> 
  tab_options(table.background.color = "#121212") 

# Creación tabla top 10 películas según su ganancia  
  
data |> 
  slice_max(Ganancias, n = 10) |> 
  select(-c(Duracion,Director,Actor_1)) |> 
  gt() |> 
  tab_header(title = "Top 10 películas según sus ganancias") |> 
  cols_label(Nombre_serie = "Película",Año_lanzamiento = "Año lanzamiento", Genero = "Género",
             Numero_de_votos = "Número de votos", IMBD_Rating = "Rating") |>
  tab_options(table.background.color = "#121212") 

# Tabla de Ganancia media por Género y Rating medio

gan_rating_por_genero = data |> 
  group_by(Genero) |> 
  summarise(mean(Ganancias,na.rm = TRUE),round(mean(IMBD_Rating,na.rm = TRUE),2))

colnames(gan_rating_por_genero) = c("genero","ganancia_media","rating_medio") # Cambio nombre de las columnas

# creación de la tabla
gt(gan_rating_por_genero) |> 
  tab_header(title = "Ganancias y rating medio según su género") |> 
  cols_label(genero = "Género",rating_medio = "Rating", ganancia_media = "Ganancias") |>
  tab_footnote(footnote = "Los datos faltantes corresponden a dos películas
               de los años 1920 y 1922",
               locations = cells_body(columns = ganancia_media,rows = 9)) |> 
  tab_options(table.background.color = "#121212")

# Dos películas en fantasía del año 1920 y 1922 (datos muy antiguos como para recolectar ganacias)

# Rellenar filas de  Drama y Accion con color: #f5c518

# Agrega columna número de votos medio a la tabla anterior

gan_rating_voto_por_genero = data |> 
  group_by(Genero) |> 
  summarise(mean(Ganancias,na.rm = TRUE),round(mean(IMBD_Rating,na.rm = TRUE),2),round(mean(Numero_de_votos,na.rm = TRUE)))

colnames(gan_rating_voto_por_genero) = c("genero","ganancia_media","rating_medio","voto_medio")

gt(gan_rating_voto_por_genero) |> 
  tab_header(title = "Ganancias,rating y número de votos medio según su género") |> 
  cols_label(genero = "Género",rating_medio = "Rating", ganancia_media = "Ganancias",voto_medio = "Número de votos") |>
  tab_footnote(footnote = "Los datos faltantes corresponden a dos películas
               de los años 1920 y 1922",
               locations = cells_body(columns = ganancia_media,rows = 9)) |> 
  tab_options(table.background.color = "#121212")





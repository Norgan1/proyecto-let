library(readr)
library(dplyr)
library(ggplot2)
library(scales)
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
  geom_bar(stat = "identity")+
  geom_col(fill = "#f5c518")+
  geom_text(aes(label = round(rating,2)), vjust = -1, color = "white", fontface = "bold")+
  scale_x_continuous(breaks = seq(1920,2020,10))+
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

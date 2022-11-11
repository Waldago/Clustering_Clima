# TEMA:####
# ANALISIS DE COMPONENTES PRINCIPALES Y CLUSTERING
# INTEGRANTES_GRUPO: MAXI FRANCO, MAXI RODRIGUEZ Y WALTER GOMEZ                                                                  
# ULTIMA_ACTUALIZACION:11/11/2022 
# OBJETIVOS: Analizaremos la diferencia climatica entre las bases en diferentes partes del pais,
# partiendo de la hipotesis que hay grupos.Haciendo un promedio anual de las cifras de cada variable.
# Luego un analisis de componentes principales. Para terminar analizando los grupos. 

# BIBLIOTECAS####
library(readr)
library(dplyr)
library(tidyverse)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(ggplot2)
library(plotly)
library(FactoMineR)

# PREPARACION DE LOS DATOS####

## CARGA DEL SET DE DATOS####

setwd("E:/EXPLOTACION DE DATOS")

estadisticas <- read_delim("estadisticas.txt", 
                           delim = "\t", escape_double = FALSE, 
                           col_types = cols(Ene = col_number(), 
                                            Feb = col_number(), Mar = col_number(), 
                                            Abr = col_number(), May = col_number(), 
                                            Jun = col_number(), Jul = col_number(), 
                                            Ago = col_number(), Sep = col_number(), 
                                            Oct = col_number(), Nov = col_number(), 
                                            Dic = col_number()), locale = locale(grouping_mark = ""), 
                           trim_ws = TRUE)
View(estadisticas)

## TRANSFORMACION DE LOS DATOS####

colnames(estadisticas)

### PRIMERO SACO EL PROMEDIO DE LAS VARIABLES A LO LARGO DEL AÑO, SELECCIONAMOS LAS COLUMNAS Y PIBOTEAMOS LA COLUMNA 'VALOR MEDIO DE'####

datos <- estadisticas %>% 
  mutate(promedio = (Ene+Feb+Mar+Abr+May+Jun+Jul+Ago+Sep+Oct+Nov+Dic)/12) %>% 
  select(Estación,
         `Valor Medio de`,
         promedio) %>% 
  spread(`Valor Medio de`,promedio)

### ANALIZAMOS LOS NA####

sum(is.na(datos))

summary(datos)

#### DECIDIMOS REEMPLAZAR LOS NA'S CON EL VALOR DE LA MEDIA####

# EJ: my_data[is.na(my_data)] <- 0
datos$`Frecuencia de días con Precipitación superior a 0.1 mm`[is.na(datos$`Frecuencia de días con Precipitación superior a 0.1 mm`)] <- 6.771
datos$`Humedad relativa (%)`[is.na(datos$`Humedad relativa (%)`)] <- 67.92
datos$`Nubosidad total (octavos)`[is.na(datos$`Nubosidad total (octavos)`)] <- 3.854
datos$`Precipitación (mm)`[is.na(datos$`Precipitación (mm)`)] <- 69.431
datos$`Temperatura (°C)`[is.na(datos$`Temperatura (°C)`)] <- 14.83
datos$`Temperatura máxima (°C)`[is.na(datos$`Temperatura máxima (°C)`)] <- 21.405
datos$`Temperatura mínima (°C)`[is.na(datos$`Temperatura mínima (°C)`)] <- 9.354
datos$`Velocidad del Viento (km/h)`[is.na(datos$`Velocidad del Viento (km/h)`)] <- 12.685

### BORRAMOS LA PRIMER COLUMNA Y HACEMOS QUE EL INDEX SEAN LOS NOMBRES DE LAS BASES####

df <- datos %>% 
  remove_rownames %>% 
  column_to_rownames(var="Estación")
# ANALISIS DE COMPONENTES PRINCIPALES####

## SE CREA LA MATRIZ DE CORRELACION####

boxplot(df2)#VEMOS LOS OUTLIERS

df2 <- scale(df)#ESCALAMOS EL DATA SET

cor_data <- cor(df)

## GRAFICO LA MATRIZ DE CORRELACION ####
# Recordemos que en Azul cuando es correlacion positiva y Rojo para correlacion negativa.
# El tamaño de la esfera indica si esta muy correlacionado o si existe poca correlacion.
plot.new()
X11()
corrplot(cor_data)

##TEST DE BARLETT ####
# chart.Correlation(data[, -1]) # Graficar correlaciones e histogramas.
# hipotesis nula=no estan correlacionadas mis variables, y la hipotesis alternativa: nos dice que 
# si hay correlacion
# p.value: 0-1, el 0 apunta a la hipotesis alt.
# Test de Barlett
# Como el p.value = 1.326649e-141 es menor 0.05 se rechaza la hipotesis nula 
# y se continua con el ACP.
options(scipen = 6) # para evitar notacion cientifica.
cortest.bartlett(cor_data, n= 76) # n es el tamaño de la muestra.

##TEST DE KMO ####
# Test de KMO
# Este indice Kaiser-Meyer-Olkin (KMO) sirve para comparar los valores
# de correlacion de las variables y sus correlaciones parciales. Si el indice KMO
# es cercano a 1, significa que puede hacerse el analisis de componentes principales.
# Overall MSA =  0.74
KMO(df2)

## COMPONENTES PRINCIPALES. ####

cp <- prcomp(df2, scale = TRUE) # Escala los numeros de esta matriz de cero a uno.

#Resumen de los Componentes Principales
summary(cp)

##GRAFICOS DE CP####

plot(cp, 
     type="l", 
     main="CP mas Significativas con Plot",
     col=c("blue4"))
abline(1,0,col=c("brown3")) # Coloco una linea horizontal en el valor uno del eje y.

cp$rotation

# Usamos un grafico de barras.
library(factoextra)
fviz_screeplot(cp, 
               addlabels = TRUE, 
               ylim = c(0, 55),
               main="CP mas significativas con Screeplot")


# Biplot
biplot(x = cp, scale = 0, cex = 0.6, col = c("grey", "brown3"))

# Biplot con puntos. Se ven las variables y los casos.
# El grafico entre la Componente Principal 1 y 2, se puede apreciar
# tres agrupamientos de variables
biplot(x = cp, scale = 0, cex = 0.6, xlabs=rep(".", nrow(df)),col = c("green", "black"))


#CLUSTER####

## CP  biplot####

pca.model <- PCA(df2,  graph = FALSE, scale.unit = T )
p1 <- fviz_pca_var(pca.model,  
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("VARIABLES - PCA")

p2 <- fviz_pca_ind(pca.model, col.var="contrib", 
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = FALSE # Avoid text overlapping
) + theme_minimal() + ggtitle("BASES - PCA")

library(ggpubr)
ggarrange(p1, p2, common.legend = TRUE) #    figura importante

## K MEDIAS####

#centers es la cantidad de grupos
km_clusters <- kmeans(x = df2, centers = 3, nstart = 50)

# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = df2, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE,
             pointsize=0.5,outlier.color="darkred") +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +  theme(legend.position = "none")
#

library(broom)

kclust <- df2 %>%  kmeans(centers = 5, iter.max = 10, nstart = 5)
#para saber cual es el k mejor, hace sumatorias de las distancias y elije la que de menos.

kclust

# para justificar   el valor k usando method = "silhouette"
fviz_nbclust(x = df2 , FUNcluster = kmeans, method = "silhouette", k.max = 11) 

##DISTANCIAS####

m.distancia <- get_dist(df2, method = "euclidean") 
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

m.distancia <- get_dist(df2, method = "manhattan") 
fviz_dist(m.distancia, gradient = list(low = "yellow", mid = "white", high = "green"))

m.distancia <- get_dist(df2, method = "pearson") 
fviz_dist(m.distancia, gradient = list(low = "green", mid = "yellow", high = "red"))

# para justificar   el valor k usando method NbClust
library(factoextra)
library(NbClust)
#euclidia es la distancia mas standard, es mas sencible a los cambios de scala. y la manhatan es mas robusta
numero_clusters <- NbClust(data = df2, distance = "manhattan", min.nc = 2,
                           max.nc = 11, method = "kmeans", index = "alllong")

fviz_nbclust(numero_clusters)

km_clusters <- eclust(x = df2, FUNcluster = "kmeans", k =3, seed = 123,
                      hc_metric = "manhattan", nstart = 50, graph = FALSE)

fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 

fviz_nbclust(df2, kmeans, method = "wss")
#Metodo wss, el punto de corte es el punto ideal de corte

fviz_nbclust(df2, kmeans, method = "silhouette")

# Numero de clusters y metodo de agrupamiento optimo
library(clValid)
comparacion <- clValid(
  obj        = df2,
  nClust     = 2:5, 
  clMethods  = c("hierarchical", "kmeans", "pam"),
  validation = c("stability", "internal"))
summary(comparacion)

## fin kmeans


# CLUSTER JERARQUICO DENDOGRAMAS####
library(pheatmap)

kn <- 3  # modificar aqui segun analisis

# scale = "none",

pheatmap(mat = df2,scale = "row", 
         clustering_distance_rows = "manhattan",
         clustering_distance_cols = "euclidean", 
         clustering_method = "ward.D2",
         cutree_rows = kn, 
         fontsize = 7, 
         border_color = "black",
         angle_col = 45,
         labels_col = c("Freq_Preci", 
                        "Humedad_%", 
                        "Nubosidad", 
                        "Precip.", 
                        "Temp.", 
                        "Temp_Max", 
                        "Temp_Min", 
                        "Vel_Viento")
         )
#de costado el dendograma es por casos y de arriba es el dendograma por variables. prestar mucha atencion en el informe de las
#diferencias entre los grupos.



pheatmap(mat = df2, 
         scale = "row", 
         clustering_distance_rows = "manhattan", 
         clustering_method = "average",
         clustering_distance_cols = "euclidean",
         cutree_rows = kn, 
         fontsize = 7, 
         border_color = "black",
         angle_col = 45,
         labels_col = c("Freq_Preci", 
                        "Humedad_%", 
                        "Nubosidad", 
                        "Precip.", 
                        "Temp.", 
                        "Temp_Max", 
                        "Temp_Min", 
                        "Vel_Viento")
         )


library(dplyr)
library(mosaic)
library(ggplot2)
library(reshape2)
library(knitr)

options(scipen = 500, digits = 4)  # quitar notación cientifica
imdb<-read.delim("imdb_movie.csv", header = TRUE, sep = ",")
head(imdb)
colnames
View(imdb)

# Preparación de los datos
# Asignación de películas a un género
imdb<- imdb %>% mutate(genero=ifelse(grepl("Drama", genres) & (!grepl("Comedy", genres)), "Drama",
                                     ifelse( grepl("Comedy", genres) & (!grepl("Drama", genres)), "Comedy",
                                             "Other")))
# Nos quedamos solo con dramas y comedias
imdb <- imdb %>% filter(genero!="Other")

# Puntuaciones de dramas y comedias para población completa
imdb %>% group_by(genero) %>% dplyr::summarise(num=n(), score=mean(imdb_score), sd=sd(imdb_score, na.rm=TRUE))


# Muestreo
# muestra de películas de año 2015
sample1<- imdb %>% filter(title_year==2015)
summ_sample1<-sample1 %>% group_by(genero) %>% summarise(n=n(), mean_score=mean(imdb_score), sd_score=sd(imdb_score))
summ_sample1
# diferencia de medias observadas para genero drama y comedia
( diff_muestra<-diff(summ_sample1$mean_score) )
# En la muestra se observa que la puntuación media de los dramas es mayor que la de las comedias. 


# Objetivo: Realizar un test de hipótesis para ver si podemos inferir a partir de los datos de la muestra
# que efectivamente las puntuaciones medias de los dramas son mayores que la de las comedias, cuando 
# consideremos las películas de todos los años. 


# Test de hipótesis (Shuffle Test)
# Hipótesis nula: las medias de los scores de los dramas son iguales a los de las comedias,
# es decir que la diferencia de medias es 0.
# Hipótesis alternativa: medias de los dramas son mayores que la de las comedias.
# NOTA: hay 39 comedias y 84 dramas
dramas<- sample1 %>% filter(genero=="Drama") %>% sample_n(39)
comedys<- sample1 %>% filter(genero=="Comedy")
sample_movies<-rbind(dramas,comedys) %>% arrange(movie_title)
# Permutaciones
shuffled_means<-mosaic::do(5000)* (
  sample_movies %>% mutate(genero=shuffle(genero)) %>%
    group_by(genero) %>%
    summarize(mean_score=mean(imdb_score))
)
shuffled_means
# calculo diferencias entre medias
medias<- shuffled_means %>% group_by(.index) %>%
  summarize(score_diff=diff(mean_score))
medias
# Grafica de diferencias
ggplot(medias,aes(score_diff)) + geom_histogram(aes(fill=(abs(score_diff)<diff_muestra)))


# p-valor de la diferencia observada:
medias %>% filter(diff_muestra<=abs(score_diff)) %>%
  nrow() / nrow(medias)
# RESULTADO: Rechazamos la hipótesis nula por ser p<0.05. Por tanto, tenemos evidencia estadística 
# de que las puntuaciones de los dramas son distintos que las de las comedias. 


# Intervalo de confianza (Bootstrap)
bootstrap_samples<- mosaic::do(1000) *(
  sample_movies %>% group_by(genero) %>% sample_n(39, replace = TRUE) %>% summarise(mean_score=mean(imdb_score))
)
bootstrap_samples
( bootstrap_dist<-bootstrap_samples  %>% group_by(.index) %>% summarize(diff_means=diff(mean_score)) )
# Distribución bootstrap
mean_dist<-mean(bootstrap_dist$diff_means)
ggplot(bootstrap_dist) + geom_histogram(aes(diff_means),color="white") +
  geom_vline(xintercept = mean_dist,color="red") +
  geom_text(aes(mean_dist*0.83,95,label=round(mean_dist,2)),colour="red")

# Calculo de el intervalo de confianza al 95% a partir de los cuantiles
confidance_level=0.95
( lower<-quantile(bootstrap_dist$diff_means,(1-confidance_level)/2) )
( upper<-quantile(bootstrap_dist$diff_means,(confidance_level + (1-confidance_level)/2)) )
  
# t-test 
comedies <- sample1 %>% filter(genero =="Comedy")
drama <- sample1 %>% filter(genero =="Drama")
t.test(drama$imdb_score,comedies$imdb_score)
# Los resultados tanto para el p-valor como el intervalo de confianza  son bastante similares
# en este caso a los obtenidos mediante los métodos de aleatorización.
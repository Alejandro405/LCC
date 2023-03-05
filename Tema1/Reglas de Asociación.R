#
# ,------.               ,--.                   ,--.         ,---.                      ,--.             ,--.                
# |  .--. ' ,---.  ,---. |  | ,--,--. ,---.   ,-|  |,---.   /  O  \  ,---.  ,---.  ,---.`--' ,--,--.,---.`--' ,---. ,--,--,  
# |  '--'.'| .-. :| .-. ||  |' ,-.  |(  .-'  ' .-. | .-. : |  .-.  |(  .-' | .-. || .--',--.' ,-.  | .--',--.| .-. ||      \ 
# |  |\  \ \   --.' '-' '|  |\ '-'  |.-'  `) \ `-' \   --. |  | |  |.-'  `)' '-' '\ `--.|  |\ '-'  \ `--.|  |' '-' '|  ||  | 
#  `--' '--' `----'.`-  / `--' `--`--'`----'   `---' `----' `--' `--'`----'  `---'  `---'`--' `--`--'`---'`--' `---' `--''--' 
#                   `---'                                                                                                      
#

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ----echo=TRUE-----------------------------------------------------------
library(arules)
library(tidyverse)
library(nycflights13)

data(airmiles)
data("Adult")


length(Adult)
dim(Adult)
Adult
inspect(Adult[1:2])


## ----extraer_reglas.echo=TRUE-----------------------------------------------------------
data("Adult")
rules <- apriori(
            Adult
            ,  parameter = list(
                              supp = 0.5
                              , conf = 0.9
                              , target = "rules"
                          )
        ) 
summary(rules) 
inspect(rules)


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## rules <- apriori(Adult,  parameter = list(supp = 0.5, conf = 0.9,minlen=2))


## ----echo=TRUE, eval=FALSE-----------------------------------------------
## rules <- apriori(Adult,  parameter = list(supp = 0.5, conf = 0.9), appearance = list(items = c("income=small", "sex=Male"))
## 
## 
## rules <- apriori(Ad <- <- ult,  parameter = list(supp = 0.5, conf = 0.9), appearance = list(none = c("income=small", "sex=Male"))
## 


## ----Datasets sin formato transacciones-----------------------------------------------------------
data("AdultUCI")
View(AdultUCI)
str(AdultUCI)

## Toca discretizar y convertir a binario,
AdultUCI$fnlwgt <-NULL  #  Destruir campons inecesarios
AdultUCI$`education-num` <- NULL 



# discretizzacion de valores numéricos
# ejemplo de funcionamiento de cut y ordered
v <- 1:100
v2 <- cut(v,c(0,25,50,75,100),labels=c("bajo","medio","alto","muyalto"))
?ordered
v3 <- ordered(v2)


## ----echo=TRUE-----------------------------------------------------------

AdultUCI$age <- ordered(cut(
  AdultUCI[[ "age"]],
  c(15,25,45,65,100)),
  labels = c("Young", "Middle-aged", "Senior", "Old")
)

AdultUCI[[ "hours-per-week"]] <- ordered(cut(
  AdultUCI[[ "hours-per-week"]],
  c(0,25,40,60,168)),
  labels = c("Part-time", "Full-time", "Over-time", "Workaholic")
)

AdultUCI[[ "capital-gain"]] <- ordered(cut(
  AdultUCI[[ "capital-gain"]],
  c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)),
  labels = c("None", "Low", "High")
)

AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
                                           c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),
                                             Inf)), labels = c("None", "Low", "High"))


reg <- apriori(AdultUCI)
inspect(head(reg))


## Método automatizado
Adult1 <- as(AdultUCI, "transactions")
class(Adult1)
length(Adult1)
dim(Adult1)
Adult1
inspect(Adult1[1:2])


## ----echo=TRUE-----------------------------------------------------------
data("Adult")
class(Adult)
length(Adult)
dim(Adult)
Adult
inspect(Adult[1:2])

# Operaciones del manejo de reglas calculadas
 data("Adult")
 r1 <- apriori(Adult[1:1000], parameter = list(support = 0.5))
 r2 <- apriori(Adult[1001:2000], parameter = list(support = 0.5))
 #Convertir en un dataframe
 dfr1 <-DATAFRAME(r1)
 r_comb <- c(r1, r2)
 duplicated(r_comb)
 intersect(r1,r2)
 union(r1,r2)
 lhs(reglas1)
 rhs(reglas1)
 class(lhs(reglas1))




## ----echo=TRUE-----------------------------------------------------------
library(arules)
lastfm <- read.csv("lastfm.csv")
lastfm[1:20,]
length(lastfm$user)   ## 289,955 filas
class(lastfm$user)
# Necesitamos convertir este atributo a factor
#para poder analizarlo con paquete {\tt arules}

lastfm$user <- factor(lastfm$user)
# Ejecuta en tu ordenador
# levels(lastfm$user)  ## 15,000 users
# levels(lastfm$artist)  ## 1,004 artists


## ---- echo=TRUE----------------------------------------------------------
reglas1 <- apriori(lastfm,parameter=list(support=.01, confidence=.5))
inspect(reglas1)


## ---- echo=TRUE----------------------------------------------------------
lista.musica.por.usuario <- split(x=lastfm[,"artist"],f=lastfm$user)
lista.musica.por.usuario[1:2]


## ---- echo=TRUE----------------------------------------------------------
## Eliminar duplicados
lista.musica.por.usuario <- lapply(lista.musica.por.usuario,unique)

# Convertimos en transacciones la lista de música.
lista.musica.por.usuario1 <- as(lista.musica.por.usuario,"transactions")

lista.musica.por.usuario[1:5]

# en la versión actual de R esto va bien
#error ¿? (en versiones anteriores de R daba error, si os pasa intentar siguientes comandos)
#lista.musica.por.usuario2 <- as(lapply(lista.musica.por.usuario, "[[", 1), "transactions")
#lista.musica.por.usuario2


## ---- echo=TRUE----------------------------------------------------------
str(lista.musica.library(arules)
library(tidyverse)


# 1. Descargar a local el dataset **consumo.csv** (en CV).


consumo <- read.csv("~/Documents/LCC/Tema1/consumo.csv")


# 2 Calcula  cuántos  compradores distintos hay en el dataset.

length(unique(consumo$Transaction))

# 3 Calcula  cuántos  productos distintos se han vendido. ¿Cuales son los 10 más vendidos? Visualiza con algún gráfico.

length(unique(consumo$Item))

items <- unique(consumo$Item)
countItems <- consumo %>% group_by(Item) %>% summarise(n()) %>% rename(n = 'n()') %>% arrange(desc(n)) %>% slice_head(n = 10)

#countItems %>% order(n) %>% slice_head(n = 10)

aux <- consumo %>% filter(Item %in% countItems$Item)

ggplot(aux, aes(x = Item)) + geom_histogram(stat = "count")
#                          + geom_bar()

# 4. Separa la fecha en año, mes y día. Obten qué años, meses y días hay  más ventas con el objetivo de tener más personal en esas fechas. Visualiza las ventas acumuladas por meses.

conusmo2 <- consumo %>% separate(Date, into = c("Año", "Mes","Día"))


# 5. Usa **split** para construir a partir de dataset una lista con nombre *lista.compra.usuarios*
# en la que cada elemento de la lista es cada comprador junto con todos los productos que ha comprado
#Este paso es crucial para poder extraer posteriormente las reglas de asociación.

lista.compra.usuario <- split(consumo$Item, consumo$Transaction)
# 6. Hacer **summary** de *lista.compra.usuarios*

 summary(lista.compra.usuario)

# 7. Contar cuántos usuarios hay en la lista  *lista.compra.usuarios*

length(lista.compra.usuario)

# 8. Convertir a tipo de datos transacciones. Guardar en Tlista.compra.usuarios.

# 9. Hacer **inspect** de los dos primeros valores de Tlista.compra.usuarios.

# 10. Generar las reglas de asociación con 80% de confianza y 15% de soporte.  (varias estos úmbrales si no son adecuadas las reglas que obtienes - demasiadas y no acaba o pocas)


# 11. Ver las reglas generadas y ordenalas por lift. Guarda el resultado en una variable nueva.

# 12. Elimina todas las reglas redundantes. Calcula el % de reglas redundantes que había.

# 13. Quedate con las reglas que son significativas.

# 14. Buscar ayuda de **itemFrequencyPlot** para visualizar las 10 transacciones más frecuentes.



# 15. Crea un dataframe (dos columnas - número de productos, lista de productos como texto) con la información de cada transacción.

## CONCLUSIONES:

 # ¿Cuales son los productos más vendidos?

 # Explica alguna regla e indica si este conocimiento te haría cambiar tu política de ventas para obtener más beneficios.

 # ¿Cómo incrementar ventas?por.usuario1)
write(head(lista.musica.por.usuario1))
write(head(lista.musica.por.usuario1),format="single")


## ---- echo=TRUE----------------------------------------------------------
itfreq1  <-itemFrequency(lista.musica.por.usuario1)
head(itfreq1)


## ---- echo=TRUE----------------------------------------------------------
itemFrequencyPlot(lista.musica.por.usuario1,support=.08,cex.names=1)


## ---- echo=TRUE----------------------------------------------------------
reglas2 <- apriori(lista.musica.por.usuario1,parameter=
  list(support=.01, confidence=.5))
reglas2
inspect(reglas2)


## ---- echo=TRUE----------------------------------------------------------
inspect(subset(reglas2, subset=lift > 1))


## ---- echo=TRUE----------------------------------------------------------
inspect(sort(subset(reglas2, subset=lift > 1), by="confidence"))



## ---- echo=TRUE----------------------------------------------------------
r1 <-subset(reglas2, subset = lhs %ain%
  c("coldplay"))
inspect(r1)




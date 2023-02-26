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
str(lista.musica.por.usuario1)
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




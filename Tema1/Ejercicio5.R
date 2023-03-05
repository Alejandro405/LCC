library(arules)
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

Tlista.compra.usuarios <- as(lista.compra.usuario, "transactions")

# 9. Hacer **inspect** de los dos primeros valores de Tlista.compra.usuarios.

inspect(Tlista.compra.usuarios[1:2])

# 10. Generar las reglas de asociación con 80% de confianza y 15% de soporte.  (varias estos úmbrales si no son
# adecuadas las reglas que obtienes - demasiadas y no acaba o pocas)

reglasCompra <- apriori(Tlista.compra.usuarios, parameter = list(supp = 0.15, conf = 0.1))

reglasComra2 <- apriori(Tlista.compra.usuarios)

# 11. Ver las reglas generadas y ordenalas por lift. Guarda el resultado en una variable nueva.
inspect(head(reglasCompra))

# 12. Elimina todas las reglas redundantes. Calcula el % de reglas redundantes que había.

# 13. Quedate con las reglas que son significativas.

# 14. Buscar ayuda de **itemFrequencyPlot** para visualizar las 10 transacciones más frecuentes.



# 15. Crea un dataframe (dos columnas - número de productos, lista de productos como texto) con la información de cada transacción.

## CONCLUSIONES:

 # ¿Cuales son los productos más vendidos?

 # Explica alguna regla e indica si este conocimiento te haría cambiar tu política de ventas para obtener más beneficios.

 # ¿Cómo incrementar ventas?
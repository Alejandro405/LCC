library(arules)
library(tidyverse)


# 1. Descargar a local el dataset **consumo.csv** (en CV).


consumo <- read.csv("/home/alejandro/Documents/LCC/Tema1/consumo.csv")


# 2 Calcula  cuántos  compradores distintos hay en el dataset.

length(unique(consumo$Transaction))

# 3 Calcula  cuántos  productos distintos se han vendido. ¿Cuales son los 10 más vendidos? Visualiza con algún gráfico.

length(unique(consumo$Item))

items <- unique(consumo$Item)
countItems <- consumo %>% group_by(Item) %>% summarise(n()) %>% rename(n = 'n()')

countItems %>% order(n) %>% slice_head(n = 10)

base::plot(x = countItems$Item, y = countItems$`n()`)

ggplot(
  data = countItems,
  mapping = aes(x = Item, y = n)
) + geom_histogram(binwidth = 5)

# 5. Usa **split** para construir a partir de dataset una lista con nombre *lista.compra.usuarios*
# en la que cada elemento de la lista es cada comprador junto con todos los productos que ha comprado
#Este paso es crucial para poder extraer posteriormente las reglas de asociación.

lista.compra.usuario <- split(consumo$Item, consumo$Transaction)
# 6. Hacer **summary** de *lista.compra.usuarios*

 

# 7. Contar cuántos usuarios hay en la lista  *lista.compra.usuarios*

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

 # ¿Cómo incrementar ventas?
#---
#  title: "Ejercicio funciones 1"

 # ```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
#```

## Ejercicio 1 - Funciones


#Dada una cadena de texto con un texto similar al siguiente ejemplo: 
  
 # > ``"28 : 291875 :	INDRA FACTORÍA TECNOLÓGICA, S.L.U. :	2"``

#>> (código : número de oferta   empresa    plazas)


#Es decir, una cadena que contiene la siguiente expresión regular - ver [expresiones regulares](https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf): 
  
  #> [[:digit:]][[:space:]][:][[:space:]][[:digit:]][[:space:]][[:alpha:]][[:space:]]  [[:digit:]]

#(la información importante separa por : y/o espacios y/o tabuladores)

#* realizar una función que se denomine ``extraer_codigo_empresa`` que devuelva en una lista la siguiente información: 
  
 # - nombre de la empresa
#- número de la ofeta
#- número de plazas

#Ayuda: Usar paquetes ``stringr`` para manipular las cadenas.

data <- "28 : 291875 :	INDRA FACTORÍA TECNOLÓGICA, S.L.U. :	2"
result <- stringr::str_split(data, "\\:")
result

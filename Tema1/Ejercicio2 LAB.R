############################################################################################################
#
#   ,------.                     ,--.                                ,---.                ,--.
#   |  .---',--.,--.,--,--, ,---.`--' ,---. ,--,--, ,---.  ,---.    /  O  \  ,---.  ,---. |  |,--. ,--.
#   |  `--, |  ||  ||      \ .--',--.| .-. ||      \ .-. :(  .-'    |  .-.  || .-. || .-. ||  | \  '  /
#   |  |`   '  ''  '|  ||  \ `--.|  |' '-' '|  ||  \   --..-'  `)   |  | |  || '-' '| '-' '|  |  \   '
#   `--'     `----' `--''--'`---'`--' `---' `--''--'`----'`----'    `--' `--'|  |-' |  |-' `--'.-'  /
#                                                                             `--'   `--'       `---'
#
############################################################################################################


calificaciones <- list(alumno1=c(3,7,9,10),
                       alumno2=c(2,7,4,6,4.5))

m1 <- matrix(1:9, nrow = 3)


################################
#
# Ejercicio 1:
#
# Usa lapply para encontrar la longitud de las notas de todos los grupos que están en "lista1"
#
################################

notas_grupo1 <- c(1, 2, 3, 4, 5, 7, 6, 5, 4, 3)

notas_grupo2_3 <-matrix(c(2, 2, 8, 3, 9, 8), nrow = 2)

lista1 <- list(
  list(
    notas_grupo1a = c(1 ,2, 3, 4, 5, 6, 7, 8, 9, 7, 6, 5, 4, 3)
    , notas_grupo1b = c(10, 8, 7, 6)
  )
  , notas_grupo2_3
)

str(lista1)



################################
#
# Ejercicio 2:
#
# Usa sapply() para encontrar la media de las notas de todos los grupos que están en lista1.
#
################################

medias <- sapply(c(lista1[[1]], lista1[2]), mean)




################################
#
# Ejercicio 3:
#
# Usa lapply() para encontrar los quantiles de las notas de todos los grupos que están en lista1.
#
################################

cuantiles <- sapply(c(lista1[[1]], lista1[2]), quantile)


################################
#
# Ejercicio 4:
#
#
#
################################


notas <- c(lista1[[1]], lista1[2])

f1 <- function (x) {
  (3^x) / 10
}

lapply(notas, f1)


################################
#
# Ejercicio 5:
#
#
#
################################



lapply(notas, function (x) {
  3^x / 10
})


################################
#
#
# Aplanar lista unlist()
#
# Ejercicio 6:
#
#
#
################################

repetidos <- function (x, lista) {
  if (!(x %in% lista)) {
    x
  }
}

a <- unlist(notas)

lapply(a, unique)

################################
#
# Ejercicio :
#
#
#
################################







################################
#
# Ejercicio :
#
#
#
################################







################################
#
# Ejercicio :
#
#
#
################################






################################
#
# Ejercicio :
#
#
#
################################






################################
#
# Ejercicio :
#
#
#
################################






################################
#
# Ejercicio :
#
#
#
################################



################################
#
# Ejercicio :
#
#
#
################################





################################
#
# Ejercicio :
#
#
#
################################



################################
#
# Ejercicio :
#
#
#
################################
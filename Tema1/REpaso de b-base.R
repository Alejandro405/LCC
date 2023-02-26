# Variables -> Subray Capitalizaci´on

summary(pi)
class(typeof(1L))


is.double(1L)


# Forzar conversiones
as.character()

# El nulo es NA
# Vectores y matrices

v1 <- 1:10
v1 <- 1:10;v1



v1[c(TRUE, FALSE)]
v1[c(TRUE, FALSE, TRUE)]

seq()

######################################################################################################################
#
# _____                 _   _                   _   ____                                      _             
# |  ___|   _ _ __   ___| |_(_) ___  _ __   __ _| | |  _ \ _ __ ___   __ _ _ __ __ _ _ __ ___ (_)_ __   __ _ 
# | |_ | | | | '_ \ / __| __| |/ _ \| '_ \ / _` | | | |_) | '__/ _ \ / _` | '__/ _` | '_ ` _ \| | '_ \ / _` |
# | |  _|| |_| | | | | (__| |_| | (_) | | | | (_| | | |  __/| | | (_) | (_| | | | (_| | | | | | | | | | | (_|
# |_|   \__,_|_| |_|\___|\__|_|\___/|_| |_|\__,_|_| |_|   |_|  \___/ \__, |_|  \__,_|_| |_| |_|_|_| |_|\__, |
#                                                                     |___/                             |___/ 
#
#####################################################################################################################


# Función anónimas (in-line)

sapply(airquality, function(x) mean(x, na.rm=TRUE))


# Dividir un vector en función de un factor 

splitByMonths <- split(airquality, airquality$Month)
splitByMonths # Gupos de valores del vector x tales que tienen el mísmo valor posicional para el vector y

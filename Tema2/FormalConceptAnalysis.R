############################################################################################################################################################
#  ,------.                               ,--. ,-----.                                    ,--.    ,---.                  ,--.              ,--.            #
#  |  .---',---. ,--.--.,--,--,--. ,--,--.|  |'  .--./ ,---. ,--,--, ,---. ,---.  ,---. ,-'  '-. /  O  \ ,--,--,  ,--,--.|  |,--. ,--.,---.`--' ,---.      #
#  |  `--,| .-. ||  .--'|        |' ,-.  ||  ||  |    | .-. ||      \ .--'| .-. :| .-. |'-.  .-'|  .-.  ||      \' ,-.  ||  | \  '  /(  .-',--.(  .-'      #
#  |  |`  ' '-' '|  |   |  |  |  |\ '-'  ||  |'  '--'\' '-' '|  ||  \ `--.\   --.| '-' '  |  |  |  | |  ||  ||  |\ '-'  ||  |  \   ' .-'  `)  |.-'  `)     #
#  `--'    `---' `--'   `--`--`--' `--`--'`--' `-----' `---' `--''--'`---' `----'|  |-'   `--'  `--' `--'`--''--' `--`--'`--'.-'  /  `----'`--'`----'      #
#                                                                                `--'                                        `---'                         #
############################################################################################################################################################



# con el mismo coste computacional que Reglas de asociación, salida doble, todos los conceptos del dataset (clustering)
# y las implicaciones de atributos (razonamiento autom, reglas asoc de conf 1)


# Formal concept => Objetos que comparten atrib, y atrib que se pressentan en los mismos objetos

# Aumentar objetos -> disminuir atributos

# Clarificación y Reducción simplifican/depura   los datos del dataset para calcular

library(fcaR)

data(planets)

planets[,3]


planetas <- FormalContext$new(planets)


planetas$attributes



# Intensión -> necesito un conjunto para la función intent()

planetas$intent(planetas$attributes[c(1,3,5)])

# Extensión

planetas$extent(set)

# Cierre de atributos o objeto (doble flecha) -> set pueden ser objetos o stributos
# Calcular el cierre de un conjuento del dataset -> dataset

planetas$closure(set)




# Clarificación y Reducción  (filas/objetos "redundantes" son equivalentes a un grupo perse)

planetas$reduce(TRUE) # Intersecciones
planetas$clarify(TRUE) # Repeticiones




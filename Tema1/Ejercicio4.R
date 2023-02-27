
library("tidyverse")

data(starwars)

# 1. Visualiza la variable peso respecto a altura en los personajes de starwars:

ggplot(data = starwars) +
  geom_point( mapping = aes(x = height, y = mass) ) +
  geom_smooth(  mapping = aes(x = height, y = mass) )


# 2. Visualiza las mismas variables pero teniendo en cuenta el género de cada personaje:

ggplot(data = starwars) +
  geom_point(mapping = aes(x = height, y = mass, color = gender))


# 3. Obtener los 10 humanos más viejos, masculinos, con planeta natal Tatooine
starwars <- starwars %>% rename(birth = birth_year)

starwars %>%
  select(name, birth, species, sex, homeworld) %>%
  filter(sex == "male", homeworld == "Tatooine") %>%
  arrange(desc(birth)) %>%
  slice_head(n = 10)


# 4. Encontrar a aquellos personajes de ojos azules y rubios/as de especie humana, procedentes de Tatooine,
#    ordenados por edad de menor a mayor. Calcular la altura media de estos personajes

starwars %>%
  select(name, eye_color, hair_color, sex, homeworld, birth, height) %>%
  filter(
    eye_color == "blue",
    hair_color == "blond",
    sex %in% c("male", "female"),
    homeworld == "Tatooine"
  ) %>%
  arrange(birth) %>%
  summarise(mean(height))   # Colapse a dataframe to a single row


# 5.Encontrar aquellos personajes de especie Human o Naboo; añadir una variable nueva con los valores pesado o ligero
#   (si su masa es mayor que 79 es pesado). Mostrar solo las variables name, height mass y heavyorlight y ordenar por
#   mass de mayor a menor.

categorizaPeso <- function (x) {
  if_else(x > 79, "pesado", "ligero")
}

starwars %>%
  select(name, height, mass, species) %>%
  filter(species %in% c("Human", "Nabo")) %>%
  select(name, height, mass) %>%
  arrange(desc(mass)) %>%
  mutate(heavyorlight = categorizaPeso(mass))


# 6. Calcular el indice de masa corporal de todos los personajes (eliminando los que tienen masa o altura NA).
#    A continuación mostrar el nombre, altura, masa y IMC de cada personaje, con orden de IMC descendente.

starwars %>%
  select(name, height, mass) %>%
  drop_na(mass, height) %>%
  mutate(IMC = 10000 * (mass / (height^2))) %>%
  arrange(desc(IMC))


# 7. Obtener los personajes cuya única nave fuese un X-wing y ordenarlos de forma descendente según su masa

starwars %>%
  select(name, starships, mass) %>%
  filter(starships == "X-wing") %>%
  arrange(desc(mass))

## # A tibble: 3 × 3
##   name              starships  mass
##   <chr>             <list>    <dbl>
## 1 Jek Tono Porkins  <chr [1]>   110
## 2 Biggs Darklighter <chr [1]>    84
## 3 Wedge Antilles    <chr [1]>    77

# 7b. Obtener los personajes con nave X-wing (aunque no sea su única nave) y ordenarlos de forma descendente según su masa

contains <- function (list, elem) {
  some(list, function (x) { x == elem})
}

starwars %>%
  select(name, starships, mass) %>%
  filter(contains(starships, "X-wing")) %>%
  arrange(desc(mass))


starwars %>%
  select(name, starships, mass) %>%
  unnest_longer(starships) %>%
  filter(starships %like% "X-wing") %>%
  arrange(desc(mass))

# 8. Obtener los personajes de masa superior a la media de las masas, obviando valores nulos, y ordenarlos de forma decreciente.

umbral <- mean(starwars$mass, na.rm =TRUE); umbral

starwars %>%
  # drop_na() %>%
  select(name, mass) %>%
  filter(mass > umbral) %>%
  arrange(desc(mass))


# 9. Obtener las alturas medias de los personajes con el campo “gender” igual a “female”,
#    “male” y “hermaphrodite”, ignorando NA.

aux <- starwars %>%
  select(height, gender) %>%
  filter(gender == "feminine" | gender == "masculine" | gender == "hermaphrodite") %>%
  group_by(gender) %>%
  summarise(mean(height, na.rm = TRUE))


# 10. Filtrar por las especies que sean “Droid”, ordenados por altura descendiente y masa (ascendiente) .
#     Reemplazar las masas y alturas con valor NA por 1; mostrar solo la media de todas esas masas y la mediana de esas alturas.

starwars %>%
  select(name, species, height, mass) %>%
  replace_na(list(mass = 1, height = 1)) %>%
  filter(species == "Droid") %>%
  arrange(desc(height), mass) %>%
  summarise(mean(mass), median(height))

# 11. Seleccionar los humanos que midan más de 170 cm y que hayan salido en Attack of the Clones, agrupandolos por
#     homeworld obviando los NA y hallar la media de sus pesos sustituyendo los NA por la mediana y mostrarlos en orden descendiente.

mediana <- median(starwars$mass, na.rm = TRUE)

starwars %>%
  #drop_na() %>%
  select(name, species, height, mass, films, homeworld) %>%
  replace_na(list(mass = mediana)) %>%
  unnest_longer(films) %>%
  filter(species == "Human", height > 170, films %in% "Attack of the Clones") %>%
  group_by(homeworld) %>%
  mutate(mean_mass = mean(mass)) %>%
  slice(1) %>% # Para eliminar filas con el mismo homeworld
  arrange(desc(mean_mass)) %>%
  select(homeworld, mean_mass)


# 12. Encontrar para cada homeworld cuantas especies diferentes lo habitan y ordenalos de mayor a menor variedad.
#     Controlar que no se tiene en cuenta NA como especie

starwars %>%
  drop_na(homeworld, species) %>%
  select(homeworld, species) %>%
  group_by(homeworld) %>%
  mutate(count = length(unique(species))) %>%
  slice(1) %>%
  arrange(desc(count))



as.na()

# 13. Filtrar los datos de aquellos personajes que hayan aparecido solo en la película “Return of the Jedi”y que tengan
#     un mundo natal, ordenados por peso. Para ello transforma los valores NA en 0.

starwars %>%
  select(name, films, homeworld, mass) %>%
  drop_na(homeworld) %>%
  replace_na(list(mass = 0)) %>%
  filter(films %in% list("Return of the Jedi")) %>%
  arrange(desc(mass))


# Solución del enunciado:

arrange(filter(mutate(starwars, mass = coalesce(mass, 0)), films %in% list("Return of the Jedi") & !is.na(homeworld)), desc(mass))

# 14. De todos los personajes de Star Wars filtrar por los que tengan mass mayor o igual a 70, agruparlos por species y
#     gender y calcular la media de height de estos (eliminando los valores NA previamente).
#     Mostrar el resultado ordenado de mayor a menor altura

starwars %>%
  select(name, mass, species, gender, height) %>%
  drop_na() %>%
  filter(mass > 70) %>%
  group_by(species, gender) %>%
  arrange(desc(height)) %>%
  summarise(mediaAlturas = mean(height))


# 15. Filtrar por aquellos personajes que tienen los ojos azules y un homeworld y birth_year asignados (diferentes de NA)
#     Añadir una columna ficticia en la que se indica la edad que tendrían si no hubiesen muerto y actualmente estemos en 2023,
#           es decir, restar a 2023 su año de nacimiento
#     Agrupar según el país dónde viven Obtener como resultado la media de los valores height, mass
#           y la columna edad previamente calculada
#     Ordenar por la columna mass de forma descendentemente

starwars %>%
  select(name, eye_color, homeworld, birth, mass, height) %>%
  drop_na(homeworld, birth) %>%
  filter(eye_color == "blue") %>%
  mutate(newAge = 2023 - birth) %>%
  group_by(homeworld) %>%
  slice(1) %>%
  summarise(height = mean(height), mass = mean(mass), newAge)


# 16. Personajes más altos y bajos

starwars1 <- starwars %>%
drop_na(height) %>%
select(name, height) %>%
arrange(desc(height))

rbind(head(starwars1, n=10), tail(starwars1, n=10)) %>%
ggplot(aes(x=reorder(name, -height), y=height)) +
geom_bar(stat="identity", aes(fill=height), colour="black", show.legend=FALSE) +
scale_fill_gradient(low="darkolivegreen1", high="darkolivegreen4") +
geom_label(aes(label=height)) +
labs(title="Star Wars",
subtitle="Personajes más altos-bajos?",
#       caption="",
y="Altura (cm)") +
theme_light() +
theme(axis.text.x=element_text(angle=45, hjust=1),
axis.title.x=element_blank(),
plot.title=element_text(size=15, face="bold"),
axis.title=element_text(size=12),
axis.line=element_line(size=1.01, color="grey35")) +
geom_vline(xintercept=10.5, linetype="dashed", color="red") +
ylim(0, 400) +
annotate("text", x=5.5, y=370, label="Top 10 más altos", color="red") +
annotate("text", x=15.5, y=370, label="Top 10 más bajos", color="red")
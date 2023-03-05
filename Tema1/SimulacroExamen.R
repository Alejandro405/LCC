library("tidyverse")
library("arules")


# EJERCICIO 1:


data(mtcars)

mtcars$wt

mtcars <- mtcars %>% mutate(
  kg = (wt  / 0.453592) * 1000,
)

MO <- mean(mtcars$kg, na.rm = TRUE)

mtcars <- mtcars %>% mutate(
  objetivoCO2 = 95 + (0.0333 * (kg - MO))
)

mtcars %>% drop_na() %>%  select(kg , objetivoCO2) %>%  arrange(desc(objetivoCO2))

aux <- mtcars %>% drop_na() %>% select(kg , objetivoCO2) %>%  arrange(desc(objetivoCO2)) %>% slice_head(n = 5)

str(mean(aux$kg))

list(aux, mean(aux$kg))



# EJERCICIO 2

mtcars <- data(mtcars)

# EJERCICIO 3
par <- list(minlen = 2)
data(Mushroom)

my_rules <- apriori(
  Mushroom,
  parameter = list(minlen = 2, supp = 0.35017, conf = 0.39374)
)

length(my_rules)
summary(my_rules)
inspect(my_rules)

summary(my_rules[2])

inspect(head(sort(my_rules)))


inspect(head(subset(my_rules, subset = lift > 1), n = 10))


a <-   subset(my_rules, subset = rhs %ain% c("VeilColor=white"))
b <-  subset(my_rules, subset = confidence > 0.3846154)

length(intersect(a, b))


ref <- rhs(sort(my_rules, by = "confidence")[3544])

subset(my_rules, subset = ref %in% rhs)
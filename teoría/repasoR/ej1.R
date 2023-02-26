#calculadora
2+3
cos(pi)
a <-   cos(pi)+1
print(a)
a <- 1L

#conversiones

b <- as.numeric(a)

# vectores

vector1 <- c(1,pi,4,5, NA, 3/0, 0/0, 
             as.numeric("hola"),1:10)

vector2 <- c(3,5,9)

vector2[c(TRUE,FALSE, TRUE, TRUE)]

vector1[c(TRUE,FALSE, TRUE)]

seq(1,10,by=2)

c(1,2,3)+ c(10,20)

vector1[5] <- 0

which(is.na(vector1))

vector1[which(is.na(vector1))]

vector1[is.na(vector1)]

vector1[-1]

vector1
mean(vector1,na.rm=TRUE)
vector1[6] <- 4
mean(vector1,na.rm=TRUE)

vector2 <- c(vector2,NA)
mean(vector2,na.rm=TRUE)


v1 <- 1:10
v1 <- sort(v1,decreasing = TRUE)

v1 <- c(1:10,20:1)
order(v1)
v1[order(v1)]

sample(v1,8,replace = TRUE)


# FACTOR

v1 <- c("bueno","malo","muy malo","genial","genial");v1
v1[1] < v1[2];v1


v2 <- factor(v1)
"muymalo" < "malo"
"genial" <="genial"
v2

v2 <- factor(v1,levels = 
               c("muy malo","malo","bueno","genial",
                 "espectacular"),ordered = TRUE)

class(v2)


str(v2)


#listas

l1 <- list(v1,"alumno2",8,"convocatoria febrero",vector2, 
           list("martinez","fernandez","raul"))

l1[6]
class(l1[6])
str(l1)
str(l1[1])
str(l1[[1]])


l1[6]
l1[6][2]

class(l1[6][[1]])
str(l1[6][[1]])
l1[6][[1]][1]

str(l1[[6]])

l1 <- list(v1,tipodato="alumno2",nota=8,
           convocatoria="convocatoria febrero",
           notasparciales=vector2, 
           datosalumno=list("martinez","fernandez","raul"))
str(l1)
l1$tipodato
l1$datosalumno
l1$datosalumno[1]
l1$datosalumno[[1]]

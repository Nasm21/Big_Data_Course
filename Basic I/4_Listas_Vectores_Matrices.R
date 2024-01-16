#Listas, Vectores y Matrices

# Listas
record <- list(name = "John Doe",
               student_id = 1234,
               grades = c(95, 82, 91, 97, 93),
               final_grade = "A")

#¿Qué tipos de variables tenemos en la lista?

class(record)
str(record)

#Acceder a un elemento de la lista se puede usar $
record$name
record$student_id
# o con el nombre
record[["student_id"]]

record2 <- list("John Doe", 1234)
record2
#Como no tiene nombres no se puede acceder a los elementos con $
record2[[2]]

#vector
x <- c("a", "a", "b", "b", "b", "c")
table(x)

codes <- c(380, 124, 818) # vector tipo numérico
country <- c("italy", "canada", "egypt") # vector tipo caracter

#También se puede asignar un valor a los elementos de los vectores
codes <- c(italy = 380, canada = 124, egypt = 818)
codes
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818); codes


#También se puede asignar un valor con la función names()
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country
codes

#Acceder a una posición específica
codes[2]
codes[c(1,3)]
codes[1:2]

#Si la posición tiene nombre, se puede acceder a su valor por el nombre
codes["canada"]
codes[c("egypt","italy")]

class(codes) #Tipo de variable del vector codes

#Otra función para generar vectores
seq(1:10) #números del 1 al 10
help("seq")
seq(1,10,by=2)
seq(1,10,2)
seq(2,10,2)
seq(0, 100, length.out = 5)

#Matrices
# Todas las entradas son del mismo tipo
mat <- matrix(1:12, 4, 3)
rownames(mat)<-c("Fila1","Fila2","Fila3","fila4")
mat
colnames(mat)<-c("Columna1","Columna2","Columna3")
mat
#Acceder a una posición de la matriz
mat[2, 3] #mat[fila, columna]

#Acceder a una fila
mat[2, ]

#Acceder a una columna
mat[ ,3]

#Acceder a varias columnas
mat[, 2:3]

mat[1:2, 2:3]

#Convertir la matriz en un dataframe
df <- as.data.frame(mat)
df

names(df) <- c("col1", "col2", "col3")
names(df)
df

#También se puede acceder con [] a una posición de un dataframe
library(dslabs)
data("murders")
head(murders)
murders[3, 1]
murders[2:3, ]

#Coerción
x <- c(1, "canada", 3)

#¿Qué tipo de vector es x?
class(x)

#Cambiar tipo de variable
x <- 1:5
y <- as.character(x); y
y <- as.numeric(y);y
class(y)

#Datos no disponibles (NA)
x <- c("1", "b", "3")
x <- as.numeric(x)
x

# Usando una base de datos
library(dslabs)
data(na_example)

# Estructura de la base de datos
str(na_example)

# Calcular la media de los datos
mean(na_example)

# Usar is.na para crear un vector lógico que indique cuales datos son NA
ind <- is.na(na_example)
ind
# Sumar cuantos datos son NA
sum(ind)


# Operador !
x <- c(1, 2, 3)
index <- c(FALSE, TRUE, FALSE)
x[index] #Quitar los true


# Calcular la media de los datos que no son NA
mean(na_example[!ind])

murders$total
#Ordenar datos
sort(murders$total) #ordena de menor a mayor

#Sin embargo no sabemos cuál es el estado con un total de 1257 asesinatos
order(murders$total)
murders$state[5]

#Función order
x <- c(31, 4, 15, 92, 65)
sort(x)

index <- order(x); index #indice de los valores ordenados
x[index]

#
x
order(x)

murders$state[1:6]
murders$abb[1:6]

ind <- order(murders$total) 
ind

murders$state[ind] 
murders$abb[ind] 

#California es el estado con más asesinatos con un total de 1257.

#max y which.max
max(murders$total)

i_max <- which.max(murders$total)#indice con más asesinatos
i_max
murders$state[i_max]

#Ejercicio
#Utilice el operador $ para acceder a los datos del tamaño de la 
#población y almacenarlos en el objeto pop. A continuación, 
#utilice la función order() para redefinir pop de modo que 
#esté ordenado. Por último, utilice el operador [ para informar del 
#menor tamaño de población.



pop <- murders$population
sort(pop)

ord <- order(pop); ord
murders$state[51] 

which.min(murders$population)

#Dataframes
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", 
          "San Juan", "Toronto")
city_temps <- data.frame(Name = city, Temperature = temp)
city_temps






# Data Strukturları

# Vektorlar ----

vektor <- c(3, 19, 112, 4731) #combine
class(vektor)

vektor_2 <- c("b", "7", "b7")    
class(vektor_2)

vektor_3 <- c(3, 19, 112, 4731, "37,5")    
class(vektor_3)


seq(1, 15) #sequence - 1:15
seq(1, 15, 2)

rep(3, 50) #replicate
rep("a", 5) 

x <- c(80, 20)
y <- rep(x, 10) 


v <- c("a", "b", "c", "d", "e")

v[1]
v[2] ; v[3]
v[-1]                          

v[1:3] ; v[3:5]

v[c(1,3,5)]


c1 <- seq(1:9)
c2 <- -11:-19

cbind(c1,c2)

rbind(c1,c2)


# Tablolar/Cədvəllər ----

# Matrix

matrix_1 <- matrix(data = 1:9, ncol = 3, nrow = 3)
matrix_2 <- matrix(data = 1:9, ncol = 3, nrow = 3, 
                   byrow = TRUE)
matrix_1
matrix_2

matrix_3 <- cbind(matrix_1,matrix_2)
matrix_4 <- rbind(matrix_1,matrix_2)
matrix_3 ; matrix_4

matrix_3[2,3]

matrix_3[2,]

matrix_3[,3]

matrix_3[2:3,c(1,6)]

# Data Frame

data_frame <- as.data.frame(matrix_4)

data_frame[2,3]
data_frame[2,]
data_frame[,3]
data_frame[2:3,c(1,3)]

data_frame[2]

data_frame[[2]]

data_frame$V2
data_frame[["V2"]]

data_frame$V2[2]


# Listlər ----

dəyər <- "14"

vektor <- c(2, 19:26, seq(25,31))

tablo <- matrix(vektor, 4, 4)

data <- as.data.frame(tablo)

list <- list(dəyər,vektor,tablo,data)

list[[1]]

list[[3]]

list[[2]][5]

list[[4]][3,4]

list[[4]][[3]]

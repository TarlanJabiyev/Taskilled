# R'da proqramlaşdırma əsasları

# "while" döngəsi ----

şərt <- "şərt doğru olduqda"
icra <- "şərt doğru olduğu müddətdə kodu icra et"

while(şərt){icra}


while(2 > 1){
  print("stop")
}


z <- 1
while(z < 12){
  print(z)
  z <- z + 1
}


# "for" döngəsi ----
sayğac <- "sayğac"
dəyərlər <- "dəyərlər"
icra <- "sayğacı hər bir dəyər üçün icra ey"

for(sayğac in dəyərlər){icra}


for (i in 1:10) {
  print(i)
}
  

vektor <- c(2, 3, 5, 7, 11, 13)
for (gəzən in c(3,5)) {
  print(vektor[gəzən])
}


for (r in 1:5) {
  for (d in c("a","b","c","d","e")) {
    print(paste(r, d))
  }
}

vektor <- c()
for (r in 1:5) {
  for (d in c('a', 'b', 'c', 'd', 'e')) {
    out <- paste(r, d)
    vektor <- c(vektor, out)
  }
}
vektor


df <- data.frame(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

median(df[["a"]])
median(df[["b"]])
median(df[["c"]])
median(df[["d"]])

colnames <- names(df)

for (i in colnames) {
  print(i)
  print(median(df[[i]]))
}


# "if" funksiyası ----
şərt <- "şərt doğru olduqda"
icra <- "şərt doğrudursa kodu icra et"
icra_2 <- "şərt doğru deyilsə kodu icra et"

if(şərt){icra}
if (şərt) {icra} else {icra_2}


x <- 7
if (x >= 9) {
  print("x 9dan böyükdür və ya 9a bərabərdir")
}

if (x >= 9) {
  print("x 9dan böyükdür və ya 9a bərabərdir")
} else {
  print("x 9dan kiçikdir")
}

if (x >= 9) {
  print("x 9dan böyükdür və ya 9a bərabərdir")
} else if (x > 5) {
  print("x 5dən böyükdür, ancaq 9dan kiçikdir")
} else {
  print("x 5dən kiçikdir")
}


for (r in 1:nrow(df)) {
  if(df[[1]][r] > 0){
    print(paste0(r," - row"))
    print(df[[1]][r])
  }
}


for (c in 1:ncol(df)) {
  for (r in 1:nrow(df)) {
    if(df[[c]][r] > 0){
      print(paste(c,"- sütun"))
      print(paste(r,"- sətir"))
      print(df[[c]][r])
    }
  }
}


df$D <- ifelse(df$d>0,"+","-")

df$D <- ifelse(df$d>0,"+",ifelse(df$d==0,"0","-"))


# Funksiya yaratmaq ----

ortalama <- function(x){
  s <- sum(x)
  n <- length(x)
  return(s/n)
}

ortalama(1:100)

formals(ortalama)
body(ortalama)


ortalama <- function(y, hesab = TRUE){
  n <- length(y)
  return(ifelse(hesab, sum(y)/n, sum(y)+7777))
}

y <- 1:100
ortalama(y, hesab = T)
ortalama(y, hesab = F)


f01 <- function(x) {
  if (x < 10) {
    return(0)
  } else {
    return(10)
  }
}

f01(5)
f01(15)


f02 <- function(input) {
  if (!is.numeric(input)) {
    stop("input numeric vektor olmalıdır")
  }
  hesab <- ((input - 25) * 0.75) + 25
  return(hesab)
}

f02(51)
f02("əlli bir")

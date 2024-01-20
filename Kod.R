# Projekt 1
# Kod do odtworzenia obliczeń

# Wczytanie potrzebnych pakietów
library(cowplot)
library(moments)
library(ggplot2)
library(tidyr)
library (dplyr)

set.seed(234)

#Podstawowe statystyki rozkładu Poissona z parametrem λ = 1*

data1 <- rpois(300, 1) # pierwszy argumnet odpowieada ilości generowanych danych , a drugi wartości lambdy

średnia <- mean(data1)
odch_stand <- sd(data1)
skośność <- skewness(data1) # Funkcja skewness i kurtosis pochodzą z pakietu moments
kurtoza <- kurtosis(data1)
statystyki1 <- data.frame(średnia, odch_stand, skośność, kurtoza) # Zebranie wszytskich wyników do jednej tabeli
statystyki1

#Podstawowe statystyki rozkładu Poissona z parametrem λ = 40*

data2 <- rpois(300, 40)

średnia <- mean(data2)
odch_stand <- sd(data2)
skośność <- skewness(data2)
kurtoza <- kurtosis(data2)
statystyki2 <- data.frame(średnia, odch_stand, skośność, kurtoza)
statystyki2

# Wizualizacja rozkładu teoretycznego poissona

x <- seq(0, 8, by = 1)
y <- dpois(x, lambda = 1)

plot(y, main = "Rozkład teoretyczny Poissona", xlab = " ", ylab = " ",pch = 21, bg = "red", cex = 1.3) 

# Wektory potrzebne w dalszej części badania

N <- 800
k <- c(5,10,20,30,50,100,200,500,1000)

# Proces losowania 800 razy i liczenia średnich ( lambda = 1)

x <- (1:9)
lambda <- data.frame(NA) # pusty data.frame do którego będziemy dodawać wyniki poszczególnych losowań
lambda1_1 <- data.frame(x) # data.frame z dziewięcioma wierszami, tutaj będą zapisywane wszytskie wyniki losowań

for ( i in 2:801) {
  
  lambda <- cbind(c(rpois(k[1], 1), rep(NA, 995)),
                  c(rpois(k[2], 1), rep(NA, 990)),
                  c(rpois(k[3], 1), rep(NA, 980)),
                  c(rpois(k[4], 1), rep(NA, 970)),
                  c(rpois(k[5], 1), rep(NA, 950)),
                  c(rpois(k[6], 1), rep(NA, 900)),
                  c(rpois(k[7], 1), rep(NA, 800)),
                  c(rpois(k[8], 1), rep(NA, 500)),
                  rpois(k[9], 1) # tabela ma wymiar 1000 x 9 , z czego dopełnieniem do wartości k są wartości NA. 
                  #Jest to stworzone w ten sposób po to aby można było przechować wszystkie losowania ( niezależnie od k) w jedej tabeli.
  )
  
  # Jako i-tą kolumnę dołączane są średnie z każdego losowania,
  #parametr na.rm = T pozwala na obliczenie średniej bez uwzględnienia NA
  
  lambda1_1[,i] <- colMeans(lambda, na.rm = T) 
  
  # Czyścimy tabela aby w następnej iteracji mogła znowu zapisać wyniki losowania
  lambda <- data.frame(NA)
  
  i = i+1
  
}

# pierwsza kolumna jest nam nie potrzebna ( Byly w niej liczby od 1 do 9)
lambda1_1 <- lambda1_1[,-c(1)]

# Nadaje nazwy wierszą , w zależnościod tego dla jakiego k są to wyniki
rownames(lambda1_1) <- c("5","10","20","30","50","100","200","500","1000")

lambda1_1

y <- (1:9)
lambda <- data.frame(NA)
lambda40_1 <- data.frame(y)

# Proces losowania 800 razy i liczenia średnich ( lambda = 40)

for ( i in 2:801) {
  
  lambda <- cbind(c(rpois(k[1], 40), rep(NA,995)),
                  c(rpois(k[2], 40), rep(NA,990)),
                  c(rpois(k[3], 40), rep(NA,980)),
                  c(rpois(k[4], 40), rep(NA,970)),
                  c(rpois(k[5], 40), rep(NA,950)),
                  c(rpois(k[6], 40), rep(NA,900)),
                  c(rpois(k[7], 40), rep(NA,800)),
                  c(rpois(k[8], 40), rep(NA,500)),
                  rpois(k[9], 40)
  )
  
  
  lambda40_1[,i] <- colMeans(lambda, na.rm = T)
  
  lambda <- data.frame(NA)
  
  i= i+1
  
}

lambda40_1 <- lambda40_1[,-c(1)]
rownames(lambda40_1) <- c("5","10","20","30","50","100","200","500","1000")

lambda40_1

# Statystyki opisowe rozkładów

# funkcja apply pozwala na wykonanie wybranej funkcji ( na przykład mean() ),
# na wybranym wymiarze tabeli ( reprezentuje to drugi argumnet : 1 dla wierszy , 2 dla kolumn)

# funkcja round pozwala zaokrąglić wyniki do wybranego miejsca po przecinku, 
# my zdecydowałyśmy sie na 3 miejsca po przecinku

# Średnia

round (apply(lambda1_1, 1, mean), 3) 

round(apply(lambda40_1, 1, mean), 3)

# Odchylenie standardowe

round (apply(lambda1_1, 1, sd), 3)

round (apply(lambda40_1, 1, sd), 3)

# Skośność

round ( apply(lambda1_1, 1, skewness), 3)

round(apply(lambda40_1, 1, skewness), 3)

# Kurtoza

round (apply(lambda1_1, 1, kurtosis), 3)

round(apply(lambda40_1, 1, kurtosis), 3)

# Stworzenie dodtakowtch tabel które są transponowanymi tabelami wyników losowań, przyda sie to w dalszej części badania

lambda1transponowana_1 <- t(lambda1_1)

lambda40transponowana_1 <- t(lambda40_1)

# Wykresy gęstości ( lambda = 1 )

# Stworzona przez nas funkcja do rysowania wykresów gęstości

D <- function(x , kolor, min , max) { # argumentami funkcji są dane jakie chcemy zwizualizować , kolor wykresu, oraz zakres osi X
  
  ggplot()+
    geom_density(aes(x = x), fill = kolor , col= "black", alpha = 0.7) +
    xlim(min, max) +
    labs ( y = NULL,
           x = NULL)
  
}

# ustalony zakres osi
# Bedzie on taki sam na każdnym wykresie ponieważ wtedy najlepiej widać jak zmienia sie wygląd rozkładu wraz ze zwiększaniem się k
min <- -1
max <- 3

# stworzenie dziewięciu wykresów gestośći , i zapisanie ich w wekrorach

r1 <- D(lambda1transponowana_1[,1], "steelblue", min, max)
r2 <- D(lambda1transponowana_1[,2], "steelblue", min, max)
r3 <- D(lambda1transponowana_1[,3], "steelblue", min, max)
r4 <- D(lambda1transponowana_1[,4], "steelblue", min, max)
r5 <- D(lambda1transponowana_1[,5], "steelblue", min, max)
r6 <- D(lambda1transponowana_1[,6], "steelblue", min, max)
r7 <- D(lambda1transponowana_1[,7], "steelblue", min, max)
r8 <- D(lambda1transponowana_1[,8], "steelblue", min, max)
r9 <- D(lambda1transponowana_1[,9], "steelblue", min, max)

# Funkcja plot_grid z pakietu cowplot pozwala na zamieszczenie wybranej ilości wykresów koło siebie 
# ułatwia to zaprezentowanie wynikiów

plot_grid(r1, r2, r3, r4, labels = c("k = 5", "k = 10","k = 20","k = 30"))
plot_grid(r5, r6, r7, r8, labels = c("k = 50", "k = 100","k = 200","k = 500"))
plot_grid(r9, labels = "k = 1000")

# Wykresy gęstości ( lambda = 40 )

min <- 33
max <- 48
p1 <- D(lambda40transponowana_1[,1], "#9ACD32", min, max)
p2 <- D(lambda40transponowana_1[,2], "#9ACD32", min, max)
p3 <- D(lambda40transponowana_1[,3], "#9ACD32", min, max)
p4 <- D(lambda40transponowana_1[,4], "#9ACD32", min, max)
p5 <- D(lambda40transponowana_1[,5], "#9ACD32", min, max)
p6 <- D(lambda40transponowana_1[,6], "#9ACD32", min, max)
p7 <- D(lambda40transponowana_1[,7], "#9ACD32", min, max)
p8 <- D(lambda40transponowana_1[,8], "#9ACD32", min, max)
p9 <- D(lambda40transponowana_1[,9], "#9ACD32", min, max)

plot_grid(p1, p2, p3, p4, labels= c("k = 5", "k = 10", "k = 20", "k = 30")) 
plot_grid(p5, p6, p7, p8, labels= c("k = 50", "k = 100", "k = 200", "k = 500"))
p9

# Boxplots ( lambda = 1 )

boxplot(lambda1transponowana_1[,1], lambda1transponowana_1[,2],
        lambda1transponowana_1[,3], lambda1transponowana_1[,4],
        names = c("k = 5", "k = 10","k = 20","k = 30"))

boxplot(lambda1transponowana_1[,5], lambda1transponowana_1[,6],
        lambda1transponowana_1[,7], lambda1transponowana_1[,8],
        lambda1transponowana_1[,9],
        names = c("k = 50", "k = 100","k = 200","k = 500", "k = 1000"))

# Boxplots ( lambda = 40 )

boxplot(lambda40transponowana_1[,1], lambda40transponowana_1[,2],
        lambda40transponowana_1[,3], lambda40transponowana_1[,4],
        names = c("k = 5", "k = 10","k = 20","k = 30"))

boxplot(lambda40transponowana_1[,5], lambda40transponowana_1[,6],
        lambda40transponowana_1[,7], lambda40transponowana_1[,8],
        lambda40transponowana_1[,9], 
        names = c("k = 50", "k = 100","k = 200","k = 500", "k = 1000"))

# Test normalności ( lambda = 1 )

Shapirotest1_1 <- data.frame(1,2,3,4,5,6,7,8,9) # stworzenie data.frame z dziewiecioma kolumnami

# Interesuje nas tylko wynik p.value z testu dlatego za pomocą  $p.value wyciagami ten wynik z wszytekigo co zwraca funkcja
# Dodajemy wyniki dla każdego k do tabeli
Shapirotest1_1[1,1] <- shapiro.test(lambda1transponowana_1[,1])$p.value
Shapirotest1_1[1,2] <- shapiro.test(lambda1transponowana_1[,2])$p.value
Shapirotest1_1[1,3] <- shapiro.test(lambda1transponowana_1[,3])$p.value
Shapirotest1_1[1,4] <- shapiro.test(lambda1transponowana_1[,4])$p.value
Shapirotest1_1[1,5] <- shapiro.test(lambda1transponowana_1[,5])$p.value
Shapirotest1_1[1,6] <- shapiro.test(lambda1transponowana_1[,6])$p.value
Shapirotest1_1[1,7] <- shapiro.test(lambda1transponowana_1[,7])$p.value
Shapirotest1_1[1,8] <- shapiro.test(lambda1transponowana_1[,8])$p.value
Shapirotest1_1[1,9] <- shapiro.test(lambda1transponowana_1[,9])$p.value

# Nadajemy nazwy kolumną według tego jakim wartością k odpowiadają 
colnames(Shapirotest1_1) <- c("5","10","20","30","50","100","200","500","1000")

round (Shapirotest1_1, 3)

# Test normalności ( lambda = 40 )

Shapirotest40_1 <- data.frame(1,2,3,4,5,6,7,8,9)

Shapirotest40_1[1,1] <- shapiro.test(lambda40transponowana_1[,1])$p.value
Shapirotest40_1[1,2] <- shapiro.test(lambda40transponowana_1[,2])$p.value
Shapirotest40_1[1,3] <- shapiro.test(lambda40transponowana_1[,3])$p.value
Shapirotest40_1[1,4] <- shapiro.test(lambda40transponowana_1[,4])$p.value
Shapirotest40_1[1,5] <- shapiro.test(lambda40transponowana_1[,5])$p.value
Shapirotest40_1[1,6] <- shapiro.test(lambda40transponowana_1[,6])$p.value
Shapirotest40_1[1,7] <- shapiro.test(lambda40transponowana_1[,7])$p.value
Shapirotest40_1[1,8] <- shapiro.test(lambda40transponowana_1[,8])$p.value
Shapirotest40_1[1,9] <- shapiro.test(lambda40transponowana_1[,9])$p.value

colnames(Shapirotest40_1) <- c("5","10","20","30","50","100","200","500","1000")

round (Shapirotest40_1, 3)

# Wariant drugi 

# W tym wariancie uzyty został ten sam kod, tylko że wyniki podniesiono do kwadratu

# Podstawowe statystyki rozkładu Poissona podniesionego do kwadrau ( lambda = 1)

data3 <- rpois(300, 1)^2

średnia <- mean(data3)
odch_stand <- sd(data3)
skośność <- skewness(data3)
kurtoza <- kurtosis(data3)
statystyki3 <- data.frame(średnia, odch_stand, skośność, kurtoza)
statystyki3

# Podstawowe statystyki rozkładu Poissona do kwadratu ( lambda = 40 )

data4 <- rpois(300, 40)^2

średnia <- mean(data4)
odch_stand <- sd(data4)
skośność <- skewness(data4)
kurtoza <- kurtosis(data4)
statystyki4 <- data.frame(średnia, odch_stand, skośność, kurtoza)
statystyki4

# Wizualizacja rozkładu Poissona do kwadratu 

y = (rpois(20,1))^2

plot(y, main = "Rozkład Poissona do kwadratu", xlab = "", ylab = "",pch = 21, bg = "red", cex = 1.3)

# Proces losowania 800 razy i liczenia srednich ( lambda = 1 )

x <- (1:9)
lambda <- data.frame(NA)
lambda1_2 <- data.frame(x)

for (i in 2:801) {
  
  lambda <- cbind(c(rpois(k[1], 1)^2, rep(NA, 995)),
                  c(rpois(k[2], 1)^2, rep(NA, 990)),
                  c(rpois(k[3], 1)^2, rep(NA, 980)),
                  c(rpois(k[4], 1)^2, rep(NA, 970)),
                  c(rpois(k[5], 1)^2, rep(NA, 950)),
                  c(rpois(k[6], 1)^2, rep(NA, 900)),
                  c(rpois(k[7], 1)^2, rep(NA, 800)),
                  c(rpois(k[8], 1)^2, rep(NA, 500)),
                  rpois(k[9], 1)^2
  )
  
  lambda1_2[,i] <- colMeans(lambda, na.rm = T)
  lambda <- data.frame(NA)
  i = i+1
  
}

lambda1_2 <- lambda1_2[,-c(1)]
rownames(lambda1_2) <- c("5","10","20","30","50","100","200","500","1000")

lambda1_2

# Proces losowania 800 razy i liczenia srednich ( lambda = 40 )

y <- (1:9)
lambda <- data.frame(NA)
lambda40_2 <- data.frame(y)

for ( i in 2:801) {
  
  lambda <- cbind(c(rpois(k[1], 40)^2, rep(NA, 995)),
                  c(rpois(k[2], 40)^2, rep(NA, 990)),
                  c(rpois(k[3], 40)^2, rep(NA, 980)),
                  c(rpois(k[4], 40)^2, rep(NA, 970)),
                  c(rpois(k[5], 40)^2, rep(NA, 950)),
                  c(rpois(k[6], 40)^2, rep(NA, 900)),
                  c(rpois(k[7], 40)^2, rep(NA, 800)),
                  c(rpois(k[8], 40)^2, rep(NA, 500)),
                  rpois(k[9], 40)^2
  )
  
  lambda40_2[,i] <- colMeans(lambda, na.rm = T)
  lambda <- data.frame(NA)
  i = i+1
  
}

lambda40_2 <- lambda40_2[,-c(1)]
rownames(lambda40_2) <- c("5","10","20","30","50","100","200","500","1000")

lambda40_2

# Statystyki opisowe rozkładów

#Średnia

round(apply(lambda1_2, 1, mean), 3)

round(apply(lambda40_2, 1, mean), 3)

# Odchylenie standardowe 

round(apply(lambda1_2, 1, sd), 3)

round(apply(lambda40_2, 1, sd), 3)

# Skośność 

round(apply(lambda1_2, 1, skewness), 3)

round(apply(lambda40_2, 1, skewness), 3)

# Kurtoza 

round(apply(lambda1_2, 1, kurtosis), 3)

round(apply(lambda40_2, 1, kurtosis), 3)

# tabela transponowane 

lambda1transponowana_2 <- t(lambda1_2)
lambda40transponowana_2 <- t(lambda40_2)

# Wykresy gęstości ( lambda = 1)

min <- -1
max <- 5

r1 <- D(lambda1transponowana_2[,1], "steelblue4", min , max)
r2 <- D(lambda1transponowana_2[,2], "steelblue4", min , max)
r3 <- D(lambda1transponowana_2[,3], "steelblue4", min , max)
r4 <- D(lambda1transponowana_2[,4], "steelblue4", min , max)
r5 <- D(lambda1transponowana_2[,5], "steelblue4", min , max)
r6 <- D(lambda1transponowana_2[,6], "steelblue4", min , max)
r7 <- D(lambda1transponowana_2[,7], "steelblue4", min , max)
r8 <- D(lambda1transponowana_2[,8], "steelblue4", min , max)
r9 <- D(lambda1transponowana_2[,9], "steelblue4", min , max)

plot_grid(r1, r2, r3, r4, labels = c("k = 5", "k = 10", "k = 20", "k = 30"))
plot_grid(r5, r6, r7, r8, labels = c("k = 50", "k = 100", "k = 200", "k =500"))
plot_grid(r9, labels = "k = 1000")

# Wykresy gęstości ( lambda = 1)

min <- 1000
max <- 2500

p1 <- D(lambda40transponowana_2[,1], "green3", min, max)
p2 <- D(lambda40transponowana_2[,2], "green3", min, max)
p3 <- D(lambda40transponowana_2[,3], "green3", min, max)
p4 <- D(lambda40transponowana_2[,4], "green3", min, max)
p5 <- D(lambda40transponowana_2[,5], "green3", min, max)
p6 <- D(lambda40transponowana_2[,6], "green3", min, max)
p7 <- D(lambda40transponowana_2[,7], "green3", min, max)
p8 <- D(lambda40transponowana_2[,8], "green3", min, max)
p9 <- D(lambda40transponowana_2[,9], "green3", min, max)

plot_grid(p1, p2, p3, p4, labels = c("k = 5", "k = 10", "k = 20", "k = 30")) 
plot_grid(p5, p6, p7, p8, labels = c("k = 50", "k = 100","k = 200","k = 500"))
p9

# boxploty ( lambda = 1 )

boxplot(lambda1transponowana_2[,1], lambda1transponowana_2[,2],
        lambda1transponowana_2[,3], lambda1transponowana_2[,4],
        names = c("k = 5", "k = 10","k = 20","k = 30"))

boxplot(lambda1transponowana_2[,5], lambda1transponowana_2[,6],
        lambda1transponowana_2[,7],lambda1transponowana_2[,8],
        lambda1transponowana_2[,9],
        names = c("k = 50", "k = 100","k = 200","k = 500", "k = 1000"))

# boxploty ( lambda = 40 )

boxplot(lambda40transponowana_2[,1], lambda40transponowana_2[,2],
        lambda40transponowana_2[,3], lambda40transponowana_2[,4],
        names = c("k = 5", "k = 10","k = 20","k = 30"))

boxplot(lambda40transponowana_2[,5], lambda40transponowana_2[,6],
        lambda40transponowana_2[,7], lambda40transponowana_2[,8],
        lambda40transponowana_2[,9],
        names = c("k = 50", "k = 100","k = 200","k = 500", "k = 1000"))

# Badanie normalności ( lambda = 1)

Shapirotest1_2 <- data.frame(1,2,3,4,5,6,7,8,9)
Shapirotest40_2 <- data.frame(1,2,3,4,5,6,7,8,9)

Shapirotest1_2[1,1] <- shapiro.test(lambda1transponowana_2[,1])$p.value
Shapirotest1_2[1,2] <- shapiro.test(lambda1transponowana_2[,2])$p.value
Shapirotest1_2[1,3] <- shapiro.test(lambda1transponowana_2[,3])$p.value
Shapirotest1_2[1,4] <- shapiro.test(lambda1transponowana_2[,4])$p.value
Shapirotest1_2[1,5] <- shapiro.test(lambda1transponowana_2[,5])$p.value
Shapirotest1_2[1,6] <- shapiro.test(lambda1transponowana_2[,6])$p.value
Shapirotest1_2[1,7] <- shapiro.test(lambda1transponowana_2[,7])$p.value
Shapirotest1_2[1,8] <- shapiro.test(lambda1transponowana_2[,8])$p.value
Shapirotest1_2[1,9] <- shapiro.test(lambda1transponowana_2[,9])$p.value

colnames(Shapirotest1_2) <- c("5","10","20","30","50","100","200","500","1000")
round(Shapirotest1_2, 3)

# Badanie normalności ( lambda = 40 )

Shapirotest40_2[1,1] <- shapiro.test(lambda40transponowana_2[,1])$p.value
Shapirotest40_2[1,2] <- shapiro.test(lambda40transponowana_2[,2])$p.value
Shapirotest40_2[1,3] <- shapiro.test(lambda40transponowana_2[,3])$p.value
Shapirotest40_2[1,4] <- shapiro.test(lambda40transponowana_2[,4])$p.value
Shapirotest40_2[1,5] <- shapiro.test(lambda40transponowana_2[,5])$p.value
Shapirotest40_2[1,6] <- shapiro.test(lambda40transponowana_2[,6])$p.value
Shapirotest40_2[1,7] <- shapiro.test(lambda40transponowana_2[,7])$p.value
Shapirotest40_2[1,8] <- shapiro.test(lambda40transponowana_2[,8])$p.value
Shapirotest40_2[1,9] <- shapiro.test(lambda40transponowana_2[,9])$p.value

colnames(Shapirotest40_2) <- c("5","10","20","30","50","100","200","500","1000")
round(Shapirotest40_2, 3)






































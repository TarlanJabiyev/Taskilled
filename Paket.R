# Paket

# R Studio’da istənilən bir funksiyasının sənədinə baxmaq ----

c()
seq()
rep()
cbind()
rbind()
as.data.frame()
print()
ifelse()


# Paket nədir və necə yüklənir? ----

base::abs(-3) # göy - funksiya
base::pi # çəhrayı - dəyər


install.packages("dslabs")
library(dslabs)

dslabs::movielens # cədvəl - data

data <- dslabs::movielens


# “tidyverse” paketi haqqında ----

install.packages("tidyverse") # CRAN
remotes::install_github("tidyverse/tidyverse") # github

library(tidyverse)

?`dplyr-package`
?`tibble-package`
?`tidyr-package`
?`stringr-package`
?`forcats-package`
?`ggplot2-package`
?`purrr-package`

library(dplyr);library(tibble);library(tidyr);library(stringr);library(forcats);library(ggplot2);library(purrr)


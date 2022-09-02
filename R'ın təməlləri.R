# R'ın təməlləri

# Data tipləri ----

class(7) # numeric
class(3.14) # numeric

typeof(7) # double
typeof(3.14) # double

class(7L) # integer
class(3.14L) # numeric

class("salam dünya") # character

class(T) # logical
class(F) # logical

class(2i + 3) # complex


# R proqramlaşdırma operatorları ----

# Hesab operatorları

# +
5 + 5

# -
5 - 5

# * 
5 * 5

# /
5 / 5

# ^
5 ^ 5

# Logical operatorlar

# "==" bərabərdirmi
# "!=" bərabər deyilmi
# "<" ; ">" ; "<=" ; ">="
# "&" is "and"
# "|" is "or"
# "!" is "not"

# | - "or"
2 | 2

# & - "and"
2 & 2

# ==
5 == 2 + 3

# !=
5 != 4
5 != 5

# > və <
(3 > 7) | (3 != 7)
(3 > 7) & (3 != 7)

# TRUE və FALSE
TRUE | FALSE
FALSE | FALSE
F | T
T & F
T & T 


# Təyinat(assignment) operatoru ----

hesab = 5 + 5
hesab

hesab_2 <- 5 + 5
5 + 5 -> hesab_3


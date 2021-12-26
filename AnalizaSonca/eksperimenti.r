library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

m <- c('3','4','5','6','7','8','9','10')
n <- c('2','3','4','5','6','7','8','9','10')

### Hitting time za graf oblike sonca z za�etkom na kliki in kon�amo na poti, ki
### ni z grafom povezana preko za�etnega vozli��a

pokritje.sonce <- read_csv('sprehod_sonce.csv',col_names = n) 
pokritje.sonce['m'] <- m
pokritje.sonce <- pokritje.sonce %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina') 


## graf glede na razli�en m pri fiksnem n

graf.pokritje.sonce <- ggplot(data = pokritje.sonce %>% filter(n=='3' | n=='6' | n=='10'),aes(x =m,y=dolzina)) + 
  geom_point() + facet_grid(.~n) + ylab('�as pokritja') 
graf.pokritje.sonce

# opaziva, da je �as skoraj neodvisen od m

## graf glede na razli�en n pri fiksnem m

graf.pokritje.sonce <- ggplot(data = pokritje.sonce %>% filter(m=='3' | m=='6' | m=='10'),aes(x =n,y=dolzina)) + 
  geom_point() + facet_grid(.~m)
graf.pokritje.sonce

# ob spreminjajo�em se n, se �as bistveno hitreje ve�a
# �as pokritja je v splo�nem C*e^n, kjer je C konstanta
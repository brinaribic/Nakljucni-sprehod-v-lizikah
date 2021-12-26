library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

m <- c('3','4','5','6','7','8','9','10')
n <- c('2','3','4','5','6','7','8','9','10')

### Hitting time za graf oblike sonca z zaèetkom na kliki in konèamo na poti, ki
### ni z grafom povezana preko zaèetnega vozlišèa

pokritje.sonce <- read_csv('sprehod_sonce.csv',col_names = n) 
pokritje.sonce['m'] <- m
pokritje.sonce <- pokritje.sonce %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina') 


## graf glede na razlièen m pri fiksnem n

graf.pokritje.sonce <- ggplot(data = pokritje.sonce %>% filter(n=='3' | n=='6' | n=='10'),aes(x =m,y=dolzina)) + 
  geom_point() + facet_grid(.~n) + ylab('Èas pokritja') 
graf.pokritje.sonce

# opaziva, da je èas skoraj neodvisen od m

## graf glede na razlièen n pri fiksnem m

graf.pokritje.sonce <- ggplot(data = pokritje.sonce %>% filter(m=='3' | m=='6' | m=='10'),aes(x =n,y=dolzina)) + 
  geom_point() + facet_grid(.~m)
graf.pokritje.sonce

# ob spreminjajoèem se n, se èas bistveno hitreje veèa
# èas pokritja je v splošnem C*e^n, kjer je C konstanta
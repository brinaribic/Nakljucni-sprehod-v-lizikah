library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

m <- c('3','4','5','6','7','8','9','10')
n <- c('2','3','4','5','6','7','8','9','10')

### Hitting time za graf oblike sonca z zaèetkom na kliki in konèamo na poti, ki
### ni z grafom povezana preko zaèetnega vozlišèa

dosega.sonce <- read_csv('hitting_sonce1.csv',col_names = n) 
dosega.sonce['m'] <- as.factor(m)
dosega.sonce <- dosega.sonce %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina') 


## graf glede na razlièen m pri fiksnem n

graf.dosega.sonce1 <- ggplot(data = dosega.sonce %>% 
                                filter(n=='3' | n=='6' | n=='10'),aes(x =reorder(m,dolzina),y=dolzina)) +
  geom_point() +
  facet_grid(.~n) +
  ylab('Èas dosega') + 
  xlab('m') +
  ggtitle('Èas dosega glede na razliène m')
  
graf.dosega.sonce1

# opaziva, da se cas spreminja linearno glede na m

## graf glede na razlièen n pri fiksnem m

graf.dosega.sonce2 <- ggplot(data = dosega.sonce %>% 
                                filter(m=='3' | m=='6' | m=='10'),aes(x=reorder(n,dolzina), y=dolzina)) + 
  geom_point() + 
  facet_grid(.~m) +
  ylab('Èas dosega') + 
  xlab('n') +
  ggtitle('Èas dosega glede na razlièen n')
graf.dosega.sonce2

# pri fiknsem n, se spreminja kvadratièno

## èas pokritja (cover time) z zaèetkom na kliki

pokritje.sonce <- read_csv('cover_sonce1.csv',col_names = n) 
pokritje.sonce['m'] <- as.factor(m)
pokritje.sonce <- pokritje.sonce %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina') 

# fiksen n

graf.pokritja.sonce1 <- ggplot(data = pokritje.sonce %>% 
                               filter(n=='3' | n=='6' | n=='10'),aes(x =reorder(m,dolzina),y=dolzina)) +
  geom_point() +
  facet_grid(.~n) +
  ylab('Èas pokritja') + 
  xlab('m') +
  ggtitle('Èas pokritja glede na razliène n')

graf.pokritja.sonce1

# fiksen m

graf.pokritja.sonce2 <- ggplot(data = pokritje.sonce %>% 
                                 filter(m=='3' | m=='6' | m=='10'),aes(x =reorder(n,dolzina),y=dolzina)) +
  geom_point() +
  facet_grid(.~m) +
  ylab('Èas pokritja') + 
  xlab('n') +
  ggtitle('Èas pokritja glede na razliène m')
graf.pokritja.sonce2


## èas vrnitve za sonce z zaèetkom na kliki

vrnitev.sonce <- read_csv('return_sonce.csv',col_names = n) 
vrnitev.sonce['m'] <- as.factor(m)
vrnitev.sonce <- vrnitev.sonce %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina') 


graf.vrnitev.sonce <- ggplot(vrnitev.sonce, aes(x=reorder(n,dolzina),y=reorder(m,dolzina),size=dolzina)) + geom_point()
graf.vrnitev.sonce


sonce.skupaj <- pokritje.sonce %>% mutate('doseg' = dosega.sonce[3] )


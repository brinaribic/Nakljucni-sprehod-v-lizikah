library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
require(readxl)
require(openxlsx)


m <- c('3','4','5','6','7','8','9','10')
n <- c('2','3','4','5','6','7','8','9','10')

## èas pokritja, èe zaènemo na kliki

pokritje.sonce <- read_csv('cover_sonce1.csv',col_names = n) 
pokritje.sonce['m'] <- as.factor(m)
pokritje.sonce <- pokritje.sonce %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina') 

pokritje.klike <- read_csv('cover_klike1.csv',col_names = n)
pokritje.klike['m'] <- as.factor(m)
pokritje.klike <- pokritje.klike %>% pivot_longer(n, names_to = 'n',values_to = 'pokritje')

pokritje.lizika <- read_xlsx("pricakovanicasi.xlsx", sheet = "List1") %>%
  slice(c(2:10))
colnames(pokritje.lizika) <- pokritje.lizika[1,]
pokritje.lizika <- pokritje.lizika[-1,]
pokritje.lizika <- pokritje.lizika %>% 
  pivot_longer(-(1), names_to="n", values_to="lizika")

pokritje.sonce['klike'] <- pokritje.klike[3]
pokritje.sonce['lizika'] <- pokritje.lizika[3]
colnames(pokritje.sonce) <- c('m','n','sonce',"graf s klikama na koncih poti",'lizika')
pokritje.sonce <- pokritje.sonce %>% pivot_longer(-c(1,2), names_to = 'vrsta',values_to = 'pokritje')

# fiksen n

par(mfrow=c(1,2))

primerjava1 <- ggplot(pokritje.sonce %>% filter(n=='3' | n=='6' | n=='10'),
                      aes(x=reorder(m,pokritje),y=pokritje,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236", "#5CB85C")) +
  geom_point() +
  facet_wrap(.~reorder(n,pokritje)) +
  xlab('m') +
  ylab('èas pokritja') + 
  ggtitle('primerjava èasa pokritja pri razliènih n') +
  labs(fill="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika",'sonce')) +
  theme_bw()
primerjava1

# fiksen m

primerjava2 <- ggplot(pokritje.sonce %>% filter(m=='3' | m=='6' | m=='10'),
                      aes(x=reorder(n,pokritje),y=pokritje,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236", "#5CB85C")) +
  geom_point() +
  facet_wrap(.~reorder(m,pokritje)) +
  xlab('n') +
  ylab('èas pokritja') + 
  ggtitle('primerjava èasa pokritja pri razliènih m') +
  labs(fill="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika",'sonce')) +
  theme_bw()
primerjava2


## èas pokritja, èe izberemo napaèen n

pokritje.sonce1 <- read_csv('cover_sonce2.csv',col_names = n) 
pokritje.sonce1['m'] <- as.factor(m)
pokritje.sonce1 <- pokritje.sonce1 %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina')

pokritje.klike1 <- read_csv('cover_klike2.csv',col_names = n)
pokritje.klike1['m'] <- as.factor(m)
pokritje.klike1 <- pokritje.klike1 %>% pivot_longer(n, names_to = 'n',values_to = 'pokritje')

pokritje.lizika1 <- read_xlsx("pricakovanicasi.xlsx", sheet = "List2") %>%
  slice(c(1:10))
colnames(pokritje.lizika1) <- pokritje.lizika1[1,]
pokritje.lizika1 <- pokritje.lizika1[-1,]
pokritje.lizika1 <- pokritje.lizika1 %>% 
  pivot_longer(-(1), names_to="n", values_to="lizika")

pokritje.sonce1['klike'] <- pokritje.klike1[3]
pokritje.sonce1['lizika'] <- pokritje.lizika1[3]
colnames(pokritje.sonce1) <- c('m','n','sonce',"graf s klikama na koncih poti",'lizika')
pokritje.sonce1 <- pokritje.sonce1 %>% pivot_longer(-c(1,2), names_to = 'vrsta',values_to = 'pokritje')

# fiksen n

primerjava3 <- ggplot(pokritje.sonce1 %>% filter(n=='3' | n=='6' | n=='10'),
                      aes(x=reorder(m,pokritje),y=pokritje,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236", "#5CB85C")) +
  geom_point() +
  facet_wrap(.~reorder(n,pokritje)) +
  xlab('m') +
  ylab('èas pokritja') + 
  ggtitle('primerjava èasa pokritja pri razliènih n') +
  labs(fill="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika",'sonce')) +
  theme_bw()
primerjava3

# fiksen m

primerjava4 <- ggplot(pokritje.sonce1 %>% filter(m=='3' | m=='6' | m=='10'),
                      aes(x=reorder(n,pokritje),y=pokritje,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236", "#5CB85C")) +
  geom_point() +
  facet_wrap(.~reorder(m,pokritje)) +
  xlab('n') +
  ylab('èas pokritja') + 
  ggtitle('primerjava èasa pokritja pri razliènih m') +
  labs(fill="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika",'sonce')) +
  theme_bw()
primerjava4


## èas dosega, èe izberemo zaèetno in konèno vozlišèe na kliki, ki nista del mostu

# tu sonce izpustiva, saj ne loèiva analize v odvisnosti od vozlišèa na mostu

doseg.lizika<- read_xlsx("pricakovanicasi.xlsx", sheet = "List4") %>%
  slice(c(2:10))
colnames(doseg.lizika) <- doseg.lizika[1,]
doseg.lizika <- doseg.lizika[-1,]
doseg.lizika <- doseg.lizika %>% 
  pivot_longer(-(1), names_to="n", values_to="lizika")

dosega.klike <- read_csv('hitting_klike1.csv',col_names = n)
dosega.klike['m'] <- as.factor(m)
dosega.klike <- dosega.klike %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina')
dosega.klike['dolzina'] <- round(dosega.klike['dolzina'],0)

dosega.klike['lizika'] <- doseg.lizika[3]
colnames(dosega.klike) <- c('m','n','graf s klikama na koncih poti','lizika')
dosega.klike <- dosega.klike %>% pivot_longer(-c(1,2), names_to = 'vrsta',values_to = 'doseg')

# fiksen n

primerjava5 <- ggplot(dosega.klike%>%filter(n=='3' | n=='6' | n=='10' | n=='8'),
                      aes(x=reorder(m,doseg),y=doseg,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236")) +
  geom_point() +
  facet_wrap(.~reorder(n,doseg)) +
  xlab('m') +
  ylab('èas dosega') + 
  ggtitle('primerjava èasov dosega pri razliènih n') +
  labs(group="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika")) +
  theme_bw()
primerjava5

# fiksen m

primerjava6 <- ggplot(dosega.klike%>%filter(m=='3' | m=='6' | m=='10' | m=='8'),
                      aes(x=reorder(n,doseg),y=doseg,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236")) +
  geom_point() +
  facet_wrap(.~reorder(m,doseg)) +
  xlab('n') +
  ylab('èas dosega') + 
  ggtitle('primerjava èasov dosega pri razliènih m') +
  labs(group="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika")) +
  theme_bw()
primerjava6

## èas dosega, èe je zaèetno vozlišèe del mostu

doseg.lizika1<- read_xlsx("pricakovanicasi.xlsx", sheet = "List3") %>%
  slice(c(1:11))
colnames(doseg.lizika1) <- doseg.lizika1[1,]
doseg.lizika1 <- doseg.lizika1[-1,]
doseg.lizika1 <- doseg.lizika1 %>% 
  pivot_longer(-(1), names_to="n", values_to="lizika")

dosega.klike1 <- read_csv('hitting_klike2.csv',col_names = n)
dosega.klike1['m'] <- as.factor(m)
dosega.klike1 <- dosega.klike1 %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina')
dosega.klike1['dolzina'] <- round(dosega.klike1['dolzina'],0)

dosega.klike1['lizika'] <- doseg.lizika1[3]
colnames(dosega.klike1) <- c('m','n','graf s klikama na koncih poti','lizika')
dosega.klike1 <- dosega.klike1 %>% pivot_longer(-c(1,2), names_to = 'vrsta',values_to = 'doseg')

# fiksen n

primerjava7 <- ggplot(dosega.klike1,
                      aes(x=reorder(m,doseg),y=doseg,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236")) +
  geom_point() +
  facet_wrap(.~reorder(n,doseg)) +
  xlab('m') +
  ylab('èas dosega') + 
  ggtitle('primerjava èasov dosega pri razliènih n') +
  labs(group="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika")) +
  theme_bw()
primerjava7

# fiksen m

primerjava8 <- ggplot(dosega.klike1%>%filter(m=='3' | m=='6' | m=='10' | m=='8'),
                      aes(x=reorder(n,doseg),y=doseg,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236")) +
  geom_point() +
  facet_wrap(.~reorder(m,doseg)) +
  xlab('n') +
  ylab('èas dosega') + 
  ggtitle('primerjava èasov dosega pri razliènih m') +
  labs(group="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika")) +
  theme_bw()
primerjava8

## èas vrnitve, èe zaèetno ni del mostu

vrnitev.lizika<- read_xlsx("pricakovanicasi.xlsx", sheet = "List7") %>%
  slice(c(2:10))
colnames(vrnitev.lizika) <- vrnitev.lizika[1,]
vrnitev.lizika <- vrnitev.lizika[-1,]
vrnitev.lizika <- vrnitev.lizika %>% 
  pivot_longer(-(1), names_to="n", values_to="lizika")

vrnitev.klike <- read_csv('return_klike1.csv',col_names = n)
vrnitev.klike['m'] <- as.factor(m)
vrnitev.klike <- vrnitev.klike %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina')
vrnitev.klike['dolzina'] <- round(vrnitev.klike['dolzina'],0)

vrnitev.klike['lizika'] <- doseg.lizika[3]
colnames(vrnitev.klike) <- c('m','n','graf s klikama na koncih poti','lizika')
vrnitev.klike <- vrnitev.klike %>% pivot_longer(-c(1,2), names_to = 'vrsta',values_to = 'doseg')

# fiksen n

primerjava9 <- ggplot(vrnitev.klike%>%filter(n=='3' | n=='6' | n=='10' | n=='8'),
                      aes(x=reorder(m,doseg),y=doseg,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236")) +
  geom_point() +
  facet_wrap(.~reorder(n,doseg)) +
  xlab('m') +
  ylab('èas vrnitev') + 
  ggtitle('primerjava èasov vrnitve pri razliènih n') +
  labs(group="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika")) +
  theme_bw()
primerjava9

# fiksen m

primerjava10 <- ggplot(vrnitev.klike%>%filter(m=='3' | m=='6' | m=='10' | m=='8'),
                      aes(x=reorder(n,doseg),y=doseg,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236")) +
  geom_point() +
  facet_wrap(.~reorder(m,doseg)) +
  xlab('n') +
  ylab('èas vrnitev') + 
  ggtitle('primerjava èasov vrnitve pri razliènih m') +
  labs(group="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika")) +
  theme_bw()
primerjava10

# zaèetno vozlišèe je del mostu

vrnitev1.lizika<- read_xlsx("pricakovanicasi.xlsx", sheet = "List8") %>%
  slice(c(2:10))
colnames(vrnitev1.lizika) <- vrnitev1.lizika[1,]
vrnitev1.lizika <- vrnitev1.lizika[-1,]
vrnitev1.lizika <- vrnitev1.lizika %>% 
  pivot_longer(-(1), names_to="n", values_to="lizika")

vrnitev1.klike <- read_csv('return_klike2.csv',col_names = n)
vrnitev1.klike['m'] <- as.factor(m)
vrnitev1.klike <- vrnitev1.klike %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina')
vrnitev1.klike['dolzina'] <- round(vrnitev1.klike['dolzina'],0)

vrnitev1.klike['lizika'] <- doseg.lizika[3]
colnames(vrnitev1.klike) <- c('m','n','graf s klikama na koncih poti','lizika')
vrnitev1.klike <- vrnitev1.klike %>% pivot_longer(-c(1,2), names_to = 'vrsta',values_to = 'doseg')

# fiksen n

primerjava10 <- ggplot(vrnitev1.klike%>%filter(n=='3' | n=='6' | n=='10' | n=='8'),
                      aes(x=reorder(m,doseg),y=doseg,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236")) +
  geom_point() +
  facet_wrap(.~reorder(n,doseg)) +
  xlab('m') +
  ylab('èas vrnitev') + 
  ggtitle('primerjava èasov vrnitve pri razliènih n') +
  labs(group="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika")) +
  theme_bw()
primerjava10

# fiksen m

primerjava11 <- ggplot(vrnitev1.klike%>%filter(m=='3' | m=='6' | m=='10' | m=='8'),
                       aes(x=reorder(n,doseg),y=doseg,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236")) +
  geom_point() +
  facet_wrap(.~reorder(m,doseg)) +
  xlab('n') +
  ylab('èas vrnitev') + 
  ggtitle('primerjava èasov vrnitve pri razliènih m') +
  labs(group="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika")) +
  theme_bw()
primerjava11

# zaèetno vozlišèe je del mostu

vrnitev2.lizika<- read_xlsx("pricakovanicasi.xlsx", sheet = "List9") %>%
  slice(c(2:10))
colnames(vrnitev2.lizika) <- vrnitev2.lizika[1,]
vrnitev2.lizika <- vrnitev2.lizika[-1,]
vrnitev2.lizika <- vrnitev2.lizika %>% 
  pivot_longer(-(1), names_to="n", values_to="lizika")

vrnitev2.klike <- read_csv('return_klike3.csv',col_names = n)
vrnitev2.klike['m'] <- as.factor(m)
vrnitev2.klike <- vrnitev2.klike %>% pivot_longer(n, names_to = 'n',values_to = 'dolzina')
vrnitev2.klike['dolzina'] <- round(vrnitev2.klike['dolzina'],0)

vrnitev2.klike['lizika'] <- doseg.lizika[3]
colnames(vrnitev2.klike) <- c('m','n','graf s klikama na koncih poti','lizika')
vrnitev2.klike <- vrnitev2.klike %>% pivot_longer(-c(1,2), names_to = 'vrsta',values_to = 'doseg')

# fiksen n

primerjava12 <- ggplot(vrnitev2.klike%>%filter(n=='3' | n=='6' | n=='10' | n=='8'),
                      aes(x=reorder(m,doseg),y=doseg,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236")) +
  geom_point() +
  facet_wrap(.~reorder(n,doseg)) +
  xlab('m') +
  ylab('èas vrnitev') + 
  ggtitle('primerjava èasov vrnitve pri razliènih n') +
  labs(group="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika")) +
  theme_bw()
primerjava12

# fiksen m

primerjava13 <- ggplot(vrnitev2.klike%>%filter(m=='3' | m=='6' | m=='10' | m=='8'),
                       aes(x=reorder(n,doseg),y=doseg,group=vrsta,colour=vrsta)) + 
  geom_line() +
  scale_color_manual(values = c("#D43F3A", "#EEA236")) +
  geom_point() +
  facet_wrap(.~reorder(m,doseg)) +
  xlab('n') +
  ylab('èas vrnitev') + 
  ggtitle('primerjava èasov vrnitve pri razliènih m') +
  labs(group="Vrsta grafa") +
  scale_fill_discrete(labels = c("graf s klikama na koncih poti", "lizika")) +
  theme_bw()
primerjava13




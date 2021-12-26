require(dplyr)
require(ggplot2)
require(readxl)
require(openxlsx)
require(tidyr)
require(readr)

cover_time_podatki <- read_xlsx("pricakovanicasi.xlsx", sheet = "List1") %>%
  slice(c(2:10))

colnames(cover_time_podatki) <- cover_time_podatki[1,]
cover_time_podatki <- cover_time_podatki[-1,]

cover_time_podatki <- cover_time_podatki %>% 
  pivot_longer(-(1), names_to="n", values_to="covertime")

s1 <- rep(3:10, each=9)
s2 <- rep(2:10, times=8)
covertime <- pull(cover_time_podatki, 3)
cover_time_podatki <- data.frame(s1, s2, covertime)
colnames(cover_time_podatki) <- c("m","n","covertime")



graf_fiksen_m <- cover_time_podatki %>%
  filter(m==3 | m==6 | m==10) %>%
  ggplot(aes(x=n, y=covertime), group=m) +
  geom_line(col="black") +
  geom_point(col="red") +
  facet_grid(~m) + 
  ylab("Čas pokritja")
  

print(graf_fiksen_m)

graf_fiksen_n <- cover_time_podatki %>%
  filter(n==2 | n==6 | n==10) %>%
  ggplot(aes(x=m, y=covertime), group=n) +
  geom_line(col="black") +
  geom_point(col="red") +
  facet_grid(~n) + 
  ylab("Čas pokritja") +
  theme_bw()

print(graf_fiksen_n)

#####
# hipoteza n^2+m

hipotezatabela <- read_xlsx("pricakovanicasi.xlsx", sheet = "List6") %>%
  slice(c(2:10))

colnames(hipotezatabela) <- hipotezatabela[1,]
hipotezatabela <- hipotezatabela[-1,]

hipotezatabela <- hipotezatabela %>% 
  pivot_longer(-(1), names_to="n", values_to="covertime")

s1 <- rep(3:10, each=9)
s2 <- rep(2:10, times=8)
time <- pull(hipotezatabela, 3)
hipotezatabela <- data.frame(s1, s2, time)
colnames(hipotezatabela) <- c("m","n","time")

hipotezatabela <- hipotezatabela %>%
  mutate(kolicnik=round(time/(n^2+m),3)) %>%
  select(-time) %>%
  pivot_wider(names_from = n, values_from = kolicnik)

hipotezatabela %>% write.xlsx("hipoteza2.xlsx")


#### čas vrnitve, konec poti

t <- read_xlsx("pricakovanicasi.xlsx", sheet = "List9") %>%
  slice(c(2:10))

colnames(t) <- t[1,]
t <- t[-1,]

t <- t %>% 
  pivot_longer(-(1), names_to="n", values_to="time")

s1 <- rep(3:10, each=9)
s2 <- rep(2:10, times=8)
time1 <- pull(t, 3)
t <- data.frame(s1, s2, time1)
colnames(t) <- c("m","n","time")



graf_fiksen_m <- t %>%
  filter(m==3 | m==6 | m==10) %>%
  ggplot(aes(x=n, y=time), group=m) +
  geom_line(col="black") +
  geom_point(col="red") +
  facet_grid(~m) + 
  ylab("Čas vrnitve") +
  theme_bw()


print(graf_fiksen_m)

graf_fiksen_n <- t %>%
  filter(n==2 | n==6 | n==10) %>%
  ggplot(aes(x=m, y=time), group=n) +
  geom_line(col="black") +
  geom_point(col="red") +
  facet_grid(~n) + 
  ylab("Čas vrnitve") +
  theme_bw()

print(graf_fiksen_n)

# simulando a criacao da tabela
tab <- data.frame(
  id = 1:10000, # c(1, 2, 3, 4, 5, 6, 7, 8, ...)
  anos = sample(0:30, 10000, replace = TRUE),
  sexo = sample(c("M", "F"), 10000, replace = TRUE),
  UF = sample(c("SP", "DF", "CE"), 10000, replace = TRUE),
  renda = rnorm(10000, mean = 5476.67, sd = 500)
)


# calculando media
mean(tab[tab$sexo == "M", "renda"]) #

# segunda maneira (dplyr)
# install.packages("dplyr")
library(dplyr)

tab |>
  filter(sexo == "M") |>
  #select(renda) |>
  summarize(mean(renda))

tab |>
  filter(sexo == "M") |>
  summarize(cor(renda, anos))


# anlise preditiva
# sort(cor(tab))
# cor(tab) >= 0.8

masc <- tab[tab$sexo == "M", ]

mod1 <- lm(renda ~ anos, data = masc)
summary(mod1)


library(ggplot2)

ggplot(data = masc)+
  geom_point(mapping = aes(x = anos, y = renda))+
  stat_smooth(mapping = aes(x = anos, y = renda),
              method = "lm", 
              geom = "smooth") 


# renda media por sexo

tab |>
  group_by(sexo, UF) |>
  summarize(mean(renda))
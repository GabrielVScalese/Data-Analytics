## Dados entre 2014 e 2020

names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.csv('./cepagri.csv', header = FALSE, sep = ";", col.names = names)

cepagri

## Dados entre 2015 e 2020

horariosValidos = cepagri[as.Date(cepagri$horario, format='%d/%m/%Y') > as.Date('31/12/2014', format='%d/%m/%Y') & as.Date(cepagri$horario, format='%d/%m/%Y') < as.Date('01/01/2021', format='%d/%m/%Y'), ]

horariosValidos

## Média dos dados agrupada por ano

library(ggplot2)

getDadosPorAno <- function (ano){
  cepagri[as.Date(cepagri$horario, format='%d/%m/%Y') > as.Date(paste(c('31/12/', ano - 1), collapse= ''), format='%d/%m/%Y') & as.Date(cepagri$horario, format='%d/%m/%Y') < as.Date(paste(c('01/01/', ano + 1), collapse= ''), format='%d/%m/%Y') , ]
}

media <- function (valores){
  soma = 0

  for (i in valores){
    if (!is.na(i) & i != ' [ERRO]')
      soma = soma + as.double(i)
  }

  soma / length(valores)
}

agruparAnos <- function (intervalo) {
  ano = intervalo
  mediaTemp = c()
  mediaVento = c()
  mediaUmid = c()
  mediaSensa = c()

  for (value in intervalo){
    dadosDoAno = getDadosPorAno (value)

    mTemp = media(dadosDoAno$temp)
    mVento = media(dadosDoAno$ven)
    mUmid = media(dadosDoAno$umid)
    mSena = media(dadosDoAno$sensa)

    mediaTemp = append(mediaTemp, mTemp)
    mediaVento = append(mediaVento, mVento)
    mediaUmid = append(mediaUmid, mUmid)
    mediaSensa = append(mediaSensa, mSena)
  }

  tabela <- data.frame(ano, mediaTemp, mediaVento, mediaUmid, mediaSensa)

  tabela
}

agrupamento <- agruparAnos (c(2015, 2016, 2017, 2018, 2019, 2020))
agrupamento

## Temperatura média entre 2015 e 2020

ggplot() + geom_bar(data = agrupamento, aes(x = ano, y = mediaTemp),
                    stat = "identity", fill = '#FF6666') + ggtitle("Temperatura média por ano")

## Comparação entre temperatura média e umidade média

x = c(2015, 2016, 2017, 2018, 2019, 2020)

par(mar = c(5, 5, 3, 5))
plot(x, agrupamento$mediaTemp, type ="l", ylab = "MT",
     main = "Comparação MT e MU", xlab = "Anos",
     col = "blue")
par(new = TRUE)
plot(x, agrupamento$mediaUmid, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
mtext("MU", side = 4, line = 3)
legend("topleft", c("MT", "MU"),
       col = c("blue", "red"), lty = c(1, 2))
 
## Comparação entre temperatura média e vento médio

par(mar = c(5, 5, 3, 5))
plot(x, agrupamento$mediaTemp, type ="l", ylab = "MT",
     main = "Comparação MT e MV", xlab = "Anos",
     col = "blue")
par(new = TRUE)
plot(x, agrupamento$mediaVento, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
mtext("MV", side = 4, line = 3)
legend("topleft", c("MT", "MV"),
       col = c("blue", "red"), lty = c(1, 2))

## Comparação entre temperatura média e sensação térmica média

par(mar = c(5, 5, 3, 5))
plot(x, agrupamento$mediaTemp, type ="l", ylab = "MT",
     main = "Comparação MT e MS", xlab = "Anos",
     col = "blue")
par(new = TRUE)
plot(x, agrupamento$mediaSensa, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
mtext("MV", side = 4, line = 3)
legend("topleft", c("MT", "MS"),
       col = c("blue", "red"), lty = c(1, 2))

## Média de umidade por mês no período de 2015 a 2020

 meses = c('Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez')

mediaUmidadeMeses <- function () {
  medias = c()

  mes = 1
  while (mes <= 12) {
    data = as.Date(cepagri$horario, format='%d/%m/%Y')
    valores = cepagri[as.double(format(data, format="%m")) == mes & as.double(format(data, format="%Y")) > 2014 & as.double(format(data, format="%Y")) < 2021, ]
    mediaDoMes = media(valores$umid)
    medias = append(medias, mediaDoMes)

    mes = mes + 1
  }

  dataFrame = data.frame(meses, medias)
}

mediaUmidade = mediaUmidadeMeses()

ggplot() + geom_bar(data = mediaUmidade, aes(x = meses,y = medias),
                    stat = "identity", fill = '#3399ff') + ggtitle("Média de umidade por mês no período de 2015 a 2020")

## Média de vento por mês no período de 2015 a  2020

mediaUmidadeMeses <- function () {
  medias = c()

  mes = 1
  while (mes <= 12) {
    data = as.Date(cepagri$horario, format='%d/%m/%Y')
    valores = cepagri[as.double(format(data, format="%m")) == mes & as.double(format(data, format="%Y")) > 2014 & as.double(format(data, format="%Y")) < 2021, ]
    mediaDoMes = media(valores$umid)
    medias = append(medias, mediaDoMes)

    mes = mes + 1
  }

  dataFrame = data.frame(meses, medias)
}

mediaUmidade = mediaUmidadeMeses()

ggplot() + geom_bar(data = mediaUmidade, aes(x = meses,y = medias),
                    stat = "identity", fill = '#3399ff') + ggtitle("Média de umidade por mês no período de 2015 a 2020")
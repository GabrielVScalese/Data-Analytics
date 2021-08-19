names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.csv('./cepagri.csv', header = FALSE, sep = ";", col.names = names)

horariosValidos = cepagri[as.Date(cepagri$horario, format='%d/%m/%Y') > as.Date('31/12/2014', format='%d/%m/%Y') & as.Date(cepagri$horario, format='%d/%m/%Y') < as.Date('01/01/2021', format='%d/%m/%Y'), ]

horariosValidos

library(ggplot2)

getDadosPorAno <- function (ano){
  cepagri[as.Date(cepagri$horario, format='%d/%m/%Y') > as.Date(paste(c('31/12/', ano - 1), collapse= ''), format='%d/%m/%Y') & as.Date(cepagri$horario, format='%d/%m/%Y') < as.Date(paste(c('01/01/', ano + 1), collapse= ''), format='%d/%m/%Y') , ]
}

mediaTemperaturaPorAno <- function (ano){
  dadosDoAno = getDadosPorAno(ano)
  valores = dadosDoAno$temp

  soma = 0

  for (i in valores){
    if (!is.na(i) & i != ' [ERRO]')
      soma = soma + as.double(i)
  }

  soma / length(valores)
}

media2014 = mediaTemperaturaPorAno (2014)
media2015 = mediaTemperaturaPorAno (2015)
media2016 = mediaTemperaturaPorAno(2016)
media2017 = mediaTemperaturaPorAno(2017)

anos = c('2014', '2015', '2016', '2017')
medias = c(media2014, media2015, media2016, media2017)

df = data.frame(anos, medias)

ggplot() + geom_bar(data = df, aes(x = anos, y = medias),
                    stat = "identity") + ggtitle("Temperatura média / ano")

# print(mediaTemperatura(as.double(dados2015$temp)))

# mean(as.double(dados2015$temp))
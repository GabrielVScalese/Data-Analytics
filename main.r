# -*- coding: utf-8 -*-
"""versao-final-data-analytics.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1fXLnImkDCkDbJX-DcsQSVm46OPMOJDxK

### **Dados entre 2014 e 2021**

---
"""

# Obtencao de toda a tabela
names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.csv(con, header = FALSE, sep = ";", col.names = names)

cepagri

"""### **Dados entre 2015 e 2020**

---


"""

# Obtencao de dados no período entre 2015 e 2020
getPeriodoValido <- function (){
  periodoValido = cepagri[as.double(format(as.Date(cepagri$horario, format='%d/%m/%Y'), format="%Y")) >= 2015 & as.double(format(as.Date(cepagri$horario, format='%d/%m/%Y'), format="%Y")) <= 2020, ]

  periodoValido <- periodoValido[!is.na(periodoValido$horario) & !is.na(periodoValido$temp) & !is.na(periodoValido$umid) & !is.na(periodoValido$vento) & !is.na(periodoValido$sensa),]
  periodoValido <- periodoValido[periodoValido$temp != ' [ERRO]',]
  periodoValido <- periodoValido[periodoValido$sensa < 90 & periodoValido$sensa > 0,]
  periodoValido <- periodoValido[periodoValido$umid > 0,]
  periodoValido <- periodoValido[periodoValido$vento > 0,]

  periodoValido$temp = as.double(periodoValido$temp)
  periodoValido
}

getPeriodoValido()

"""### **Análise superficial dos dados**"""

# Aqui vemos o menor e maior valor de cada objeto a ser estudado (temp, vento, umid e sensa)
summary(getPeriodoValido())

"""### **Anos e meses com maiores médias de temperatura, vento, umidade e sensação térmica**"""

mediasPorMesEAno <- function () {

  meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro") # vetor com todos os meses do ano
  dados <- c("Maior temp", "Mais vento", "Maior umid", "Maior sensa") # vetor com todos os dados a serem analisados

  #meses
  mesVent <- "" # mes com maior media de ventos
  mediaVentMes <- 0 
  mesTemp <- "" # mes com maior media de temperatura
  mediaTempMes <- 0
  mesUmi <- "" # mes com maior media de umidade
  mediaUmiMes <- 0
  mesSens <- "" # mes com maior media de sensação termica
  mediaSensMes <- 0
  dadosMeses <- c() # vetor para armazenar os dados obtidos futuramente

  #anos
  anoVent <- 0 # ano com maior media de ventos
  mediaVentAno <- 0
  anoTemp <- 0 # ano com maior media de temperatura
  mediaTempAno <- 0
  anoUmi <- 0 # ano com maior media de umidade
  mediaUmiAno <- 0 
  anoSens <- 0 # ano com maior media de sensação termica
  mediaSensAno <- 0
  dadosAnos <- c() # vetor para armazenar os dados obtidos futuramente


  # Seleção dos meses
  mes = 1
  while (mes <= 12) { # passa por todos os meses do ano
    periodoValido = getPeriodoValido() # pega os dados relacionados ao periodo válido
    data = as.Date(periodoValido$horario, format='%d/%m/%Y') # formatação em data

    valores = periodoValido[as.double(format(data, format="%m")) == mes, ]  # formatação dos valores
    
    #temperatura
    if(mean(valores$temp) > mediaTempMes){ # verifica se a temperatura armazenada é menor que a do mes atual
        # caso seja, é adicionado o mês atual e o valor da temperatura nos vetores de dados
        mediaTempMes = mean(valores$temp) 
        mesTemp = meses[mes]
    }
    #vento
    if(mean(valores$ven) > mediaVentMes){ # verifica se o vento armazenado é menor que o do mes atual
        # caso seja, é adicionado o mês atual e o valor do vento nos vetores de dados
        mediaVentMes = mean(valores$ven)
        mesVent = meses[mes]
    }
    #umidade
    if(mean(valores$umid) > mediaUmiMes){ # verifica se a umidade armazenada é menor que a do mes atual
        # caso seja, é adicionado o mês atual e o valor da umidade nos vetores de dados
        mediaUmiMes = mean(valores$umid)
        mesUmi = meses[mes]
    }
    #Sensação Térmica
    if(mean(valores$sensa) > mediaSensMes){ # verifica se a sensação térmica armazenada é menor que a do mes atual
        # caso seja, é adicionado o mês atual e o valor da sensação térmica nos vetores de dados
        mediaSensMes = mean(valores$sensa)
        mesSens = meses[mes]
    }

    mes = mes + 1 # soma o mês para continuar o while
  }

  dadosMeses = append(dadosMeses, mesTemp) # }
  dadosMeses = append(dadosMeses, mesVent) # }
  dadosMeses = append(dadosMeses, mesUmi)  # } --> junta os dados 
  dadosMeses = append(dadosMeses, mesSens) # }

  # Seleção dos anos
  ano = 2015
  while(ano <= 2020){  # passa por todos os anos
    periodoValido = getPeriodoValido()
    data = as.Date(periodoValido$horario, format='%d/%m/%Y') # formatação em data
    valoresAnos = periodoValido[as.double(format(data, format="%Y")) == ano,] # formatação dos valores

    #temperatura
    if(mean(valoresAnos$temp) > mediaTempAno){ # verifica se a temperatura armazenada é menor que a do ano atual
        # caso seja, é adicionado o ano atual e o valor da temperatura nos vetores de dados
        mediaTempAno = mean(valoresAnos$temp)
        anoTemp = ano
    }
    #vento
    if(mean(valoresAnos$ven) > mediaVentAno){ # verifica se o vento armazenado é menor que o do ano atual
        # caso seja, é adicionado o ano atual e o valor do vento nos vetores de dados
        mediaVentAno = mean(valoresAnos$ven)
        anoVent = ano
    }
    #umidade
    if(mean(valoresAnos$umid) > mediaUmiAno){ # verifica se a umidade armazenada é menor que a do ano atual
        # caso seja, é adicionado o ano atual e o valor da umidade nos vetores de dados
        mediaUmiAno = mean(valoresAnos$umid)
        anoUmi = ano
    }
    #Sensação Térmica
    if(mean(valoresAnos$sensa) > mediaSensAno){ # verifica se a sensação térmica armazenada é menor que a do ano atual
        # caso seja, é adicionado o ano atual e o valor da sensação térmica nos vetores de dados
        mediaSensAno = mean(valoresAnos$sensa)
        anoSens = ano
    }

    ano = ano + 1 # soma o mês para continuar o while
  }

  dadosAnos = append(dadosAnos, anoTemp) # }
  dadosAnos = append(dadosAnos, anoVent) # } 
  dadosAnos = append(dadosAnos, anoUmi)  # } --> junta os dados
  dadosAnos = append(dadosAnos, anoSens) # }

  tabela = data.frame(Criterio = dados, Mes = dadosMeses, Ano = dadosAnos) # cria um data frame com os resultados obtidos durante o método

  tabela
}

agrupamento <- mediasPorMesEAno() # armazena o resultado do método acima 
agrupamento # mostra o resultado da tabela


write.csv(agrupamento,"./3.csv", row.names = TRUE)

"""### **Média dos objetos agrupada por ano**"""

library(ggplot2)

# Metodo que retorna a media de todos os objetos (temp, vento, umid e sensa) por ano, utilizando um Data Frame
mediasDosAnos <- function () {
  periodoValido <- getPeriodoValido()
  periodoValido$horario <- as.double(format(as.Date(periodoValido$horario, format = '%d/%m/%Y'), format = '%Y'))

  mediaTemp <- as.vector(tapply(periodoValido$temp, periodoValido$horario, mean))
  mediaUmid <- as.vector(tapply(periodoValido$umid, periodoValido$horario, mean))
  mediaVento <- as.vector(tapply(periodoValido$vento, periodoValido$horario, mean))
  mediaSensa <- as.vector(tapply(periodoValido$sensa, periodoValido$horario, mean))

  tabela <- data.frame(ano = unique(periodoValido$horario), temp = mediaTemp, vento = mediaVento, umid = mediaUmid, sensa = mediaSensa)
}
 
mediasDosAnos <- mediasDosAnos ()
mediasDosAnos

# Para uso posterior no relatório
write.csv(mediasDosAnos,"./1.csv", row.names = TRUE)

"""### **Médias agrupadas por meses**"""

# Metodo que retorna a media de cada objeto (temp, umid, vento e sensa) por mes
mediasMeses <- function () {
  periodoValido <- getPeriodoValido()
  periodoValido$horario <- as.double(format(as.Date(periodoValido$horario, format = '%d/%m/%Y'), format = '%m'))
  
  mediaTemp <- as.vector(tapply(periodoValido$temp, periodoValido$horario, mean))
  mediaUmid <- as.vector(tapply(periodoValido$umid, periodoValido$horario, mean))
  mediaVento <- as.vector(tapply(periodoValido$vento, periodoValido$horario, mean))
  mediaSensa <- as.vector(tapply(periodoValido$sensa, periodoValido$horario, mean))

  df = data.frame(Mes = unique(month.abb[periodoValido$horario]), Temp = mediaTemp, Umid = mediaUmid, Vento = mediaVento, Sensa = mediaSensa)
  df
}

mediasMeses <- mediasMeses()
mediasMeses

write.csv(mediasMeses,"./2.csv", row.names = TRUE)

"""### **Correlação entre os objetos**"""

# Biblioteca para tratamento de correlacao
install.packages("corrplot")
library(corrplot)

# Obtencao de periodo valido e uma rapida alteracao, elimando a coluna horario desnecessaria
periodoValido = getPeriodoValido()
periodoValido <- subset(periodoValido, select = -c(horario))

# Obtencao de matriz de correlacao dos objetos (temp, vento, umid e sensa)
matrizCorrelacao = cor(periodoValido)
matrizCorrelacao

# Grafico da correlacao
corrplot(matrizCorrelacao)

"""### **Correlação entre temperatura e umidade**"""

# Biblioteca para a criação de gráficos
install.packages("ggplot2")
library(ggplot2)

# Obtenção de periodo válido e criação de gráfico a partir dos dados do período válido (correlação da temperatura com a umidade)
periodoValido = getPeriodoValido()

grafico1 <- ggplot(periodoValido, aes(x=temp, y=umid)) + geom_point() + 
geom_smooth(method=lm , color="red", se=FALSE) + labs(x = 'Temperatura (ºC)',y = 'Umidade (%)') + ggtitle("Correlação entre temperatura e umidade") +
theme(
  plot.title = element_text(size = 17, hjust = 0.5, face = 'bold'),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  axis.text = element_text(size = 13),
) 

grafico1

"""### **Correlação entre temperatura e vento**

"""

# Criação de gráfico a partir dos dados do período válido (correlação da temperatura com o vento)
grafico2 <- ggplot(periodoValido, aes(x=temp, y=vento)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) + labs(x = 'Temperatura (ºC)',y = 'Vento (km/h)') + ggtitle("Correlação entre temperatura e vento") +
theme(
  plot.title = element_text(size = 17, hjust = 0.5, face = 'bold'),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  axis.text = element_text(size = 13),
) 

grafico2

"""### **Correlação entre temperatura e sensação térmica**"""

# Criação de gráfico a partir dos dados do período válido (correlação da temperatura com a sensação térmica)
grafico3 <- ggplot(periodoValido, aes(x=temp, y=sensa)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) + labs(x = 'Temperatura (ºC)',y = 'Sensação Térmica (ºC)')+ ggtitle("Correlação entre temperatura e sensação térmica") +
theme(
  plot.title = element_text(size = 17, hjust = 0.5, face = 'bold'),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  axis.text = element_text(size = 13),
) 

grafico3

"""### **Média de umidade por mês no período entre 2015 e 2020**"""

# Metodo que retorna a media de umidade por mes no periodo entre 2015 e 2020
mediaUmidadeMeses <- function () {
  periodoValido <- getPeriodoValido()
  periodoValido$horario <- as.double(format(as.Date(periodoValido$horario, format = '%d/%m/%Y'), format = '%m'))
  
  mediaUmid <- tapply(periodoValido$umid, periodoValido$horario, mean)

  df = data.frame(Mes = unique(month.abb[periodoValido$horario]), mediaUmid = mediaUmid)
  df
}

mediaUmidadeMeses = mediaUmidadeMeses()

# Construcao do grafico a partir do df, onde tambem define-se o estilo de cada legenda
ggplot() + geom_bar(data = mediaUmidadeMeses, aes(x = factor(Mes, levels = unique(Mes)), y = mediaUmid),
                    stat = "identity", fill = '#3399ff') + ggtitle("Média de umidade por mês no período de 2015 a 2020") + labs(y = 'Média (%)', x = 'Mes', caption = 'Fonte de Dados: Cepagri') + theme(
  plot.title = element_text(hjust = 0.5, face = 'bold', size = 15),
  plot.subtitle = element_text(hjust = 0.5),
  plot.caption = element_text(size = 12.5),
  axis.title = element_text(size = 13),
  axis.text = element_text(size = 13)
)

"""### **Média de vento por mês no período entre 2015 e 2020**"""

# Metodo que retorna a media de vento por mes no periodo entre 2015 e 2020
mediaVentoMeses <- function () {
  periodoValido <- getPeriodoValido()
  periodoValido$horario <- as.double(format(as.Date(periodoValido$horario, format = '%d/%m/%Y'), format = '%m'))
  
  mediaVento <- tapply(periodoValido$vento, periodoValido$horario, mean)

  df = data.frame(mes = unique(month.abb[periodoValido$horario]), mediaVento = mediaVento)
  df
}

mediaVentoMeses <- mediaVentoMeses()

# Construcao do grafico a partir do df, onde tambem define-se o estilo de cada legenda
ggplot() + geom_bar(data = mediaVentoMeses, aes(x = factor(mes, levels = unique(mes)),y = mediaVento),
                    stat = "identity", fill = '#ffb84d') + ggtitle("Média de vento por mês no período de 2015 a 2020") + labs(y = 'Média (km/h)', x = 'Mês', caption = 'Fonte de Dados: Cepagri') + theme(
  plot.title = element_text(hjust = 0.5, face = 'bold', size = 15),
  plot.subtitle = element_text(hjust = 0.5),
  plot.caption = element_text(size = 12.5),
  axis.title = element_text(size = 13),
  axis.text = element_text(size = 13)
)

"""### **Distribuição dos 100 dias mais quentes entre 2015 e 2020**"""

# Metodo que retorna as cem maiores temperaturas no periodo entre 2015 e 2020
getCemMaioresTemp <- function () {
  periodoValido = getPeriodoValido()
  periodoValido$horario <- as.double(format(as.Date(periodoValido$horario, format = '%d/%m/%Y'), format = '%m'))
  
  temps <- periodoValido[as.double(periodoValido$temp) > 35.8, ]

  temps[order(temps$temp), ] # Ordenando df por temperatura
  temps <- tail(temps, -7) # Removendo as linhas adicionais
  temps[order(temps$temp), ] # Uma outra ordenacao se torna necessaria apos a remocao de elementos
}

# Metodo que retorna uma tabela com a distribuicao das temperaturas
distribuicaoDasCemMaioresTemps <- function (temps) {
  quantidades <- tapply(temps$temp, temps$horario, function (valores){
    length(valores)
  })

  dataFrame = data.frame(mes = names(quantidades), quantidade = as.vector(quantidades))
}

distribuicaoDasMaioresTemps = distribuicaoDasCemMaioresTemps(getCemMaioresTemp())

porcentagens <- paste(round(100 * distribuicaoDasMaioresTemps$quantidade/ 100, 1), '%') # Obtencao da porcentagem de cada mes

# Construcao do grafico de setores
pie(distribuicaoDasMaioresTemps$quantidade, labels = porcentagens, main = "Distribuição dos 100 dias mais quentes entre 2015 e 2020", col = rainbow(length(distribuicaoDasMaioresTemps$quantidade))) 
legend("topleft", c('Janeiro', 'Fevereiro', 'Setembro', 'Outubro'), cex = 0.8,
   fill = rainbow(length(distribuicaoDasMaioresTemps$quantidade)))

"""### **Distribuição dos 100 dias mais frios entre 2015 e 2020**

---


"""

# Metodo que retorna as cem maiores temperaturas no periodo entre 2015 e 2020
getCemMenoresTemp <- function () {
  periodoValido = getPeriodoValido()
  periodoValido$horario <- as.double(format(as.Date(periodoValido$horario, format = '%d/%m/%Y'), format = '%Y'))
  
  temps <- periodoValido[as.double(periodoValido$temp) < 7.5, ]
 
  temps[order(temps$temp), ] # Ordenando df por temperatura
  temps <- tail(temps, -37) # Removendo as linhas adicionais
  temps[order(temps$temp), ] # Uma outra ordenacao se torna necessaria apos a remocao de elementos
}

# Metodo que retorna uma tabela com a distribuicao das temperaturas
distribuicaoDasCemMenoresTemps <- function (temps) {
  quantidades <- tapply(temps$temp, temps$horario, function (valores){
    length(valores)
  })

  dataFrame = data.frame(Ano = names(quantidades), Quantidade = as.vector(quantidades))
}


distribuicaoDasMenoresTemps = distribuicaoDasCemMenoresTemps(getCemMenoresTemp())
distribuicaoDasMenoresTemps

write.csv(distribuicaoDasMenoresTemps,"./4.csv", row.names = TRUE)
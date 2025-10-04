# Instalar o pacote (se ainda n∆o tiver)
install.packages("readxl")

# Carregar o pacote
library(readxl)
library(dplyr)
library(lubridate)

# Ler o arquivo
dados <- read_excel("C:/Users/isado/Downloads/DataFEA Ingressantes 2024_sem ID.xlsx")

# Visualizar as primeiras linhas
head(dados)

dados1 <- read_excel(
  "C:/Users/isado/Downloads/DataFEA Ingressantes 2024_sem ID.xlsx",
  skip = 1
)


###################### idade ##########################################

# Visualizar as primeiras linhas
head(dados1)

# Converter a coluna 5 para Date
dados1$Data <- as.Date(dados1[[5]], format = "%d/%m/%Y")

# Filtrar datas maiores que 10/03/2025
datas_maiores <- dados1$Data[dados1$Data > as.Date("2007-03-01")]

# Mostrar resultado
datas_maiores



# Substituir a coluna original pelas datas convertidas para ano
dados$`Formato: DD/MM/AAAA` <- year(dmy(dados1$`Formato: DD/MM/AAAA`))

# Visualizar resultado
head(dados1$`Formato: DD/MM/AAAA`)

table(dados1$`Formato: DD/MM/AAAA`)


# Filtrar apenas anos atÇ 2008
dados1 <- dados1 %>%
  filter(`Formato: DD/MM/AAAA` <= 2009)

# Contar quantos por ano
dados1 %>%
  group_by(`Formato: DD/MM/AAAA`) %>%
  summarise(quantidade = n()) %>%
  arrange(`Formato: DD/MM/AAAA`)


# Vetor com os anos de nascimento
anos <- c(1965, 1967, 1974, 1980, 1983, 1984, 1989, 1990, 1991, 1992, 1993, 1994,
          1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006)

# Frequància de pessoas em cada ano
freq <- c(2, 1, 2, 1, 2, 2, 1, 3, 1, 4, 4, 3, 3, 8, 8, 11, 20, 13, 18, 26, 21, 73, 187, 112)

# Calcular idade em 2025
idade <- 2025 - anos

# Idade mÇdia ponderada
idade_media <- sum(idade * freq) / sum(freq)
idade_media

dados_filtrados <- dados1[, c(5, 13, 14)]
library(dplyr)

resumo <- dados1 %>%
  group_by(`Formato: DD/MM/AAAA`) %>%
  summarise(
    Feminino = sum(!is.na(Feminino)),
    Masculino = sum(!is.na(Masculino))
  )

# Criar o data frame
dados <- data.frame(
  Ano = c(1965,1967,1974,1980,1983,1984,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006),
  Feminino = c(0,0,0,0,0,1,0,2,1,1,1,0,0,2,0,2,6,3,3,7,7,27,65,45),
  Masculino = c(2,1,2,1,2,1,1,1,0,3,3,3,3,6,8,9,14,10,15,19,14,46,122,67)
)

# Calcular idade mÇdia por sexo
idade_feminino <- sum((2025 - dados$Ano) * dados$Feminino) / sum(dados$Feminino)
idade_masculino <- sum((2025 - dados$Ano) * dados$Masculino) / sum(dados$Masculino)

idade_feminino
idade_masculino

# Calcular idade de cada ano
dados$Idade <- 2025 - dados$Ano

# Quantos Femininos menores de 18
menores_feminino <- sum(dados$Feminino[dados$Idade < 18])

# Quantos Masculinos menores de 18
menores_masculino <- sum(dados$Masculino[dados$Idade < 18])

menores_feminino
menores_masculino

########################  pais ############################################

dados1 <- dados1[, 280:294]

dados1 <- dados1[, -1]


dados1 <- dados1[, c(5, 6, 12, 13)]


# Removendo linhas com NA em todas as colunas
dados2 <- dados1[rowSums(!is.na(dados1)) > 0, ]


dados3 <- dados1 %>%
  select(1,2, 3 ,4)%>%
  filter(rowSums(!is.na(.)) == 1)

library(dplyr)

dados1_so_na <- dados1 %>%
  select(1, 2, 3, 4) %>%
  filter(rowSums(!is.na(.)) == 0)


################################# criterio brasil ###################################



####### criterio 1 : objetos presentes na casa ##############

head(dados1)

dados_obj <- dados1[, 205:264]

# garante que tudo Ç texto
dados_obj[] <- lapply(dados_obj, as.character)

# troca "N∆o h†" por "0" e "4+" por "4"
dados_obj[dados_obj == "N∆o h†"] <- "0"
dados_obj[dados_obj == "4+"] <- "45"

dados_obj <- as.data.frame(lapply(dados_obj, function(x) as.numeric(as.character(x))))


dados_obj[, 2][dados_obj[, 2] == 1] <- 3
dados_obj[, 3][dados_obj[, 3] == 2] <- 7
dados_obj[, 4][dados_obj[, 4] == 3] <- 10
dados_obj[, 5][dados_obj[, 5] == 45] <- 14
dados_obj[, 7][dados_obj[, 7] == 1] <- 3
dados_obj[, 8][dados_obj[, 8] == 2] <- 7
dados_obj[, 9][dados_obj[, 9] == 3] <- 10
dados_obj[, 10][dados_obj[, 10] == 45] <- 13
dados_obj[, 12][dados_obj[, 12] == 1] <- 3
dados_obj[, 13][dados_obj[, 13] == 2] <- 5
dados_obj[, 14][dados_obj[, 14] == 3] <- 8
dados_obj[, 15][dados_obj[, 15] == 45] <- 11
dados_obj[, 17][dados_obj[, 17] == 1] <- 3
dados_obj[, 18][dados_obj[, 18] == 2] <- 6
dados_obj[, 19][dados_obj[, 19] == 3] <- 8
dados_obj[, 20][dados_obj[, 20] == 45] <- 11
dados_obj[, 22][dados_obj[, 22] == 1] <- 3
dados_obj[, 23][dados_obj[, 23] == 2] <- 6
dados_obj[, 24][dados_obj[, 24] == 3] <- 6
dados_obj[, 25][dados_obj[, 25] == 45] <- 6
dados_obj[, 27][dados_obj[, 27] == 1] <- 2
dados_obj[, 28][dados_obj[, 28] == 2] <- 3
dados_obj[, 29][dados_obj[, 29] == 3] <- 5
dados_obj[, 30][dados_obj[, 30] == 45] <- 5
dados_obj[, 32][dados_obj[, 32] == 1] <- 2
dados_obj[, 33][dados_obj[, 33] == 2] <- 4
dados_obj[, 34][dados_obj[, 34] == 3] <- 6
dados_obj[, 35][dados_obj[, 35] == 45] <- 6
dados_obj[, 37][dados_obj[, 37] == 1] <- 2
dados_obj[, 38][dados_obj[, 38] == 2] <- 4
dados_obj[, 39][dados_obj[, 39] == 3] <- 6
dados_obj[, 40][dados_obj[, 40] == 45] <- 6
dados_obj[, 42][dados_obj[, 42] == 1] <- 1
dados_obj[, 43][dados_obj[, 43] == 2] <- 3
dados_obj[, 44][dados_obj[, 44] == 3] <- 4
dados_obj[, 45][dados_obj[, 45] == 45] <- 6
dados_obj[, 47][dados_obj[, 47] == 1] <- 2
dados_obj[, 48][dados_obj[, 48] == 2] <- 4
dados_obj[, 49][dados_obj[, 49] == 3] <- 4
dados_obj[, 50][dados_obj[, 50] == 45] <- 4
dados_obj[, 52][dados_obj[,52] == 1] <- 1
dados_obj[, 53][dados_obj[, 53] == 2] <- 3
dados_obj[, 54][dados_obj[, 54] == 3] <- 3
dados_obj[, 55][dados_obj[, 55] == 45] <- 3
dados_obj[, 57][dados_obj[, 57] == 1] <- 2
dados_obj[, 58][dados_obj[, 58] == 2] <- 2
dados_obj[, 59][dados_obj[, 59] == 3] <- 2
dados_obj[, 60][dados_obj[, 60] == 45] <- 2


# Substituir NA por 0
dados_obj[is.na(dados_obj)] <- 0

# Criar a coluna soma_brasil somando todas as colunas numÇricas

# Supondo que as colunas de interesse comeáam da coluna 1 atÇ a £ltima
dados_obj$soma_brasil_obj <- rowSums(dados_obj[, 1:ncol(dados_obj)])





########### ccriterio 2: agua #######################

dados_agua <- dados1[, 265:267]

# Criar uma nova coluna numÇrica, com 4 onde houver "Rede geral de distribuiá∆o" e 0 caso contr†rio
dados_agua$encanada <- ifelse(dados_agua[, 1] == "Rede geral de distribuiá∆o", 4, 0)

dados_agua <- data.frame(lapply(dados_agua, function(x) {
  x <- as.numeric(x)       # converte para n£mero
  x[is.na(x)] <- 0         # substitui NAs por 0
  return(x)
}))

dados_agua <- dados_agua[, -c(1,3)]


# Criar a coluna soma_brasil somando todas as colunas numÇricas
# Supondo que as colunas de interesse comeáam da coluna 1 atÇ a £ltima
dados_agua$soma_brasil_agua <- rowSums(dados_agua[, 1:ncol(dados_agua)])



############################ criterio 3: asfaltada #######################


dados_rua <- dados1[, 268:269]


# Criar uma nova coluna numÇrica, com 2 onde houver "Asfaltada/ Pavimentada" e 0 caso contr†rio
dados_rua$asfaltada <- ifelse(dados_rua[, 1] == "Asfaltada/ Pavimentada", 2, 0)

dados_rua <- data.frame(lapply(dados_rua, function(x) {
  x <- as.numeric(x)       # converte para n£mero
  x[is.na(x)] <- 0         # substitui NAs por 0
  return(x)
}))

dados_rua <- dados_rua[, -c(1)]


# Criar a coluna soma_brasil somando todas as colunas numÇricas
# Supondo que as colunas de interesse comeáam da coluna 1 atÇ a £ltima
dados_rua$soma_brasil_rua <- rowSums(dados_rua[, 1:ncol(dados_rua)])



######### criterio 4: educaá∆o #########################################

dados_educa <- dados1[, 270:275]  


# Criar uma nova coluna numÇrica, com 0 onde houver "Analfabeto(a)/ Fund. I incompleto" e 0 caso contr†rio
dados_educa$analfabeto <- ifelse(dados_educa[, 1] == "Analfabeto(a)/ Fund. I incompleto", 0, 0)


# Criar uma nova coluna numÇrica, com 0 onde houver "	Fund. I completo/ Fund. II incompleto" e 0 caso contr†rio
dados_educa$fundamental_incompleto_2 <- ifelse(dados_educa[, 2] == "Fund. I completo/ Fund. II incompleto", 1, 0)

# Criar uma nova coluna numÇrica, com 0 onde houver "	Fund. I completo/ Fund. II incompleto" e 0 caso contr†rio
dados_educa$fundamental_completo_2 <- ifelse(dados_educa[, 3] == "Fund. II completo/MÇdio incompleto", 2, 0)


# Criar uma nova coluna numÇrica, com 0 onde houver "	MÇdio completo/ Superior incompleto" e 0 caso contr†rio
dados_educa$medio_completo <- ifelse(dados_educa[, 4] == "MÇdio completo/ Superior incompleto", 4, 0)


# Criar uma nova coluna numÇrica, com 0 onde houver "Superior completo" e 0 caso contr†rio
dados_educa$superior_complet0 <- ifelse(dados_educa[, 5] == "Superior completo", 7, 0)


# Criar uma nova coluna numÇrica, com 0 onde houver "P¢s-Graduaá∆o" e 0 caso contr†rio
dados_educa$superior_complet1 <- ifelse(dados_educa[, 6] == "P¢s-Graduaá∆o", 7, 0)


dados_educa <- dados_educa[, -c(1,2,3,4,5,6)]


dados_educa<- data.frame(lapply(dados_educa, function(x) {
  x <- as.numeric(x)       # converte para n£mero
  x[is.na(x)] <- 0         # substitui NAs por 0
  return(x)
}))


# Criar a coluna soma_brasil somando todas as colunas numÇricas
# Supondo que as colunas de interesse comeáam da coluna 1 atÇ a £ltima
dados_educa$soma_brasil_educa <- rowSums(dados_educa[, 1:ncol(dados_educa)])




################ juntando dataframes


df1<- dados_obj[, 61, drop = FALSE]

df2<- dados_agua[, 3, drop = FALSE]

df3<- dados_rua[, 3, drop = FALSE]

df4<- dados_educa[, 7, drop = FALSE]

df_criteriobrasil <- cbind(df1, df2, df3, df4)

df_criteriobrasil$pontuacao_final <- rowSums(df_criteriobrasil, na.rm = TRUE)

df_criteriobrasil_pontuaá∆o_final<- df_criteriobrasil[, 5, drop = FALSE]


####### soma

df_criteriobrasil_pontuaá∆o_final$classificacao <- cut(
  df_criteriobrasil_pontuaá∆o_final$pontuacao_final,
  breaks = c(-Inf, 17, 23, 29, 38, 45, 100),
  labels = c("DE", "C2", "C1", "B2", "B1", "A"),
  right = TRUE
)

# Contagem de cada classificaá∆o
table(df_criteriobrasil_pontuaá∆o_final$classificacao)

##############################  modalidade de entrada em relaá∆o a renda ###########################


dados_modalidade <- dados1[, 163:165]
modalidade_renda <- cbind(dados_modalidade, df_criteriobrasil_pontuaá∆o_final)

ac<- modalidade_renda[, c(1, 5)]

ep <- modalidade_renda[, c(2, 5)]

ppi <- modalidade_renda[, c(3, 5)]

# Faz uma tabela cruzando as colunas
tabela_freq <- table(ac[,1], ac[,2])

tabela_freq


# Faz uma tabela cruzando as colunas
tabela_freq1 <- table(ep[,1], ep[,2])

tabela_freq1

# Faz uma tabela cruzando as colunas
tabela_freq2 <- table(ppi[,1], ppi[,2])

tabela_freq2


#################### modalidade de entrada- enem, fuvest ou provao  em realáao a renda ###########################


dados_prova <- dados1[, 159:161]

dados_prova <- cbind(dados_prova, df_criteriobrasil_pontuaá∆o_final)

fuvest<- dados_prova[, c(1, 5)]

enem<- dados_prova[, c(2, 5)]

provao<- dados_prova[, c(3, 5)]


# Faz uma tabela cruzando as colunas
tabela_freq3 <- table(fuvest[,1], fuvest[,2])

tabela_freq3


# Faz uma tabela cruzando as colunas
tabela_freq4 <- table(enem[,1], enem[,2])

tabela_freq4

# Faz uma tabela cruzando as colunas
tabela_freq5 <- table(provao[,1], provao[,2])

tabela_freq5


######################curso por renda####################


dados_fea <- dados1[, 135:140]


dados_fea <- cbind(dados_fea, df_criteriobrasil_pontuaá∆o_final)


admdiu<- dados_fea[, c(1, 8)]

admnot<- dados_fea[, c(2, 8)]

atuaria<- dados_fea[, c(3, 8)]


cont<- dados_fea[, c(4, 8)]

econodiu<- dados_fea[, c(5, 8)]

econonot<- dados_fea[, c(6, 8)]


# Faz uma tabela cruzando as colunas
tabela_freq6 <- table(admdiu[,1], admdiu[,2])

tabela_freq6


# Faz uma tabela cruzando as colunas
tabela_freq7 <- table(admnot[,1], admnot[,2])

tabela_freq7

# Faz uma tabela cruzando as colunas
tabela_freq8 <- table(atuaria[,1], atuaria[,2])

tabela_freq8



# Faz uma tabela cruzando as colunas
tabela_freq9 <- table(cont[,1], cont[,2])

tabela_freq9


# Faz uma tabela cruzando as colunas
tabela_freq10 <- table(econodiu[,1], econodiu[,2])

tabela_freq10



# Faz uma tabela cruzando as colunas
tabela_freq11 <- table(econonot[,1], econonot[,2])

tabela_freq11







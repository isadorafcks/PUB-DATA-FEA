# Instalar o pacote (se ainda nÆo tiver)
install.packages("readxl")

# Carregar o pacote
library(readxl)
library(dplyr)
library(lubridate)

# Ler o arquivo
dados <- read_excel("C:/Users/isado/Downloads/ingressantes25.xlsx")

# Visualizar as primeiras linhas
head(dados)


###################### idade ##########################################
library(dplyr)
library(lubridate)

# Definir a data de referˆncia
data_ref <- as.Date("2025-03-01")

dados <- dados %>%
  mutate(
    DataNascimento = dmy(dados[[4]]),  # Coluna 4 = datas
    Sexo = dados[[7]],                 # Coluna 5 = sexo
    Idade = as.numeric(difftime(data_ref, DataNascimento, units = "days")) / 365.25
  )

# Calcular m‚dia de idade por sexo
media_por_sexo <- dados %>%
  group_by(Sexo) %>%
  summarise(
    Media_Idade = mean(Idade, na.rm = TRUE),
    n = n()
  )

print(media_por_sexo)


########################  1 op‡Æo  ############################################


library(dplyr)

# Supondo que seu dataframe se chama "dados"

resultados_2025 <- dados %>%
  mutate(Curso = case_when(
    grepl("Administra‡Æo", dados[[34]], ignore.case = TRUE) ~ "Administra‡Æo",
    grepl("Contabilidade", dados[[34]], ignore.case = TRUE) ~ "Contabilidade",
    grepl("Economia", dados[[34]], ignore.case = TRUE) ~ "Economia",
    grepl("Atu ria", dados[[34]], ignore.case = TRUE) ~ "Atu ria",
    TRUE ~ dados[[34]]
  ),
  PrimeiraOpcao = ifelse(dados[[35]] == "1¦ op‡Æo", 1, 0)) %>%
  group_by(Curso) %>%
  summarise(
    Percentual = round(100 * mean(PrimeiraOpcao), 1)
  )

print(resultados_2025)






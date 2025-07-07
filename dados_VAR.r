# DADOS para análise do VECM

library(writexl)
library(GetBCBData)
library(tidyverse)
library(dplyr)
library(beepr)
library(lubridate)

# Definindo o diretório de trabalho
choose.files() # Apenas para triggar o funcionamento correto do choose.dir()
diretorio <- choose.dir()
setwd(diretorio)
getwd()

###############========= Importando dados do BCB e consolidando base de dados inicial =========###############3

dados_mensais <- GetBCBData::gbcbd_get_series(
        id = c(
                "LFT" = 10634, "COMPROMISSADAS" = 1839,
                "IPCA" = 13522, "IPCAmm" = 433, "CRED_LIVRE" = 20634,
                "IBCBR" = 24364, "ICBRUSD" = 29042,
                "PTAXV" = 3696),
        first.date = "2011-01-01",
        last.date = "2024-12-01",
        format.data = "wide"
)

dados_mensais$ref.date <- as.Date(dados_mensais$ref.date)
colnames(dados_mensais) <- c("Data", "LFT", "COMPROMISSADAS", "IPCA", "IPCAmm", 
                              "CRED_LIVRE", "IBCBR", "ICBRUSD", "PTAXV")


#=== Puxando META para inflação anual e transformando em valores mensais
dados_anuais <- GetBCBData::gbcbd_get_series(
        id = c("META" = 13521),
        first.date = "2011-01-01",
        last.date = "2024-12-01",
        format.data = "wide"
)

dados_anuais$ref.date <- as.Date(dados_anuais$ref.date)
colnames(dados_anuais) <- c("Data", "META")

# Extrapolando META anual para todos os meses de cada ano
dados_mensalizados <- dados_anuais %>%
  mutate(Ano = year(ref.date)) %>%
  group_split(row_number()) %>%
  map_dfr(~ {
    tibble(
      Data = seq(ymd(paste0(.x$Ano, "-01-01")), ymd(paste0(.x$Ano, "-12-01")), by = "1 month"),
      META = .x$META
    )
  })

# Puxando SELIC_OVER diária e transformando em mensal
dados_diarios <- GetBCBData::gbcbd_get_series(
        id = c("SELIC_OVER" = 432),
        first.date = "2011-01-01",
        last.date = "2024-12-01",
        format.data = "wide"
)

dados_diarios$ref.date <- as.Date(dados_diarios$ref.date)
colnames(dados_diarios) <- c("Data", "SELIC_OVER")
View(dados_diarios)
colnames(dados_diarios)

dados_diarios <- dados_diarios %>%
  mutate(
    ano_mes = floor_date(Data, "month")
  ) %>%
  group_by(ano_mes) %>%
  slice_tail(n = 1) %>%  # Pega o último valor do mês
  ungroup() %>%
  select(Data = ano_mes, SELIC_OVER)

nrow(dados_diarios)
colnames(dados_diarios)

# ==== Combinando os dados mensais, diárias e anuais que foram mensalizados ====
dados <- dados_mensais %>%
  full_join(dados_mensalizados, by = "Data") %>%
  full_join(dados_diarios, by = "Data") %>%
  arrange(Data)

head(dados)

# Removendo linhas com NA
dados <- na.omit(dados)


# Criando a variável LFT_COMPROMISSADAS
dados <- dados %>%
  mutate(LFT_COMPROMISSADAS = LFT + COMPROMISSADAS)

# Criando a variável GAP_IPCA_META
dados <- dados %>%
  mutate(GAP_IPCA_META = IPCA - META)

View(dados)

# Criando a coluna de ÍNDICE do IPCA (dez/24 = 100)
#primeiro, transformando IPCAmm em decimal
dados <- dados %>%
    mutate(IPCAmm = IPCAmm / 100)

dados <- dados %>%
  arrange(desc(Data)) %>%
  mutate(
    IPCAmm_acum = cumprod(1 + IPCAmm),
  ) %>%
  mutate(
    Indice_IPCA = IPCAmm_acum / first(IPCAmm_acum)
  ) %>%
  arrange(Data)



View(dados)

# ====== Puxando dados de ATIVO_BANCARIO da máquina ======== # 
ATIVO_BANCARIO <- read.csv(choose.files(), sep = ";", dec = ",")
ATIVO_BANCARIO$Data <- as.Date(ATIVO_BANCARIO$Data, format = "%d/%m/%Y")

dados <- full_join(dados, ATIVO_BANCARIO, by = "Data")

dados <- dados %>% 
    mutate(
        LFT = LFT * Indice_IPCA,
        COMPROMISSADAS = COMPROMISSADAS * Indice_IPCA,
        LFT_COMPROMISSADAS = LFT_COMPROMISSADAS * Indice_IPCA,
        CRED_LIVRE = CRED_LIVRE * Indice_IPCA,
    )

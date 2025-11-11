######## ======= Automatização do Boletim Macro Semanal ======= ########

# --- 1. Carregar Pacotes ---
library(tidyquant)   # Para dados do Yahoo Finance (IBOV, SP, IFIX, DXY, Proxies)
library(GetBCBData)  # Para dados do Banco Central (CDI, PTAX)
library(dplyr)       # Para manipulação de dados
library(tidyr)       # Para pivotar e preencher dados
library(lubridate)   # Para facilitar o manejo de datas
library(writexl)     # Para exportar para Excel
library(bizdays)     # Para lidar com dias úteis e feriados
library(showtext)    # Para usar fontes customizadas
library(ggplot2)     # Para gráficos


# Configurando a fonte
font_add("plusjakarta", regular = "C:/Users/GabrielHenriqueMarti/AppData/Local/Microsoft/Windows/Fonts/PLUSJAKARTASANS-Regular.ttf")
showtext_auto()

# --- CONFIGURAÇÕES DE DIRETORIO ---- #
# Definir o diretório de trabalho
choose.files()
diretorio <- choose.dir()
getwd()


# Cria nova pasta com timestamp para salvar os outputs
timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S") 
nome_pasta_output <- paste0("Boletim_", timestamp)

caminho_output <- file.path(diretorio, nome_pasta_output)

# Cria a pasta no sistema
dir.create(caminho_output)
cat("Todos os outputs serão salvos em:", caminho_output, "\n")


# --- DEFINIÇÕES PERTINENTES ---
# Defina aqui o último dia útil para o qual o relatório será calculado
data_ancora <- as.Date("2025-11-07")

# --- 2. Definir Parâmetros ---
data_inicio <- Sys.Date() - years(5) # Coleta 5 anos de dados
data_fim <- Sys.Date()

cat("Iniciando coleta de dados de", 
    format(data_inicio, "%Y-%m-%d"), 
    "até", 
    format(data_fim, "%Y-%m-%d"), "\n")

# --- 3. Grupo 1: Yahoo Finance (IBOV, SP500, IFIX, DXY) + Proxies ANBIMA ---
tickers_yf <- c(
  "IBOV" = "^BVSP",
  "SP500" = "^GSPC",
  "IFIX (Proxy)" = "XFIX11.SA",
  "DXY" = "DX-Y.NYB",
  "IRF-M (Proxy)" = "IRFM11.SA",
  "IMA-B (Proxy)" = "IMAB11.SA"
)

# tq_get baixa todos os dados em formato 'tidy' (longo)
dados_yf_raw <- tq_get(
  tickers_yf,
  get = "stock.prices",
  from = data_inicio,
  to = data_fim
)

head(dados_yf_raw)

# Limpamos: selecionamos só o fechamento ajustado e pivotamos
dados_yf <- dados_yf_raw %>%
  
  # (atualmente chamada 'IBOV') para o nome padrão 'symbol'
  rename(symbol = IBOV) %>% 
  
  # PASSO 2 (Original): Agora o select funciona
  select(date, symbol, adjusted) %>%
  
  # PASSO 3 (Original): Renomeia os tickers (^BVSP) para os nomes amigáveis (IBOV)
  mutate(symbol = recode(symbol, !!!setNames(names(tickers_yf), tickers_yf))) %>%
  
  # PASSO 4 (Original): Pivota para o formato 'wide'
  pivot_wider(names_from = symbol, values_from = adjusted)

cat("Sucesso: Dados do Yahoo Finance tratados.\n")

View(dados_yf)

# --- 4. Grupo 2: Banco Central (CDI, PTAX) ---
codigos_bcb <- c(
  "CDI" = 4392,
  "PTAXV" = 1
)

# gbcbd_get_series baixa e já formata
dados_bcb_raw <- gbcbd_get_series(
  codigos_bcb,
  first.date = data_inicio,
  last.date = data_fim,
  format.data = "long" # Formato longo para facilitar
)

# Limpamos: pivotamos e ajustamos o CDI
dados_bcb <- dados_bcb_raw %>%
  select(ref.date, series.name, value) %>%
  pivot_wider(names_from = series.name, values_from = value) %>%
  
  # A Série 4392 vem como % (ex: 10.5). Dividimos por 100 para decimal (0.105)
  mutate(CDI = CDI / 100) # <-- ESTA LINHA CONTINUA IGUAL

cat("Sucesso: Dados do Banco Central (SGS) coletados (Usando Série 4392).\n")

head(dados_bcb)


# --- 5. Consolidação Final ---

# Junta os dataframes
df_consolidado <- full_join(dados_yf, dados_bcb, by = c("date" = "ref.date")) %>%
  arrange(date) # Ordena pela data

# Converte todos os 'NaN' (Invalid Number) em 'NA' (Missing)
# Usamos 'across' para aplicar a função em todas as colunas numéricas
df_consolidado_clean <- df_consolidado %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))

cat("Limpeza: 'Invalid Numbers' (NaN) convertidos para NA.\n")

# Agora o 'fill' vai funcionar para os NAs originais E para os que eram NaN
dados <- df_consolidado_clean %>%
  fill(everything(), .direction = "down") %>%
  # Remove NAs no início da série (antes dos dados começarem)
  filter(date >= data_inicio) %>% 
  na.omit() # Remove qualquer linha que ainda tenha NA


# --- 6. Resultado ---
cat("\n--- Painel de Mercado Consolidado (R) - Limpo ---\n")
print(tail(dados))

View(dados)


####### ====== Segunda parte: tabelas e gráficos ====== #######

# Gerando lista de feriados ANBIMA para excluir na construção do índice CDI
feriados_br <- holidays(cal = "Brazil/ANBIMA")

# Converter para 'Date' para garantir a correspondência
feriados_br <- as.Date(feriados_br) 

cat("Lista de feriados ANBIMA recuperada com sucesso.\n")
cat("Total de feriados carregados:", length(feriados_br), "\n")
cat("Intervalo de feriados:", format(min(feriados_br), "%Y"), "a", format(max(feriados_br), "%Y"), "\n")


# --- 1. Preparar Dados: Transformar Taxa CDI em Índice (CORRIGIDO com Feriados) ---
# (Este bloco estava correto e agora deve funcionar)

dados_idx <- dados %>%
  arrange(date) %>%
  mutate(dia_semana = wday(date)) %>%
  mutate(eh_feriado = date %in% feriados_br) %>% # Esta linha agora funcionará
  mutate(
    CDI_fator_diario = ifelse(dia_semana == 1 | dia_semana == 7 | eh_feriado == TRUE, 
                              1, 
                              (1 + CDI)^(1/252))
  ) %>%
  mutate(CDI_Indice = cumprod(CDI_fator_diario)) %>%
  select(-CDI, -CDI_fator_diario, -dia_semana, -eh_feriado) %>%
  rename(CDI = CDI_Indice)

cat("Sucesso: Índice Acumulado do CDI corrigido (sem render fds e feriados).\n")




# --- 3. Definir Janelas de Cálculo (Baseado na Data Âncora) ---

# Função helper (sem alteração)
get_price <- function(df, target_date) {
  df %>%
    # Filtra NA data-alvo ou antes
    filter(date <= target_date) %>%
    # Pega o último dia disponível (ignora fins de semana/feriados)
    slice_tail(n = 1) 
}

# 3.1. Definir as datas de referência
# P1 (Preço Final) é sempre o preço na data âncora
data_p1 <- data_ancora

# P0 (Preços Iniciais) são relativos à data P1
data_semana_p0 <- data_p1 - days(7)      # Ex: 07/11 -> 31/10
data_1m_p0 <- data_p1 %m-% months(1)     # Ex: 07/11 -> 07/10
data_12m_p0 <- data_p1 %m-% months(12)   # Ex: 07/11 -> 07/11/24
data_24m_p0 <- data_p1 %m-% months(24)
data_48m_p0 <- data_p1 %m-% months(48)

# YTD (Exceção: ancorado no último dia do ano anterior)
data_ytd_p0 <- floor_date(data_p1, "year") - days(1) # Ex: 31/12/2024

cat("Datas P1 e P0 definidas com base na âncora:", 
    format(data_ancora, "%Y-%m-%d"), "\n")

# 3.2. Coletar os valores (preços/índices) em cada data
lista_precos <- bind_rows(
  get_price(dados_idx, data_p1) %>% mutate(Periodo = "P_Hoje"), # P1
  
  # Todos os P0
  get_price(dados_idx, data_semana_p0) %>% mutate(Periodo = "P_Semana"),
  get_price(dados_idx, data_ytd_p0) %>% mutate(Periodo = "P_YTD"),
  get_price(dados_idx, data_1m_p0) %>% mutate(Periodo = "P_1m"),
  get_price(dados_idx, data_12m_p0) %>% mutate(Periodo = "P_12m"),
  get_price(dados_idx, data_24m_p0) %>% mutate(Periodo = "P_24m"),
  get_price(dados_idx, data_48m_p0) %>% mutate(Periodo = "P_48m")
)

# --- DEPURAÇÃO (Verifique se as datas P0 estão corretas) ---
cat("\n--- VERIFICANDO DADOS BRUTOS (ÂNCORA) ---\n")
print(
  lista_precos %>% 
    select(Periodo, date, IBOV, PTAXV) %>%
    arrange(date)
)



# --- 4. Definir Indicadores e Ordem ---
# Estes são os nomes da sua imagem (esquerda) e os nomes no nosso 'dados' (direita)
mapa_indicadores <- c(
  "IBOV" = "IBOV",
  "S&P500" = "SP500",
  "IFIX" = "IFIX (Proxy)",
  "CDI" = "CDI", # Agora mapeia para o nosso CDI_Indice
  "Dólar" = "PTAXV",
  "IMA-B" = "IMA-B (Proxy)"
)

# --- 5. Calcular Variações e Montar a Tabela Final ---
tabela_rentabilidade <- lista_precos %>%
  
  # 1. Manter apenas as colunas que nos interessam
  select(Periodo, all_of(unname(mapa_indicadores))) %>%
  
  # 2. Pivotar para formato longo (Indice, Valor)
  pivot_longer(cols = -Periodo, names_to = "Ticker", values_to = "Valor") %>%
  
  # 3. Pivotar para formato wide (colunas P_Hoje, P_Semana, etc)
  pivot_wider(names_from = Periodo, values_from = Valor) %>%
  
  # 4. Calcular as variações (P1 / P0) - 1
  mutate(
    Semana = (P_Hoje / P_Semana) - 1,
    YTD = (P_Hoje / P_YTD) - 1,
    `1m` = (P_Hoje / P_1m) - 1,
    `12m` = (P_Hoje / P_12m) - 1,
    `24m` = (P_Hoje / P_24m) - 1,
    `48m` = (P_Hoje / P_48m) - 1
  ) %>%
  
  # 5. Renomear os tickers para os nomes amigáveis (Dólar, IFIX, etc.)
  mutate(Índice = recode(Ticker, !!!setNames(names(mapa_indicadores), unname(mapa_indicadores)))) %>%
  
  # 6. Limpar e reordenar as colunas
  select(Índice, Semana, YTD, `1m`, `12m`, `24m`, `48m`) %>%
  
  # 7. Garantir a ordem das LINHAS (de acordo com o vetor mapa_indicadores)
  slice(match(names(mapa_indicadores), Índice))

# --- 6. Exibir e Exportar ---
cat("\n--- Tabela de Rentabilidade (Pronta para Excel) ---\n")
print(tabela_rentabilidade)

# --- 6. Exibir e Exportar ---
cat("\n--- Tabela de Rentabilidade (Pronta para Excel) ---\n")
print(tabela_rentabilidade)

# Definir o caminho completo do arquivo de Excel
caminho_excel <- file.path(caminho_output, "tabela_rentabilidade.xlsx")

# Salvar o arquivo
write_xlsx(tabela_rentabilidade, caminho_excel)

cat("Tabela salva em:", caminho_excel, "\n")


####### ============================================= #######
####### ==== Gerando os gráficos de PTAXV e DXY  ==== ######

cor_grafico <- "#189CD8"
familia_fonte <- "plusjakarta"

# --- 1. Preparar dados para os gráficos ---

# 1.1. Filtrar os últimos 30 dias (para a linha)
# Usamos a data_ancora como P1
dados_30d <- dados %>%
  filter(date >= (data_ancora - days(30)) & date <= data_ancora)

# 1.2. Filtrar a última semana (para a área de destaque)
dados_7d <- dados %>%
  filter(date >= (data_ancora - days(7)) & date <= data_ancora)

cat("Dados de 30 e 7 dias filtrados para os gráficos.\n")


# --- 2. Gráfico PTAXV (Dólar) ---

# 2.1. Calcular os limites do eixo Y para o PTAXV
# Vamos dar uma pequena margem (ex: 1%) acima do máximo e abaixo do mínimo
limites_ptax <- c(
  min(dados_30d$PTAXV, na.rm = TRUE) * 0.99, 
  max(dados_30d$PTAXV, na.rm = TRUE) * 1.01
)

# 2.2. Criar o gráfico
grafico_ptax <- ggplot(
  data = dados_30d, 
  aes(x = date, y = PTAXV)
) +
  geom_area(
    data = dados_7d, 
    aes(y = PTAXV),
    fill = cor_grafico,
    alpha = 0.3,
    na.rm = TRUE
  ) +
  geom_line(
    color = cor_grafico,
    linewidth = 1,
    na.rm = TRUE
  ) +
  
  # --- AJUSTE 2: Definir os limites do Eixo Y ---
  # Isso força o 'zoom' na área de dados, em vez de começar em 0
  coord_cartesian(
    ylim = limites_ptax,
    expand = FALSE 
  ) +
  
  # 2.3. Aplicar o tema
  theme_classic() +
  theme(
    text = element_text(family = familia_fonte),
    axis.text = element_text(family = familia_fonte, color = "black", size = 10),
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

# 2.4. Exibir o gráfico PTAX
print(grafico_ptax)
showtext_opts(dpi = 300)

ggsave(filename = file.path(caminho_output, "grafico_ptax.png"),
        plot = grafico_ptax,
        bg = "transparent",
        width = 6,
        height = 6,
        dpi = 300
)


# --- 3. Gráfico DXY (Índice Dólar) ---

# 3.1. (NOVO) Calcular os limites do eixo Y para o DXY
limites_dxy <- c(
  min(dados_30d$DXY, na.rm = TRUE) * 0.99, 
  max(dados_30d$DXY, na.rm = TRUE) * 1.01
)

# 3.2. Criar o gráfico
grafico_dxy <- ggplot(
  data = dados_30d, 
  aes(x = date, y = DXY)
) +
  geom_area(
    data = dados_7d, 
    aes(y = DXY),
    fill = cor_grafico,
    alpha = 0.3,
    na.rm = TRUE
  ) +
  geom_line(
    color = cor_grafico,
    linewidth = 1,
    na.rm = TRUE
  ) +

  # --- AJUSTE 2: Definir os limites do Eixo Y ---
  coord_cartesian(
    ylim = limites_dxy,
    expand = FALSE
  ) +
  
  # 3.3. Aplicar o mesmo tema
  theme_classic() +
  theme(
    text = element_text(family = familia_fonte),
    axis.text = element_text(family = familia_fonte, color = "black", size = 10),
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

# 3.4. Exibir o gráfico DXY
print(grafico_dxy)
showtext_opts(dpi = 300)

ggsave(filename = file.path(caminho_output, "grafico_dxy.png"), 
        plot = grafico_dxy, 
        bg = "transparent", 
        width = 6, 
        height = 6, 
        dpi = 300
)

######## ======= Fim do Script ======= ########


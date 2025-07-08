# DADOS para análise do VECM

library(writexl)
library(GetBCBData)
library(tidyverse)
library(dplyr)
library(seasonal)
library(beepr)
library(lubridate)
library(x13binary)
library(ggplot2)
library(scales)
library(patchwork)


# Definindo o diretório de trabalho

setwd("C:/Users/gabri/OneDrive/Área de Trabalho/Acadêmico/Monografia - LOCAL/Elaboração")
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

View(dados_mensais)

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
  mutate(Ano = year(Data)) %>%
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
  mutate(COMPROMISSADAS = COMPROMISSADAS / 1000) # antes, garante que COMPROMISSADAS e LFT estão em milhões

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


### ====== Transformando os dados para trimestes ======== ###

# Cria coluna de trimestre e ano
dados_trimestral <- dados %>%
  mutate(
    Ano = year(Data),
    Trimestre = quarter(Data),
    Data_trimestre = case_when(
      Trimestre == 1 ~ as.Date(paste0(Ano, "-03-01")),
      Trimestre == 2 ~ as.Date(paste0(Ano, "-06-01")),
      Trimestre == 3 ~ as.Date(paste0(Ano, "-09-01")),
      Trimestre == 4 ~ as.Date(paste0(Ano, "-12-01"))
    )
  ) %>%
  group_by(Data_trimestre) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            Ano = first(Ano),
            Trimestre = first(Trimestre)) %>% # Se quiser manter, senão remova
  ungroup() %>%
  select(-Ano, -Trimestre) %>% # Aqui dentro do pipe!
  arrange(Data_trimestre)


colnames(dados_trimestral)
dados <- dados_trimestral

View(dados)
colnames(dados) <- c("Data", "LFT", "COMPROMISSADAS", "IPCA", "IPCAmm", 
                              "CRED_LIVRE", "IBCBR", "ICBRUSD", "PTAXV", 
                              "META", "SELIC_OVER", "LFT_COMPROMISSADAS",
                              "GAP_IPCA_META", "IPCAmm_acum", "Indice_IPCA")



# ====== Puxando dados de ATIVO_BANCARIO da máquina ======== # 
ativobancario <- read.csv(choose.files(), sep = ";", dec = ",")
ativobancario$Data <- as.Date(ativobancario$Data, format = "%d/%m/%Y")

View(ativobancario)

ativobancario <- ativobancario %>%
  mutate(ATIVO_BANCARIO = ATIVO_BANCARIO / 1000) # garante que ATIVO_BANCARIO está em milhões

View(ativobancario)

# Incluindo o ativo bancário na base de dados
dados <- full_join(dados, ativobancario, by = "Data")

colnames(dados)


# ======= Ajustando os dados para o IPCA a preços de dezembro/24 ======== #
dados <- dados %>% 
    mutate(
        LFT = LFT * Indice_IPCA,
        COMPROMISSADAS = COMPROMISSADAS * Indice_IPCA,
        LFT_COMPROMISSADAS = LFT_COMPROMISSADAS * Indice_IPCA,
        CRED_LIVRE = CRED_LIVRE * Indice_IPCA,
        ATIVO_BANCARIO = ATIVO_BANCARIO * Indice_IPCA,
    )


#====== Passando log para as variáveis que precisam de log ========#
dados <- dados %>%
  mutate(
    LFT = log(LFT),
    COMPROMISSADAS = log(COMPROMISSADAS),
    LFT_COMPROMISSADAS = log(LFT_COMPROMISSADAS),
    CRED_LIVRE = log(CRED_LIVRE),
    ATIVO_BANCARIO = log(ATIVO_BANCARIO),
    IBCBR = log(IBCBR),
    ICBRUSD = log(ICBRUSD)
  )


colnames(dados)

# ====== Definindo as variáveis pertinentes ao VECM ======== #
dados_vecm <- dados %>%
  select(
    Data, LFT_COMPROMISSADAS, GAP_IPCA_META, 
    CRED_LIVRE, IBCBR, ICBRUSD, PTAXV, 
    ATIVO_BANCARIO, SELIC_OVER
  )

colnames(dados_vecm) <- c(
  "Trimestre", "LOGLFT_COMPROMISSADAS", "GAP_IPCA_META", 
  "LOGCRED_LIVRE", "LOGIBCBR", "LOGICBRUSD", "PTAXV", 
  "LOGATIVO_BANCARIO", "SELIC_OVER"
)

View(dados_vecm)



# ========= Definindo o conjunto de dados e estatísticas descritivas ========= #
# Primeira analise para as variáveis de interesse
variaveis <- dados_vecm[, c(
  "LOGLFT_COMPROMISSADAS", "GAP_IPCA_META", "LOGCRED_LIVRE",
  "LOGIBCBR", "LOGICBRUSD", "PTAXV", "LOGATIVO_BANCARIO", "SELIC_OVER"
)]


View(variaveis)
str(variaveis)

# ======= Calculando as estatisticas descritivas ======== #
# Cria uma função para calcular estatísticas por coluna
calc_stats <- function(x) {
  c(
    Media = mean(x, na.rm = TRUE),
    Mediana = median(x, na.rm = TRUE),
    DP = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE)
  )
}

# Aplica a função a cada variável
descriptive_stats <- t(sapply(variaveis, calc_stats))

# Converte para data frame
descriptive_stats <- as.data.frame(descriptive_stats)
descriptive_stats$Variavel <- rownames(descriptive_stats)
rownames(descriptive_stats) <- NULL

# Reorganiza colunas
descriptive_stats <- descriptive_stats[, c("Variavel", names(descriptive_stats)[1:7])]

# Visualiza
View(descriptive_stats)

# Salva as estatísticas descritivas em um arquivo Excel
write_xlsx(descriptive_stats, "Descriptive_StatisticsNovo.xlsx")



# ========= Plotando as séries temporais originais e ajustadas sazonalmente ========= #


# Escolha as variáveis que deseja plotar (sem duplicidade)
variables_to_plot <- unique(c(
  "LOGLFT_COMPROMISSADAS", "GAP_IPCA_META", "LOGCRED_LIVRE",
  "LOGIBCBR", "LOGICBRUSD", "PTAXV", "LOGATIVO_BANCARIO", "SELIC_OVER"
))

# Defina os rótulos personalizados para o eixo Y
y_axis_labels <- c(
  LOGLFT_COMPROMISSADAS = "(log)",
  GAP_IPCA_META = "(p.p.)",
  LOGCRED_LIVRE = "(log)",
  LOGIBCBR = "(log)",
  LOGICBRUSD = "(log)",
  PTAXV = "(BRL/USD)",
  LOGATIVO_BANCARIO = "(log)",
  SELIC_OVER = "(%, a.a.)"
)

# Montando ts para as variáveis selecionadas (dados trimestrais)
variaveis_ts <- lapply(seq_along(names(variaveis)), function(i) {
  col <- names(variaveis)[i]
  ts(variaveis[[col]],
    start = c(
      as.numeric(substr(dados_vecm$Trimestre[1], 1, 4)), # Ano
      as.numeric(substr(dados_vecm$Trimestre[1], 6, 6)) # Trimestre (1, 2, 3 ou 4)
    ),
    frequency = 4 # Trimestral
  )
})

names(variaveis_ts) <- names(variaveis)
# Atribui dinamicamente cada time series a uma variavel
for (col in names(variaveis_ts)) {
  assign(col, variaveis_ts[[col]])
}

print(names(variaveis_ts))
str(variaveis_ts)

# --- Gera data.frame com séries originais (sem ajuste sazonal) ---
library(zoo)

original_dates <- as.yearqtr(time(variaveis_ts[[1]])) |> as.Date()
original_data <- data.frame(
  Trimestre = original_dates,
  sapply(variaveis_ts, as.numeric)
)

# --- Gráficos individuais das séries originais (linha azul) ---
for (var in variables_to_plot) {
  if (var %in% colnames(original_data)) {
    y_label <- ifelse(!is.null(y_axis_labels[[var]]), y_axis_labels[[var]], var)
    p <- ggplot(original_data, aes_string(x = "Trimestre", y = var)) +
      geom_line(color = "blue", size = 1) +
      labs(title = paste("Evolução de", var, "(Original)"), x = "Tempo (trimestral)", y = y_label) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        axis.line = element_line(color = "black")
      )
    ggsave(filename = paste0(var, "_original_time_series.png"), plot = p, width = 8, height = 6)
  }
}

# --- Gráfico agregado das séries originais ---
original_plot_list <- list()
for (var in variables_to_plot) {
  if (var %in% colnames(original_data)) {
    y_label <- ifelse(!is.null(y_axis_labels[[var]]), y_axis_labels[[var]], var)
    p <- ggplot(original_data, aes_string(x = "Trimestre", y = var)) +
      geom_line(color = "blue", size = 1) +
      labs(title = paste0(var, " (Original)"), x = "Tempo (trimestral)", y = y_label) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        axis.line = element_line(color = "black")
      )
    original_plot_list[[var]] <- p
  }
}

combined_original_plot <- wrap_plots(original_plot_list, ncol = 3)
ggsave("all_time_series_original.png", plot = combined_original_plot, width = 15, height = 10)
cat("Gráfico agregado das séries originais salvo como 'all_time_series_original.png'.\n")


# Verify the class of the time series objects
lapply(variaveis_ts, class)

par(mfrow = c(8, 1))
for (col in names(variaveis)) {
  plot(variaveis_ts[[col]], main = col, xlab = "Tempo", ylab = col, yaxt = "n")
  axis(2, at = pretty(variaveis_ts[[col]]), labels = format(pretty(variaveis_ts[[col]]), scientific = FALSE))
}


# Apply the SEASONAL x13-arima-seats with custom parameters
variaveis_ts_ajuste <- lapply(variaveis_ts, seas)

# Dynamically assign each adjusted series to a variable with "_ajustado" suffix
for (name in names(variaveis)) {
  assign(paste0(name, "_ajustado"), variaveis_ts_ajuste[[name]])
}

# Verify the adjusted variables are created
ls(pattern = "_ajustado")

# Print the class of each adjusted series
lapply(variaveis_ts_ajuste, class)

# Extracting the final adjusted series
variaveis_ts_ajuste_final <- lapply(variaveis_ts_ajuste, final)

# Dynamically assign each final adjusted series to a variable with "_SA" suffix
for (name in names(variaveis_ts_ajuste_final)) {
  assign(paste0(name, "_SA"), variaveis_ts_ajuste_final[[name]])
}

# Creating plots for each adjusted variable
Nomes_ajustados <- ls(pattern = "_SA") # Get all adjusted variable names

# Set up the layout to display multiple plots in one image
par(mfrow = c(1, 1))

for (name in Nomes_ajustados) {
  plot(get(name), main = paste("Ajuste Sazonal de", name))
  axis(2, at = pretty(get(name)), labels = format(pretty(get(name)), scientific = FALSE))
}

lapply(variaveis_ts_ajuste_final, class)

# Extract dates from one of the time series (assuming all have the same time index)
dates <- format(as.yearmon(time(variaveis_ts_ajuste_final[[1]])), "%Y-%m")

# Create a data frame with all final adjusted series
LOGDADOS_VAR_SA <- data.frame(
  Trimestre = dates,
  lapply(variaveis_ts_ajuste_final, as.numeric) # Convert each time series to numeric
)

View(LOGDADOS_VAR_SA)

# Save the data frame to a CSV file
write.csv2(LOGDADOS_VAR_SA, "LOGDADOS_VAR_SArevisado.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ----- Plotando os dados ajustados e em LOG -----

# Carrega os dados ajustados
series_ajustadas <- read.csv("LOGDADOS_VAR_SArevisado.csv", sep = ";", dec = ",", fileEncoding = "UTF-8")
View(series_ajustadas)

# Garante que a coluna Trimestre está no formato Date
if (!inherits(series_ajustadas$Trimestre, "Date")) {
  series_ajustadas$Trimestre <- as.Date(paste0(series_ajustadas$Trimestre, "-01"))
}

# Função para plotar séries temporais (nível)
plot_time_series <- function(data, time_column, variables_to_plot, y_axis_labels) {
  for (var in variables_to_plot) {
    if (var %in% colnames(data)) {
      y_label <- ifelse(!is.null(y_axis_labels[[var]]), y_axis_labels[[var]], var)
      p <- ggplot(data, aes_string(x = time_column, y = var)) +
        geom_line(color = "#00AA00", size = 1) +
        labs(title = paste("Evolução de", var), x = "Tempo (trimestral)", y = y_label) +
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          axis.line = element_line(color = "black")
        )
      ggsave(filename = paste0(var, "_SA_log_time_series.png"), plot = p, width = 8, height = 6)
    } else {
      cat(paste("Variável", var, "não encontrada no dataset.\n"))
    }
  }
}

# Chama a função para plotar séries em nível (log)
plot_time_series(series_ajustadas, time_column = "Trimestre", variables_to_plot = variables_to_plot, y_axis_labels = y_axis_labels)

# --- Gráfico agregado séries em nível (log) ---
plot_list <- list()
for (var in variables_to_plot) {
  if (var %in% colnames(series_ajustadas)) {
    y_label <- ifelse(!is.null(y_axis_labels[[var]]), y_axis_labels[[var]], var)
    p <- ggplot(series_ajustadas, aes_string(x = "Trimestre", y = var)) +
      geom_line(color = "#00AA00", size = 1) +
      labs(title = var, x = "Tempo (trimestral)", y = y_label) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        axis.line = element_line(color = "black")
      )
    plot_list[[var]] <- p
  }
}

combined_plot <- wrap_plots(plot_list, ncol = 3)
ggsave("all_time_series_log.png", plot = combined_plot, width = 15, height = 10)
cat("Gráfico agregado das séries em log salvo como 'all_time_series_log.png'.\n")





# Cria as variáveis em primeira diferença
# --- Cria as variáveis de diferença no data frame ---
diff_data <- series_ajustadas
for (var in variables_to_plot) {
  if (var %in% colnames(series_ajustadas)) {
    diff_data[[paste0("diff_", var)]] <- c(NA, diff(series_ajustadas[[var]]))
  }
}

diff_data <- diff_data %>%
  select(Trimestre, starts_with("diff_"))

# Cria um excel com as séries temporais em primeira diferença
write_xlsx(diff_data, "diff_LOGDADOS_VAR_SArevisado.xlsx")

# --- Atualiza a lista de variáveis para as diferenças ---
diff_variables_to_plot <- paste0("diff_", variables_to_plot)

# --- Rótulos personalizados para as diferenças ---
diff_y_axis_labels <- c(
  diff_LOGLFT_COMPROMISSADAS = "Δ LFT Compromissadas (log)",
  diff_GAP_IPCA_META = "Δ Gap IPCA/Meta (p.p.)",
  diff_LOGCRED_LIVRE = "Δ Crédito Livre (log)",
  diff_LOGIBCBR = "Δ IBC-BR (log)",
  diff_LOGICBRUSD = "Δ IC-BR USD (log)",
  diff_PTAXV = "Δ PTAX Venda (BRL/USD)",
  diff_LOGATIVO_BANCARIO = "Δ Ativo Bancário (log)",
  diff_SELIC_OVER = "Δ Selic Over (%, a.a.)"
)


# Função para plotar séries temporais das diferenças
plot_time_series_diff <- function(data, time_column, variables_to_plot, y_axis_labels) {
  for (var in variables_to_plot) {
    if (var %in% colnames(data)) {
      plot_data <- data[!is.na(data[[var]]), ]
      y_label <- ifelse(!is.null(y_axis_labels[[var]]), y_axis_labels[[var]], var)
      p <- ggplot(plot_data, aes_string(x = time_column, y = var)) +
        geom_line(color = "red", size = 1) +
        labs(title = paste("Evolução da diferença de", var), x = "Tempo (trimestral)", y = y_label) +
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          axis.line = element_line(color = "black")
        )
      ggsave(filename = paste0(var, "_diff_log_time_series.png"), plot = p, width = 8, height = 6)
    } else {
      cat(paste("Variável", var, "não encontrada no dataset.\n"))
    }
  }
}

# Chama a função para plotar as diferenças
plot_time_series_diff(diff_data, time_column = "Trimestre", variables_to_plot = diff_variables_to_plot, y_axis_labels = diff_y_axis_labels)

cat("Gráficos (nível e diferença) com séries em log foram salvos como arquivos PNG.\n")

# --- Gráfico agregado séries em diferença (diff log) ---
diff_plot_list <- list()
for (var in diff_variables_to_plot) {
  if (var %in% colnames(diff_data)) {
    y_label <- ifelse(!is.null(diff_y_axis_labels[[var]]), diff_y_axis_labels[[var]], var)
    p <- ggplot(diff_data, aes_string(x = "Trimestre", y = var)) +
      geom_line(color = "red", size = 1) +
      labs(title = var, x = "Tempo (trimestral)", y = y_label) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        axis.line = element_line(color = "black")
      )
    diff_plot_list[[var]] <- p
  }
}

combined_diff_plot <- wrap_plots(diff_plot_list, ncol = 3)
ggsave("all_time_series_diff_log.png", plot = combined_diff_plot, width = 15, height = 10)
cat("Gráfico agregado das séries em diferença (diff log) salvo como 'all_time_series_diff_log.png'.\n")

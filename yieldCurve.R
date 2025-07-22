# Script para plottar a curva de juros dos EUA

library(xm12)
library(zoo)
library(writexl)
library(xts)
library(ragg)
library(dplyr)
library(pbapply)
library(quantmod)
library(beepr)
library(showtext)
library(sysfonts)

# A URL para os dados diários de taxas de juros do Tesouro dos EUA é:
# https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield

# Definindo o diretório de trabalho
choose.files()
diretorio <- choose.dir()
setwd(diretorio)
getwd()

# Acesso às taxas diárias do Tesouro dos EUA
YEAR <- 2025
RATES <- read.csv2(paste0(
       "https://home.treasury.gov/resource-center/data-chart-center/interest-rates/",
       "daily-treasury-rates.csv/", YEAR, "/all?type=daily_treasury_yield_curve&",
       "field_tdr_date_value=", YEAR, "&page&_format=csv"
), header = TRUE, sep = ",")
RATES <- as.data.frame(RATES)

# Verificando os dados e filtrando as colunas necessárias
View(RATES)

ncol(RATES)
print(colnames(RATES))

# Filtrando as colunas de interesse
RATES <- RATES %>% select(-X1.Mo, -X1.5.Month, -X4.Mo, -X2.Mo, -X3.Mo)

ncol(RATES)
View(RATES)

# Converte os dados para o formato xts
RATES <- xts(x = RATES[, 2:ncol(RATES)], order.by = as.Date(RATES$Date, format = "%m/%d/%Y"))
# Renomeando as colunas para facilitar a identificação
colnames(RATES) <- c(
       "6m", "1y", "2y", "3y", "5y",
       "7y", "10y", "20y", "30y"
)

storage.mode(RATES) <- "numeric"
print(colnames(RATES))
View(RATES)

# Definindo RATES como data.frame para exportar para Excel
RATES_xl <- data.frame(
       Data = index(RATES),
       coredata(RATES)
)


# Gera timestamp no formato YYYYMMDD_HHMMSS e salva o arquivo Excel
timestamp <- format(Sys.time(), "%Y%m%d")
write_xlsx(RATES_xl, paste0("USYieldCurve_", timestamp, ".xlsx"))
str(RATES)


# Carregar a fonte "plusjakarta" do Google Fonts
font_add("plusjakarta",
       regular =
              "INSIRA O CAMINHO PARA A FONTE DESEJADA"
)
showtext_auto()

# Função para plotar curvas de datas escolhidas usando base R
plot_yield_curves_base <- function(RATES_xts, dates, legend_labels = NULL, colors = NULL) {
       valid_dates <- dates[dates %in% as.character(index(RATES_xts))]
       if (length(valid_dates) == 0) stop("Nenhuma das datas fornecidas existe nos dados.")
       if (is.null(legend_labels)) legend_labels <- format(as.Date(valid_dates), "%d/%m/%Y")

       # Cores: cinza, verde-água, e azul para a mais recente
       if (is.null(colors)) {
              colors <- rep("grey40", length(valid_dates))
              if (length(valid_dates) >= 2) {
                     colors[length(valid_dates) - 1] <- "#40E0D0" # Verde-água (turquoise)
              }
              if (length(valid_dates) >= 1) {
                     idx_recent <- which.max(as.Date(valid_dates))
                     colors[idx_recent] <- "#189CD8" # Azul para a mais recente
              }
       }

       mat <- t(coredata(RATES_xts[valid_dates, ]))
       maturities <- colnames(RATES_xts)

       par(bg = "#F2F2F2", family = "plusjakarta", las = 1)
       matplot(
              x = seq_along(maturities),
              y = mat,
              type = "l",
              lty = 1,
              lwd = 2,
              col = colors,
              xaxt = "n",
              yaxt = "s",
              xlab = "",
              ylab = "",
              main = "",
              bty = "l"
       )
       axis(1,
              at = seq_along(maturities),
              labels = maturities, family = "plusjakarta"
       )
       box(bty = "l")
       legend("topleft",
              inset = 0, xpd = NA,
              legend = legend_labels, col = colors, lty = 1,
              lwd = 2, bty = "n",
              text.font = 2, cex = 1, x.intersp = 0.7, seg.len = 1.5,
       )
       showtext::showtext_opts(dpi = 96)
}


# Defina as datas que você deseja plotar:
datas_para_plotar <- c("2025-06-30", "2025-07-03")
plot_yield_curves_base(RATES, datas_para_plotar)

# Salvar o gráfico como PNG (método base R)
png(
       filename = paste0("yield_curve_plot_base_", timestamp, ".png"),
       width = 800, height = 500, bg = "transparent"
)

plot_yield_curves_base(RATES, datas_para_plotar)
dev.off()

# ====================== Puxando dados da API do SIDRA e do SGS - IPCA m/m ========================
library(sidrar)
library(zoo)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(GetBCBData)

choose.files() # workaround para forçar a função choose.dir() a funcionar, pois pode bugar
diretorio <- choose.dir()
setwd(diretorio)
getwd()


# ==== Coletando dados da tabela 7060 - IPCA variação mensal e pesos
tabela_7060 <- get_sidra(7060, api = "/t/7060/n1/all/v/63,66/p/last%2041/c315/all/d/v63%202,v66%204")
View(tabela_7060)

# Selecionando colunas relevantes incluindo código da variável
itens_7060 <- tabela_7060 %>%
  select(
    data = "Mês (Código)",
    variable = "Geral, grupo, subgrupo, item e subitem",
    variavel_codigo = "Variável (Código)",
    variavel_nome = "Variável",
    value = "Valor"
  ) %>%
  as_tibble()

View(itens_7060)

# Verificando os códigos das variáveis para entender a estrutura
print("Códigos únicos das variáveis:")
print(unique(itens_7060$variavel_codigo))
print("Nomes únicos das variáveis:")
print(unique(itens_7060$variavel_nome))

# Separando variações mensais (v=63) dos pesos (v=66) usando código da variável
ipca_variacoes <- itens_7060 %>%
  filter(variavel_codigo == "63") %>%
  select(data, variable, value) %>%
  mutate(data = ym(data)) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )

ipca_pesos <- itens_7060 %>%
  filter(variavel_codigo == "66") %>%
  select(data, variable, value) %>%
  mutate(data = ym(data)) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )

View(ipca_variacoes)
View(ipca_pesos)

# Transformando para formato transposto (datas como colunas) preservando os nomes dos itens
ipca_variacoes_transposta <- ipca_variacoes %>%
  pivot_longer(cols = -data, names_to = "item", values_to = "variacao") %>%
  pivot_wider(names_from = data, values_from = variacao) %>%
  # Reordenando para ter o item como primeira coluna
  select(item, everything())

ipca_pesos_transposta <- ipca_pesos %>%
  pivot_longer(cols = -data, names_to = "item", values_to = "peso") %>%
  pivot_wider(names_from = data, values_from = peso) %>%
  # Reordenando para ter o item como primeira coluna
  select(item, everything())

View(ipca_variacoes_transposta)

# ====================== Análise dos itens disponíveis para núcleos ========================

# Verificando todos os itens disponíveis
print("Itens disponíveis na tabela IPCA:")
print(ipca_variacoes_transposta$item)

# ====================== Definição dos componentes dos núcleos ========================

# NÚCLEO SERVIÇOS SUBJACENTES - Serviços menos voláteis e mais persistentes
servicos_subjacentes <- c(
  "Serviços de saúde e cuidados pessoais",
  "Educação",
  "Recreação e cultura",
  "Despesas pessoais"
)

# NÚCLEO SERVIÇOS INTENSIVOS EM TRABALHO - Serviços com alta participação de mão de obra
# Focando apenas nos serviços mais intensivos em trabalho
servicos_intensivos_trabalho <- c(
  "Serviços de saúde e cuidados pessoais",
  "Educação",
  "Despesas pessoais"
  # Removido "Recreação e cultura" para diferenciação - menos intensivo em trabalho
)

# NÚCLEO INDUSTRIAIS SUBJACENTES - Bens industriais excluindo mais voláteis
industriais_subjacentes <- c(
  "Artigos de residência",
  "Vestuário", 
  "Equipamentos e manutenção da habitação",
  "Medicamentos",
  "Higiene pessoal"
)

# Função para encontrar itens que contêm as palavras-chave (busca flexível)
encontrar_itens <- function(lista_itens, palavras_chave) {
  itens_encontrados <- c()
  for (palavra in palavras_chave) {
    matches <- lista_itens[grepl(palavra, lista_itens, ignore.case = TRUE)]
    itens_encontrados <- c(itens_encontrados, matches)
    print(paste("Buscando por '", palavra, "' - encontrados:", paste(matches, collapse = ", ")))
  }
  return(unique(itens_encontrados))
}

# Encontrando itens reais disponíveis para cada núcleo
print("\n=== BUSCANDO ITENS PARA CADA NÚCLEO ===")
print("SERVIÇOS SUBJACENTES:")
servicos_subj_real <- encontrar_itens(ipca_variacoes_transposta$item, servicos_subjacentes)
print("SERVIÇOS INTENSIVOS EM TRABALHO:")
servicos_trab_real <- encontrar_itens(ipca_variacoes_transposta$item, servicos_intensivos_trabalho)
print("INDUSTRIAIS SUBJACENTES:")
industriais_subj_real <- encontrar_itens(ipca_variacoes_transposta$item, industriais_subjacentes)

print("=== COMPONENTES DOS NÚCLEOS ===")
print("SERVIÇOS SUBJACENTES:")
print(servicos_subj_real)
print("\nSERVIÇOS INTENSIVOS EM TRABALHO:")
print(servicos_trab_real)
print("\nINDUSTRIAIS SUBJACENTES:")
print(industriais_subj_real)

# Verificando se há diferença entre os núcleos de serviços
print("\n=== VERIFICAÇÃO DE DIFERENÇAS ===")
print("Definições originais:")
print(paste("Serviços Subjacentes:", paste(servicos_subjacentes, collapse = ", ")))
print(paste("Serviços Intensivos Trabalho:", paste(servicos_intensivos_trabalho, collapse = ", ")))

print("\nItens encontrados na base:")
print(paste("Serviços Subjacentes encontrados:", paste(servicos_subj_real, collapse = ", ")))
print(paste("Serviços Intensivos Trabalho encontrados:", paste(servicos_trab_real, collapse = ", ")))

print("\nComparação:")
print("Itens únicos em Serviços Subjacentes:")
print(setdiff(servicos_subj_real, servicos_trab_real))
print("Itens únicos em Serviços Intensivos em Trabalho:")
print(setdiff(servicos_trab_real, servicos_subj_real))
print("Itens comuns aos dois núcleos:")
print(intersect(servicos_subj_real, servicos_trab_real))

# Verificando se os vetores são realmente diferentes
print(paste("Os vetores são idênticos?", identical(servicos_subj_real, servicos_trab_real)))

# FORÇANDO DIFERENCIAÇÃO - Vou criar manualmente as diferenças
print("\n=== FORÇANDO DIFERENCIAÇÃO MANUAL ===")

# Primeiro, vamos ver se "Recreação e cultura" realmente existe
recreacao_items <- ipca_variacoes_transposta$item[grepl("Recreação", ipca_variacoes_transposta$item, ignore.case = TRUE)]
print(paste("Itens com 'Recreação':", paste(recreacao_items, collapse = ", ")))

cultura_items <- ipca_variacoes_transposta$item[grepl("Cultura", ipca_variacoes_transposta$item, ignore.case = TRUE)]
print(paste("Itens com 'Cultura':", paste(cultura_items, collapse = ", ")))

# Criando os núcleos manualmente com diferenciação forçada
print("\n=== CRIANDO NÚCLEOS COM DIFERENCIAÇÃO FORÇADA ===")

# Núcleo Serviços Subjacentes - TODOS os itens encontrados
servicos_subj_real_forcado <- unique(c(
  ipca_variacoes_transposta$item[grepl("Serviços de saúde", ipca_variacoes_transposta$item, ignore.case = TRUE)],
  ipca_variacoes_transposta$item[grepl("Educação", ipca_variacoes_transposta$item, ignore.case = TRUE)],
  ipca_variacoes_transposta$item[grepl("Recreação", ipca_variacoes_transposta$item, ignore.case = TRUE)],
  ipca_variacoes_transposta$item[grepl("Cultura", ipca_variacoes_transposta$item, ignore.case = TRUE)],
  ipca_variacoes_transposta$item[grepl("Despesas pessoais", ipca_variacoes_transposta$item, ignore.case = TRUE)]
))

# Núcleo Serviços Intensivos em Trabalho - SEM recreação/cultura
servicos_trab_real_forcado <- unique(c(
  ipca_variacoes_transposta$item[grepl("Serviços de saúde", ipca_variacoes_transposta$item, ignore.case = TRUE)],
  ipca_variacoes_transposta$item[grepl("Educação", ipca_variacoes_transposta$item, ignore.case = TRUE)],
  ipca_variacoes_transposta$item[grepl("Despesas pessoais", ipca_variacoes_transposta$item, ignore.case = TRUE)]
))

print("Serviços Subjacentes (forçado):")
print(servicos_subj_real_forcado)
print("Serviços Intensivos Trabalho (forçado):")
print(servicos_trab_real_forcado)

print("\nDiferença forçada:")
print(paste("Únicos em Subjacentes:", paste(setdiff(servicos_subj_real_forcado, servicos_trab_real_forcado), collapse = ", ")))
print(paste("Únicos em Intensivos:", paste(setdiff(servicos_trab_real_forcado, servicos_subj_real_forcado), collapse = ", ")))

# Sobrescrevendo as variáveis originais
servicos_subj_real <- servicos_subj_real_forcado
servicos_trab_real <- servicos_trab_real_forcado

# ====================== Cálculo dos núcleos de inflação ========================

# Função aprimorada para calcular núcleos com pesos dinâmicos
calcular_nucleo_ponderado <- function(variacoes_df, pesos_df, itens_selecionados, nome_nucleo) {
  # Filtrando apenas os itens selecionados
  variacoes_filtradas <- variacoes_df %>%
    filter(item %in% itens_selecionados)

  pesos_filtrados <- pesos_df %>%
    filter(item %in% itens_selecionados)

  if (nrow(variacoes_filtradas) == 0) {
    print(paste("AVISO: Nenhum item encontrado para", nome_nucleo))
    return(NULL)
  }

  print(paste("Calculando", nome_nucleo, "com", nrow(variacoes_filtradas), "itens:"))
  print(variacoes_filtradas$item)

  # Calculando média ponderada para cada data
  datas <- names(variacoes_filtradas)[-1] # Excluindo coluna 'item'

  resultado <- data.frame(item = nome_nucleo)

  for (data_col in datas) {
    if (data_col %in% names(pesos_filtrados)) {
      variacoes_data <- variacoes_filtradas[[data_col]]
      pesos_data <- pesos_filtrados[[data_col]]

      # Removendo NAs
      valid_idx <- !is.na(variacoes_data) & !is.na(pesos_data)

      if (sum(valid_idx) > 0) {
        # Calculando média ponderada
        media_ponderada <- weighted.mean(variacoes_data[valid_idx], pesos_data[valid_idx])
        resultado[[data_col]] <- round(media_ponderada, 2)
      } else {
        resultado[[data_col]] <- NA
      }
    } else {
      resultado[[data_col]] <- NA
    }
  }

  return(resultado)
}

# Calculando os três núcleos
print("\n=== CALCULANDO NÚCLEOS COM DIFERENCIAÇÃO FORÇADA ===")

nucleo_servicos_subjacentes <- calcular_nucleo_ponderado(
  ipca_variacoes_transposta,
  ipca_pesos_transposta,
  servicos_subj_real,
  "Núcleo Serviços Subjacentes"
)

nucleo_servicos_trabalho <- calcular_nucleo_ponderado(
  ipca_variacoes_transposta,
  ipca_pesos_transposta,
  servicos_trab_real,
  "Núcleo Serviços Intensivos em Trabalho"
)

nucleo_industriais_subjacentes <- calcular_nucleo_ponderado(
  ipca_variacoes_transposta,
  ipca_pesos_transposta,
  industriais_subj_real,
  "Núcleo Industriais Subjacentes"
)

# Combinando todos os núcleos em uma tabela
nucleos_calculados <- bind_rows(
  nucleo_servicos_subjacentes,
  nucleo_servicos_trabalho,
  nucleo_industriais_subjacentes
)

View(nucleos_calculados)

print("\n=== NÚCLEOS CALCULADOS ===")
print(nucleos_calculados)
head(nucleos_calculados)

# ====================== Puxando dados da API do BCB - IPCA m/m ========================

# Importando dados do BCB
IPCASGS <- GetBCBData::gbcbd_get_series(
  id = c(
    "ADMINISTRADOS" = 4449,
    "LIVRES" = 11428, "NAODURAVEIS" = 10841, "SEMIDURAVEIS" = 10842,
    "DURAVEIS" = 10843, "SERVICOS" = 10844, "INDUSTRIAIS" = 27863,
    "DIFUSAO" = 21379, "IPCAMS" = 4466, "IPCAEX0" = 11427,
    "IPCAEX3" = 27839, "IPCAEXFE" = 28751
  ),
  first.date = "2022-01-01",
  last.date = Sys.Date(),
  format.data = "wide"
)

colnames(IPCASGS)
head(IPCASGS)

# Transpondo a tabela: transformando datas em colunas e itens em linhas
IPCASGS_transposta <- IPCASGS %>%
  pivot_longer(cols = -ref.date, names_to = "item", values_to = "variacao") %>%
  pivot_wider(names_from = ref.date, values_from = variacao)

View(IPCASGS_transposta)

# ====================== Inserindo núcleos calculados na tabela SGS ========================

# Primeiro, vamos inserir os núcleos de serviços após a linha "SERVICOS" (linha 6)
# E o núcleo industriais após a linha "INDUSTRIAIS" (linha 7)

# Criando uma cópia da tabela SGS para manipulação
IPCASGS_com_nucleos <- IPCASGS_transposta

# Encontrando as posições dos itens SERVICOS e INDUSTRIAIS
pos_servicos <- which(IPCASGS_com_nucleos$item == "SERVICOS")
pos_industriais <- which(IPCASGS_com_nucleos$item == "INDUSTRIAIS")

print(paste("Posição SERVICOS:", pos_servicos))
print(paste("Posição INDUSTRIAIS:", pos_industriais))

# Inserindo núcleos de serviços após SERVICOS
if(length(pos_servicos) > 0) {
  # Extraindo os dois núcleos de serviços
  nucleos_servicos <- nucleos_calculados[1:2, ]  # Primeiras 2 linhas são os núcleos de serviços
  
  # Inserindo após a posição de SERVICOS
  IPCASGS_com_nucleos <- bind_rows(
    IPCASGS_com_nucleos[1:pos_servicos, ],           # Linhas até SERVICOS (inclusive)
    nucleos_servicos,                                 # Núcleos de serviços
    IPCASGS_com_nucleos[(pos_servicos + 1):nrow(IPCASGS_com_nucleos), ]  # Resto das linhas
  )
}

# Recalculando a posição de INDUSTRIAIS após inserção
pos_industriais_nova <- which(IPCASGS_com_nucleos$item == "INDUSTRIAIS")
print(paste("Nova posição INDUSTRIAIS:", pos_industriais_nova))

# Inserindo núcleo industriais após INDUSTRIAIS
if(length(pos_industriais_nova) > 0) {
  # Extraindo o núcleo industrial
  nucleo_industrial <- nucleos_calculados[3, ]  # Terceira linha é o núcleo industrial
  
  # Inserindo após a posição de INDUSTRIAIS
  IPCASGS_com_nucleos <- bind_rows(
    IPCASGS_com_nucleos[1:pos_industriais_nova, ],                    # Linhas até INDUSTRIAIS (inclusive)
    nucleo_industrial,                                                 # Núcleo industrial
    IPCASGS_com_nucleos[(pos_industriais_nova + 1):nrow(IPCASGS_com_nucleos), ]  # Resto das linhas
  )
}

View(IPCASGS_com_nucleos)
print("=== TABELA SGS COM NÚCLEOS INSERIDOS ===")
print(IPCASGS_com_nucleos$item)

# ====================== Criando tabela consolidada ========================

# Mesclando IPCA_Variacoes_SIDRA com IPCA_SGS (incluindo núcleos)
# IPCA_SGS começará imediatamente abaixo da última linha de IPCA_Variacoes_SIDRA

tabela_consolidada <- bind_rows(
  ipca_variacoes_transposta,    # Dados do SIDRA primeiro
  IPCASGS_com_nucleos          # Dados do SGS com núcleos em seguida
)

View(tabela_consolidada)
print("=== TABELA CONSOLIDADA CRIADA ===")
print(paste("Total de itens na tabela consolidada:", nrow(tabela_consolidada)))
print("Primeiros 10 itens:")
print(head(tabela_consolidada$item, 10))
print("Últimos 10 itens:")
print(tail(tabela_consolidada$item, 10))

# ====================== Exportação completa incluindo tabela consolidada ========================

# Exportando para Excel
timestamp <- format(Sys.time(), "%Y%m%d")
write_xlsx(
  list(
    "IPCA_Consolidado" = tabela_consolidada,
    "IPCA_Variacoes_SIDRA" = ipca_variacoes_transposta,
    "IPCA_Pesos_SIDRA" = ipca_pesos_transposta,
    "IPCA_SGS_com_Nucleos" = IPCASGS_com_nucleos,
    "Nucleos_Calculados" = nucleos_calculados
  ),
  path = paste0("IPCAs_", timestamp, ".xlsx")
)

print(paste("Arquivo exportado:", paste0("IPCAs_", timestamp, ".xlsx")))
print("Planilhas incluídas:")
print("- IPCA_Consolidado: Tabela única com SIDRA + SGS + núcleos inseridos")
print("- IPCA_Variacoes_SIDRA: Variações mensais de todos os itens (SIDRA)")
print("- IPCA_Pesos_SIDRA: Pesos mensais de todos os itens (SIDRA)")
print("- IPCA_SGS_com_Nucleos: Séries do SGS/BCB com núcleos inseridos")
print("- Nucleos_Calculados: Núcleos de inflação calculados separadamente")

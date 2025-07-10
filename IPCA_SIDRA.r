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


# Puxar a tabela criando a data de forma dinâmica até o mês atual.
#    Criamos um intervalo fechado que o pacote aceita.
data_inicio <- "202201"
data_fim <- format(Sys.Date(), "%Y%m")
periodo_completo <- paste0(data_inicio, "-", data_fim)

# Informar o usuário qual período está sendo usado
print(paste("Buscando dados para o período:", periodo_completo))

# 3. Fazer a chamada à API com o período dinâmico e fechado
#    Note que agora o argumento 'period' recebe uma string como "202201-202507"
tabela_7060 <- get_sidra(
  x = 7060,
  period = periodo_completo, # Usando o período que acabamos de criar
  variable = c(63, 66),
  classific = "c315",
  geo = "Brazil"
)

View(tabela_7060)

# Selecionando colunas relevantes incluindo código da variável e código do item
itens_7060 <- tabela_7060 %>%
  select(
    data = "Mês (Código)",
    variable = "Geral, grupo, subgrupo, item e subitem",
    item_codigo = "Geral, grupo, subgrupo, item e subitem (Código)",
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

# Criando mapeamento código-item para identificação
mapeamento_codigo_item <- itens_7060 %>%
  select(item_codigo, variable) %>%
  distinct() %>%
  arrange(item_codigo)

print("=== MAPEAMENTO CÓDIGO-ITEM ===")
print(mapeamento_codigo_item)

# Lista de códigos fornecida pelo usuário para serviços subjacentes
codigos_servicos_subjacentes <- c(
  7432, # Aliment. fora do domicilio
  7448, # Aluguel residencial
  7449, # Condomínio
  7453, # Mudança
  7548, # Consertos e manutenção
  7639, # Transporte escolar
  7643, # Seguro voluntario de veículo
  7647, # Conserto de automóvel
  7648, # Estacionamento
  107656, # Aluguel de veículo
  7653, # Pintura de veículo
  7685, # Médico
  7686, # Dentista
  12414, # Aparelho ortodontico
  12435, # Fisioterapia
  12436, # Psicologo
  7690, # Serviços laboratoriais e hospitalares
  7715, # Costureira
  12421, # Manicure
  47654, # Cabeleireiro e barbeiro
  7721, # Depilação
  7724, # Despachante
  7727, # Serviço Bancário
  47655, # Sobrancelha
  7733, # Clube
  47657, # Tratamento de animais (clínica)
  47658, # Casa noturna
  47661, # Serviço de higiene para animais
  47662 # Cinema, teatro e concertos
)

# Lista de códigos para serviços intensivos em trabalho
codigos_servicos_intensivos_trabalho <- c(
  7685, # Médico
  7686, # Dentista
  12435, # Fisioterapia
  12436, # Psicologo
  7715, # Costureira
  12421, # Manicure
  47654, # Cabeleireiro e barbeiro
  7724, # Despachante
  47655, # Sobrancelha
  107641, # Mão de obra
  7720 # Empregado doméstico
)

# Identificando os nomes dos itens correspondentes aos códigos fornecidos
itens_servicos_subjacentes_por_codigo <- mapeamento_codigo_item %>%
  filter(item_codigo %in% as.character(codigos_servicos_subjacentes)) %>%
  pull(variable)

print("=== ITENS PARA NÚCLEO SERVIÇOS SUBJACENTES (POR CÓDIGO) ===")
print("Códigos fornecidos:")
print(codigos_servicos_subjacentes)
print("Itens correspondentes encontrados:")
print(itens_servicos_subjacentes_por_codigo)
print(paste("Total de itens encontrados:", length(itens_servicos_subjacentes_por_codigo)))

# Verificando se alguns códigos não foram encontrados
codigos_nao_encontrados <- setdiff(as.character(codigos_servicos_subjacentes), mapeamento_codigo_item$item_codigo)
if (length(codigos_nao_encontrados) > 0) {
  print("Códigos não encontrados na base:")
  print(codigos_nao_encontrados)
}

# Identificando os nomes dos itens para serviços intensivos em trabalho
itens_servicos_intensivos_trabalho_por_codigo <- mapeamento_codigo_item %>%
  filter(item_codigo %in% as.character(codigos_servicos_intensivos_trabalho)) %>%
  pull(variable)

print("=== ITENS PARA NÚCLEO SERVIÇOS INTENSIVOS EM TRABALHO (POR CÓDIGO) ===")
print("Códigos fornecidos:")
print(codigos_servicos_intensivos_trabalho)
print("Itens correspondentes encontrados:")
print(itens_servicos_intensivos_trabalho_por_codigo)
print(paste("Total de itens encontrados:", length(itens_servicos_intensivos_trabalho_por_codigo)))

# Verificando se alguns códigos não foram encontrados
codigos_nao_encontrados_trabalho <- setdiff(as.character(codigos_servicos_intensivos_trabalho), mapeamento_codigo_item$item_codigo)
if (length(codigos_nao_encontrados_trabalho) > 0) {
  print("Códigos não encontrados na base (serviços intensivos trabalho):")
  print(codigos_nao_encontrados_trabalho)
}

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
  # Convertendo data para character para garantir compatibilidade
  mutate(data = as.character(data)) %>%
  pivot_longer(cols = -data, names_to = "item", values_to = "variacao") %>%
  pivot_wider(names_from = data, values_from = variacao) %>%
  # Reordenando para ter o item como primeira coluna
  select(item, everything())

ipca_pesos_transposta <- ipca_pesos %>%
  # Convertendo data para character para garantir compatibilidade
  mutate(data = as.character(data)) %>%
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

# NÚCLEO SERVIÇOS SUBJACENTES - Usando códigos específicos fornecidos pelo usuário
print("SERVIÇOS SUBJACENTES (por códigos específicos):")
servicos_subj_real <- itens_servicos_subjacentes_por_codigo
print(paste("Total de itens:", length(servicos_subj_real)))
print("Itens incluídos:")
if (length(servicos_subj_real) > 0) {
  for (i in seq_len(min(10, length(servicos_subj_real)))) {
    print(paste(" -", servicos_subj_real[i]))
  }
  if (length(servicos_subj_real) > 10) {
    print(paste("... e mais", length(servicos_subj_real) - 10, "itens"))
  }
} else {
  print("Nenhum item encontrado!")
}

# NÚCLEO SERVIÇOS INTENSIVOS EM TRABALHO - Usando códigos específicos
print("SERVIÇOS INTENSIVOS EM TRABALHO (por códigos específicos):")
servicos_trab_real <- itens_servicos_intensivos_trabalho_por_codigo
print(paste("Total de itens:", length(servicos_trab_real)))
print("Itens incluídos:")
if (length(servicos_trab_real) > 0) {
  for (i in seq_len(min(10, length(servicos_trab_real)))) {
    print(paste(" -", servicos_trab_real[i]))
  }
  if (length(servicos_trab_real) > 10) {
    print(paste("... e mais", length(servicos_trab_real) - 10, "itens"))
  }
} else {
  print("Nenhum item encontrado!")
}

print("INDUSTRIAIS SUBJACENTES:")
industriais_subj_real <- encontrar_itens(ipca_variacoes_transposta$item, industriais_subjacentes)

print("=== COMPONENTES DOS NÚCLEOS ===")
print("SERVIÇOS SUBJACENTES (baseado em códigos específicos):")
print(paste("Quantidade:", length(servicos_subj_real)))
print("Primeiros 10 itens:")
print(head(servicos_subj_real, 10))

print("\nSERVIÇOS INTENSIVOS EM TRABALHO (baseado em códigos específicos):")
print(paste("Quantidade:", length(servicos_trab_real)))
print("Primeiros 10 itens:")
print(head(servicos_trab_real, 10))

print("\nINDUSTRIAIS SUBJACENTES:")
print(industriais_subj_real)

# Verificando se há diferença entre os núcleos de serviços
print("\n=== VERIFICAÇÃO DE DIFERENÇAS ===")
print("Ambos os núcleos de serviços agora usam códigos específicos")

print("\nItens encontrados na base:")
print(paste("Serviços Subjacentes encontrados:", length(servicos_subj_real), "itens específicos"))
print(paste("Serviços Intensivos Trabalho encontrados:", length(servicos_trab_real), "itens específicos"))

print("\nComparação:")
print("Itens únicos em Serviços Subjacentes:")
print(paste("Total:", length(setdiff(servicos_subj_real, servicos_trab_real))))
print("Itens únicos em Serviços Intensivos em Trabalho:")
print(setdiff(servicos_trab_real, servicos_subj_real))
print("Itens comuns aos dois núcleos:")
print(intersect(servicos_subj_real, servicos_trab_real))

# Verificando se os vetores são realmente diferentes
print(paste("Os vetores são idênticos?", identical(servicos_subj_real, servicos_trab_real)))


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
print("\n=== CALCULANDO NÚCLEOS DE INFLAÇÃO ===")
print("Serviços Subjacentes: baseado em códigos específicos fornecidos pelo usuário")
print("Serviços Intensivos em Trabalho: baseado em códigos específicos fornecidos pelo usuário")
print("Industriais Subjacentes: baseado em busca por palavras-chave")

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
tail(IPCASGS)

# Transpondo a tabela: transformando datas em colunas e itens em linhas
IPCASGS_transposta <- IPCASGS %>%
  # Convertendo ref.date para o mesmo formato das datas do SIDRA
  mutate(ref.date = as.character(ref.date)) %>%
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
if (length(pos_servicos) > 0) {
  # Extraindo os dois núcleos de serviços
  nucleos_servicos <- nucleos_calculados[1:2, ] # Primeiras 2 linhas são os núcleos de serviços

  # Inserindo após a posição de SERVICOS
  IPCASGS_com_nucleos <- bind_rows(
    IPCASGS_com_nucleos[1:pos_servicos, ], # Linhas até SERVICOS (inclusive)
    nucleos_servicos, # Núcleos de serviços
    IPCASGS_com_nucleos[(pos_servicos + 1):nrow(IPCASGS_com_nucleos), ] # Resto das linhas
  )
}

# Recalculando a posição de INDUSTRIAIS após inserção
pos_industriais_nova <- which(IPCASGS_com_nucleos$item == "INDUSTRIAIS")
print(paste("Nova posição INDUSTRIAIS:", pos_industriais_nova))

# Inserindo núcleo industriais após INDUSTRIAIS
if (length(pos_industriais_nova) > 0) {
  # Extraindo o núcleo industrial
  nucleo_industrial <- nucleos_calculados[3, ] # Terceira linha é o núcleo industrial

  # Inserindo após a posição de INDUSTRIAIS
  IPCASGS_com_nucleos <- bind_rows(
    IPCASGS_com_nucleos[1:pos_industriais_nova, ], # Linhas até INDUSTRIAIS (inclusive)
    nucleo_industrial, # Núcleo industrial
    IPCASGS_com_nucleos[(pos_industriais_nova + 1):nrow(IPCASGS_com_nucleos), ] # Resto das linhas
  )
}

View(IPCASGS_com_nucleos)
print("=== TABELA SGS COM NÚCLEOS INSERIDOS ===")
print(IPCASGS_com_nucleos$item)

# ====================== Criando tabela consolidada ========================

# Mesclando IPCA_Variacoes_SIDRA com IPCA_SGS (incluindo núcleos)
# IPCA_SGS começará imediatamente abaixo da última linha de IPCA_Variacoes_SIDRA

tabela_consolidada <- bind_rows(
  ipca_variacoes_transposta, # Dados do SIDRA primeiro
  IPCASGS_com_nucleos # Dados do SGS com núcleos em seguida
)

View(tabela_consolidada)
print("=== TABELA CONSOLIDADA CRIADA ===")
print(paste("Total de itens na tabela consolidada:", nrow(tabela_consolidada)))
print("Primeiros 10 itens:")
print(head(tabela_consolidada$item, 10))
print("Últimos 10 itens:")
print(tail(tabela_consolidada$item, 10))

# ====================== Criando números-índices (base jan-22 = 100) ========================

print("\n=== CRIANDO NÚMEROS-ÍNDICES ===")

# Função para calcular números-índices
calcular_numeros_indices <- function(tabela_dados, base_data = "2022-01-01") {
  # Criando cópia da tabela
  tabela_indices <- tabela_dados

  # Verificando as colunas de data disponíveis
  colunas_data <- names(tabela_dados)[-1] # Excluindo coluna 'item'
  print(paste("Primeiras 5 colunas de data:", paste(colunas_data[1:5], collapse = ", ")))

  # Procurando pela data base exata ou a mais próxima de janeiro 2022
  if (base_data %in% colunas_data) {
    data_base_final <- base_data
    print(paste("Data base encontrada:", data_base_final))
  } else {
    # Procurar por colunas que comecem com "2022-01"
    jan_2022_cols <- colunas_data[grepl("^2022-01", colunas_data)]
    if (length(jan_2022_cols) > 0) {
      data_base_final <- jan_2022_cols[1]
      print(paste("Usando data base mais próxima:", data_base_final))
    } else {
      # Usar a primeira coluna disponível
      data_base_final <- colunas_data[1]
      print(paste("AVISO: Janeiro 2022 não encontrado. Usando primeira data:", data_base_final))
    }
  }

  # Para cada linha (item), calcular o índice acumulado
  for (i in 1:nrow(tabela_indices)) {
    # Extraindo apenas as colunas de datas (excluindo 'item')
    variacoes <- as.numeric(tabela_dados[i, -1])
    names(variacoes) <- colunas_data

    # Encontrando a posição da data base
    base_pos <- which(names(variacoes) == data_base_final)

    if (length(base_pos) == 0) {
      base_pos <- 1
    }

    # Calculando índices acumulados
    indices <- numeric(length(variacoes))
    indices[base_pos] <- 100 # Base = 100

    # Calculando para frente (após a base)
    if (base_pos < length(variacoes)) {
      for (j in (base_pos + 1):length(variacoes)) {
        if (!is.na(variacoes[j]) && !is.na(indices[j - 1])) {
          indices[j] <- indices[j - 1] * (1 + variacoes[j] / 100)
        } else {
          indices[j] <- NA
        }
      }
    }

    # Calculando para trás (antes da base)
    if (base_pos > 1) {
      for (j in (base_pos - 1):1) {
        if (!is.na(variacoes[j + 1]) && !is.na(indices[j + 1])) {
          indices[j] <- indices[j + 1] / (1 + variacoes[j + 1] / 100)
        } else {
          indices[j] <- NA
        }
      }
    }

    # Substituindo na tabela (arredondando para 2 casas decimais)
    for (k in 1:length(indices)) {
      tabela_indices[i, k + 1] <- round(indices[k], 2) # k+1 porque primeira coluna é 'item'
    }
  }

  return(tabela_indices)
}

# Calculando números-índices para a tabela consolidada
tabela_consolidada_indices <- calcular_numeros_indices(tabela_consolidada, "2022-01-01")

# Tratamento especial para DIFUSÃO - copiar os valores originais
pos_difusao <- which(tabela_consolidada_indices$item == "DIFUSAO")
if (length(pos_difusao) > 0) {
  print("Mantendo valores originais para DIFUSÃO")
  tabela_consolidada_indices[pos_difusao, -1] <- tabela_consolidada[pos_difusao, -1]
}

View(tabela_consolidada_indices)
print("=== NÚMEROS-ÍNDICES CALCULADOS (Base: Jan-2022 = 100) ===")

# ====================== Exportação completa incluindo números-índices ========================

# Exportando para Excel
timestamp <- format(Sys.time(), "%Y%m%d")
write_xlsx(
  list(
    "IPCA_Consolidado" = tabela_consolidada,
    "IPCA_Indices" = tabela_consolidada_indices,
    "IPCA_Variacoes_SIDRA" = ipca_variacoes_transposta,
    "IPCA_Pesos_SIDRA" = ipca_pesos_transposta,
    "IPCA_SGS_com_Nucleos" = IPCASGS_com_nucleos,
    "Nucleos_Calculados" = nucleos_calculados
  ),
  path = paste0("IPCAs_", timestamp, ".xlsx")
)

print(paste("Arquivo exportado:", paste0("IPCAs_", timestamp, ".xlsx")))
print("Planilhas incluídas:")
print("- IPCA_Consolidado: Tabela única com SIDRA + SGS + núcleos inseridos (variações mensais)")
print("- IPCA_Indices: Números-índices com base Jan-2022 = 100")
print("- IPCA_Variacoes_SIDRA: Variações mensais de todos os itens (SIDRA)")
print("- IPCA_Pesos_SIDRA: Pesos mensais de todos os itens (SIDRA)")
print("- IPCA_SGS_com_Nucleos: Séries do SGS/BCB com núcleos inseridos")
print("- Nucleos_Calculados: Núcleos de inflação calculados separadamente")

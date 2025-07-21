import pyettj
import pandas as pd
import matplotlib.pyplot as plt
from datetime import datetime

# Datas de interesse no formato correto (DD/MM/AAAA)
datas = ["30/06/2025", "11/07/2025", "18/07/2025"]  # Voltando para as datas originais

# Criando um dicionário para armazenar as curvas de juros reais
curvas = {}

# Coletando dados para cada data
for data in datas:
    try:
        curva = pyettj.get_ettj(data)  # Obtém a curva de juros
        print(f"Dados retornados para {data}:\n", curva.head())  # Exibir as primeiras linhas para depuração
        print(f"Colunas disponíveis: {curva.columns.tolist()}")  # Verificar os nomes das colunas
        curvas[data] = curva
    except Exception as e:
        print(f"Erro ao buscar dados para {data}: {e}")

# Verificar se alguma curva foi coletada antes de tentar plotar
if not curvas:
    print("Nenhuma curva foi carregada. Verifique os dados do pyettj.")
else:
    # Criando o gráfico com fundo transparente
    plt.figure(figsize=(10, 6))  # Removido facecolor para transparência
    ax = plt.gca()  # Obtém os eixos atuais
    
    # Definir cores personalizadas
    cores = ['black', 'turquoise', '#189CD8']  # Preta, verde-água, azul hex
    
    for i, (data, curva) in enumerate(curvas.items()):
        prazo_col = "Dias Corridos"
        taxa_col = "DI x pré 252"

        if prazo_col in curva.columns and taxa_col in curva.columns:
            # Filtrar até o vértice 2520 dias e converter para anos
            curva_filtrada = curva[curva[prazo_col] <= 2520].copy()
            curva_filtrada['prazo_anos'] = curva_filtrada[prazo_col] / 365  # Converter dias para anos
            
            # Converter a data de extração para datetime e calcular anos futuros
            data_extracao = datetime.strptime(data, "%d/%m/%Y")
            ano_base = data_extracao.year
            curva_filtrada['ano_futuro'] = ano_base + curva_filtrada['prazo_anos']

            plt.plot(curva_filtrada['ano_futuro'], curva_filtrada[taxa_col], 
                    label=data, color=cores[i])
        else:
            print(f"Colunas esperadas não encontradas para {data}")

    # Configuração do gráfico
    # plt.title("Curva de Juros")  # Título removido
    plt.legend()
    plt.grid(False)  # Remove as linhas de grade

    # Salvar como PNG com fundo transparente
    plt.savefig(r'C:\Users\GabrielHenriqueMarti\Desktop\ETTJ\ettj_curva.png', dpi=300, bbox_inches='tight', transparent=True)
    print("Gráfico salvo como 'C:\\Users\\GabrielHenriqueMarti\\Desktop\\ETTJ\\ettj_curva.png'")
    
    # Exibir gráfico
    plt.show()
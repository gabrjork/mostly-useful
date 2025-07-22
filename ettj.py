import pyettj
import pandas as pd
import matplotlib.pyplot as plt
from datetime import datetime
import matplotlib.font_manager as fm

# Configurar a fonte Plus Jakarta Sans
font_path = "[font path here]"
prop = fm.FontProperties(fname=font_path)
plt.rcParams['font.family'] = prop.get_name()

# Datas de interesse no formato correto (DD/MM/AAAA)
datas = ["30/06/2025", "11/07/2025", "21/07/2025"]  # Voltando para as datas originais

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
    
    # Configurar cores dos eixos e textos
    ax.spines['bottom'].set_color('#595959')  # Cor do eixo X
    ax.spines['left'].set_color('#595959')    # Cor do eixo Y
    ax.spines['top'].set_color('none')        # Remove eixo superior
    ax.spines['right'].set_color('none')      # Remove eixo direito
    ax.tick_params(colors='#595959')          # Cor dos números dos eixos
    ax.xaxis.label.set_color('#595959')       # Cor do rótulo do eixo X
    ax.yaxis.label.set_color('#595959')       # Cor do rótulo do eixo Y

    # Salvar como PNG com fundo transparente
    plt.savefig(r'[path here]\ettj_curva.png', dpi=300, bbox_inches='tight', transparent=True)
    print("Gráfico salvo como '[path here]\ettj_curva.png'")
    
    # Exibir gráfico
    plt.show()

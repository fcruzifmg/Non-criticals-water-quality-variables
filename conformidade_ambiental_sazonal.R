#Extraindo nùmero de VSRMF por período sazonal
#Período seco
library(readxl)
base_red_periodo_seco = read_excel(path = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Entropia/Resultados/Tabelas/Redundancia_var_PMQQS_periodo_seco_LDA_unico_sem_outliers_ingles.xlsx")

#Carregando base com identificação das sub-regiões
base_id_sub_regioes = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Base dados/Regioes_QA_PMQQS_2017_2020_LDA_unico_sem_outliers.xlsx")
colnames(base_id_sub_regioes)=c("Nome completo Estações PMQQS", "Estações PMQQS", "Sub-regiões")

#Juntando as base dados com join
library(dplyr)
base_red_periodo_seco_comp = full_join(x = base_id_sub_regioes, by = "Estações PMQQS", y = base_red_periodo_seco)

#Eliminando atributos desnecessários
base_red_periodo_seco_comp = base_red_periodo_seco_comp[,-c(1:2)]

#Splitando base dados do período seco
base_red_periodo_seco_comp_list = split(x = base_red_periodo_seco_comp, f = base_red_periodo_seco_comp$`Sub-regiões`)

#Eliminando informação sobre a sub-região nos objetos
base_red_periodo_seco_comp_list
for(i in 1:length(base_red_periodo_seco_comp_list)){
  base_red_periodo_seco_comp_list[[i]]=base_red_periodo_seco_comp_list[[i]][,-c(1)]
}

#Criando lista para receber a ID das variáveis VSRMF por sub-região
variaveis_licv_sub_regiao_pmqqs_seco = vector("list", 14)
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs_seco)){
  variaveis_licv_sub_regiao_pmqqs_seco[[i]]=matrix(data = 0, nrow = 1, ncol = ncol(base_red_periodo_seco_comp_list[[i]]))
  variaveis_licv_sub_regiao_pmqqs_seco[[i]]=data.frame(variaveis_licv_sub_regiao_pmqqs_seco[[i]])
}

names(variaveis_licv_sub_regiao_pmqqs_seco)=names(base_red_periodo_seco_comp_list)

variaveis_licv_sub_regiao_pmqqs_seco
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs_seco)){
  for(j in 1:nrow(variaveis_licv_sub_regiao_pmqqs_seco[[i]])){
    for(k in 1:ncol(variaveis_licv_sub_regiao_pmqqs_seco[[i]])){
      if(all(base_red_periodo_seco_comp_list[[i]][k]>79.99)==TRUE){
        variaveis_licv_sub_regiao_pmqqs_seco[[i]][k]=colnames(base_red_periodo_seco_comp_list[[i]][k])
      } else {
        variaveis_licv_sub_regiao_pmqqs_seco[[i]][k]=NA
      }
    }
  }
}

#Eliminando colunas com valor NA
variaveis_licv_sub_regiao_pmqqs_seco
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs_seco)){
  variaveis_licv_sub_regiao_pmqqs_seco[[i]]=data.frame(t(variaveis_licv_sub_regiao_pmqqs_seco[[i]]))
  variaveis_licv_sub_regiao_pmqqs_seco[[i]]=na.omit(variaveis_licv_sub_regiao_pmqqs_seco[[i]])
}

variaveis_licv_sub_regiao_pmqqs_seco
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs_seco)){
  colnames(variaveis_licv_sub_regiao_pmqqs_seco[[i]])=c("var")
}

#Período chuvoso
library(readxl)
base_red_periodo_chuvoso = read_excel(path = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Entropia/Resultados/Tabelas/Redundancia_var_PMQQS_periodo_chuvoso_LDA_unico_sem_outliers_ingles.xlsx")

#Gerando gráfico VSRMF por período sazonal
#Carregando base com identificação das sub-regiões
base_id_sub_regioes = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Base dados/Regioes_QA_PMQQS_2017_2020_LDA_unico_sem_outliers.xlsx")
colnames(base_id_sub_regioes)=c("Nome completo Estações PMQQS", "Estações PMQQS", "Sub-regiões")

#Juntando as base dados com join
library(dplyr)
base_red_periodo_chuvoso_comp = full_join(x = base_id_sub_regioes, by = "Estações PMQQS", y = base_red_periodo_chuvoso)

#Eliminando atributos desnecessários
base_red_periodo_chuvoso_comp = base_red_periodo_chuvoso_comp[,-c(1:2)]

#Splitando base dados do período chuvoso
base_red_periodo_chuvoso_comp_list = split(x = base_red_periodo_chuvoso_comp, f = base_red_periodo_chuvoso_comp$`Sub-regiões`)

#Eliminando informação sobre a sub-região nos objetos
base_red_periodo_chuvoso_comp_list
for(i in 1:length(base_red_periodo_chuvoso_comp_list)){
  base_red_periodo_chuvoso_comp_list[[i]]=base_red_periodo_chuvoso_comp_list[[i]][,-c(1)]
}

#Criando lista para receber a ID das variáveis VSRMF por sub-região
variaveis_licv_sub_regiao_pmqqs_chuvoso = vector("list", 14)
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs_chuvoso)){
  variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]]=matrix(data = 0, nrow = 1, ncol = ncol(base_red_periodo_chuvoso_comp_list[[i]]))
  variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]]=data.frame(variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]])
}

names(variaveis_licv_sub_regiao_pmqqs_chuvoso)=names(base_red_periodo_chuvoso_comp_list)

variaveis_licv_sub_regiao_pmqqs_chuvoso
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs_chuvoso)){
  for(j in 1:nrow(variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]])){
    for(k in 1:ncol(variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]])){
      if(all(base_red_periodo_chuvoso_comp_list[[i]][k]>79.99)==TRUE){
        variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]][k]=colnames(base_red_periodo_chuvoso_comp_list[[i]][k])
      } else {
        variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]][k]=NA
      }
    }
  }
}

#Eliminando colunas com valor NA
variaveis_licv_sub_regiao_pmqqs_chuvoso
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs_chuvoso)){
  variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]]=data.frame(t(variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]]))
  variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]]=na.omit(variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]])
}

names(variaveis_licv_sub_regiao_pmqqs_chuvoso)=names(variaveis_licv_sub_regiao_pmqqs_seco)


variaveis_licv_sub_regiao_pmqqs_chuvoso
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs_chuvoso)){
  colnames(variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]])=c("var")
}

#Carregando base dados PMQQS
#Mantive o nome outliers "base_outliers", mas a análise na verdade deve ser feita para toda a base relacionada as variáveis propostas a descontinuar
library(readxl)
base_outliers = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Dados/Bases ajustadas ingles/base_PMQQS_2017_2020_ajustada_analise_LDA_unico_ingles.xlsx")
base_outliers = as.data.frame(base_outliers)

#Gerando informação de mês e período sazonal aos dados
library(lubridate)
base_outliers$DataAmostra = dmy(base_outliers$DataAmostra)

base_outliers = mutate(.data = base_outliers, Mes = month(base_outliers$DataAmostra))
base_outliers = mutate(.data = base_outliers, "Período sazonal" = "")

base_outliers
for(i in 1:nrow(base_outliers)){
  for(j in 1:ncol(base_outliers)){
    if (base_outliers[i,81]==10|base_outliers[i,81]==11|base_outliers[i,81]==12|base_outliers[i,81]==1|base_outliers[i,81]==2|base_outliers[i,81]==3){
      base_outliers[i,82]="Wet season"
    } else if (base_outliers[i,81]==4|base_outliers[i,81]==5|base_outliers[i,81]==6|base_outliers[i,81]==7|base_outliers[i,81]==8|base_outliers[i,81]==9){
      base_outliers[i,82]="Dry season"
    }
  }
}

#Carregando base relativa as sub-regiões
base_regioes = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Base dados/Regioes_QA_PMQQS_2017_2020_LDA_unico_sem_outliers.xlsx")
base_regioes = as.data.frame(base_regioes)

#Imputando informação refetente as sub-regiões na base dados
base_outliers = mutate(.data = base_outliers, "Subregião"="")

base_outliers$Subregião = plyr::mapvalues(x = base_outliers$CodigoDoPonto, from = base_regioes$Código, to = base_regioes$`Região Hidrológica`)

#Splitando a base dados pela sazonalidade
base_outliers_sazonal = split(x = base_outliers, f = base_outliers$`Período sazonal`)

#Splitando base sazonal por sub-região
base_outliers_sazonal
for(k in 1:length(base_outliers_sazonal)){
  base_outliers_sazonal[[k]]=split(x = base_outliers_sazonal[[k]], f = base_outliers_sazonal[[k]][,83])
}

#Extraindo base apenas das LICV por sub-região
base_lista_licv_sub = list(variaveis_licv_sub_regiao_pmqqs_seco, variaveis_licv_sub_regiao_pmqqs_chuvoso)
names(base_lista_licv_sub)=c("Dry season","Wet season")

base_lista_licv_sub
for(k in 1:length(base_lista_licv_sub)){
  for(j in 1:length(base_lista_licv_sub[[k]])){
    base_lista_licv_sub[[k]][[j]]=do.call(rbind, base_lista_licv_sub[[k]][[j]])
    base_lista_licv_sub[[k]][[j]]=c(base_lista_licv_sub[[k]][[j]])
  }
}

base_dados_licv_sazonal = vector(mode = "list", length = length(base_lista_licv_sub))
for(k in 1:length(base_dados_licv_sazonal)){
  base_dados_licv_sazonal[[k]]=vector(mode = "list", length = length(base_lista_licv_sub[[k]]))
}

base_dados_licv_sazonal
for(k in 1:length(base_dados_licv_sazonal)){
  for(j in 1:length(base_dados_licv_sazonal[[k]])){
    base_dados_licv_sazonal[[k]][[j]]=select(.data = base_outliers_sazonal[[k]][[j]], base_lista_licv_sub[[k]][[j]])
  }
}

#Identificando lista com base selecionada
names(base_dados_licv_sazonal)=names(base_outliers_sazonal)

base_dados_licv_sazonal
for(k in 1:length(base_dados_licv_sazonal)){
  names(base_dados_licv_sazonal[[k]])=LETTERS[1:14]
}

#Gerando tabela de todas as LICV
nomes_todas_licv = vector(mode = "list", length = 2)
for(k in 1:length(nomes_todas_licv)){
  nomes_todas_licv[[k]]=vector(mode = "list", length = length(base_lista_licv_sub[[k]]))
}

nomes_todas_licv
for(k in 1:length(nomes_todas_licv)){
  for(j in 1:length(nomes_todas_licv[[k]])){
    nomes_todas_licv[[k]][[j]]=as.data.frame(base_lista_licv_sub[[k]][[j]])
  }
}

nomes_todas_licv
for(k in 1:length(nomes_todas_licv)){
  nomes_todas_licv[[k]]=do.call(rbind, nomes_todas_licv[[k]])
}

nomes_todas_licv_df = do.call(rbind, nomes_todas_licv)

nomes_todas_licv_df = unique(nomes_todas_licv_df[,1])

nomes_todas_licv_df = as.data.frame(nomes_todas_licv_df)

#Expoprtar lista com nomes de todas as LICV sazonais
nomes_licv_sazonais = write.xlsx(x = nomes_todas_licv_df, file = "D:/UFMG/OneDrive/Área de Trabalho/nomes_licv_sazonais.xlsx")

#Carregando base com os limites admissíveis do CONAMA
base_conama = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Análise conformidade ambiental sazonal/limites_qa_conama_licv_sazonais.xlsx")
base_conama = as.data.frame(base_conama)
base_conama$limite=as.numeric(base_conama$limite)

##Extraindo novamente apenas o nome das LICVS
nomes_licv_sazonais_vetor = vector(mode = "list", length = length(base_dados_licv_sazonal))
for(k in 1:length(nomes_licv_sazonais_vetor)){
  nomes_licv_sazonais_vetor[[k]]=vector(mode = "list", length = length(base_dados_licv_sazonal[[k]]))
}

names(nomes_licv_sazonais_vetor)=names(base_dados_licv_sazonal)

nomes_licv_sazonais_vetor
for(k in 1:length(nomes_licv_sazonais_vetor)){
  names(nomes_licv_sazonais_vetor[[k]])=LETTERS[1:14]
}

nomes_licv_sazonais_vetor
for(k in 1:length(nomes_licv_sazonais_vetor)){
  for(j in 1:length(nomes_licv_sazonais_vetor[[k]])){
    nomes_licv_sazonais_vetor[[k]][[j]]=colnames(base_dados_licv_sazonal[[k]][[j]])
  }
}

#Transpondo e formatando base do conama
base_conama_t = as.data.frame(t(base_conama))
base_conama_t=base_conama_t[-c(3),]
colnames(base_conama_t)=base_conama_t[c(1),]
base_conama_t=base_conama_t[-c(1),]

#Eliminando NA em bases
base_dados_licv_sazonal
for(k in 1:length(base_dados_licv_sazonal)){
  for(j in 1:length(base_dados_licv_sazonal[[k]])){
    base_dados_licv_sazonal[[k]][[j]]=na.omit(base_dados_licv_sazonal[[k]][[j]])
  }
}

#Gerando base para receber os limites do conama de cada objeto da lista
base_conama_lista = vector(mode = "list", length = length(base_dados_licv_sazonal))
for(k in 1:length(base_conama_lista)){
  base_conama_lista[[k]]=vector(mode = "list", length = length(base_dados_licv_sazonal[[k]]))
}

names(base_conama_lista)=names(base_dados_licv_sazonal)

base_conama_lista
for(k in 1:length(base_conama_lista)){
  names(base_conama_lista[[k]])=LETTERS[1:14]
}

#Extraindo bases específicas do conama de cada objeto
base_conama_lista
for(k in 1:length(base_conama_lista)){
  for(j in 1:length(base_conama_lista[[k]])){
    base_conama_lista[[k]][[j]]=select(.data = base_conama_t, nomes_licv_sazonais_vetor[[k]][[j]])
  }
}

#Criando base para receber resultado do teste lógico comparando as bases de cada variável em cada sub-região com os limites admissíveis dados pelo conama
base_teste_conformidade_normas = vector(mode = "list", length = length(base_dados_licv_sazonal))
for(k in 1:length(base_teste_conformidade_normas)){
  base_teste_conformidade_normas[[k]]=vector(mode = "list", length = length(base_dados_licv_sazonal[[k]]))
}

names(base_teste_conformidade_normas)=names(base_dados_licv_sazonal)

base_teste_conformidade_normas
for(k in 1:length(base_teste_conformidade_normas)){
  names(base_teste_conformidade_normas[[k]])=LETTERS[1:14]
}

base_teste_conformidade_normas
for(k in 1:length(base_teste_conformidade_normas)){
  for(j in 1:length(base_teste_conformidade_normas[[k]])){
    base_teste_conformidade_normas[[k]][[j]]=as.data.frame(matrix(data = 0, nrow = 1, ncol = ncol(base_conama_lista[[k]][[j]])))
  }
}

base_teste_conformidade_normas
for(k in 1:length(base_teste_conformidade_normas)){
  for(j in 1:length(base_teste_conformidade_normas[[k]])){
    colnames(base_teste_conformidade_normas[[k]][[j]])=colnames(base_conama_lista[[k]][[j]])
  }
}

#Fazendo teste lógico
base_teste_conformidade_normas
for(k in 1:length(base_teste_conformidade_normas)){
  for(j in 1:length(base_teste_conformidade_normas[[k]])){
    for(m in 1:ncol(base_teste_conformidade_normas[[k]][[j]])){
      if(all(base_dados_licv_sazonal[[k]][[j]][,m]>base_conama_lista[[k]][[j]][1,m])==TRUE){
        base_teste_conformidade_normas[[k]][[j]][1,m]=1
      } else if (all(base_dados_licv_sazonal[[k]][[j]][,m]>base_conama_lista[[k]][[j]][,m])==FALSE){
        base_teste_conformidade_normas[[k]][[j]][1,m]=0
      }
    }
  }
}

#Base com número de variáveis não-críticas em não-conformidade com as normas ambientais
base_numero_var_nao_criticas_nao_conformes_normas = vector(mode = "list", length = length(base_teste_conformidade_normas))
for(k in 1:length(base_numero_var_nao_criticas_nao_conformes_normas)){
  base_numero_var_nao_criticas_nao_conformes_normas[[k]]=vector(mode = "list", length = length(base_teste_conformidade_normas[[k]]))
}

names(base_numero_var_nao_criticas_nao_conformes_normas)=names(base_lista_licv_sub)

base_numero_var_nao_criticas_nao_conformes_normas
for(k in 1:length(base_numero_var_nao_criticas_nao_conformes_normas)){
  names(base_numero_var_nao_criticas_nao_conformes_normas[[k]])=LETTERS[1:14]
}

#Calculando número de variáveis não-críticas em não-conformidade com as normas ambientais
base_numero_var_nao_criticas_nao_conformes_normas
for(k in 1:length(base_numero_var_nao_criticas_nao_conformes_normas)){
  for(j in 1:length(base_numero_var_nao_criticas_nao_conformes_normas[[k]])){
    base_numero_var_nao_criticas_nao_conformes_normas[[k]][[j]]=apply(X = base_teste_conformidade_normas[[k]][[j]], MARGIN = 1, FUN = sum)
  }
}

#Gerando base com número de variáveis não-críticas sazonais
base_numero_var_nao_criticas_sazonais = vector(mode = "list", length = length(base_dados_licv_sazonal))
for(k in 1:length(base_numero_var_nao_criticas_sazonais)){
  base_numero_var_nao_criticas_sazonais[[k]]=vector(mode = "list", length = length(base_teste_conformidade_normas[[k]]))
}

names(base_numero_var_nao_criticas_sazonais)=names(base_dados_licv_sazonal)

base_numero_var_nao_criticas_sazonais
for(k in 1:length(base_numero_var_nao_criticas_sazonais)){
  names(base_numero_var_nao_criticas_sazonais[[k]])=LETTERS[1:14]
}

base_numero_var_nao_criticas_sazonais
for(k in 1:length(base_numero_var_nao_criticas_sazonais)){
  for(j in 1:length(base_numero_var_nao_criticas_sazonais[[k]])){
    base_numero_var_nao_criticas_sazonais[[k]][[j]]=ncol(base_dados_licv_sazonal[[k]][[j]])
  }
}

#Gerando base para receber o resultado final da análise de variáveis não-críticas sazonal
base_numero_var_nao_criticas_sazonais_final = vector(mode = "list", length = length(base_numero_var_nao_criticas_sazonais))
for(k in 1:length(base_numero_var_nao_criticas_sazonais_final)){
  base_numero_var_nao_criticas_sazonais_final[[k]]=vector(mode = "list", length = length(base_numero_var_nao_criticas_sazonais[[k]]))
}

names(base_numero_var_nao_criticas_sazonais_final)=names(base_numero_var_nao_criticas_sazonais)

base_numero_var_nao_criticas_sazonais_final
for(k in 1:length(base_numero_var_nao_criticas_sazonais_final)){
  names(base_numero_var_nao_criticas_sazonais_final[[k]])=LETTERS[1:14]
}

#Criando dataframe para receber resultados
base_numero_var_nao_criticas_sazonais_final
for(k in 1:length(base_numero_var_nao_criticas_sazonais_final)){
  for(j in 1:length(base_numero_var_nao_criticas_sazonais_final[[k]])){
    base_numero_var_nao_criticas_sazonais_final[[k]][[j]]=as.data.frame(matrix(data = 0, nrow = 1, ncol = 3))
    colnames(base_numero_var_nao_criticas_sazonais_final[[k]][[j]])=c("n", "Subregion", "Seasonal period")
  }
}

#Gerando lista com resultados
base_numero_var_nao_criticas_sazonais_final
for(k in 1:length(base_numero_var_nao_criticas_sazonais_final)){
  for(j in 1:length(base_numero_var_nao_criticas_sazonais_final[[k]])){
    base_numero_var_nao_criticas_sazonais_final[[k]][[j]][1,1]=base_numero_var_nao_criticas_sazonais[[k]][[j]]-base_numero_var_nao_criticas_nao_conformes_normas[[k]][[j]]
    base_numero_var_nao_criticas_sazonais_final[[k]][[j]][1,2]=names(base_numero_var_nao_criticas_nao_conformes_normas[[k]][j])
    base_numero_var_nao_criticas_sazonais_final[[k]][[j]][1,3]= names(base_numero_var_nao_criticas_nao_conformes_normas[k])
  }
}

#Integrando bases
base_numero_var_nao_criticas_sazonais_final
for(k in 1:length(base_numero_var_nao_criticas_sazonais_final)){
  base_numero_var_nao_criticas_sazonais_final[[k]]=do.call(rbind, base_numero_var_nao_criticas_sazonais_final[[k]])
}

base_numero_var_nao_criticas_sazonais_final_df = do.call(rbind, base_numero_var_nao_criticas_sazonais_final)

#Exportando tabela com resultados
base_numero_var_nao_criticas_sazonais_final_df_export = write.xlsx(x = base_numero_var_nao_criticas_sazonais_final_df, file = "D:/UFMG/OneDrive/Área de Trabalho/umero_var_nao_criticas_sazonais_final.xlsx")

#Gerando gráfico
library(ggplot2)
barplot_licv_sub_regiao_pmqqs_sazonal = ggplot(data = base_numero_var_nao_criticas_sazonais_final_df, mapping = aes(x = Subregion, 
                                                                                                               y = n, fill = `Seasonal period`))+ geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.9)) +
  scale_fill_discrete(type = c("firebrick2", "deepskyblue1"))+
  theme_bw()+
  xlab("Water quality - sub-regions") + ylab("Number of non-critical water quality variables") +
  geom_text(aes(label = n, fontface=4), position = position_dodge(0.9), vjust = -0.5)+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))




plot(barplot_licv_sub_regiao_pmqqs_sazonal)

#Gráfico Tese
library(ggplot2)
barplot_licv_sub_regiao_pmqqs_sazonal = ggplot(data = base_numero_var_nao_criticas_sazonais_final_df, mapping = aes(x = Subregion, 
                                                                                                                    y = n, fill = `Seasonal period`))+ geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.9)) +
  labs(fill = "Período sazonal") +
  scale_fill_discrete(type = c("firebrick2", "deepskyblue1"), labels = c("Estação seca", "Estação chuvosa"))+
  theme_bw()+
  xlab("Sub-regiões de qualidade da água") + ylab("N° de variáveis de qualidade da água não-críticas") +
  geom_text(aes(label = n, fontface=4), position = position_dodge(0.9), vjust = -0.5)+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))




plot(barplot_licv_sub_regiao_pmqqs_sazonal)


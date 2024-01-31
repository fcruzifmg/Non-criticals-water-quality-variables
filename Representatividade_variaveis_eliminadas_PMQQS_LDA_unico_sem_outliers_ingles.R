#Carregando base dados sobre a redundância referente ao período seco
library(readxl)
base_red_periodo_seco = read_excel(path = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Entropia/Resultados/Tabelas/Redundancia_var_PMQQS_periodo_seco_LDA_unico_sem_outliers_ingles.xlsx")

#Carregando base com identificação das sub-regiões
base_id_sub_regioes = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Base dados/Regioes_QA_PMQQS_2017_2020_LDA_unico_sem_outliers.xlsx")
colnames(base_id_sub_regioes)=c("Nome completo Estações PMQQS", "Estações PMQQS", "Sub-regiões")

#Juntando as base dados com join
library(dplyr)
base_red_periodo_seco_comp = full_join(x = base_id_sub_regioes, y = base_red_periodo_seco, by = "Estações PMQQS")

#Ajustando a posição das colunas e eliminando atributos desnecessários
base_red_periodo_seco_comp = base_red_periodo_seco_comp[,-c(1:2)]

#Carregando base dados sobre a redundância referente ao período chuvoso
library(readxl)
base_red_periodo_chuvoso = read_excel(path = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Entropia/Resultados/Tabelas/Redundancia_var_PMQQS_periodo_chuvoso_LDA_unico_sem_outliers_ingles.xlsx")

#Carregando base com identificação das sub-regiões
base_id_sub_regioes = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Base dados/Regioes_QA_PMQQS_2017_2020_LDA_unico_sem_outliers.xlsx")
colnames(base_id_sub_regioes)=c("Nome completo Estações PMQQS", "Estações PMQQS", "Sub-regiões")

#Juntando as base dados com join
library(dplyr)
base_red_periodo_chuvoso_comp = full_join(x = base_id_sub_regioes, y = base_red_periodo_chuvoso, by = "Estações PMQQS")

#Ajustando a posição das colunas e eliminando atributos desnecessários
base_red_periodo_chuvoso_comp = base_red_periodo_chuvoso_comp[,-c(1:2)]

#Gerando base conjunta relativa a redundância de ambos os períodos seco e chuvoso
base_red_seco_chuvoso = rbind(base_red_periodo_chuvoso_comp, base_red_periodo_seco_comp)

#Gerando lista com dados de redundância agrupados por sub-região
base_red_seco_chuvoso_list = split(x = base_red_seco_chuvoso, f = base_red_seco_chuvoso$`Sub-regiões`)

#Eliminando identificação da sub-região no banco de dados de cada sub-região
base_red_seco_chuvoso_list
for(i in 1:length(base_red_seco_chuvoso_list)){
  base_red_seco_chuvoso_list[[i]]=base_red_seco_chuvoso_list[[i]][,-c(1)]
}

#Identificando variáveis com grau de redundância superior a 80% em todas as estações por região por períoodo sazonal
variaveis_licv_sub_regiao_pmqqs = vector("list", 14)
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs)){
  variaveis_licv_sub_regiao_pmqqs[[i]]=matrix(data = 0, nrow = 1, ncol = ncol(base_red_seco_chuvoso_list[[i]]))
  variaveis_licv_sub_regiao_pmqqs[[i]]=data.frame(variaveis_licv_sub_regiao_pmqqs[[i]])
}

names(variaveis_licv_sub_regiao_pmqqs)=names(base_red_seco_chuvoso_list)

variaveis_licv_sub_regiao_pmqqs
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs)){
  for(j in 1:nrow(variaveis_licv_sub_regiao_pmqqs[[i]])){
    for(k in 1:ncol(variaveis_licv_sub_regiao_pmqqs[[i]])){
      if(all(base_red_seco_chuvoso_list[[i]][k]>79.99)==TRUE){
        variaveis_licv_sub_regiao_pmqqs[[i]][k]=colnames(base_red_seco_chuvoso_list[[i]][k])
      } else {
        variaveis_licv_sub_regiao_pmqqs[[i]][k]=NA
      }
    }
  }
}

#Identificando as variáveis
variaveis_licv_sub_regiao_pmqqs
for(k in 1:length(variaveis_licv_sub_regiao_pmqqs)){
  colnames(variaveis_licv_sub_regiao_pmqqs[[k]])=colnames(base_red_seco_chuvoso_list[[k]])
}

#Eliminando colunas com valor NA
variaveis_licv_sub_regiao_pmqqs
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs)){
  variaveis_licv_sub_regiao_pmqqs[[i]]=data.frame(t(variaveis_licv_sub_regiao_pmqqs[[i]]))
  variaveis_licv_sub_regiao_pmqqs[[i]]=na.omit(variaveis_licv_sub_regiao_pmqqs[[i]])
}


variaveis_licv_sub_regiao_pmqqs
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs)){
  colnames(variaveis_licv_sub_regiao_pmqqs[[i]])=c("var")
}

#Exportando resultados
library(openxlsx)
variaveis_licv_sub_regiao_pmqqs_export = write.xlsx(x = variaveis_licv_sub_regiao_pmqqs, file = "D:/UFMG/OneDrive/Área de Trabalho/variaveis_licv_sub_regiao_pmqqs_LDA_unico_sem_outliers_ingles.xlsx")

#Calculando número total de variáveis LICV por sub-região
numero_licv_sub_regiao_pmqqs = vector("list", 14)
for(i in 1:length(numero_licv_sub_regiao_pmqqs)){
  numero_licv_sub_regiao_pmqqs[[i]]=data.frame(matrix(data = 0, nrow = 1, ncol = 2))
  colnames(numero_licv_sub_regiao_pmqqs[[i]])=c("Sub-region PMQQS", "Number of LICV")
}

names(numero_licv_sub_regiao_pmqqs)=LETTERS[1:14]

numero_licv_sub_regiao_pmqqs
for(i in 1:length(numero_licv_sub_regiao_pmqqs)){
  for(j in 1:nrow(numero_licv_sub_regiao_pmqqs[[i]])){
    for(k in 1:ncol(numero_licv_sub_regiao_pmqqs[[i]])){
      numero_licv_sub_regiao_pmqqs[[i]][1,1]=names(numero_licv_sub_regiao_pmqqs[i])
      numero_licv_sub_regiao_pmqqs[[i]][1,2]=nrow(variaveis_licv_sub_regiao_pmqqs[[i]])
    }
  }
}

numero_licv_sub_regiao_pmqqs_df = do.call(rbind, numero_licv_sub_regiao_pmqqs)

#Gerando gráficos do número de LICV por sub-região
library(ggplot2)
barplot_licv_sub_regiao_pmqqs = ggplot(data = numero_licv_sub_regiao_pmqqs_df, mapping = aes(x = numero_licv_sub_regiao_pmqqs_df$`Sub-region PMQQS`, 
                                                                                          y = numero_licv_sub_regiao_pmqqs_df$`Number of LICV`)) + geom_bar(stat = "identity", width = 0.5) +
  xlab("Water quality - sub-regions") + ylab("N° VRMF") +
  geom_text(aes(label=`Number of LICV`), vjust=-0.3, size=4) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))


plot(barplot_licv_sub_regiao_pmqqs)

##Gerando gráfico do número de LICV por sub-região após ajustes pela remoção de variáveis em não-conformidade com as normas ambientais
#Calculando número total de variáveis LICV por sub-região
#Carregando base final de LICV
library(readxl)
variaveis_licv_sub_regiao_pmqqs = vector("list", 14)

variaveis_licv_sub_regiao_pmqqs
for(k in 1:length(variaveis_licv_sub_regiao_pmqqs)){
  variaveis_licv_sub_regiao_pmqqs[[k]] = read_excel(path = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Entropia/Resultados/Tabelas/Variaveis_licv_sub_regiao_pmqqs_LDA_unico_sem_outliers_final.xlsx", 
                                                           sheet = k)
}


numero_licv_sub_regiao_pmqqs = data.frame(matrix(data = 0, nrow = 14, ncol = 2))

colnames(numero_licv_sub_regiao_pmqqs)=c("Water quality subregion", "N° VRMF")

numero_licv_sub_regiao_pmqqs$`Water quality subregion` = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K","L", "M", "N")

numero_licv_sub_regiao_pmqqs
for(i in 1:nrow(numero_licv_sub_regiao_pmqqs)){
  for(j in 1:ncol(numero_licv_sub_regiao_pmqqs)){
    numero_licv_sub_regiao_pmqqs[i,2] = nrow(variaveis_licv_sub_regiao_pmqqs[[i]])
  }
}


#Exportando base com o número de LICV
library(openxlsx)
numero_licv_sub_regiao_pmqqs_export_final = write.xlsx(x = numero_licv_sub_regiao_pmqqs, file = "D:/UFMG/OneDrive/Área de Trabalho/numero_licv_sub_regiao_pmqqs.xlsx")
  
#Gerando gráficos do número de LICV por sub-região
library(ggplot2)
barplot_licv_sub_regiao_pmqqs = ggplot(data = numero_licv_sub_regiao_pmqqs, mapping = aes(x = numero_licv_sub_regiao_pmqqs$`Water quality subregion`, 
                                                                                             y = numero_licv_sub_regiao_pmqqs$`N° VRMF`)) + geom_bar(stat = "identity", width = 0.5) +
  xlab("Water quality subregion") + ylab("Number of non-critical water quality variables") +
  geom_text(aes(label=`N° VRMF`), vjust=-0.3, size=6) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"))


plot(barplot_licv_sub_regiao_pmqqs)

#EGráfico "variáveis não-críticas" por sub-região - Tese
library(ggplot2)
barplot_licv_sub_regiao_pmqqs = ggplot(data = numero_licv_sub_regiao_pmqqs, mapping = aes(x = numero_licv_sub_regiao_pmqqs$`Water quality subregion`, 
                                                                                          y = numero_licv_sub_regiao_pmqqs$`N° VRMF`)) + geom_bar(stat = "identity", width = 0.5) +
  xlab("Sub-regiões de qualidade da água") + ylab("N° de variáveis não-críticas") +
  geom_text(aes(label=`N° VRMF`), vjust=-0.3, size=6) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"))


plot(barplot_licv_sub_regiao_pmqqs)



#-----------------------------------
#-----------------------------------

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

names(variaveis_licv_sub_regiao_pmqqs_seco)=names(variaveis_licv_sub_regiao_pmqqs)


variaveis_licv_sub_regiao_pmqqs_seco
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs_seco)){
  colnames(variaveis_licv_sub_regiao_pmqqs_seco[[i]])=c("var")
}

#Gerando lista para receber número de variáveis LICV/sub-região por período sazonal
numero_licv_sub_regiao_pmqqs_seco = vector("list", 14)
for(i in 1:length(numero_licv_sub_regiao_pmqqs_seco)){
  numero_licv_sub_regiao_pmqqs_seco[[i]] = as.data.frame(matrix(data = 0, nrow = 1, ncol = 3))
  colnames(numero_licv_sub_regiao_pmqqs_seco[[i]]) = c("Sub-region PMQQS", "Number of VSRMF", "Seasonal period")
}

names(numero_licv_sub_regiao_pmqqs_seco)=names(base_red_seco_chuvoso_list)

numero_licv_sub_regiao_pmqqs_seco
for(i in 1:length(numero_licv_sub_regiao_pmqqs_seco)){
  for(j in 1:nrow(numero_licv_sub_regiao_pmqqs_seco[[i]])){
    for(k in 1:ncol(numero_licv_sub_regiao_pmqqs_seco[[i]])){
      numero_licv_sub_regiao_pmqqs_seco[[i]][1,1]=names(numero_licv_sub_regiao_pmqqs_seco[i])
      numero_licv_sub_regiao_pmqqs_seco[[i]][1,2]=nrow(variaveis_licv_sub_regiao_pmqqs_seco[[i]])
      numero_licv_sub_regiao_pmqqs_seco[[i]][1,3]="Dry season"
    }
  }
}

numero_licv_sub_regiao_pmqqs_seco_df = do.call(rbind, numero_licv_sub_regiao_pmqqs_seco)

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

#Gerando lista para receber nÚmero de variáveis VSRMF/sub-região por período sazonal
numero_licv_sub_regiao_pmqqs_chuvoso = vector("list", 14)
for(i in 1:length(numero_licv_sub_regiao_pmqqs_chuvoso)){
  numero_licv_sub_regiao_pmqqs_chuvoso[[i]] = data.frame(matrix(data = 0, nrow = 1, ncol = 3))
  colnames(numero_licv_sub_regiao_pmqqs_chuvoso[[i]]) = c("Sub-region PMQQS", "Number of VSRMF", "Seasonal period")
}

names(numero_licv_sub_regiao_pmqqs_chuvoso)=names(base_red_seco_chuvoso_list)

numero_licv_sub_regiao_pmqqs_chuvoso
for(i in 1:length(numero_licv_sub_regiao_pmqqs_chuvoso)){
  for(j in 1:nrow(numero_licv_sub_regiao_pmqqs_chuvoso[[i]])){
    for(k in 1:ncol(numero_licv_sub_regiao_pmqqs_chuvoso[[i]])){
      numero_licv_sub_regiao_pmqqs_chuvoso[[i]][1,1]=names(numero_licv_sub_regiao_pmqqs_chuvoso[i])
      numero_licv_sub_regiao_pmqqs_chuvoso[[i]][1,2]=nrow(variaveis_licv_sub_regiao_pmqqs_chuvoso[[i]])
      numero_licv_sub_regiao_pmqqs_chuvoso[[i]][1,3]="Wet season"
    }
  }
}

numero_licv_sub_regiao_pmqqs_chuvoso_df = do.call(rbind, numero_licv_sub_regiao_pmqqs_chuvoso)

#Gerando base integrada
numero_licv_sub_regiao_pmqqs_seco_chuvoso = rbind(numero_licv_sub_regiao_pmqqs_seco_df, numero_licv_sub_regiao_pmqqs_chuvoso_df)

#Gerando gráfico de barras da distribuição das VSRMF por período sazonal/sub-região
library(ggplot2)
barplot_licv_sub_regiao_pmqqs_sazonal = ggplot(data = numero_licv_sub_regiao_pmqqs_seco_chuvoso, mapping = aes(x = `Sub-region PMQQS`, 
                                                                                             y = `Number of VSRMF`, fill = `Seasonal period`))+ geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.9)) +
  scale_fill_discrete(type = c("red4", "blue4"))+
  theme_light()+
  xlab("Water quality - sub-regions") + ylab("Number of non-critical water quality variables") +
  geom_text(aes(label = `Number of VSRMF`, fontface=2), position = position_dodge(0.9), vjust = -0.5)+
  theme(axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))




plot(barplot_licv_sub_regiao_pmqqs_sazonal)

#---------------------------
#---------------------------

#Gerando tabela de frequência das variáveis VSRMF
variaveis_licv_sub_regiao_pmqqs
for(i in 1:length(variaveis_licv_sub_regiao_pmqqs)){
  colnames(variaveis_licv_sub_regiao_pmqqs[[i]]) = c("Redundant variables (LICV)")
}

#Gerando tabela com regiões empilhadas para geração da tabela de frequência
variaveis_licv_sub_regiao_pmqqs_df = do.call(rbind, variaveis_licv_sub_regiao_pmqqs)


#Gerando tabela de frequência
freq_var_licv_pmqqs = table(variaveis_licv_sub_regiao_pmqqs_df)
freq_var_licv_pmqqs_df = as.data.frame(freq_var_licv_pmqqs)

#Ajustando nomes dos atributos
library(dplyr)
freq_var_licv_pmqqs_df = arrange(df = freq_var_licv_pmqqs_df, desc(freq_var_licv_pmqqs_df$Freq))

colnames(freq_var_licv_pmqqs_df)=c("VRMF",	"No. sub-regions")

#Exportando_tabela_freq_licv_pmqqs
library(openxlsx)
freq_var_licv_pmqqs_df_export = write.xlsx(x = freq_var_licv_pmqqs_df, file = "D:/UFMG/OneDrive/Área de Trabalho/Freq_variaveis_VSRMF_pmqqs_LDA_unico_sem_outliers_ingles.xlsx")

#Gerando tabela de frequência das variáveis não-críticas corrigidas pelos limites das normas ambientais

freq_var_licv_pmqqs_df_final = do.call(rbind, variaveis_licv_sub_regiao_pmqqs)
freq_var_licv_pmqqs_df_final = table(freq_var_licv_pmqqs_df_final)
freq_var_licv_pmqqs_df_final_df = as.data.frame(freq_var_licv_pmqqs_df_final)
freq_var_licv_pmqqs_df_final_df = arrange(.data = freq_var_licv_pmqqs_df_final_df, desc(freq_var_licv_pmqqs_df_final_df$Freq))

#Exportando tabela de frequência final
tabela_freq_licv_final_export = write.xlsx(x = freq_var_licv_pmqqs_df_final_df, file = "D:/UFMG/OneDrive/Área de Trabalho/Freq_variaveis_VRMF_pmqqs_LDA_unico_sem_outliers_ingles_final.xlsx")


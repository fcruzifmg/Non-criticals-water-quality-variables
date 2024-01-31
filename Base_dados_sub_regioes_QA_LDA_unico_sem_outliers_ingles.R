#Período seco - PMQQS
#Carregando base dados  completa
library(readxl)
base_dados_completa_PMQQS_seco_sem_outliers = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Bases sazonais/Resultados/base_pmqqs_seco_LDA_unico_sem_outliers_ingles.xlsx")

#Eliminando campos desnecessários
base_dados_completa_PMQQS_seco_sem_outliers = base_dados_completa_PMQQS_seco_sem_outliers[,-c(2,3)]

#Criando ID para cada sub-região de QA
library(dplyr)
base_dados_completa_PMQQS_seco_sem_outliers = mutate(.data = base_dados_completa_PMQQS_seco_sem_outliers, "Sub-região" = "")
base_dados_completa_PMQQS_seco_sem_outliers = select(.data = base_dados_completa_PMQQS_seco_sem_outliers, CodigoDoPonto, `Sub-região`, everything())

#Carregando base com ID das sub-regiões associadas a cada estação
base_ID_sub_regioes = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Base dados/Regioes_QA_PMQQS_2017_2020_LDA_unico_sem_outliers.xlsx")

#Atribuindo ID sub-região para cada entrada da base dados completa
base_dados_completa_PMQQS_seco_sem_outliers
for(i in 1:nrow(base_dados_completa_PMQQS_seco_sem_outliers)){
  for(k in 1:nrow(base_ID_sub_regioes)){
    if (base_dados_completa_PMQQS_seco_sem_outliers[i,1]==base_ID_sub_regioes[k,2]){
      base_dados_completa_PMQQS_seco_sem_outliers[i,2] = base_ID_sub_regioes[k,3]
    }
  }
}

#Transformando a base em dataframe
base_dados_completa_PMQQS_seco_sem_outliers_df = as.data.frame(base_dados_completa_PMQQS_seco_sem_outliers)


#Atribuindo cada sub-região a uma lista
lista_base_dados_PMQQS_sub_regiao_seco_sem_outliers = split(x = base_dados_completa_PMQQS_seco_sem_outliers_df, f = base_dados_completa_PMQQS_seco_sem_outliers_df$`Sub-região`)

#Exportando base dados segmentada por sub-região QA
library(openxlsx)
write.xlsx(x = lista_base_dados_PMQQS_sub_regiao_seco_sem_outliers, file = "D:/UFMG/OneDrive/Área de Trabalho/lista_base_dados_PMQQS_sub_regiao_seco_LDA_unico_sem_outliers_ingles.xlsx")
#----------------------------------------------------
#Período chuvoso - PMQQS
#Carregando base dados  completa
library(readxl)
base_dados_completa_PMQQS_chuvoso_sem_outliers = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Bases sazonais/Resultados/base_pmqqs_chuvoso_LDA_unico_sem_outliers_ingles.xlsx")

#Eliminando campos desnecessários
base_dados_completa_PMQQS_chuvoso_sem_outliers = base_dados_completa_PMQQS_chuvoso_sem_outliers[,-c(2,3)]

#Criando ID para cada sub-região de QA
library(dplyr)
base_dados_completa_PMQQS_chuvoso_sem_outliers = mutate(.data = base_dados_completa_PMQQS_chuvoso_sem_outliers, "Sub-região" = "")
base_dados_completa_PMQQS_chuvoso_sem_outliers = select(.data = base_dados_completa_PMQQS_chuvoso_sem_outliers, CodigoDoPonto, `Sub-região`, everything())

#Carregando base com ID das sub-regiões associadas a cada estação
base_ID_sub_regioes = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Base dados/Regioes_QA_PMQQS_2017_2020_LDA_unico_sem_outliers.xlsx")

#Atribuindo ID sub-região para cada entrada da base dados completa
base_dados_completa_PMQQS_chuvoso_sem_outliers
for(i in 1:nrow(base_dados_completa_PMQQS_chuvoso_sem_outliers)){
  for(k in 1:nrow(base_ID_sub_regioes)){
    if (base_dados_completa_PMQQS_chuvoso_sem_outliers[i,1]==base_ID_sub_regioes[k,2]){
      base_dados_completa_PMQQS_chuvoso_sem_outliers[i,2] = base_ID_sub_regioes[k,3]
    }
  }
}

#Transformando a base em dataframe
base_dados_completa_PMQQS_chuvoso_sem_ouliers_df = as.data.frame(base_dados_completa_PMQQS_chuvoso_sem_outliers)

#Atribuindo cada sub-região a uma lista
lista_base_dados_PMQQS_sub_regiao_chuvoso_sem_outliers = split(x = base_dados_completa_PMQQS_chuvoso_sem_ouliers_df, f = base_dados_completa_PMQQS_chuvoso_sem_ouliers_df$`Sub-região`)

#Exportando base dados segmentada por sub-região QA
library(openxlsx)
write.xlsx(x = lista_base_dados_PMQQS_sub_regiao_chuvoso_sem_outliers, file = "D:/UFMG/OneDrive/Área de Trabalho/lista_base_dados_PMQQS_sub_regiao_chuvoso_LDA_unico_sem_outliers_ingles.xlsx")






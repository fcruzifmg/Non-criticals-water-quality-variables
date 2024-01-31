#Carregando base dados sem outliers
library(readxl)
base_pmqqs_sem_outliers = read_excel(path = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Dados/Bases ajustadas ingles/base_PMQQS_2017_2020_ajustada_LDA_unico_sem_outliers_ingles.xlsx")

#Derivando informação relativa ao mês de cada campanha de monitoramento
library(lubridate)
library(dplyr)
base_pmqqs_sem_outliers$DataAmostra=dmy(base_pmqqs_sem_outliers$DataAmostra)
base_pmqqs_sem_outliers = mutate(.data = base_pmqqs_sem_outliers, "Mês" = "")
base_pmqqs_sem_outliers$Mês = month(x = base_pmqqs_sem_outliers$DataAmostra)
base_pmqqs_sem_outliers = select(.data = base_pmqqs_sem_outliers, CodigoDoPonto, DataAmostra, Mês, everything())

#Função para extrair as base sazonais
base_sazonal_sem_outliers = function(base, vetor_id_meses){
  base_sazonal_sem_outliers_df = NULL
  base_sazonal_sem_outliers_list = vector(mode = "list", length = 6)
  for(k in 1:length(base_sazonal_sem_outliers_list)){
    base_sazonal_sem_outliers_list[[k]]=filter(.data = base, Mês == c(vetor_id_meses[k,]))
  }
  base_sazonal_sem_outliers_df = do.call(rbind, base_sazonal_sem_outliers_list)
  return(base_sazonal_sem_outliers_df)
}

#Extraindo base relativa ao período chuvoso
meses_chuvoso = matrix(data = c(10,11,12,1,2,3), nrow = 6, ncol = 1)
colnames(meses_chuvoso) = c("Mês")

base_pmqqs_chuvoso_sem_outliers = base_sazonal_sem_outliers(base = base_pmqqs_sem_outliers, vetor_id_meses = meses_chuvoso)

#Exportando base relativa ao período chuvoso
library(openxlsx)
base_pmqqs_chuvoso_sem_outliers_excel = write.xlsx(x = base_pmqqs_chuvoso_sem_outliers, file = "D:/UFMG/OneDrive/Área de Trabalho/base_pmqqs_chuvoso_LDA_unico_sem_outliers_ingles.xlsx")

#Extraindo base relativa ao período seco
meses_seco = matrix(data = c(4,5,6,7,8,9), nrow = 6, ncol = 1)
colnames(meses_seco) = c("Mês")

base_pmqqs_seco_sem_outliers = base_sazonal_sem_outliers(base = base_pmqqs_sem_outliers, vetor_id_meses = meses_seco)

#Exportando base relativa ao período seco
library(openxlsx)
base_pmqqs_seco_sem_outliers_excel = write.xlsx(x = base_pmqqs_seco_sem_outliers, file = "D:/UFMG/OneDrive/Área de Trabalho/base_pmqqs_seco_LDA_unico_sem_outliers_ingles.xlsx")



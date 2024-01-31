#Carregando bases de dados
library(readxl)
source(file = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Entropia/Projetos/Entropia_marginal_PMQQS_manual_2017_2020_periodo_chuvoso_alternativo_sem_outliers_ingles.R", 
       echo = FALSE, print.eval = FALSE, verbose = FALSE, max.deparse.length = 150, width.cutoff = 92L)

base_entropia_marginal_PMQQS = vector("list", 39)
for(i in 1:39){
  base_entropia_marginal_PMQQS[[i]] = read_excel(path = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Entropia/Resultados/Tabelas/Entropia_marginal_PMQQS_periodo_chuvoso_LDA_unico_sem_outliers_ingles.xlsx", 
                                                 sheet = i)
}

names(base_entropia_marginal_PMQQS) = names(base_id_sem_entropia_nula)

base_entropia_max_PMQQS = vector("list", 39)
for(i in 1:39){
  base_entropia_max_PMQQS[[i]] = read_excel(path = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Entropia/Resultados/Tabelas/Entropia_max_PMQQS_periodo_chuvoso_LDA_unico_sem_outliers_ingles.xlsx", 
                                                 sheet = i)
}

names(base_entropia_max_PMQQS) = names(base_id_sem_entropia_nula)

#Criando objeto para receber o resultado do cálculo da redundância das variáveis aleatórias
redundancia_var_PMQQS = vector("list", 39)
for(i in 1:length(redundancia_var_PMQQS)){
  redundancia_var_PMQQS[[i]] = as.data.frame(matrix(data = 0, nrow = 1, ncol = ncol(base_id_sem_entropia_nula[[i]])))
}

redundancia_var_PMQQS
for(k in 1:length(redundancia_var_PMQQS)){
  for(i in 1:1){
    for(j in 1:ncol(redundancia_var_PMQQS[[k]])){
      redundancia_var_PMQQS[[k]][i,j] = (1-(base_entropia_marginal_PMQQS[[k]][i,j]/base_entropia_max_PMQQS[[k]][i,j]))*100
      redundancia_var_PMQQS[[k]][i,j] = round(x = redundancia_var_PMQQS[[k]][i,j], digits = 1)
    }
  }
}

#Substituindo Nan por 100
redundancia_var_PMQQS
for(k in 1:length(redundancia_var_PMQQS)){
  for(i in 1:nrow(redundancia_var_PMQQS[[k]])){
    for(j in 1:ncol(redundancia_var_PMQQS[[k]])){
      if (redundancia_var_PMQQS[[k]][i,j]=="NaN"){
        redundancia_var_PMQQS[[k]][i,j]=100
      } else {
        redundancia_var_PMQQS[[k]][i,j] = redundancia_var_PMQQS[[k]][i,j]
      }
    }
  }
}

#Retornando a ID das estações
names(redundancia_var_PMQQS) = names(base_id_sem_entropia_nula)

redundancia_var_PMQQS
for(i in 1:length(redundancia_var_PMQQS)){
  colnames(redundancia_var_PMQQS[[i]]) = colnames(base_id_sem_entropia_nula[[i]])
}

#Retornando variáveis com entropia nula
redundancia_var_entropia_nula_PMQQS = vector("list", 39)
for(k in 1:length(redundancia_var_entropia_nula_PMQQS)){
  redundancia_var_entropia_nula_PMQQS[[k]] = matrix(data = 100, nrow = 1, ncol = ncol(base_entropia_nula_PMQQS[[k]]))
}

redundancia_var_entropia_nula_PMQQS_df = vector("list", 39)
for(k in 1:length(redundancia_var_entropia_nula_PMQQS_df)){
  redundancia_var_entropia_nula_PMQQS_df[[k]] = as.data.frame(redundancia_var_entropia_nula_PMQQS[[k]])
}

names(redundancia_var_entropia_nula_PMQQS_df) = names(base_id_sem_entropia_nula)

redundancia_var_entropia_nula_PMQQS_df
for(k in 1:length(redundancia_var_PMQQS)){
  colnames(redundancia_var_entropia_nula_PMQQS_df[[k]]) = colnames(base_entropia_nula_PMQQS[[k]])
}

#Gerando base completa redundância
redundancia_var_PMQQS_comp = vector("list", 39)
for(k in 1:length(redundancia_var_PMQQS_comp)){
  redundancia_var_PMQQS_comp[[k]] = cbind2(x = redundancia_var_PMQQS[[k]], y = redundancia_var_entropia_nula_PMQQS_df[[k]])
}


#Incluindo nome das estações no dataframe com os valores das redundâncias calculados
names(redundancia_var_PMQQS_comp)=names(base_id_sem_entropia_nula)

redundancia_var_agrupadas_PMQQS = do.call(rbind, redundancia_var_PMQQS_comp)

#Truncando valor da redundância
redundancia_var_agrupadas_PMQQS
for(i in 1:nrow(redundancia_var_agrupadas_PMQQS)){
  for (j in 1:ncol(redundancia_var_agrupadas_PMQQS)) {
    if (redundancia_var_agrupadas_PMQQS[i,j]<0){
      redundancia_var_agrupadas_PMQQS[i,j]=0
    } else if (redundancia_var_agrupadas_PMQQS[i,j]>100){
      redundancia_var_agrupadas_PMQQS[i,j]=100
    } else {
      redundancia_var_agrupadas_PMQQS[i,j]=redundancia_var_agrupadas_PMQQS[i,j]
    }
  }
}

#Reordenando as colunas do dataframe
library(dplyr)
redundancia_var_agrupadas_PMQQS = redundancia_var_agrupadas_PMQQS[,order(names(redundancia_var_agrupadas_PMQQS))]


#Exportando documento Excel da determinação da redundânncia das variáveis da rede PMQQS
library(dplyr)
redundancia_var_agrupadas_PMQQS = mutate(.data = redundancia_var_agrupadas_PMQQS, "Estações PMQQS" = rownames(redundancia_var_agrupadas_PMQQS))
redundancia_var_agrupadas_PMQQS = select(.data = redundancia_var_agrupadas_PMQQS, `Estações PMQQS`, everything())

library(openxlsx)
redundancia_var_PMQQS_Excel = write.xlsx(x = redundancia_var_agrupadas_PMQQS, file = "D:/UFMG/OneDrive/Área de Trabalho/Redundancia_var_PMQQS_periodo_chuvoso_LDA_unico_sem_outliers_ingles.xlsx")


#-----------------------------------------

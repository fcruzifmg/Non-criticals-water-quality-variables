#Carregando bibliotecas
library(readxl)
library(dplyr)
library(lubridate)

#Preparando a base
base_PMQQS_2017_2020 = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Dados/Bases ajustadas ingles/base_PMQQS_2017_2020_ajustada_analise_LDA_unico_ingles.xlsx")

#Eliminando outliers das variáveis de cada estação
#Dividindo base em lista pelo nome da estação
base_PMQQS_2017_2020_lista_estacao = split(x = base_PMQQS_2017_2020, f = base_PMQQS_2017_2020$CodigoDoPonto)

#Transformando bases em dataframes
base_PMQQS_2017_2020_lista_estacao
for(k in 1:length(base_PMQQS_2017_2020_lista_estacao)){
  base_PMQQS_2017_2020_lista_estacao[[k]]=as.data.frame(base_PMQQS_2017_2020_lista_estacao[[k]])
}

#Gerando sub-lista para receber cada variável de cada estação
base_PMQQS_2017_2020_lista_variável = vector(mode = "list", length = length(base_PMQQS_2017_2020_lista_estacao))
for(k in 1:length(base_PMQQS_2017_2020_lista_estacao)){
  base_PMQQS_2017_2020_lista_variável[[k]]=vector(mode = "list", length = 78)
}

#Distribuindo as variáveis na sub-lista por estação
base_PMQQS_2017_2020_lista_variável
for(k in 1:length(base_PMQQS_2017_2020_lista_variável)){
  for(j in 1:length(base_PMQQS_2017_2020_lista_variável[[k]])){
    base_PMQQS_2017_2020_lista_variável[[k]][[j]]=base_PMQQS_2017_2020_lista_estacao[[k]][,c(1,2,j+2)]
  }
}

#Imputando nome aos objetos na lista e sub-listas das variáveis da base PMQQS
names(base_PMQQS_2017_2020_lista_variável)=names(base_PMQQS_2017_2020_lista_estacao)

#Gerando lista para receber o vetor com o índice dos outliers de cada variável
outliers_variaveis_pmqqs = vector(mode = "list", length = length(base_PMQQS_2017_2020_lista_estacao))
for(k in 1:length(outliers_variaveis_pmqqs)){
  outliers_variaveis_pmqqs[[k]]=vector(mode = "list", length = 78)
}

#Gerando lista de vetores dos índices dos outliers de cada variável
library(univOutl)
outliers_variaveis_pmqqs
for(k in 1:length(outliers_variaveis_pmqqs)){
  for(j in 1:length(outliers_variaveis_pmqqs[[k]])){
    outliers_variaveis_pmqqs[[k]][[j]]=LocScaleB(x = base_PMQQS_2017_2020_lista_variável[[k]][[j]][,3], k = 3, method = "Gini", logt = FALSE)
  }
}


outliers_variaveis_pmqqs
for(k in 1:length(outliers_variaveis_pmqqs)){
  for(j in 1:length(outliers_variaveis_pmqqs[[k]])){
    if (length(outliers_variaveis_pmqqs[[k]][[j]]$upOutl)>0){
      outliers_variaveis_pmqqs[[k]][[j]]=outliers_variaveis_pmqqs[[k]][[j]]$upOutl
    } else {
      outliers_variaveis_pmqqs[[k]][[j]]=c(0)
    }
  }
}

#Extraindo campos relacionados aos outliers
library(dplyr)
library(stringr)
medidas_outliers_pmqqs_2017_2022 = vector(mode = "list", length = length(base_PMQQS_2017_2020_lista_variável))
for(k in 1:length(medidas_outliers_pmqqs_2017_2022)){
  medidas_outliers_pmqqs_2017_2022[[k]]=vector(mode = "list", length = length(base_PMQQS_2017_2020_lista_variável[[k]]))
}

medidas_outliers_pmqqs_2017_2022
for(k in 1:length(base_PMQQS_2017_2020_lista_variável)){
  for(j in 1:length(base_PMQQS_2017_2020_lista_variável[[k]])){
    medidas_outliers_pmqqs_2017_2022[[k]][[j]]=base_PMQQS_2017_2020_lista_variável[[k]][[j]][c(outliers_variaveis_pmqqs[[k]][[j]]),]
  }
}

id_lista_medidas_outliers_pmqqs_2017_2022 = vector(mode = "list", length = length(medidas_outliers_pmqqs_2017_2022))
for(k in 1:length(id_lista_medidas_outliers_pmqqs_2017_2022)){
  id_lista_medidas_outliers_pmqqs_2017_2022[[k]]=which(str_detect(outliers_variaveis_pmqqs[[k]], "^0$"))
}

medidas_outliers_pmqqs_2017_2022_df = vector(mode = "list", length = length(medidas_outliers_pmqqs_2017_2022))
for(k in 1:length(medidas_outliers_pmqqs_2017_2022_df)){
  medidas_outliers_pmqqs_2017_2022_df[[k]]=medidas_outliers_pmqqs_2017_2022[[k]][-id_lista_medidas_outliers_pmqqs_2017_2022[[k]]]
}

#Ajustando campos das tabelas com os dados dos outliers
medidas_outliers_pmqqs_2017_2022_df
for(k in 1:length(medidas_outliers_pmqqs_2017_2022_df)){
  for(j in 1:length(medidas_outliers_pmqqs_2017_2022_df[[k]])){
    medidas_outliers_pmqqs_2017_2022_df[[k]][[j]]=dplyr::mutate(.data = medidas_outliers_pmqqs_2017_2022_df[[k]][[j]], "Variavel"="")
  }
}

medidas_outliers_pmqqs_2017_2022_df
for(k in 1:length(medidas_outliers_pmqqs_2017_2022_df)){
  for(j in 1:length(medidas_outliers_pmqqs_2017_2022_df[[k]])){
    medidas_outliers_pmqqs_2017_2022_df[[k]][[j]]$Variavel=colnames(medidas_outliers_pmqqs_2017_2022_df[[k]][[j]][3])
  }
}

medidas_outliers_pmqqs_2017_2022_df
for(k in 1:length(medidas_outliers_pmqqs_2017_2022_df)){
  for(j in 1:length(medidas_outliers_pmqqs_2017_2022_df[[k]])){
    colnames(medidas_outliers_pmqqs_2017_2022_df[[k]][[j]])=c("Código", "Data", "Resultado","Variável")
  }
}

medidas_outliers_pmqqs_2017_2022_df
for(k in 1:length(medidas_outliers_pmqqs_2017_2022_df)){
  medidas_outliers_pmqqs_2017_2022_df[[k]]=do.call(rbind, medidas_outliers_pmqqs_2017_2022_df[[k]])
}

medidas_outliers_pmqqs_2017_2022_df = do.call(rbind, medidas_outliers_pmqqs_2017_2022_df)

#Exportando tabela com todos os outliers identificados pelo critério de Gini
library(openxlsx)
outliers_variaveis_pmqqs_xlsx = write.xlsx(x = medidas_outliers_pmqqs_2017_2022_df, file = "D:/UFMG/OneDrive/Área de Trabalho/outliers_Gini_variaveis_pmqqs_2017_2020.xlsx")


#Gerando lista com número de outliers identificados por variável
percentual_outliers_dados_válidos = vector(mode = "list", length = length(base_PMQQS_2017_2020_lista_variável))
for(k in 1:length(percentual_outliers_dados_válidos)){
  percentual_outliers_dados_válidos[[k]]=vector(mode = "list", length = length(base_PMQQS_2017_2020_lista_variável[[k]]))
}

percentual_outliers_dados_válidos
for(k in 1:length(percentual_outliers_dados_válidos)){
  for(j in 1:length(percentual_outliers_dados_válidos[[k]])){
    percentual_outliers_dados_válidos[[k]][[j]]=(sum(outliers_variaveis_pmqqs[[k]][[j]]!=0)/length(na.omit(base_PMQQS_2017_2020_lista_variável[[k]][[j]][,3])))*100
    percentual_outliers_dados_válidos[[k]][[j]]=round(percentual_outliers_dados_válidos[[k]][[j]],1)
  }
}

#Retornando identificação das variáveis
percentual_outliers_dados_válidos
for(k in 1:length(percentual_outliers_dados_válidos)){
  percentual_outliers_dados_válidos[[k]]=do.call(cbind, percentual_outliers_dados_válidos[[k]])
}

percentual_outliers_dados_válidos
for(k in 1:length(percentual_outliers_dados_válidos)){
  colnames(percentual_outliers_dados_válidos[[k]])=colnames(base_PMQQS_2017_2020[,c(3:80)])
}

names(percentual_outliers_dados_válidos)=names(base_PMQQS_2017_2020_lista_estacao)

percentual_outliers_dados_válidos
for(k in 1:length(percentual_outliers_dados_válidos)){
  percentual_outliers_dados_válidos[[k]]=as.data.frame(percentual_outliers_dados_válidos[[k]])
}

#Exportando tabela com percentual de outliers de cada variável por estação
percentual_outliers_dados_válidos_export = write.xlsx(x = percentual_outliers_dados_válidos, file = "D:/UFMG/OneDrive/Área de Trabalho/percentual_outliers_pmqqs_2017_2020.xlsx")

#Gerando base com eliminação dos campos referentes aos outliers
base_PMQQS_2017_2020_lista_variável_sem_outliers = vector(mode = "list", length = length(base_PMQQS_2017_2020_lista_estacao))
for(k in 1:length(base_PMQQS_2017_2020_lista_variável_sem_outliers)){
  base_PMQQS_2017_2020_lista_variável_sem_outliers[[k]]=vector(mode = "list", length = 78)
}

base_PMQQS_2017_2020_lista_variável_sem_outliers
for(k in 1:length(base_PMQQS_2017_2020_lista_variável_sem_outliers)){
  for(j in 1:length(base_PMQQS_2017_2020_lista_variável_sem_outliers[[k]])){
    if(sum(str_detect(outliers_variaveis_pmqqs[[k]][[j]], "^0$")>0)==TRUE){
      base_PMQQS_2017_2020_lista_variável_sem_outliers[[k]][[j]]=base_PMQQS_2017_2020_lista_variável[[k]][[j]]
    } else {
      base_PMQQS_2017_2020_lista_variável_sem_outliers[[k]][[j]]=base_PMQQS_2017_2020_lista_variável[[k]][[j]][-c(outliers_variaveis_pmqqs[[k]][[j]]),]
    }
  }
}


names(base_PMQQS_2017_2020_lista_variável_sem_outliers) = names(base_PMQQS_2017_2020_lista_variável)

#Gerando a base integrada com remoção de outliers
library(plyr)
base_PMQQS_2017_2020_lista_estacao_sem_outliers = vector(mode = "list", length = length(base_PMQQS_2017_2020_lista_estacao))
for(k in 1:length(base_PMQQS_2017_2020_lista_estacao_sem_outliers)){
  base_PMQQS_2017_2020_lista_estacao_sem_outliers[[k]]=join_all(dfs = base_PMQQS_2017_2020_lista_variável_sem_outliers[[k]], type = "full", match = "all")
}

base_PMQQS_2017_2020_sem_outliers = do.call(rbind, base_PMQQS_2017_2020_lista_estacao_sem_outliers)

#Exportando base sem outliers
library(openxlsx)
base_PMQQS_2017_2020_ajustada_LDA_unico_sem_outliers_excel = write.xlsx(x = base_PMQQS_2017_2020_sem_outliers, file = "D:/UFMG/OneDrive/Área de Trabalho/base_PMQQS_2017_2020_ajustada_LDA_unico_sem_outliers.xlsx")

library(dplyr)
library(lubridate)
base_PMQQS_2017_2020_sem_outliers$DataAmostra=dmy(base_PMQQS_2017_2020_sem_outliers$DataAmostra)
base_PMQQS_2017_2020_sem_outliers  = mutate(.data = base_PMQQS_2017_2020_sem_outliers, dia = day(base_PMQQS_2017_2020_sem_outliers$DataAmostra), mes = month(base_PMQQS_2017_2020_sem_outliers$DataAmostra), ano = year(base_PMQQS_2017_2020_sem_outliers$DataAmostra))
#-------------------------------------------------------
base_alternativa_analise_cluster = vector(mode = "list", length = length(base_PMQQS_2017_2020_lista_variável))
for(k in 1:length(base_alternativa_analise_cluster)){
  base_alternativa_analise_cluster[[k]]=join_all(dfs = base_PMQQS_2017_2020_lista_variável[[k]], type = "full", match = "all")
}

base_alternativa_analise_cluster_df = do.call(rbind, base_alternativa_analise_cluster)

library(dplyr)
library(lubridate)
base_alternativa_analise_cluster_df$DataAmostra=dmy(base_alternativa_analise_cluster_df$DataAmostra)
base_alternativa_analise_cluster_df  = mutate(.data = base_alternativa_analise_cluster_df, dia = day(base_alternativa_analise_cluster_df$DataAmostra), mes = month(base_alternativa_analise_cluster_df$DataAmostra), ano = year(base_alternativa_analise_cluster_df$DataAmostra))

#Gerando as séries mensais
#agosto-setembro/2017
ago_2017 = filter(.data = base_alternativa_analise_cluster_df, mes == 8, ano == 2017)
ago_2017 = ago_2017[, -c(2, 81, 82, 83)]

set_2017 = filter(.data = base_alternativa_analise_cluster_df, mes == 9, ano == 2017)
set_2017 = set_2017[, -c(2, 81, 82, 83)]

#outubro-2017
out_2017 = filter(.data = base_alternativa_analise_cluster_df, mes == 10, ano == 2017)
out_2017 = out_2017[, -c(2, 81, 82, 83)]

#novembro-2017
nov_2017 = filter(.data = base_alternativa_analise_cluster_df, mes == 11, ano == 2017)
nov_2017 = nov_2017[, -c(2, 81, 82, 83)]

#dezembro-2017
dez_2017 = filter(.data = base_alternativa_analise_cluster_df, mes == 12, ano == 2017)
dez_2017 = dez_2017[, -c(2, 81, 82, 83)]

#janeiro-2018
jan_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 1, ano == 2018)
jan_2018 = jan_2018[, -c(2, 81, 82, 83)]

#fevereiro-2018
fev_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 2, ano == 2018)
fev_2018 = fev_2018[, -c(2, 81, 82, 83)]

#março-2018
mar_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 3, ano == 2018)
mar_2018 = mar_2018[, -c(2, 81, 82, 83)]

#abril-2018
abr_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 4, ano == 2018)
abr_2018 = abr_2018[, -c(2, 81, 82, 83)]

#maio-2018
mai_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 5, ano == 2018)
mai_2018 = mai_2018[, -c(2, 81, 82, 83)]

#junho-2018
jun_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 6, ano == 2018)
jun_2018 = jun_2018[, -c(2, 81, 82, 83)]

#julho-2018
jul_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 7, ano == 2018)
jul_2018 = jul_2018[, -c(2, 81, 82, 83)]

#agosto-2018
ago_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 8, ano == 2018)
ago_2018 = ago_2018[, -c(2, 81, 82, 83)]

#setembro-2018
set_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 9, ano == 2018)
set_2018 = set_2018[, -c(2, 81, 82, 83)]

#outubro-2018
out_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 10, ano == 2018)
out_2018 = out_2018[, -c(2, 81, 82, 83)]

#novembro-2018
nov_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 11, ano == 2018)
nov_2018 = nov_2018[, -c(2, 81, 82, 83)]

#dezembro-2018
dez_2018 = filter(.data = base_alternativa_analise_cluster_df, mes == 12, ano == 2018)
dez_2018 = dez_2018[, -c(2, 81, 82, 83)]

#janeiro-2019
jan_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 1, ano == 2019)
jan_2019 = jan_2019[, -c(2, 81, 82, 83)]

#fevereiro-2019
fev_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 2, ano == 2019)
fev_2019 = fev_2019[, -c(2, 81, 82, 83)]

#março-2019
mar_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 3, ano == 2019)
mar_2019 = mar_2019[, -c(2, 81, 82, 83)]

#abril-2019
abr_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 4, ano == 2019)
abr_2019 = abr_2019[, -c(2, 81, 82, 83)]

#maio-2019
mai_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 5, ano == 2019)
mai_2019 = mai_2019[, -c(2, 81, 82, 83)]

#junho-2019
jun_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 6, ano == 2019)
jun_2019 = jun_2019[, -c(2, 81, 82, 83)]

#julho-2019
jul_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 7, ano == 2019)
jul_2019 = jul_2019[, -c(2, 81, 82, 83)]

#agosto-2019
ago_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 8, ano == 2019)
ago_2019 = ago_2019[, -c(2, 81, 82, 83)]

#setembro-2019
set_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 9, ano == 2019)
set_2019 = set_2019[, -c(2, 81, 82, 83)]

#outubro-2019
out_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 10, ano == 2019)
out_2019 = out_2019[, -c(2, 81, 82, 83)]

#novembro-2019
nov_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 11, ano == 2019)
nov_2019 = nov_2019[, -c(2, 81, 82, 83)]

#dezembro-2019
dez_2019 = filter(.data = base_alternativa_analise_cluster_df, mes == 12, ano == 2019)
dez_2019 = dez_2019[, -c(2, 81, 82, 83)]

#janeiro-2020
jan_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 1, ano == 2020)
jan_2020 = jan_2020[, -c(2, 81, 82, 83)]

#fevereiro-2020
fev_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 2, ano == 2020)
fev_2020 = fev_2020[, -c(2, 81, 82, 83)]

#março-2020
mar_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 3, ano == 2020)
mar_2020 = mar_2020[, -c(2, 81, 82, 83)]

#abril-2020
abr_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 4, ano == 2020)
abr_2020 = abr_2020[, -c(2, 81, 82, 83)]

#maio-2020
mai_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 5, ano == 2020)
mai_2020 = mai_2020[, -c(2, 81, 82, 83)]

#junho-2020
jun_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 6, ano == 2020)
jun_2020 = jun_2020[, -c(2, 81, 82, 83)]

#julho-2020
jul_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 7, ano == 2020)
jul_2020 = jul_2020[, -c(2, 81, 82, 83)]

#agosto-2020
ago_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 8, ano == 2020)
ago_2020 = ago_2020[, -c(2, 81, 82, 83)]

#setembro-2020
set_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 9, ano == 2020)
set_2020 = set_2020[, -c(2, 81, 82, 83)]

#outubro-2020
out_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 10, ano == 2020)
out_2020 = out_2020[, -c(2, 81, 82, 83)]

#novembro-2020
nov_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 11, ano == 2020)
nov_2020 = nov_2020[, -c(2, 81, 82, 83)]

#dezembro-2020
dez_2020 = filter(.data = base_alternativa_analise_cluster_df, mes == 12, ano == 2020)
dez_2020 = dez_2020[, -c(2, 81, 82, 83)] 

#Agrupando os dados
library(plyr)
lista_dados_mensais_PMQQS = list(set_2017, ago_2017, out_2017, nov_2017, dez_2017,
                                 jan_2018, fev_2018, mar_2018, abr_2018, mai_2018, jun_2018,
                                 jul_2018, ago_2018, set_2018, out_2018, nov_2018, dez_2018,
                                 jan_2019, fev_2019, mar_2019, abr_2019, mai_2019, jun_2019,
                                 jul_2019, ago_2019, set_2019, out_2019, nov_2019, dez_2019,
                                 jan_2020, fev_2020, mar_2020,
                                 jul_2020, ago_2020, set_2020, out_2020, nov_2020, dez_2020)

id_meses_ano = list("set_2017", "ago_2017", "out_2017", "nov_2017", "dez_2017",
                    "jan_2018", "fev_2018", "mar_2018", "abr_2018", "mai_2018", "jun_2018",
                    "jul_2018", "ago_2018", "set_2018", "out_2018", "nov_2018", "dez_2018",
                    "jan_2019", "fev_2019", "mar_2019", "abr_2019", "mai_2019", "jun_2019",
                    "jul_2019", "ago_2019", "set_2019", "out_2019", "nov_2019", "dez_2019",
                    "jan_2020", "fev_2020", "mar_2020",
                    "jul_2020", "ago_2020", "set_2020", "out_2020", "nov_2020", "dez_2020")

names(lista_dados_mensais_PMQQS)=id_meses_ano


join_lista = function(lista_base){
  lista_series = vector(mode = "list", length = length(lista_base)-1)
  lista_series[[1]]=full_join(x = lista_base[[1]], y = lista_base[[2]], by = "CodigoDoPonto", copy = TRUE)
  for(k in 2:length(lista_series)){
    lista_series[[k]]=full_join(x = lista_series[[k-1]], y = lista_base[[k+1]], by = "CodigoDoPonto", copy = TRUE)
  }
  return(lista_series[[length(lista_series)]])
}


base_analise_cluster_PMQQS_2017_2020_alternativa = join_lista(lista_base = lista_dados_mensais_PMQQS)

#Exportando base dados ajustada para Análise de Cluster
library(openxlsx)
base_analise_cluster_excel = write.xlsx(x = base_analise_cluster_PMQQS_2017_2020_alternativa, 
                                        file = "D:/UFMG/OneDrive/Área de Trabalho/base_analise_cluster_PMQQS_2017_2020_alternativa.xlsx")


#------------------------------------------------------

#Gerando as séries mensais
#agosto-setembro/2017
ago_2017 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 8, ano == 2017)
ago_2017 = ago_2017[, -c(2, 81, 82, 83)]

set_2017 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 9, ano == 2017)
set_2017 = set_2017[, -c(2, 81, 82, 83)]

#outubro-2017
out_2017 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 10, ano == 2017)
out_2017 = out_2017[, -c(2, 81, 82, 83)]

#novembro-2017
nov_2017 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 11, ano == 2017)
nov_2017 = nov_2017[, -c(2, 81, 82, 83)]

#dezembro-2017
dez_2017 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 12, ano == 2017)
dez_2017 = dez_2017[, -c(2, 81, 82, 83)]

#janeiro-2018
jan_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 1, ano == 2018)
jan_2018 = jan_2018[, -c(2, 81, 82, 83)]

#fevereiro-2018
fev_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 2, ano == 2018)
fev_2018 = fev_2018[, -c(2, 81, 82, 83)]

#março-2018
mar_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 3, ano == 2018)
mar_2018 = mar_2018[, -c(2, 81, 82, 83)]

#abril-2018
abr_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 4, ano == 2018)
abr_2018 = abr_2018[, -c(2, 81, 82, 83)]

#maio-2018
mai_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 5, ano == 2018)
mai_2018 = mai_2018[, -c(2, 81, 82, 83)]

#junho-2018
jun_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 6, ano == 2018)
jun_2018 = jun_2018[, -c(2, 81, 82, 83)]

#julho-2018
jul_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 7, ano == 2018)
jul_2018 = jul_2018[, -c(2, 81, 82, 83)]

#agosto-2018
ago_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 8, ano == 2018)
ago_2018 = ago_2018[, -c(2, 81, 82, 83)]

#setembro-2018
set_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 9, ano == 2018)
set_2018 = set_2018[, -c(2, 81, 82, 83)]

#outubro-2018
out_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 10, ano == 2018)
out_2018 = out_2018[, -c(2, 81, 82, 83)]

#novembro-2018
nov_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 11, ano == 2018)
nov_2018 = nov_2018[, -c(2, 81, 82, 83)]

#dezembro-2018
dez_2018 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 12, ano == 2018)
dez_2018 = dez_2018[, -c(2, 81, 82, 83)]

#janeiro-2019
jan_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 1, ano == 2019)
jan_2019 = jan_2019[, -c(2, 81, 82, 83)]

#fevereiro-2019
fev_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 2, ano == 2019)
fev_2019 = fev_2019[, -c(2, 81, 82, 83)]

#março-2019
mar_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 3, ano == 2019)
mar_2019 = mar_2019[, -c(2, 81, 82, 83)]

#abril-2019
abr_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 4, ano == 2019)
abr_2019 = abr_2019[, -c(2, 81, 82, 83)]

#maio-2019
mai_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 5, ano == 2019)
mai_2019 = mai_2019[, -c(2, 81, 82, 83)]

#junho-2019
jun_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 6, ano == 2019)
jun_2019 = jun_2019[, -c(2, 81, 82, 83)]

#julho-2019
jul_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 7, ano == 2019)
jul_2019 = jul_2019[, -c(2, 81, 82, 83)]

#agosto-2019
ago_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 8, ano == 2019)
ago_2019 = ago_2019[, -c(2, 81, 82, 83)]

#setembro-2019
set_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 9, ano == 2019)
set_2019 = set_2019[, -c(2, 81, 82, 83)]

#outubro-2019
out_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 10, ano == 2019)
out_2019 = out_2019[, -c(2, 81, 82, 83)]

#novembro-2019
nov_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 11, ano == 2019)
nov_2019 = nov_2019[, -c(2, 81, 82, 83)]

#dezembro-2019
dez_2019 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 12, ano == 2019)
dez_2019 = dez_2019[, -c(2, 81, 82, 83)]

#janeiro-2020
jan_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 1, ano == 2020)
jan_2020 = jan_2020[, -c(2, 81, 82, 83)]

#fevereiro-2020
fev_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 2, ano == 2020)
fev_2020 = fev_2020[, -c(2, 81, 82, 83)]

#março-2020
mar_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 3, ano == 2020)
mar_2020 = mar_2020[, -c(2, 81, 82, 83)]

#abril-2020
abr_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 4, ano == 2020)
abr_2020 = abr_2020[, -c(2, 81, 82, 83)]

#maio-2020
mai_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 5, ano == 2020)
mai_2020 = mai_2020[, -c(2, 81, 82, 83)]

#junho-2020
jun_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 6, ano == 2020)
jun_2020 = jun_2020[, -c(2, 81, 82, 83)]

#julho-2020
jul_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 7, ano == 2020)
jul_2020 = jul_2020[, -c(2, 81, 82, 83)]

#agosto-2020
ago_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 8, ano == 2020)
ago_2020 = ago_2020[, -c(2, 81, 82, 83)]

#setembro-2020
set_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 9, ano == 2020)
set_2020 = set_2020[, -c(2, 81, 82, 83)]

#outubro-2020
out_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 10, ano == 2020)
out_2020 = out_2020[, -c(2, 81, 82, 83)]

#novembro-2020
nov_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 11, ano == 2020)
nov_2020 = nov_2020[, -c(2, 81, 82, 83)]

#dezembro-2020
dez_2020 = filter(.data = base_PMQQS_2017_2020_sem_outliers, mes == 12, ano == 2020)
dez_2020 = dez_2020[, -c(2, 81, 82, 83)] 


#Agrupando os dados
library(plyr)
lista_dados_mensais_PMQQS = list(set_2017, ago_2017, out_2017, nov_2017, dez_2017,
                                 jan_2018, fev_2018, mar_2018, abr_2018, mai_2018, jun_2018,
                                 jul_2018, ago_2018, set_2018, out_2018, nov_2018, dez_2018,
                                 jan_2019, fev_2019, mar_2019, abr_2019, mai_2019, jun_2019,
                                 jul_2019, ago_2019, set_2019, out_2019, nov_2019, dez_2019,
                                 jan_2020, fev_2020, mar_2020,
                                 jul_2020, ago_2020, set_2020, out_2020, nov_2020, dez_2020)

id_meses_ano = list("set_2017", "ago_2017", "out_2017", "nov_2017", "dez_2017",
                    "jan_2018", "fev_2018", "mar_2018", "abr_2018", "mai_2018", "jun_2018",
                    "jul_2018", "ago_2018", "set_2018", "out_2018", "nov_2018", "dez_2018",
                    "jan_2019", "fev_2019", "mar_2019", "abr_2019", "mai_2019", "jun_2019",
                    "jul_2019", "ago_2019", "set_2019", "out_2019", "nov_2019", "dez_2019",
                    "jan_2020", "fev_2020", "mar_2020",
                    "jul_2020", "ago_2020", "set_2020", "out_2020", "nov_2020", "dez_2020")

names(lista_dados_mensais_PMQQS)=id_meses_ano


join_lista = function(lista_base){
  lista_series = vector(mode = "list", length = length(lista_base)-1)
  lista_series[[1]]=full_join(x = lista_base[[1]], y = lista_base[[2]], by = "CodigoDoPonto", copy = TRUE)
  for(k in 2:length(lista_series)){
    lista_series[[k]]=full_join(x = lista_series[[k-1]], y = lista_base[[k+1]], by = "CodigoDoPonto", copy = TRUE)
  }
  return(lista_series[[length(lista_series)]])
}


base_analise_cluster_PMQQS_2017_2020_sem_outliers = join_lista(lista_base = lista_dados_mensais_PMQQS)


#Exportando base dados ajustada para Análise de Cluster
library(openxlsx)
base_analise_cluster_excel = write.xlsx(x = base_analise_cluster_PMQQS_2017_2020_sem_outliers, 
                                  file = "D:/UFMG/OneDrive/Área de Trabalho/base_analise_cluster_PMQQS_2017_2020_sem_outliers.xlsx")
















































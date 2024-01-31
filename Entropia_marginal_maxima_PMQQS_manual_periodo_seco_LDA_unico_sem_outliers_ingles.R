#Função para retornar o número de casas decimais de um número
num.decimals <- function(x) {
  stopifnot(class(x)=="numeric")
  x <- sub("0+$","",x)
  x <- sub("^.+[.]","",x)
  nchar(x)
}

#Função para extrair as casas decimais da base PMQQS
decimais_PMQQS = function(base){
  max_cd = NULL
  min_cd = NULL
  cd_df = NULL
  max_cd_df = NULL
  max_cd = num.decimals(max(base, na.rm = TRUE))
  min_cd = num.decimals(min(base, na.rm = TRUE))
  cd_df = data.frame(max_cd, min_cd)
  max_cd_df = max(cd_df)-1
  return(max_cd_df)
}


#Função para o cálculo da entropia máxima (Distribuição uniforme)
entropia_max_modify = function(base){
  library(entropy)
  series = NULL
  series_list = vector("list", 100)
  bins = NULL
  bins_dados_quantizados = vector("list", 100)
  decimas_base = NULL
  dados_quantizados = vector("list", 100)
  tab_frequencia = vector("list", 100)
  entropia_list = vector("list", 100)
  entropia_max = NULL
  set.seed(12345)
  series = matrix(data = 0, nrow = nrow(base), ncol = 100)
  for(i in 1:nrow(series)){
    for(j in 1:ncol(series)){
      series[i,j] = runif(n = 1, min = min(base, na.rm = TRUE), max = max(base, na.rm = TRUE))
    }
  }
  series_list
  for(i in 1:100){
    series_list[[i]] = series[,i]
  }
  bins = 3.49*(sqrt(var(base, na.rm = TRUE)))*(nrow(base)^(-1/3))
  bins_dados_quantizados
  for(i in 1:100){
    bins_dados_quantizados[[i]] = matrix(data = 0, nrow = nrow(base), ncol = 1)
  }
  bins_dados_quantizados
  for(k in 1:length(bins_dados_quantizados)){
    for(i in 1:nrow(bins_dados_quantizados[[k]])){
      for(j in 1:ncol(bins_dados_quantizados[[k]])){
        bins_dados_quantizados[[k]][i] = bins
      }
    }
  }
  decimas_base = decimais_PMQQS(base)
  dados_quantizados
  for(i in 1:length(dados_quantizados)){
    dados_quantizados[[i]] = matrix(data = 0, nrow = nrow(bins_dados_quantizados[[i]]), ncol = ncol(bins_dados_quantizados[[i]]))
  }
  dados_quantizados
  for(k in 1:length(dados_quantizados)){
    for(i in 1:nrow(dados_quantizados[[k]])){
      for(j in 1:ncol(dados_quantizados[[k]])){
        dados_quantizados[[k]][i] = bins_dados_quantizados[[k]][i]*(((2*series_list[[k]][i]) + bins_dados_quantizados[[k]][i])/(2*bins_dados_quantizados[[k]][i]))
        dados_quantizados[[k]][i] = round(x = dados_quantizados[[k]][i], digits = decimas_base)
      }
    }
  }
  tab_frequencia
  for(i in 1:100){
    tab_frequencia[[i]] = table(dados_quantizados[[i]])
  }
  entropia_list
  for(i in 1:100){
    entropia_list[[i]] = entropy(y = tab_frequencia[[i]], unit = "log2")
  }
  entropia_list_df = t(list2DF(entropia_list))
  entropia_max = data.frame(sort(entropia_list_df))
  entropia_max = entropia_max[c(90),]
  return(entropia_max)
}

#-------------------------------------
#Funçãoo para quebra de uma base no formato dataframe para lista (lista de variáveis, sendo cada objeto da lista um dataframe de variável de QA)
split_colunas = function(base){
  base_list = vector(mode = "list", length = ncol(base))
  for(i in 1:ncol(base)){
    for(j in 1:ncol(base)){
      base_list[[i]] = base[i]
    }
  }
  return(base_list)
}

#-----------------------------------
#Carregando base e organizando-a no formato de lista (Cada estação é um objeto da lista)
library(readxl)
Base_PMQQS_entropia_max = vector("list", 39)
for(i in 1:39){
  Base_PMQQS_entropia_max[[i]] = read_excel(path = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Entropia/Resultados/Tabelas/Base_PMQQS_sem_var_entropia_nula_periodo_seco_LDA_unico_sem_outliers_ingles.xlsx", 
                                            sheet = i)
}

Base_PMQQS_entropia_max_df = vector("list", 39)
for(i in 1:39){
  Base_PMQQS_entropia_max_df[[i]] = data.frame(Base_PMQQS_entropia_max[[i]])
}

#----------------------------------
#Calculo da máxima entropia da base PMQQS
Entropia_max_base_PMQQS = vector("list", 39)
for(i in 1:39){
  Entropia_max_base_PMQQS[[i]] = vector(mode = "list", length = length(Base_PMQQS_entropia_max_df[[i]]))
}

set.seed(12345)
Entropia_max_base_PMQQS
for(i in 1:39){
  for(j in 1:length(Base_PMQQS_entropia_max_df[[i]])){
    Entropia_max_base_PMQQS[[i]][j] = entropia_max_modify(Base_PMQQS_entropia_max_df[[i]][j])
  }
}



#Gerando os dataframes das bases por estação
Entropia_max_base_PMQQS_df = vector("list", 39)
for(i in 1:39){
  Entropia_max_base_PMQQS_df[[i]] = list2DF(Entropia_max_base_PMQQS[[i]])
}

#Retornando informações da base de esta??es sem variáveis de entropia nula
source(file = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Entropia/Projetos/Entropia_marginal_PMQQS_manual_2017_2020_periodo_seco_LDA_unico_sem_outliers_ingles.R", 
       width.cutoff = 92L)

names(Entropia_max_base_PMQQS_df) = names(base_id_sem_entropia_nula)

Entropia_max_base_PMQQS_df
for (i in 1:39){
  names(Entropia_max_base_PMQQS_df[[i]]) = names(base_id_sem_entropia_nula[[i]])
}

#----------------------------------
#Exportando dataframes para Excel
library(openxlsx)
write.xlsx(Entropia_max_base_PMQQS_df, file = "D:/UFMG/OneDrive/Área de Trabalho/Entropia_max_PMQQS_periodo_seco_LDA_unico_sem_outliers_ingles.xlsx")









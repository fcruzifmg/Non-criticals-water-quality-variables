#Identificando variáveis com entropia nula (nenhuma variabilidade temporal)
#Carregando base dados
library(readxl)
base_manual_PMQQS = read_excel("D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Análise - inglês/Bases sazonais/Resultados/base_pmqqs_seco_LDA_unico_sem_outliers_ingles.xlsx")

#eliminando campos desnecessários
base_manual_PMQQS = base_manual_PMQQS[,-c(2, 3)]

#Renomeando campo com a identificação das estações
colnames(base_manual_PMQQS)[1] = "ID"

#transformando base para o formato dataframe
base_manual_PMQQS = as.data.frame(base_manual_PMQQS)

#Extraindo base de cada estação
base_parametros = split(x = base_manual_PMQQS, f = base_manual_PMQQS[,1])

#Eliminando ID das estações em cada dataframe da lista
base_parametros
for(i in 1:length(base_parametros)){
  base_parametros[[i]] = base_parametros[[i]][,-c(1)]
}

#Eliminando variáveis com entropia nula
var_entropia_nula = function(base, na.rm = TRUE){
  sd = NULL
  base_ajustada = NULL
  sd = apply(X = base, MARGIN = 2, FUN = function(X) (sd(X, na.rm = TRUE)))
  for (j in 1:ncol(base)){
    if (sd[j] != 0|is.na(sd[j])==TRUE){
      base_ajustada[j] = base[j]
    } else {
      base_ajustada[j] = NULL
    }
  }
  return(base_ajustada)
}

base_id_entropia_nula = vector("list", 39)
for(k in 1:length(base_id_entropia_nula)){
  base_id_entropia_nula[[k]] = var_entropia_nula(base_parametros[[k]])
}

ajuste_fino_elimina_entropia_nula = function(base_ajustada){
  base_ajustada_n = NULL
  base_ajustada_df = NULL
  base_ajustada_n = do.call(cbind, base_ajustada)
  base_ajustada_df = as.data.frame(base_ajustada_n)
  return(base_ajustada_df)
}

base_id_sem_entropia_nula = vector("list", 39)
for(i in 1:length(base_id_sem_entropia_nula)){
  base_id_sem_entropia_nula[[i]] = ajuste_fino_elimina_entropia_nula(base_id_entropia_nula[[i]])
}

#-----------------------------------------
#Eliminando nome das variáveis eliminadas e retornando-os às variáveis remanescentes
indices_var_entropia_nula = function(base){
  sd = NULL
  ind = NULL
  sd = apply(X = base, MARGIN = 2, FUN = function(X) (sd(X, na.rm = TRUE)))
  ind = which(sd==0)
  return(ind)
}

ind_var_entropia_nula_pmqqs = vector("list", 39)
for(i in 1:length(ind_var_entropia_nula_pmqqs)){
  ind_var_entropia_nula_pmqqs[[i]] = indices_var_entropia_nula(base_parametros[[i]])
}

ind_var_entropia_nula_pmqqs_names = vector("list", 39)
for(i in 1:39){
  ind_var_entropia_nula_pmqqs_names[[i]] = names(ind_var_entropia_nula_pmqqs[[i]])
}

names(ind_var_entropia_nula_pmqqs_names) = names(base_parametros)

nomes_var_base_pmqqs = colnames(base_manual_PMQQS[,-1])

nomes_var_sem_entropia_nula = vector("list", 39)
for(i in 1:length(nomes_var_sem_entropia_nula)){
  nomes_var_sem_entropia_nula[[i]] = nomes_var_base_pmqqs[-ind_var_entropia_nula_pmqqs[[i]]]
}

base_id_sem_entropia_nula
for(i in 1:length(base_id_sem_entropia_nula)){
  colnames(base_id_sem_entropia_nula[[i]]) = nomes_var_sem_entropia_nula[[i]]
}

names(base_id_sem_entropia_nula) = names(base_parametros)

#------------------------------------------
#Gerando base relacionada a entropia nula
base_entropia_nula_PMQQS = vector("list", 39)

base_entropia_nula_PMQQS
for(k in 1:length(base_entropia_nula_PMQQS)){
  base_entropia_nula_PMQQS[[k]] = data.frame(t(as.data.frame(ind_var_entropia_nula_pmqqs_names[[k]])))
}

base_entropia_nula_PMQQS
for(k in 1:length(base_entropia_nula_PMQQS)){
    colnames(base_entropia_nula_PMQQS[[k]]) = base_entropia_nula_PMQQS[[k]]
}

base_entropia_nula_PMQQS
for(k in 1:length(base_entropia_nula_PMQQS)){
  for(i in 1:nrow(base_entropia_nula_PMQQS[[k]])){
    for(j in 1:ncol(base_entropia_nula_PMQQS[[k]])){
      base_entropia_nula_PMQQS[[k]][i,j] = 0
    }
  }
}

names(base_entropia_nula_PMQQS)=names(base_parametros)

#----------------------------------------------------
#Calculando intervalo de classes por Scott (1979) - livro V. J. Singh pag. 544 e número de classes
library(entropy)
bins = function(base){
  bins_m = NULL
  bins_base = NULL
  for(i in 1:length(base)){
    bins_base[i] = apply(X = base[i], MARGIN = 2, FUN = function (X) (x = 3.49*sd(X, na.rm = TRUE)*(nrow(base[i])^(-1/3))))
    bins_m = as.matrix(bins_base)
  }
  return(bins_m) 
}

bins_base_pmqqs = vector("list", 39)
for(i in 1:length(bins_base_pmqqs)){
  bins_base_pmqqs[[i]] = bins(base_id_sem_entropia_nula[[i]])
}

bins_base_pmqqs_df = vector("list", 39)
for(i in 1:length(bins_base_pmqqs)){
  bins_base_pmqqs_df[[i]] = as.data.frame(t(bins_base_pmqqs[[i]]))
}


bins_base_pmqqs_comp = vector("list", 39)
for(i in 1: length(bins_base_pmqqs_comp)){
  bins_base_pmqqs_comp[[i]] = matrix(data = 0,nrow = nrow(base_id_sem_entropia_nula[[i]]), ncol = ncol(base_id_sem_entropia_nula[[i]]))
}

bins_base_pmqqs_comp_df = vector("list", 39)
for(i in 1:length(bins_base_pmqqs_comp_df)){
  bins_base_pmqqs_comp_df[[i]] = data.frame(bins_base_pmqqs_comp[[i]])
}

bins_ajustados = function(base_ajustada, bins){
  bins_ajustados = data.frame()
  for(i in 1:nrow(base_ajustada)){
    for(j in 1:ncol(base_ajustada)){
      bins_ajustados[i,j] = bins[1,j]
    }
  }
return(bins_ajustados)
}


bins_base_pmqqs_comp_df
for(i in 1:length(bins_base_pmqqs_comp_df)){
  bins_base_pmqqs_comp_df[[i]] = bins_ajustados(base_ajustada = bins_base_pmqqs_comp_df[[i]], bins = bins_base_pmqqs_df[[i]])
}

names(bins_base_pmqqs_comp_df)=names(base_parametros)

bins_base_pmqqs_comp_df
for(k in 1:length(bins_base_pmqqs_comp_df)){
  names(bins_base_pmqqs_comp_df[[k]])=names(base_id_sem_entropia_nula[[k]])
}

#------------------------------------
#Extraindo os decimais de cada observação
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

#Calculando as casas decimais das variáveis de cada estação
casas_decimais_var_PMQQS = vector("list", 39)
for(i in 1:length(casas_decimais_var_PMQQS)){
  casas_decimais_var_PMQQS[[i]] = apply(X = base_id_sem_entropia_nula[[i]], MARGIN = 2, FUN = function(X)(decimais_PMQQS(X)))
}

#Gerando dataframes do número de casas decimais das variáveis de cada estação
casas_decimais_var_PMQQS_df = vector("list", 39)
for(i in 1:39){
  casas_decimais_var_PMQQS_df[[i]] = data.frame(t(casas_decimais_var_PMQQS[[i]]))
}

#Gerando listas aninhadas para receber as casas decimais
casas_decimais_var_PMQQS_comp = vector("list", 39)
for(i in 1:length(casas_decimais_var_PMQQS_comp)){
  casas_decimais_var_PMQQS_comp[[i]] = data.frame(matrix(data = 0, ncol = ncol(base_id_sem_entropia_nula[[i]]), nrow = nrow(base_id_sem_entropia_nula[[i]])))
}

#Funçãoo para atribuir o número de casas decimais de cada variável a cada dado dado observado
var_ajustados = function(base_decimais, decimais){
  var_ajustados = data.frame()
  for(i in 1:nrow(base_decimais)){
    for(j in 1:ncol(base_decimais)){
      var_ajustados[i,j] = decimais[1,j]
    }
  }
  return(var_ajustados)
}

#Gerando base completa de casas decimais da base PMQQS
casas_decimais_var_PMQQS_comp
for(i in 1:length(casas_decimais_var_PMQQS_comp)){
  casas_decimais_var_PMQQS_comp[[i]] = var_ajustados(base_decimais = casas_decimais_var_PMQQS_comp[[i]], decimais = casas_decimais_var_PMQQS_df[[i]])
}

casas_decimais_var_PMQQS_comp
for(i in 1:length(casas_decimais_var_PMQQS_comp)){
  casas_decimais_var_PMQQS_comp[[i]] = data.frame(lapply(casas_decimais_var_PMQQS_comp[[i]], as.numeric))
}

#-------------------------------------------------------
#Quantização dos dados observados
#Função para a quantização das variáveis
var_quantizacao = function(base, bins, casas_decimais, na.rm = TRUE){
  base_quantizada = data.frame()
  for(i in 1:nrow(base)){
    for(j in 1:ncol(base)){
      base_quantizada[i,j] =  bins[i,j]*(((2*base[i,j]) + bins[i,j])/(2*bins[i,j]))
      base_quantizada[i,j] = round(x = base_quantizada[i,j], digits = casas_decimais[i,j])
    }
  }
  return(base_quantizada)
}

#Gerando lista para receber a base de dados quantizada
var_quantizadas_PMQQS = vector("list", 39)
for(i in 1:length(var_quantizadas_PMQQS)){
  var_quantizadas_PMQQS[[i]] = matrix(data = 0, ncol = ncol(base_id_sem_entropia_nula[[i]]), nrow = nrow(base_id_sem_entropia_nula[[i]]))
}

var_quantizadas_PMQQS_comp = vector("list", 39)
for(i in 1:length(var_quantizadas_PMQQS)){
    var_quantizadas_PMQQS_comp[[i]] = as.data.frame(var_quantizadas_PMQQS[[i]])
}

#Quantização das variáveis
var_quantizadas_PMQQS_comp
for(i in 1:length(var_quantizadas_PMQQS_comp)){
  var_quantizadas_PMQQS_comp[[i]] = var_quantizacao(base = base_id_sem_entropia_nula[[i]], bins = bins_base_pmqqs_comp_df[[i]], casas_decimais = casas_decimais_var_PMQQS_comp[[i]])
}

names(var_quantizadas_PMQQS_comp)=names(base_id_sem_entropia_nula)

var_quantizadas_PMQQS_comp
for(k in 1:length(var_quantizadas_PMQQS_comp)){
  names(var_quantizadas_PMQQS_comp[[k]])=names(base_id_sem_entropia_nula[[k]])
}

#------------------------------------
#Listas aninhadas das variáveis quantizadas
#Criando o objeto que receberá as listas aninhadas das variáveis
lista_var_quantizadas_PMQQS = vector("list", 39)

#Função para quebrar os data frames em listas
tranf_dataframes_listas = function(dataframes){
  listas = vector("list", ncol(dataframes))
    for(i in 1:length(listas)){
      listas[[i]] = dataframes[,i]
    }
return(listas)
}


#Gerando as listas aninhadas para receber as tabelas de frequência
lista_var_quantizadas_PMQQS
for(i in 1:length(lista_var_quantizadas_PMQQS)){
  lista_var_quantizadas_PMQQS[[i]] = tranf_dataframes_listas(var_quantizadas_PMQQS_comp[[i]]) 
}

#------------------------------------
#Gerando as tabelas de frequência
#Função para gerar as tabelas de frequência
tabela_freq = function(base){
  freq_dados = NULL
    for(i in 1:length(base)){
      freq_dados[[i]] = table(base[[i]])
    }
  return(freq_dados)
}

#Gerando lista que irá receber as tabelas de frequência das variáveis
freq_variaveis_PMQQS = vector("list", 39)
for(i in 1:length(freq_variaveis_PMQQS)){
  freq_variaveis_PMQQS[[i]] = tabela_freq(lista_var_quantizadas_PMQQS[[i]])
}

#------------------------------------
#Entropia marginal de cada variável
#Função para gerar a entropia marginal
library(entropy)
entropia_var = function(var_discretizada){
  entropia_marginal = vector("list", length = length(var_discretizada))
  for(i in 1:length(var_discretizada)){
    entropia_marginal[[i]] = entropy(y = var_discretizada[[i]], unit = "log2")
    entropia_marginal[[i]] = round(x = entropia_marginal[[i]], digits = 15)
  }
  return(entropia_marginal)
}

#Cálculo da entropia marginal das variáveis
entropia_marginal_var_pmqqs = vector("list", 39)
for(i in 1:length(entropia_marginal_var_pmqqs)){
  entropia_marginal_var_pmqqs[[i]] = entropia_var(freq_variaveis_PMQQS[[i]])
}

#-----------------------------
#Gerando os data frames de entropia de cada estação
entropia_marginal_var_pmqqs_df = vector("list", 39)
for(i in 1:length(entropia_marginal_var_pmqqs_df)){
  entropia_marginal_var_pmqqs_df[[i]] = list2DF(entropia_marginal_var_pmqqs[[i]])
}
names(entropia_marginal_var_pmqqs_df) = names(base_id_sem_entropia_nula)

#Retornando o nome das variáveis nos dataframes das estações com entropia calculada
entropia_marginal_var_pmqqs_df
for(i in 1:length(entropia_marginal_var_pmqqs_df)){
  colnames(entropia_marginal_var_pmqqs_df[[i]]) = colnames(base_id_sem_entropia_nula[[i]])
}

#-------------------------------------
#Exportando dataframes para Excel
library(openxlsx)

write.xlsx(entropia_marginal_var_pmqqs_df, file = "D:/UFMG/OneDrive/Área de Trabalho/Entropia_marginal_PMQQS_periodo_seco_LDA_unico_sem_outliers_ingles.xlsx")
write.xlsx(base_id_sem_entropia_nula, file = "D:/UFMG/OneDrive/Área de Trabalho/Base_PMQQS_sem_var_entropia_nula_periodo_seco_LDA_unico_sem_outliers_ingles.xlsx")
write.xlsx(base_entropia_nula_PMQQS, file = "D:/UFMG/OneDrive/Área de Trabalho/Base_PMQQS_Entropia_nula_periodo_seco_LDA_unico_sem_outliers_ingles.xlsx")











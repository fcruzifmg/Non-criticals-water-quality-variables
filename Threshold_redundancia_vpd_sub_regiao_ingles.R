#Carregar base dados com redundância dos períodos seco e chuvoso
library(readxl)
red_var_pmqqs_seco = read_excel(path = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Entropia/Resultados/Tabelas/Redundancia_var_PMQQS_periodo_seco_LDA_unico_sem_outliers.xlsx")
red_var_pmqqs_chuvoso = read_excel(path = "D:/UFMG/OneDrive/Doutorado - UFMG/Tese/Resultados/Eixo I/Análise alternativa sem outliers e correção do LDA/Entropia/Resultados/Tabelas/Redundancia_var_PMQQS_periodo_chuvoso_LDA_unico_sem_outliers.xlsx")


#Elimiando a id das estações
red_var_pmqqs_seco = red_var_pmqqs_seco[,-c(1)]
red_var_pmqqs_chuvoso = red_var_pmqqs_chuvoso[,-c(1)]

#Espalhando os dados das estações em objetos de uma lista
red_var_pmqqs_seco_lista = vector(mode = "list", length = ncol(red_var_pmqqs_seco))
red_var_pmqqs_chuvoso_lista = vector(mode = "list", length = ncol(red_var_pmqqs_chuvoso))

#Splitando as bases
red_var_pmqqs_seco_lista
for(k in 1:length(red_var_pmqqs_seco_lista)){
  red_var_pmqqs_seco_lista[[k]]=red_var_pmqqs_seco[,c(k)]
}

red_var_pmqqs_chuvoso_lista
for(k in 1:length(red_var_pmqqs_chuvoso_lista)){
  red_var_pmqqs_chuvoso_lista[[k]]=red_var_pmqqs_chuvoso[,c(k)]
}

#Transformando bases em dataframes
red_var_pmqqs_seco_lista
for(k in 1:length(red_var_pmqqs_seco_lista)){
  red_var_pmqqs_seco_lista[[k]]=as.data.frame(red_var_pmqqs_seco_lista[[k]])
}

red_var_pmqqs_chuvoso_lista
for(k in 1:length(red_var_pmqqs_chuvoso_lista)){
  red_var_pmqqs_chuvoso_lista[[k]]=as.data.frame(red_var_pmqqs_chuvoso_lista[[k]])
}

#Renomeando única variável para empilhar posteriormente as bases
red_var_pmqqs_seco_lista
for(k in 1:length(red_var_pmqqs_seco_lista)){
  colnames(red_var_pmqqs_seco_lista[[k]])=c("Var")
}

red_var_pmqqs_chuvoso_lista
for(k in 1:length(red_var_pmqqs_chuvoso_lista)){
  colnames(red_var_pmqqs_chuvoso_lista[[k]])=c("Var")
}

#Empilhando lista
red_var_pmqqs_seco_lista = do.call(rbind, red_var_pmqqs_seco_lista)
red_var_pmqqs_chuvoso_lista = do.call(rbind, red_var_pmqqs_chuvoso_lista)

#Integrando as bases
red_pmqqs_final = rbind(red_var_pmqqs_chuvoso_lista, red_var_pmqqs_seco_lista)

#Gerando base sem considerar a redundância em 100%
red_pmqqs_final_sem_red_100 = red_pmqqs_final[-c(which(red_pmqqs_final$Var==100)),]
red_pmqqs_final_sem_red_100 = as.data.frame(red_pmqqs_final_sem_red_100)
colnames(red_pmqqs_final_sem_red_100)=c("Var")

#Gerando histograma da redundância da bacia do rio Doce
library(ggplot2)

histo_red_pmqqs_doce = ggplot(data = red_pmqqs_final, mapping = aes(x = red_pmqqs_final$Var)) + geom_histogram() +
  labs(title = "Distribuição da redundância informacional na bacia do rio Doce") + xlab(label = "Redundância informacional") + ylab(label = "Densidade")

plot(histo_red_pmqqs_doce)  

histo_red_sem_100_pmqqs_doce = ggplot(data = red_pmqqs_final_sem_red_100, mapping = aes(x = red_pmqqs_final_sem_red_100$Var)) + geom_histogram() +
  labs(title = "Distribuição da redundância informacional na bacia do rio Doce") + xlab(label = "Redundância informacional") + ylab(label = "Densidade")

plot(histo_red_sem_100_pmqqs_doce)

#Gerando dataframe para receber as estatísticas
estatisticas_red = data.frame(matrix(data = 0, nrow = 4, ncol = 3))

#Nomeando colunas
colnames(estatisticas_red) = c("Estatísitca", "Redundância completa", "Redundância sem 100%")

#Imputando nomes das estatísticas calculadas
estatisticas_red$Estatísitca = c("Média", "Mediana", "dQ", "dD")

#Calculando as estatísticas
#Base Red completa
estatisticas_red[1,2] = mean(x = red_pmqqs_final$Var)
estatisticas_red[2,2] = median(x = red_pmqqs_final$Var)
estatisticas_red[3,2] = ((quantile(x = red_pmqqs_final$Var, probs = 0.50)-quantile(x = red_pmqqs_final$Var, probs = 0.25))/0.6745)
estatisticas_red[4,2] = ((quantile(x = red_pmqqs_final$Var, probs = 0.90)-quantile(x = red_pmqqs_final$Var, probs = 0.50))/1.2816)

#Base Red sem red=100%
estatisticas_red[1,3] = mean(x = red_pmqqs_final_sem_red_100$Var)
estatisticas_red[2,3] = median(x = red_pmqqs_final_sem_red_100$Var)
estatisticas_red[3,3] = ((quantile(x = red_pmqqs_final_sem_red_100$Var, probs = 0.50)-quantile(x = red_pmqqs_final_sem_red_100$Var, probs = 0.25))/0.6745)
estatisticas_red[4,3] = ((quantile(x = red_pmqqs_final_sem_red_100$Var, probs = 0.90)-quantile(x = red_pmqqs_final_sem_red_100$Var, probs = 0.50))/1.2816)

#Ajustando base para uma casa decimal
estatisticas_red$`Redundância completa` = round(x = estatisticas_red$`Redundância completa`, digits = 1)
estatisticas_red$`Redundância sem 100%`= round(x = estatisticas_red$`Redundância sem 100%`, digits = 1)

#Exportando base com as estatísiticas
library(openxlsx)
estatisticas_redundancia_pmqqs_Doce_export = write.xlsx(x = estatisticas_red, file = "D:/UFMG/OneDrive/Área de Trabalho/Estatisticas_redundancia_pmqqs_Doce.xlsx")
  
#Gerando resultados para gerar gráfico de linhas
 
base_precentis_redundancia = data.frame(matrix(data = 0, nrow = 21, ncol = 2))
colnames(base_precentis_redundancia) =c("Quantile", "Information redundancy")

base_precentis_redundancia
for(i in 2:nrow(base_precentis_redundancia)){
  for(k in 1:ncol(base_precentis_redundancia)){
    base_precentis_redundancia[i,1]=0
    base_precentis_redundancia[i,1]=base_precentis_redundancia[i-1,1]+0.05
  }
}

base_precentis_redundancia
for(i in 2:nrow(base_precentis_redundancia)){
  for(k in 1:ncol(base_precentis_redundancia)){
    base_precentis_redundancia[i,2]=0
    base_precentis_redundancia[i,2]=quantile(x = red_pmqqs_final$Var, probs = base_precentis_redundancia[i,1])
  }
} 

#Gráfico threshold VRMF
library(ggplot2)
limiar_VRMF = ggplot(data = base_precentis_redundancia, mapping = aes(x = base_precentis_redundancia$Quantile, y = base_precentis_redundancia$`Information redundancy`)) +
  geom_col() + geom_line(size=1, color="blue") + geom_point(size=1.5, color="blue", shape = 15) + labs(title = "Distribution of Information redundancy") + xlab(label = "Quantile") + ylab(label = "Information redundancy") + theme_classic() +
  theme(title = element_text(face = "bold", size = 18), axis.title.x = element_text(face = "bold", size = 17), axis.title.y = element_text(face = "bold", size = 17), 
        axis.text = element_text(face = "bold", size = 14)) +
  geom_hline(yintercept = 80, colour = "red")
plot(limiar_VRMF)  

#Gráfico threshold VRMF - Tese
library(ggplot2)
limiar_VRMF = ggplot(data = base_precentis_redundancia, mapping = aes(x = base_precentis_redundancia$Quantile, y = base_precentis_redundancia$`Information redundancy`)) +
  geom_col() + xlab(label = "Percentis") + ylab(label = "Redundância informacional") + theme_classic() +
  theme(title = element_text(face = "bold", size = 18), axis.title.x = element_text(face = "bold", size = 17), axis.title.y = element_text(face = "bold", size = 17), 
        axis.text = element_text(face = "bold", size = 14)) +
  geom_hline(yintercept = 80, colour = "red")
plot(limiar_VRMF)  



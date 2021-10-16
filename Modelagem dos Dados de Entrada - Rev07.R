#-----------------------------------------------------
# Simulacao computacional por Eventos Discretos
# Modelagem dos dados de entrada II
# Dados de entrada: "nome da variavel"
# Prof. Leandro Gauss
#-----------------------------------------------------

# Instalar pacotes
#install.packages("readxl")                # ler arquivos do excell
#install.packages("e1071")                 # calcular coeficiente de assimetria
#install.packages("fitdistrplus")          # ajusta a distribuicao aos dados
#install.packages("actuar")

# Carregar pacotes
library(readxl)                            # trabalha com arquivos em excell
library(e1071)                             # calcular coeficiente de assimetria
library(fitdistrplus)                      # ajusta a distribuicao aos dados
library(actuar)

#------------------------------------------------------
# Avaliacao do data set original
#------------------------------------------------------

# Ler arquivos em csv
#planilha = read.csv("Exemplo Supermecado - Dados Coletados.csv")

# Gerar numeros aleatorios
#set.seed(120)                            # semente p/ geracao de numeros aleatorios
#dados = rnorm(200,5,1)                   # dist. norm. com 200 observacoes, media 5 e desv.pad. 1

# Ler arquivos em excel
planilha = read_xlsx("Exemplo Supermecado - Dados Coletados.xlsx")
dados = planilha$Tempo                     # coluna de interesse

# resumo dos dados
summary(dados)

# Calcular medidas de posiçao
mean(dados)                                # media
median(dados)                              # mediana
moda = (table(round(dados,1)))
moda[moda == max(moda)]                    # moda
min(dados)                                 # minimo
max(dados)                                 # maximo

# Calcular medidas de dispersao
range(dados)                               # amplitude
var(dados)                                 # variancia
sd(dados)                                  # desvio padrao
sd(dados) / mean(dados)                    # coeficiente de variacao
skewness(dados)                            # coeficient de assimetria

#Box plot
par(mfrow = c(2,2))                        # organiza janela de graficos 2 x 2
boxplot(dados,vertical=T,                  # box-plot na vertical
        main="Box-plot (dados brutos)",    # titulo 
        xlab="Tempo",                      # legenda eixo x
        ylab="Valores",                    # legenda eixo y
        col="grey")                        # cor do box

# Regra de Sturges
k = round(1+(3.3*log10(length(dados))),0)  # calcula o numero de intervalos do histograma

# Histograma
hist(dados, 
     freq = T,                             # retona frequencia das classes
     nclass = k,                           # numero de classes (NULL calcula as classes automaticamente)
     main = "Histograma (dados brutos)",   # titulo
     xlab = "Classes",                     # legenda do eixo x
     ylab = "Frequencia",                  # legenda do eixo y
     col = "grey")                         # cor da barra

# Identificar outliers
Q1 = as.numeric(quantile(dados, 0.25)); Q1 # primeiro quartil
Q3 = as.numeric(quantile(dados, 0.75)); Q3 # terceiro quartil
A = Q3 - Q1; A                             # amplitude do quartil
otl = 3.0                                  # criterio de remocao de outliers [moderados = 1.5 e extremos = 3.0]
MQ1 = subset(dados,
             dados < Q1-(otl*A)); MQ1      # valores abaixo de Q1
MQ3 = subset(dados,
             dados > Q3+(otl*A)); MQ3      # valores acima de Q3

#------------------------------------------------------
# Avaliacao do data set sem outliers
#------------------------------------------------------

linf = ifelse(length(MQ1) == 0,
              min(dados),
              max(MQ1))                    # menor valor a ser considerado no dataset

lSUP = ifelse(length(MQ3) == 0,
              max(dados),
              min(MQ3))                    # maior valor a ser considerado no dataset

dados_so = subset(dados, dados >= linf &
                         dados < lSUP)     # novo data set sem outliers

sort(dados_so)                             # ordenando novo data set de forma crescente

# resumo dos dados
summary(dados_so)

# Calcular medidas de posição
mean(dados_so)                             # media
median(dados_so)                           # mediana
moda_so = (table(round(dados_so,1)))
moda_so[moda_so == max(moda_so)]           # moda
min(dados_so)                              # minimo
max(dados_so)                              # maximo

# Calcular medidas de dispersao
range(dados_so)                            # amplitude
var(dados_so)                              # variancia
sd(dados_so)                               # desvio padrao
sd(dados_so) / mean(dados_so)              # coeficiente de variacao
skewness(dados_so)                         # coeficient de assimetria

# Analise de correlacao
plot(x = dados_so[1:length(dados_so)-1],   # valores eixo x
     y = dados_so[2:length(dados_so)],     # valores eixo y
     main = "Correlacao (dados tratados)", # titulo
     xlab = "Tempo",                       # legenda eixo x
     ylab = "Tempo k+1",                   # legenda eixo y
     pch = 21,                             # tipo de marcado (circulo preenchido)
     bg = "grey",                          # preenchimento do marcador
     col = "black")                        # contorno do marcado

# Regra de Sturges
kso = round(1+(3.3*log10(length(dados_so))),0) # calcula o numero de intervalos do histograma

# Histograma
hist(dados_so, 
     freq = T,                             # retona frequencia das classes
     nclass = kso,                         # numero de classes (NULL calcula as classes automaticamente)
     main = "Histograma (dados tratados)", # titulo
     xlab = "Classes",                     # legenda do eixo x
     ylab = "Frequencia",                  # legenda do eixo y
     col = "grey")                         # cor da barra

#------------------------------------------------------
# Ajuste da distribuicao aos dados
#------------------------------------------------------

# Ajustar as dist. de probabilidade
f_gamma   = fitdist(dados_so, "gamma")     # ajusta dist. gamma
f_lnorm   = fitdist(dados_so, "lnorm")     # ajusta dist. lognormal
f_weibull = fitdist(dados_so, "weibull")   # ajusta dist. weibull
f_exp     = fitdist(dados_so, "exp")       # ajusta dist. exponencial
f_norm    = fitdist(dados_so, "norm")      # ajusta dist. normal
f_unif    = fitdist(dados_so, "unif")      # ajusta dist. unifiorme
f_pois    = fitdist(dados_so, "pois")      # ajusta dist. poisson
f_logis   = fitdist(dados_so, "logis")     # ajusta dist. logistica

# Criar lista de dist. de probabilidades
dist_list = list((try(f_gamma, silent = T)),
                 (try(f_lnorm, silent = T)),
                 (try(f_weibull, silent = T)),
                 (try(f_exp, silent = T)),
                 (try(f_norm, silent = T)),
                 (try(f_unif, silent = T)),
                 (try(f_pois, silent = T)),
                 (try(f_logis, silent = T)))

# Remover da lista as dist. de probabilidades com erro de ajuste
if (typeof(dist_list[[1]]) == "character") {dist_list = dist_list[-1]}
if (typeof(dist_list[[1]]) == "character") {dist_list = dist_list[-1]}
if (typeof(dist_list[[1]]) == "character") {dist_list = dist_list[-1]}
if (typeof(dist_list[[1]]) == "character") {dist_list = dist_list[-1]}
if (typeof(dist_list[[1]]) == "character") {dist_list = dist_list[-1]}
if (typeof(dist_list[[1]]) == "character") {dist_list = dist_list[-1]}
if (typeof(dist_list[[1]]) == "character") {dist_list = dist_list[-1]}
if (typeof(dist_list[[1]]) == "character") {dist_list = dist_list[-1]}
if (typeof(dist_list[[2]]) == "character") {dist_list = dist_list[-2]}
if (typeof(dist_list[[3]]) == "character") {dist_list = dist_list[-3]}
if (typeof(dist_list[[4]]) == "character") {dist_list = dist_list[-4]}
if (typeof(dist_list[[5]]) == "character") {dist_list = dist_list[-5]}
if (typeof(dist_list[[6]]) == "character") {dist_list = dist_list[-6]}
if (typeof(dist_list[[7]]) == "character") {dist_list = dist_list[-7]}
if (typeof(dist_list[[8]]) == "character") {dist_list = dist_list[-8]}

# Criar vetor de nomes
dist_name = c((try(f_gamma$distname, silent = T)),
              (try(f_lnorm$distname, silent = T)),
              (try(f_weibull$distname, silent = T)),
              (try(f_exp$distname, silent = T)),
              (try(f_norm$distname, silent = T)),
              (try(f_unif$distname, silent = T)),
              (try(f_pois$distname, silent = T)),
              (try(f_logis$distname, silent = T)))

# Remover do vetor as dist. de probabilidades com erro de ajuste
dist_name = dist_name[nchar(dist_name) < 10]

# Testes de aderencia
res = gofstat(dist_list,
              fitnames = c(dist_name))

# Resultados dos testes de aderencia 
res$ks                                     # valor do teste de Komolgorov-Smirnov
res$chisq                                  # valor do teste de Chi-quadrado
p = res$chisqpvalue; p                     # p-valor
nm = names(p)[which(p == (max(p)))]; nm    # melhor ajuste com base no p-valor

# Apresentar os parametros da melhor distribuicao
dist = (if (nm == "gamma") {f_gamma}
else if (nm == "lnorm") {f_lnorm}
else if (nm == "weibull") {f_weibull}
else if (nm == "exp") {f_exp}
else if (nm == "norm") {f_norm}
else if (nm == "unif") {f_unif}
else if (nm == "pois") {f_pois}
else {f_logis})
summary(dist)                              

# Gerar os graficos da melhor distribuiccao
plot.legend = c(nm)                        # legendas dos graficos

denscomp(list(dist),
         legendtext = plot.legend)         # gera o histograma de densidade

cdfcomp (list(dist),
         legendtext = plot.legend)         # gera o grafico da funcao densidade cumulativa      

qqcomp  (list(dist),
         legendtext = plot.legend)         # gera o grafico de quantil teorico x empirico

ppcomp  (list(dist),
         legendtext = plot.legend)         # gera o grafico de probabilidade teorica x empirica 

#------------------------------------------------------
library(leaflet)
library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(remotes)
library("XML")
library(rmapshaper)

# Importei os arquivos
getwd()
setwd("c:/Users/Pichau/Documents/MBA/Estatistica Espacial/Trabalho Final")

## Lendo o shapefile com os bairros da cidade de Recife
houston = read_sf(dsn = "Houston_City_Limit.shp")
houston

st_crs(houston)

ggplot(houston) +
  geom_sf(fill = "White") +
  theme_light() +
  coord_sf(datum = st_crs(32615))

base = read_csv("Base Houston.csv")
base

str(houston)
head(houston$ENTITY_NAM, 60)


# Plotando as localizacoes dos delitos
ggplot(data = houston) +
  geom_sf(fill = "White") +
  geom_point(data = base,
             aes(x = lon,
                 y = lat),
             colour = 'Dark Blue') +
  theme_light()

# Outra forma de plotar

library(ggmap)
qmplot(x = lon, 
       y = lat, 
       data = base,
       colour = I('red'), 
       size = I(1.5), 
       darken = 0.3)

# Pontos coloridos por tipo de crime (offense)
library(ggmap)
qmplot(x = lon, 
       y = lat, 
       data = base,
       colour = offense, 
       size = I(1.5), 
       darken = 0.3)

# Plotando as localizacoes dos delitos identificando os tipos dedelito
ggplot(data = houston) +
  geom_sf(fill = "White") +
  geom_point(data = base,
             aes(x = lon,
                 y = lat,
                 colour = offense),
             size = 0.9) +
  theme_light()

# Mapa 3
# Plotando as localizacoes pelo tipo do delitos em mapas separados
ggplot(data = houston) +
  geom_sf(fill = "White") +
  geom_point(data = base,
             aes(x = lon,
                 y = lat),
             colour = 'Dark Blue',
             size = 0.5) +
  theme_light() +
  facet_wrap(~offense)

#Modificando os titulos dos graficos
ggplot(data = houston) +
  geom_sf(fill = "White") +
  geom_point(data = base,
             aes(x = lon,
                 y = lat),
             colour = 'Dark Blue',
             size = 0.5) +
  theme_light() +
  facet_wrap(~offense,
             labeller = labeller(offense = c("aggravated assault" = "assalto grave",
                                          "auto theft" = "roubo de carro",
                                          "burglary" = "vandalismo",
                                          "murder" = "assassinato",
                                          "rape" = "estupro",
                                          "robbery" = "roubo")))

# -------------- Estimando a intensidade de um padrao de pontos  ---------------------#
#-------------------------------------------------------------------------------------#

#Carregando o pacote spatstat

library(spatstat)

#Checando o CRS de NYShp
st_crs(houston)
?as.owin
#Tentando criar o objeto owin
#owin - observation window
houston = as.owin(houston) #ERRO!!!!! CRS = 4326 ou WGS84

#Importando o shape como um Spatial Polygons
#Definindo o shapefile como uma janela onde os pontos serao plotados
#owin - observation window
library(maptools)
houston2 = st_read("Houston_City_Limit.shp")
st_crs(houston2)

houston2 = ms_simplify(input = houston2,
                      keep = 0.02,
                      keep_shapes = TRUE)

houston2 <- st_transform(houston2, crs = 32615)


## Simplificando o shape de NY


houston_owin <- as.owin(houston2)
st_crs(houston_owin)

base <- as.owin(base)

base2 = st_as_sf(x = base,
                   coords = c("lon","lat"),
                   crs = 4326)

base3 = st_transform(x = base2,
                       crs = 32615)

coordenadas = st_coordinates(x = base3)

#Visualiando o objeto coordenadas
head(coordenadas)

#Criando o padrao de pontos a ser plotado com houston owin (houston2)
houstonppp = ppp(x = coordenadas[,1], 
             y = coordenadas[,2], 
             window=houston_owin)

#Plotando o objeto owin
plot(houston_owin, axes = TRUE)

#Estimando o raio
#raio.est = bw.diggle(houstonppp)
#raio.est


st_crs(houston)$proj4string

#Estimando a intensidade com kernel Gaussiano
# houstonkde.g = density.ppp(x = houstonppp, 
                      # sigma = raio.est, 
                       #kernel ="gaussian")

#Plotando a intensidade estimada  
#plot(houstonkde.g, 
#     main="Funcao intensidade", 
#     cex.main=0.5)

#Realizando o teste de Clark-Evans para verificar nao aleatoriedade espacial completa
clarkevans.test(X = houstonppp)

#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(X = houstonppp, 
                alternative = "less")

#Estimando o efeito de primeira ordem (intensidade) usando diferentes kernels
Houston.q = density.ppp(x = houstonppp, 
                      sigma = 1, 
                      kernel = "quartic")
Houston.g = density.ppp(x = houstonppp, 
                      sigma = 0.01, 
                      kernel ="gaussian")
Houston.e = density.ppp(x = houstonppp, 
                      sigma = 0.01, 
                      kernel = "epanechnikov")
# todos ficam parecidos

?density.ppp
par(mfrow=c(2,2))
par(mar=c(2.5,2.5,1.5,.5))
plot(houstonppp, 
     pch=21, 
     cex=0.01, 
     bg="blue", main="Ocorrencias", 
     cex.main=0.5)
plot(Houston.q, 
     main="Kernel Quartico", 
     cex.main=0.00001)
plot(Houston.g, 
     main="Kernel Normal", 
     cex.main=0.5)
plot(Houston.e, 
     main="Kernel Epanechnikov", 
     cex.main=0.5)
par(mfrow=c(1,1))


#Avaliando o impacto de diferentes raios (tau)

houston.tau1 = density.ppp(x = houstonppp, 
                         sigma=300, 
                         kernel="gaussian")
houston.tau2 = density.ppp(x = houstonppp, 
                         sigma=500, 
                         kernel="gaussian")
houston.tau3 = density.ppp(x = houstonppp, 
                         sigma=700, 
                         kernel="gaussian")
houston.tau4 = density.ppp(x = houstonppp, 
                         sigma=900, 
                         kernel="gaussian")
houston.tau5 = density.ppp(x = houstonppp, 
                         sigma=1100, 
                         kernel="gaussian")

#Plotando os dados e as funcoes intensidades estimadas pelos diversos valores de sigma
par(mfrow=c(3,2))
plot(houstonppp, 
     pch=21, 
     cex=0.9, 
     bg="blue", 
     main="Ocorrencias", 
     cex.main=0.5)
plot(houston.tau1, 
     main="Sigma=300", 
     cex.main=0.5)
plot(houston.tau2, 
     main="Sigma=500", 
     cex.main=0.5)
plot(houston.tau3, 
     main="Sigma=700", 
     cex.main=0.5)
plot(houston.tau4, 
     main="Sigma=900", 
     cex.main=0.5)
plot(houston.tau5, 
     main="Sigma=1100", 
     cex.main=0.5)
par(mfrow=c(1,1))

# 500 parece melhor

# Outra forma de estimar a densidade

# Com o ggplot
ggplot(data = houston) +
  geom_sf(fill = "White") +
  geom_point(data = base,
             aes(x = lon, y = lat),
             colour = 'Dark Blue',
             size = 0.5) +
  theme_light() +
  stat_density2d(data = base,
                 aes(x = lon, y = lat, fill = ..level..),
                 alpha = 0.8,
                 h = 0.025,
                 n = 400,
                 geom = "polygon") +
  scale_fill_gradient(low = "Light Yellow", 
                      high= "Dark Red") +
  facet_wrap(~offense)

# Com o ggmap
density_ggmap = qmplot(x = lon, 
                       y = lat, 
                       data = base,
                       colour = I('red'), 
                       size = I(0.9), 
                       darken = 0.3) +
  stat_density2d(data = base,
                 aes(x = lon, y = lat, fill = ..level..),
                 alpha = 0.8,
                 h = 0.025,
                 n = 400,
                 geom = "polygon") +
  scale_fill_gradient(low = "black", 
                      high= "red")

density_ggmap + 
  scale_fill_gradient(low = "Light Yellow", 
                      high= "Dark Red") +
  facet_wrap(~offense)

#Estimando a funcao G
houston.G = Gest(houstonppp)

#Gest - estima a funcao de distribuicao G de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Estimando a funcao L
#houston.L = Lest(houstonppp)

#Lest - estima a funcao K de Ripley centrada de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Estimando a funcao F
houston.F = Fest(houstonppp)

#Fest - estima a funcao F de um padrao de pontos
#Argumento
#X - um objeto da classe ppp

#Plotando a funcao G
plot(houston.G, main="Funcao G")

#Plotando as funcoes G, K e F
par(mfrow = c(2,2))
par(mar=c(4,2.5,1.5,.5))
plot(houston.G, 
     main="Funcao G", 
     xlab = "metro")
#plot(houston.L, 
#     main="Funcao L", 
#     xlab = "metro")
plot(houston.F, 
     main="Funcao F", 
     xlab = "metro")
par(mfrow = c(1,1))

#-------------------------------------------------------------------------------------#
# ----------------------- Checando se e razoavel assumir CSR   -----------------------#
#-------------------------------------------------------------------------------------#

#Realizando o teste de Clark-Evans para verificar nao aleatoriedade espacial completa
clarkevans.test(X = houstonppp)

#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(X = houstonppp, 
                alternative = "less")

#clarkevans.test - realiza o teste de Clark-Evans
#Argumento
#X - um objeto da classe ppp
#alternative - hipotese alternativa do teste (default - padrao nao aleatorio)

#Realizando o teste de Clark-Evans para verificar regularidade espacial
# pvalor = 1 é regular
clarkevans.test(X = houstonppp, 
                alternative = "greater")


#Funcoes para estimar os envelopes das funcoes F, G e L
Env_Lest = envelope(Y = houstonppp, 
                    fun = Lest, 
                    nsim=10) #alto custo computacional
Env_Gest = envelope(Y = houstonppp, 
                    fun = Gest, 
                    nsim=10)
Env_Fest = envelope(Y = houstonppp, 
                    fun = Fest, 
                    nsim=10)

#envelope - Clacula as bandas de confianca das funcoes
#Argumento
#Y - um objeto da classe ppp
#fun - funcao que deseja computar as bandas de confianca
#nsim - numero de padroes de pontos simulados

#Plotando as funcoes e seus respectivos envelopes
par(mfrow=c(2,2))
plot(houstonppp, 
     pch=21, 
     cex=0.9, 
     bg="blue")
plot(Env_Lest, 
     main = "Envelope L")
plot(Env_Gest, 
     main = "Envelope G")
plot(Env_Fest, 
     main = "Envelope F")
par(mfrow=c(1,1))


# Avaliar para cada tipo de offense
table(base$offense)

assalto_grave <- base %>% filter(offense == "aggravated assault")

roubo_carro <- base %>% filter(offense == "auto theft")

roubo <- base %>% filter(offense == "burglary")

assassinato <- base %>% filter(offense == "murder")

estupro <- base %>% filter(offense == "rape")

assalto <- base %>% filter(offense == "robbery")

# assalto_grave
##
##

assalto_grave_geo = st_as_sf(x = assalto_grave,
                 coords = c("lon","lat"),
                 crs = 4326)

# transformando em crs projetadas para aplicar o test clark evans

assalto_grave_proj = st_transform(x = assalto_grave_geo,
                     crs = 32615)

coordenadas_ag = st_coordinates(x = assalto_grave_proj)

#Criando o padrao de pontos a ser plotado com houston owin (houston2)
agppp = ppp(x = coordenadas_ag[,1], 
                 y = coordenadas_ag[,2], 
                 window=houston_owin)

#Funcoes para estimar os envelopes das funcoes F, G e L
#Env_Lest = envelope(Y = houstonppp, 
#                    fun = Lest, 
#                    nsim=10) #alto custo computacional
Env_Gest = envelope(Y = agppp, 
                    fun = Gest, 
                    nsim=10)
Env_Fest = envelope(Y = agppp, 
                    fun = Fest, 
                    nsim=10)

#envelope - Clacula as bandas de confianca das funcoes
#Argumento
#Y - um objeto da classe ppp
#fun - funcao que deseja computar as bandas de confianca
#nsim - numero de padroes de pontos simulados

#Plotando as funcoes e seus respectivos envelopes
par(mfrow=c(2,2))
plot(agppp, 
     pch=21, 
     cex=0.9, 
     bg="blue")
plot(Env_Gest, 
     main = "Envelope G")
plot(Env_Fest, 
     main = "Envelope F")
par(mfrow=c(1,1))

#Realizando o teste de Clark-Evans para verificar nao aleatoriedade espacial completa
clarkevans.test(X = agppp)

# para aplicar este teste a base assalto_grave tem que estar em CRS 29193 quando for criar o ppp

#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(X = agppp, 
                alternative = "less")

# roubo_carro
##
##

roubo_carro_geo = st_as_sf(x = roubo_carro,
                             coords = c("lon","lat"),
                             crs = 4326)

# transformando em crs projetadas para aplicar o test clark evans

roubo_carro_proj = st_transform(x = roubo_carro_geo,
                                  crs = 32615)

coordenadas_ag = st_coordinates(x = roubo_carro_proj)

#Criando o padrao de pontos a ser plotado com houston owin (houston2)
rcppp = ppp(x = coordenadas_ag[,1], 
            y = coordenadas_ag[,2], 
            window=houston_owin)

#Funcoes para estimar os envelopes das funcoes F, G e L
#Env_Lest = envelope(Y = houstonppp, 
#                    fun = Lest, 
#                    nsim=10) #alto custo computacional
Env_Gest = envelope(Y = rcppp, 
                    fun = Gest, 
                    nsim=10)
Env_Fest = envelope(Y = rcppp, 
                    fun = Fest, 
                    nsim=10)

#envelope - Clacula as bandas de confianca das funcoes
#Argumento
#Y - um objeto da classe ppp
#fun - funcao que deseja computar as bandas de confianca
#nsim - numero de padroes de pontos simulados

#Plotando as funcoes e seus respectivos envelopes
par(mfrow=c(2,2))
plot(rcppp, 
     pch=21, 
     cex=0.9, 
     bg="blue")
plot(Env_Gest, 
     main = "Envelope G")
plot(Env_Fest, 
     main = "Envelope F")
par(mfrow=c(1,1))

#Realizando o teste de Clark-Evans para verificar nao aleatoriedade espacial completa
clarkevans.test(X = rcppp)

# para aplicar este teste a base assalto_grave tem que estar em CRS 29193 quando for criar o ppp

#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(X = rcppp, 
                alternative = "less")

# roubo
##
##

roubo_geo = st_as_sf(x = roubo,
                           coords = c("lon","lat"),
                           crs = 4326)

# transformando em crs projetadas para aplicar o test clark evans

roubo_proj = st_transform(x = roubo_geo,
                                crs = 32615)

coordenadas_r = st_coordinates(x = roubo_proj)

#Criando o padrao de pontos a ser plotado com houston owin (houston2)
rppp = ppp(x = coordenadas_r[,1], 
            y = coordenadas_r[,2], 
            window=houston_owin)

#Funcoes para estimar os envelopes das funcoes F, G e L
#Env_Lest = envelope(Y = houstonppp, 
#                    fun = Lest, 
#                    nsim=10) #alto custo computacional
Env_Gest = envelope(Y = rppp, 
                    fun = Gest, 
                    nsim=10)
Env_Fest = envelope(Y = rppp, 
                    fun = Fest, 
                    nsim=10)

#envelope - Clacula as bandas de confianca das funcoes
#Argumento
#Y - um objeto da classe ppp
#fun - funcao que deseja computar as bandas de confianca
#nsim - numero de padroes de pontos simulados

#Plotando as funcoes e seus respectivos envelopes
par(mfrow=c(2,2))
plot(rppp, 
     pch=21, 
     cex=0.9, 
     bg="blue")
plot(Env_Gest, 
     main = "Envelope G")
plot(Env_Fest, 
     main = "Envelope F")
par(mfrow=c(1,1))


#Realizando o teste de Clark-Evans para verificar nao aleatoriedade espacial completa
clarkevans.test(X = rppp)

# para aplicar este teste a base assalto_grave tem que estar em CRS 29193 quando for criar o ppp

#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(X = rppp, 
                alternative = "less")

# assassinato
##
##

assassinato_geo = st_as_sf(x = assassinato,
                     coords = c("lon","lat"),
                     crs = 4326)

# transformando em crs projetadas para aplicar o test clark evans

assassinato_proj = st_transform(x = assassinato_geo,
                          crs = 32615)

coordenadas_ass = st_coordinates(x = assassinato_proj)

#Criando o padrao de pontos a ser plotado com houston owin (houston2)
assppp = ppp(x = coordenadas_ass[,1], 
           y = coordenadas_ass[,2], 
           window=houston_owin)

#Funcoes para estimar os envelopes das funcoes F, G e L
#Env_Lest = envelope(Y = houstonppp, 
#                    fun = Lest, 
#                    nsim=10) #alto custo computacional
Env_Gest = envelope(Y = assppp, 
                    fun = Gest, 
                    nsim=10)
Env_Fest = envelope(Y = assppp, 
                    fun = Fest, 
                    nsim=10)

#envelope - Clacula as bandas de confianca das funcoes
#Argumento
#Y - um objeto da classe ppp
#fun - funcao que deseja computar as bandas de confianca
#nsim - numero de padroes de pontos simulados

#Plotando as funcoes e seus respectivos envelopes
par(mfrow=c(2,2))
plot(assppp, 
     pch=21, 
     cex=0.9, 
     bg="blue")
plot(Env_Gest, 
     main = "Envelope G")
plot(Env_Fest, 
     main = "Envelope F")
par(mfrow=c(1,1))

#Realizando o teste de Clark-Evans para verificar nao aleatoriedade espacial completa
clarkevans.test(X = assppp)

# para aplicar este teste a base assalto_grave tem que estar em CRS 29193 quando for criar o ppp

#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(X = assppp, 
                alternative = "less")

# estupro
##
##

estupro_geo = st_as_sf(x = estupro,
                           coords = c("lon","lat"),
                           crs = 4326)

# transformando em crs projetadas para aplicar o test clark evans

estupro_proj = st_transform(x = estupro_geo,
                                crs = 32615)

coordenadas_est = st_coordinates(x = estupro_proj)

#Criando o padrao de pontos a ser plotado com houston owin (houston2)
estppp = ppp(x = coordenadas_est[,1], 
             y = coordenadas_est[,2], 
             window=houston_owin)

#Funcoes para estimar os envelopes das funcoes F, G e L
#Env_Lest = envelope(Y = houstonppp, 
#                    fun = Lest, 
#                    nsim=10) #alto custo computacional
Env_Gest = envelope(Y = estppp, 
                    fun = Gest, 
                    nsim=10)
Env_Fest = envelope(Y = estppp, 
                    fun = Fest, 
                    nsim=10)

#envelope - Clacula as bandas de confianca das funcoes
#Argumento
#Y - um objeto da classe ppp
#fun - funcao que deseja computar as bandas de confianca
#nsim - numero de padroes de pontos simulados

#Plotando as funcoes e seus respectivos envelopes
par(mfrow=c(2,2))
plot(estppp, 
     pch=21, 
     cex=0.9, 
     bg="blue")
plot(Env_Gest, 
     main = "Envelope G")
plot(Env_Fest, 
     main = "Envelope F")
par(mfrow=c(1,1))

#Realizando o teste de Clark-Evans para verificar nao aleatoriedade espacial completa
clarkevans.test(X = estppp)

# para aplicar este teste a base assalto_grave tem que estar em CRS 29193 quando for criar o ppp

#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(X = estppp, 
                alternative = "less")

# assalto
##

assalto_geo = st_as_sf(x = assalto,
                       coords = c("lon","lat"),
                       crs = 4326)

# transformando em crs projetadas para aplicar o test clark evans

assalto_proj = st_transform(x = assalto_geo,
                            crs = 32615)

coordenadas_assalto = st_coordinates(x = assalto_proj)

#Criando o padrao de pontos a ser plotado com houston owin (houston2)
assaltoppp = ppp(x = coordenadas_assalto[,1], 
             y = coordenadas_assalto[,2], 
             window=houston_owin)

#Funcoes para estimar os envelopes das funcoes F, G e L
#Env_Lest = envelope(Y = houstonppp, 
#                    fun = Lest, 
#                    nsim=10) #alto custo computacional
Env_Gest = envelope(Y = assaltoppp, 
                    fun = Gest, 
                    nsim=10)
Env_Fest = envelope(Y = assaltoppp, 
                    fun = Fest, 
                    nsim=10)

#envelope - Clacula as bandas de confianca das funcoes
#Argumento
#Y - um objeto da classe ppp
#fun - funcao que deseja computar as bandas de confianca
#nsim - numero de padroes de pontos simulados

#Plotando as funcoes e seus respectivos envelopes
par(mfrow=c(2,2))
plot(assaltoppp, 
     pch=21, 
     cex=0.9, 
     bg="blue")
plot(Env_Gest, 
     main = "Envelope G")
plot(Env_Fest, 
     main = "Envelope F")
par(mfrow=c(1,1))

#Realizando o teste de Clark-Evans para verificar nao aleatoriedade espacial completa
clarkevans.test(X = assaltoppp)

# para aplicar este teste a base assalto_grave tem que estar em CRS 29193 quando for criar o ppp

#Realizando o teste de Clark-Evans para verificar agregacao espacial
clarkevans.test(X = assaltoppp, 
                alternative = "less")
# todos apresentaram algum padrao de agrupamento de acordo com o teste clark evans e corroboram com a nao aleatorieadade espacial completa

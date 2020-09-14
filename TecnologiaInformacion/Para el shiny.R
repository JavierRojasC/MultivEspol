library(shinydashboard)
library(shiny)
library(corrplot)
library(highcharter)
library(plotly)
library(corrr) 
library(ggplot2)
library(psych)
library(stats)
library(factoextra)
library(reshape2)
library(vegan)
library(MASS)
library(dplyr)
library(shinythemes)
library(readr)

eq_tec <- read_delim('https://raw.githubusercontent.com/JavierRojasC/MultivEspol/master/TecnologiaInformacion/Equipamiento%20Tec%20(1).txt', 
                                  "\t", col_types = cols(`Computadora portatil` = col_double()), 
                                  locale = locale(grouping_mark = "."), 
                                  trim_ws = TRUE)



internet_1_ <- read_delim("https://raw.githubusercontent.com/JavierRojasC/MultivEspol/master/TecnologiaInformacion/internet%20(1).txt", 
                          "\t", escape_double = FALSE, col_names = FALSE, 
                          locale = locale(decimal_mark = ","), 
                          trim_ws = TRUE)


pob_celyred <- read_delim("https://raw.githubusercontent.com/JavierRojasC/MultivEspol/master/TecnologiaInformacion/Poblacion%20cel%20y%20red%20(1).txt", 
                                     "\t", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                     trim_ws = TRUE)

lugar_int <- read_delim("https://raw.githubusercontent.com/JavierRojasC/MultivEspol/master/TecnologiaInformacion/Lugar%20uso%20Internet%20(1).txt", 
                                    "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                 encoding = "ISO-8859-1"), trim_ws = TRUE)
#leer los datos de "LUGAR USO DE INTERNET"


#Datos "EQUIPAMIENTO TECNOLOGICO A NIVEL NACIONAL"

names(eq_tec)=c("Periodo","Comp de escritorio","Comp portatil","Comp escritorio y portatil","Telf fija","Telf celular")

##Datos de poblacion con celular y redes sociales

#pob_celyred=read.delim("Poblacion cel y red.txt",dec = ",",header = T)
names(pob_celyred)=c("Des","Pob Total","Pob mayor a 5","Pob cel act","Pob smartphone","Pob smartphone y red")


###########################################################################

l2=dcast(lugar_int,Periodo~Desagregacion, value.var = "Hogar")

#INdicadores de acceso a la tecnologia en el area urbana y rural en el mes de diciembre
hchart(l2, "line", hcaes(x = Periodo, y =Urbana )) %>% hc_add_theme(hc_theme_google())
hchart(l2, "line", hcaes(x = Periodo, y =Rural)) %>% hc_add_theme(hc_theme_google())

#####################################################################

#Martiz de correalcion de los datos "LUGAR USO DE INTERNET"
cor_lg=round(cor(lugar_int[,3:8]),2)
hchart(cor_lg, "treemap", hcaes(value = n, color = unique))

#Martiz de correalcion de los datos "EQUIPAMIENTO TECNOLOGICO A NIVEL NACIONAL"
cor_eq=round(cor(eq_tec[,2:6]),2)
hchart(cor_eq, "treemap", hcaes(value = n, color = unique))

#Martiz de correalcion de los datos "Poblacion con celular y redes sociales"
cor_pb=round(cor(pob_celyred[,2:6]),2)
hchart(cor_pb, "treemap", hcaes(value = n, color = unique))

#############################################################

#Analis de componentes principales "LUGAR USO DE INTERNET"

#sin rotar
ACProt= principal(lugar_int[,3:8],nfactors=2,rotate = "none",use = "pairwise")


scores <- ACProt$scores
loads <- ACProt$loadings
scale.loads <- 4
x <- scores[,1]
y <- scores[,2]
plt_cp <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP") 
plt_cp<- plt_cp %>% layout(title="Biplot Componentes Principales")

for (k in 1:nrow(loads)){
  x <- c(0, loads[k,1])*scale.loads
  y <- c(0, loads[k,2])*scale.loads
  plt_cp<- plt_cp%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                               opacity = 1,name= colnames(lugar_int[,3:8])[k])
}

plt_cp



#rotado varimax
ACProtvar= principal(lugar_int[,3:8],nfactors=2,rotate = "varimax",use = "pairwise")#equivalente al anterior

scores <- ACProtvar$scores
loads <- ACProtvar$loadings
scale.loads <- 4
x <- scores[,1]
y <- scores[,2]
plt_cpv <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP") 
plt_cpv<- plt_cpv %>% layout(title="Biplot Componentes Principales Rotacion Varimax")

for (k in 1:nrow(loads)){
  x <- c(0, loads[k,1])*scale.loads
  y <- c(0, loads[k,2])*scale.loads
  plt_cpv<- plt_cpv%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                 opacity = 1,name= colnames(lugar_int[,3:8])[k])
}

plt_cpv

#Analis de componentes principales "EQUIPAMIENTO TECNOLOGICO A NIVEL NACIONAL"
#sin rotar
ACProt2= principal(eq_tec[,2:6],nfactors=2,rotate = "none",use = "pairwise")

scores <- ACProt2$scores
loads <- ACProt2$loadings
scale.loads <- 4
x <- scores[,1]
y <- scores[,2]
plt_cp2 <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP") 
plt_cp2<- plt_cp2 %>% layout(title="Biplot Componentes Principales")

for (k in 1:nrow(loads)){
  x <- c(0, loads[k,1])*scale.loads
  y <- c(0, loads[k,2])*scale.loads
  plt_cp2<- plt_cp2%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                 opacity = 1,name= colnames(eq_tec[,2:6])[k])
}

plt_cp2

#rotado varimax
ACProtvar2= principal(eq_tec[,2:6],nfactors=2,rotate = "varimax",use = "pairwise")#equivalente al anterior

scores <- ACProtvar2$scores
loads <- ACProtvar2$loadings
scale.loads <- 4
x <- scores[,1]
y <- scores[,2]
plt_cpv2 <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP") 
plt_cpv2<- plt_cpv2 %>% layout(title="Biplot Componentes Principales Rotacion Varimax")

for (k in 1:nrow(loads)){
  x <- c(0, loads[k,1])*scale.loads
  y <- c(0, loads[k,2])*scale.loads
  plt_cpv2<- plt_cpv2%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                   opacity = 1,name= colnames(eq_tec[,2:6])[k])
}

plt_cpv2

#Analis de componentes principales "Poblacion con celular y redes sociales"
#sin rotar
ACProt3= principal(pob_celyred[,2:6],nfactors=2,rotate = "none",use = "pairwise")


scores <- ACProt3$scores
loads <- ACProt3$loadings
scale.loads <- 4
x <- scores[,1]
y <- scores[,2]
plt_cp3 <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP") 
plt_cp3<- plt_cp3 %>% layout(title="Biplot Componentes Principales")

for (k in 1:nrow(loads)){
  x <- c(0, loads[k,1])*scale.loads
  y <- c(0, loads[k,2])*scale.loads
  plt_cp3<- plt_cp3%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                 opacity = 1,name= colnames(pob_celyred[,2:6])[k])
}

plt_cp3

#rotado varimax
ACProtvar3= principal(pob_celyred[,2:6],nfactors=2,rotate = "varimax",use = "pairwise")#equivalente al anterior


scores <- ACProtvar3$scores
loads <- ACProtvar3$loadings
scale.loads <- 4
x <- scores[,1]
y <- scores[,2]
plt_cpv3 <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP") 
plt_cpv3<- plt_cpv3 %>% layout(title="Biplot Componentes Principales Rotacion Varimax")

for (k in 1:nrow(loads)){
  x <- c(0, loads[k,1])*scale.loads
  y <- c(0, loads[k,2])*scale.loads
  plt_cpv3<- plt_cpv3%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                   opacity = 1,name= colnames(pob_celyred[,2:6])[k])
}

plt_cpv3


#######################################################
#analisis factorial "LUGAR USO DE INTERNET"
# Ejes principales
fit.pa<-fa(lugar_int[,3:8],nfactors=2,fm='pa',rotate='none',scores='regression',n.obs=220)

scores <- fit.pa$scores
loads <- fit.pa$loadings
scale.loads <- 4
x <- scores[,1]
y <- scores[,2]
plt_af <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP") 
plt_af<- plt_af %>% layout(title="Biplot Analisis Factorial")

for (k in 1:nrow(loads)){
  x <- c(0, loads[k,1])*scale.loads
  y <- c(0, loads[k,2])*scale.loads
  plt_af<- plt_af%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                               opacity = 1,name= colnames(lugar_int[,3:8])[k])
}

plt_af



#rotacion varimax
fit.pa.rot.var<-fa(lugar_int[,3:8],nfactors=2,fm='pa',rotate='varimax',n.obs=220)


scoresv <- fit.pa.rot.var$scores
loadsv <- fit.pa.rot.var$loadings
scale.loads <- 4
xv <- scoresv[,1]
yv <- scoresv[,2]


plt_af_vr <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP")
plt_af_vr<-plt_af_vr%>% layout(title="Biplot Analisis Factorial rotacion Varimax")
 

for (k in 1:nrow(loadsv)){
  x <- c(0, loadsv[k,1])*scale.loads
  y <- c(0, loadsv[k,2])*scale.loads
  plt_af_vr<- plt_af_vr%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                     opacity = 1,name= colnames(lugar_int[,3:8])[k])
}

plt_af_vr

#analisis factorial "EQUIPAMIENTO TECNOLOGICO A NIVEL NACIONAL"

# Ejes principales
fit.pa2<-fa(eq_tec[,2:6],nfactors=2,fm='pa',rotate='none',scores='regression',n.obs=220)

scores2 <- fit.pa2$scores
loads2 <- fit.pa2$loadings
scale.loads <- 4
x2 <- scores2[,1]
y2 <- scores2[,2]

plt_af2 <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP")
plt_af2<- plt_af2 %>% layout(title="Biplot Analisis Factorial")
for (k in 1:nrow(loads2)){
  x <- c(0, loads2[k,1])*scale.loads
  y <- c(0, loads2[k,2])*scale.loads
  plt_af2<- plt_af2%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                 opacity = 1,name= colnames(eq_tec[,2:6])[k])
}

plt_af2

#rotacion varimax
fit.pa.rot.var2<-fa(eq_tec[,2:6],nfactors=2,fm='pa',rotate='varimax',n.obs=220)

scores2v <- fit.pa.rot.var2$scores
loads2v <- fit.pa.rot.var2$loadings
scale.loads <- 4
xv2 <- scores2v[,1]
yv2 <- scores2v[,2]

plt_af2v <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP")
plt_af2v<- plt_af2v %>% layout(title="Biplot Analisis Factorial")

for (k in 1:nrow(loads2v)){
  x <- c(0, loads2v[k,1])*scale.loads
  y <- c(0, loads2v[k,2])*scale.loads
  plt_af2v<- plt_af2v%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                   opacity = 1,name= colnames(eq_tec[,2:6])[k])
}

plt_af2v

#analisis factorial "Poblacion con celular y redes sociales"
# Ejes principales
fit.pa3<-fa(pob_celyred[,2:6],nfactors=2,fm='pa',rotate='none',scores='regression',n.obs=220)

scores3 <- fit.pa3$scores
loads3 <- fit.pa3$loadings
scale.loads <- 4
x3 <- scores3[,1]
y3 <- scores3[,2]

plt_af3 <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP")
plt_af3<- plt_af3 %>% layout(title="Biplot Analisis Factorial")
for (k in 1:nrow(loads3)){
  x <- c(0, loads3[k,1])*scale.loads
  y <- c(0, loads3[k,2])*scale.loads
  plt_af3<- plt_af3%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                 opacity = 1,name= colnames(pob_celyred[,2:6])[k])
}

plt_af3



#rotacion varimax
fit.pa.rot.var3<-fa(pob_celyred[,2:6],nfactors=2,fm='pa',rotate='varimax',n.obs=220)

scores3v <- fit.pa.rot.var3$scores
loads3v <- fit.pa.rot.var3$loadings
scale.loads <- 4
xv3 <- scores3v[,1]
yv3 <- scores3v[,2]

plt_af3v <- plot_ly() %>%
  add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                  colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Cargas del ACP")
plt_af3v<- plt_af3v %>% layout(title="Biplot Analisis Factorial")
for (k in 1:nrow(loads3v)){
  x <- c(0, loads3v[k,1])*scale.loads
  y <- c(0, loads3v[k,2])*scale.loads
  plt_af3v<- plt_af3v%>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                   opacity = 1,name= colnames(pob_celyred[,2:6])[k])
}

plt_af3v

##########################################################

#Analsis multidimencional "LUGAR USO DE INTERNET"

dist1=vegdist(lugar_int[,3:8],method="jaccard",diag=T) 
fi=isoMDS(dist1,k=2)

loads <- lugar_int[,3:8]

scale.loads <- 4
x <- fi$points[,1]
y <- fi$points[,2]
plt_ad <- plot_ly() %>%
  add_trace(x=x, y=y,type='scatter', mode="markers",
            marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),
            text=~paste("Fecha:",lugar_int$Periodo))
plt_ad<- plt_ad %>% layout(title="Biplot de Analisis Multidimensional")

plt_ad

#Grafico Sheppard
shep=Shepard(as.dist(dist1),fi$points)
#plot(shep)



x <- shep$x
y <- shep$y
plt_shep <- plot_ly() %>%
  add_trace(x=x, y=y,type='scatter', mode="markers",
            marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7))%>% 
  layout(title="Grafico Sheppard")
plt_shep

#Analsis multidimencional "EQUIPAMIENTO TECNOLOGICO A NIVEL NACIONAL"

dist2=vegdist(eq_tec[,2:6],method="jaccard",diag=T) 
fi2=isoMDS(dist2,k=2)


loads <- (eq_tec[,2:6])

scale.loads <- 4
x <- fi2$points[,1]
y <- fi2$points[,2]
plt_ad2 <- plot_ly() %>%
  add_trace(x=x, y=y,type='scatter', mode="markers",
            marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),
            text=~paste("Fecha:",eq_tec$Periodo))
plt_ad2<- plt_ad2 %>% layout(title="Biplot de Analisis Multidimensional")

plt_ad2

#Grafico Sheppard
shep2=Shepard(as.dist(dist2),fi2$points)
#plot(shep)


x <- shep2$x
y <- shep2$y
plt_shep2 <- plot_ly() %>%
  add_trace(x=x, y=y,type='scatter', mode="markers",
            marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7))%>% 
  layout(title="Grafico Sheppard")
plt_shep2



#Analsis multidimencional "Poblacion con celular y redes sociales"

dist3=vegdist(pob_celyred[,2:6],method="jaccard",diag=T) 
fi3=isoMDS(dist3,k=2)

loads <- (pob_celyred[,2:6])

scale.loads <- 4
x <- fi3$points[,1]
y <- fi3$points[,2]
plt_ad3 <- plot_ly() %>%
  add_trace(x=x, y=y,type='scatter', mode="markers",
            marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),
            text=~paste("Fecha:",pob_celyred$Des))
plt_ad3<- plt_ad3 %>% layout(title="Biplot de Analisis Multidimensional")

plt_ad3

#Grafico Sheppard
shep3=Shepard(as.dist(dist3),fi3$points)
#plot(shep)


x <- shep3$x
y <- shep3$y
plt_shep3 <- plot_ly() %>%
  add_trace(x=x, y=y,type='scatter', mode="markers",
            marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7))%>% 
  layout(title="Grafico Sheppard")

plt_shep3

#################################################
#Analisis de correspondencia Lugar uso de Internet
matriz1=lugar_int[,3:8]
matriz1=as.matrix(matriz1)
matriz1=as.table(matriz1)
library(ca)
acs=ca(matriz1)


gr=data.frame(acs$colnames,acs$colcoord[,1],acs$colcoord[,2],acs$colmass)
colnames(gr)=c("Desagregacion","Dim1","Dim2","Size")
hchart(gr, "bubble",hcaes(x =gr$Dim1,y=gr$Dim2,group=gr$Desagregacion,size=gr$Size)) %>% 
  hc_title(text ="Análisis de Correspondencia Lugar uso de Internet")%>% 
  hc_xAxis(title=list(text="Dimension 1"))%>% hc_yAxis(title=list(text="Dimension 2"))

#Analisis de correspondencia  EQUIPAMIENTO TECNOLOGICO A NIVEL NACIONAL
matriz2=eq_tec[,2:6]
matriz2=as.matrix(matriz2)
matriz2=as.table(matriz2)
library(ca)
acs2=ca(matriz2)
gr2=data.frame(acs2$colnames,acs2$colcoord[,1],acs2$colcoord[,2],acs2$colmass)
colnames(gr2)=c("Desagregacion","Dim1","Dim2","Size")
hchart(gr2, "bubble",hcaes(x =gr2$Dim1,y=gr2$Dim2,group=gr2$Desagregacion,size=gr2$Size)) %>% 
  hc_title(text ="Análisis de Correspondencia Equipamiento tecnologico a nivel Nacional ")%>% 
  hc_xAxis(title=list(text="Dimension 1"))%>% hc_yAxis(title=list(text="Dimension 2"))

#Analisis de correspondencia  Poblacion con uso de celulares y redes sociales

matriz3=pob_celyred[,2:6]
matriz3=as.matrix(matriz3)
matriz3=as.table(matriz3)

library(ca)
acs3=ca(matriz3)
gr3=data.frame(acs3$colnames,acs3$colcoord[,1],acs3$colcoord[,2],acs3$colmass)
colnames(gr3)=c("Desagregacion","Dim1","Dim2","Size")
hchart(gr3, "bubble",hcaes(x =gr3$Dim1,y=gr3$Dim2,group=gr3$Desagregacion,size=gr3$Size)) %>% 
  hc_title(text ="Análisis de Correspondencia  Poblacion con uso de celulares y redes sociales")%>% 
  hc_xAxis(title=list(text="Dimension 1"))%>% hc_yAxis(title=list(text="Dimension 2"))





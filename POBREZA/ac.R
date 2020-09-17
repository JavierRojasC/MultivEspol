library(haven)
library(foreign)
library(dplyr)
library(highcharter)


datos_emp_subempr<-read.csv2("https://raw.githubusercontent.com/JavierRojasC/MultivEspol/master/POBREZA/1.2.4.extpob_ciudades%20(1).csv", header= T,dec=".",sep=";")

#Madison
agrup_datos_emp_subempr=data.frame(datos_emp_subempr[c(4:29),c(1:17)]) 
names(agrup_datos_emp_subempr)[names(agrup_datos_emp_subempr) == 'Índice'] <- 'Indice'
agrup_datos_emp_subempr$X.1=make_date(agrup_datos_emp_subempr$X.1)
agrup_datos_emp_subempr=agrup_datos_emp_subempr[order(as.Date(agrup_datos_emp_subempr$X.1, format="Year")),] #Ordenar por Periodo

datos_sever_ext_pob=data.frame(agrup_datos_emp_subempr$X.1,agrup_datos_emp_subempr$X.4,agrup_datos_emp_subempr$X.7,agrup_datos_emp_subempr$X.9,agrup_datos_emp_subempr$X.12,agrup_datos_emp_subempr$X.15)
colnames(datos_sever_ext_pob)=c("Periodo","Pichincha","Guayas","Azuay","El Oro","Tungurahua")
datos_sever_ext_pob$Guayas=as.numeric(datos_sever_ext_pob$Guayas)
datos_sever_ext_pob$Pichincha=as.numeric(datos_sever_ext_pob$Pichincha)
datos_sever_ext_pob$Azuay=as.numeric(datos_sever_ext_pob$Azuay)
datos_sever_ext_pob$`El Oro`=as.numeric(datos_sever_ext_pob$`El Oro`)
datos_sever_ext_pob$`Tungurahua`=as.numeric(datos_sever_ext_pob$`Tungurahua`)

datos_sever_ext_pob=data.frame(datos_sever_ext_pob %>% group_by(Ano=year(Periodo)) %>% summarise(mean(Pichincha),             
                                                                                                 mean(Guayas), mean(Azuay), mean(`El Oro`),mean(Tungurahua)))
colnames(datos_sever_ext_pob)=c("Periodo","Pichincha","Guayas","Azuay","El Oro","Tungurahua")
usr1=datos_sever_ext_pob[,-1]
rownames(usr1)=datos_sever_ext_pob$Periodo
usr1=ca(usr1)
gr=data.frame(usr1$colnames,usr1$colcoord[,1],usr1$colcoord[,2],usr1$colmass)
colnames(gr)=c("Ciudades","Dim1","Dim2","Size")
AC1 <- hchart(gr, "bubble",hcaes(x =gr$Dim1,y=gr$Dim2,group=gr$Ciudades,size=gr$Size)) %>% 
  hc_title(text ="Analisís de Correspondencia")%>% 
  hc_xAxis(title=list(text="Dimension 1"))%>% hc_yAxis(title=list(text="Dimension 2"))



grm=data.frame(usr1$rownames,usr1$rowcoord[,1],usr1$rowcoord[,2],usr1$rowmass)
colnames(grm)=c("Periodos","Dim1","Dim2","Size")
AC2 <- hchart(grm, "bubble",hcaes(x =grm$Dim1,y=grm$Dim2,group=grm$Periodos,size=grm$Size)) %>% 
  hc_title(text ="Analisís de Correspondencia")%>% 
  hc_xAxis(title=list(text="Dimension 1"))%>% hc_yAxis(title=list(text="Dimension 2"))













datos_pob_ciu=read.csv2("https://raw.githubusercontent.com/JavierRojasC/MultivEspol/master/POBREZA/1.1.4.pobre_ciudades%20(1).csv", header= T,dec=".",sep=";", fileEncoding="latin1")
agrup_datos_pob_ciu=data.frame(datos_pob_ciu[c(4:29),c(2:17)])
names(agrup_datos_pob_ciu)[names(agrup_datos_pob_ciu) == 'Índice'] <- 'Indice'
agrup_datos_pob_ciu$X.1=make_date(agrup_datos_pob_ciu$X.1)

agrup_datos_pob_ciu=agrup_datos_pob_ciu[order(as.Date(agrup_datos_pob_ciu$X.1, format="Year")),] #Ordenar por fecha

datos_severidad_pob_ciu=data.frame(agrup_datos_pob_ciu$X.1,agrup_datos_pob_ciu$X.4,agrup_datos_pob_ciu$X.6,agrup_datos_pob_ciu$X.9,agrup_datos_pob_ciu$X.12,agrup_datos_pob_ciu$X.15)
colnames(datos_severidad_pob_ciu)=c("Periodo","Pichincha","Guayas","Azuay","El Oro","Tungurahua")


datos_severidad_pob_ciu$Guayas=as.numeric(datos_severidad_pob_ciu$Guayas)
datos_severidad_pob_ciu$Pichincha=as.numeric(datos_severidad_pob_ciu$Pichincha)
datos_severidad_pob_ciu$Azuay=as.numeric(datos_severidad_pob_ciu$Azuay)
datos_severidad_pob_ciu$`El Oro`=as.numeric(datos_severidad_pob_ciu$`El Oro`)
datos_severidad_pob_ciu$`Tungurahua`=as.numeric(datos_severidad_pob_ciu$`Tungurahua`)


datos_severidad_pob_ciu=data.frame(datos_severidad_pob_ciu %>% group_by(Periodo=year(Periodo)) %>%summarise(mean(`Pichincha`),             
                                                                                                            mean(Guayas), mean(Azuay), mean(`El Oro`),mean(`Tungurahua`)))
colnames(datos_severidad_pob_ciu)=c("Periodo","Pichincha","Guayas","Azuay","El Oro","Tungurahua")


usr2=datos_severidad_pob_ciu[,-1]
rownames(usr2)=datos_severidad_pob_ciu$Periodo
usr2=ca(usr2)
gr2=data.frame(usr2$colnames,usr2$colcoord[,1],usr2$colcoord[,2],usr2$colmass)
colnames(gr2)=c("Ciudades","Dim1","Dim2","Size")
AC3 <- hchart(gr2, "bubble",hcaes(x =gr2$Dim1,y=gr2$Dim2,group=gr2$Ciudades,size=gr2$Size)) %>% 
  hc_title(text ="Analisís de Correspondencia ")%>% 
  hc_xAxis(title=list(text="Dimension 1"))%>% hc_yAxis(title=list(text="Dimension 2"))

grm2=data.frame(usr2$rownames,usr2$rowcoord[,1],usr2$rowcoord[,2],usr2$rowmass)
colnames(grm2)=c("Periodos","Dim1","Dim2","Size")
AC4 <- hchart(grm2, "bubble",hcaes(x =grm2$Dim1,y=grm2$Dim2,group=grm2$Periodos,size=grm2$Size)) %>% 
  hc_title(text ="Analisís de Correspondencia ")%>% 
  hc_xAxis(title=list(text="Dimension 1"))%>% hc_yAxis(title=list(text="Dimension 2"))















datos_hogar<-read.spss("https://github.com/JavierRojasC/MultivEspol/blob/master/POBREZA/enemdu_viv_hog_201912%20(1).sav?raw=true",to.data.frame=T)

(cont_tip_viv=data.frame(table(datos_hogar$vi02,datos_hogar$area)))

viv1 <- hchart(cont_tip_viv, "column", hcaes(x =cont_tip_viv$Var2,y=cont_tip_viv$Freq, group=cont_tip_viv$Var1)) %>% 
  hc_title(text ="Histograma de Tipos de Vivienda según el Area") %>%
  hc_xAxis(title=list(text="Area")) %>% hc_yAxis(title=list(text="Cantidad de Tipo de Vivienda"))





datos_consumidor<-read.spss("https://github.com/JavierRojasC/MultivEspol/blob/master/POBREZA/enemdu_consumidor_201912%20(1).sav?raw=true",to.data.frame=T)

(cont_sit_hog=(data.frame(table(datos_consumidor$area,datos_consumidor$c02))))

viv2 <- hchart(cont_sit_hog, "column", hcaes(x =cont_sit_hog$Var1,y=cont_sit_hog$Freq, group=cont_sit_hog$Var2)) %>%
  hc_title(text ="Situación económica del hogar en los proximos 3 meses según los encuestados por Area") %>%
  hc_xAxis(title=list(text="Area"))%>% hc_yAxis(title=list(text="Cantidad de personas"))




datos_persona<-read.spss("https://github.com/JavierRojasC/MultivEspol/blob/master/POBREZA/enemdu_persona_201912%20(1).sav?raw=true",to.data.frame=T)

(cont_sit_pob<-data.frame(table(datos_persona$pobreza,datos_persona$dominio)))
(cont_sit_pob=cont_sit_pob[cont_sit_pob$Var1 == "POBRE",])
contsit <- cont_sit_pob[-6,]

viv3 <- highchart()%>%
  hc_add_series(contsit, type="item", hcaes(x =contsit$Var2,y=contsit$Freq),showInLegend = TRUE,
       size = "120%", center = list("50%", "88%"),startAngle = -100,endAngle  = 100) %>% 
  hc_colors(colors = c('#EB001F', '#64A12D','#008AC5','#9203FC', '#FC9A03' )) %>%  
  hc_title(text ="Respuesta a la pregunta: Pobreza en las principales ciudades")%>% 
  hc_legend(labelFormat = '{name}: <span style="opacity: 0.5">{y}</span>')

highchart()%>%
  hc_add_series(contsit, type="item", hcaes(x =contsit$Var2,y=contsit$Freq),showInLegend = TRUE,size = "120%",
                center = list("50%", "88%"))

#Comparativa de Pobreza e Indigencia en todo el Ecuador
comp_pob_ind<-data.frame(table(datos_persona$dominio,datos_persona$epobreza,datos_persona$pobreza))
(comp_pob_ind<-comp_pob_ind[comp_pob_ind$Var3 == "POBRE",])
(comp_pob_ind<-comp_pob_ind[c(-6,-12),])

viv4 <- hchart(comp_pob_ind, "column", hcaes(x =comp_pob_ind$Var2,y=comp_pob_ind$Freq, group=comp_pob_ind$Var1)) %>% 
  hc_title(text ="Comparativa entre Pobreza e Indigencia")%>%
  hc_xAxis(title=list(text="Área"),categories=(text=c("Pobreza","Indigencia"))) %>% 
  hc_yAxis(title=list(text="Cantidad de personas"))




(cont_sit_prox=(data.frame(table(datos_consumidor$c19))))


viv5<-hchart(cont_sit_prox, "item", hcaes(label=cont_sit_prox$Var1,colors = c('#EB001F', '#64A12D','#008AC5' ),name=cont_sit_prox$Var1,y=cont_sit_prox$Freq),showInLegend = TRUE,
             size = "120%",center = list("50%", "88%"),startAngle = -100,endAngle  = 100) %>% 
  hc_title(text ="Respuesta a la pregunta: <i>Situación económica del país en los proximos 3 meses</i>") %>% 
  hc_legend(labelFormat = '{name}: <span style="opacity: 0.5">{y}</span>')
viv5


cont_cont_pon=table(datos_persona$condact,datos_persona$pobreza)
cont_cont_pon1=data.frame(cont_cont_pon)

viv6 <- hchart(cont_cont_pon1, "bar", hcaes(x=cont_cont_pon1$Var1,y=cont_cont_pon1$Freq,group=cont_cont_pon1$Var2)) %>% 
  hc_title(text ="Barras de  Condición Laboral según la Situación Socio-Económica")%>%
  hc_xAxis(title=list(text="Condición Laboral"))%>% hc_yAxis(title=list(text="Cantidad de Respuestas")) 
viv6




(cont_niv_edu=data.frame(table(datos_persona$p10a,datos_persona$area)))

viv7 <- hchart(cont_niv_edu, "bar", hcaes(x=cont_niv_edu$Var2,y=cont_niv_edu$Freq, group=cont_niv_edu$Var1)) %>% 
  hc_add_theme(hc_theme_gridlight())%>% hc_title(text ="Barras de Nivel de Educacion")%>%
  hc_xAxis(title=list(text="Area"))%>% hc_yAxis(title=list(text="Cantidad de Personas Encuestadas"))







(cont_niv_grad=(data.frame(table(datos_persona$p10a,datos_persona$p10b)[-1,])))

viv8 <- hchart(cont_niv_grad, "bar", hcaes(x=cont_niv_grad$Var1,y=cont_niv_grad$Freq, group=cont_niv_grad$Var2)) %>% 
  hc_add_theme(hc_theme_gridlight()) %>% hc_title(text ="Barras Segun Grado Alcanzado")%>%
  hc_xAxis(title=list(text="Area"))%>% hc_yAxis(title=list(text="Cantidad de Personas Encuestadas"))



(cont_raz_clases=(data.frame(table(datos_persona$p09))))

viv9 <- hchart(cont_raz_clases , "lollipop", hcaes(x=cont_raz_clases$Var1,y=cont_raz_clases$Freq))  %>%
  hc_title(text ="Gráfica de razones que intervienen para no concluir los estudios de los encuestados") %>%
  hc_xAxis(title=list(text="Razones para no concluir estudios")) %>% hc_yAxis(title=list(text="Cantidad de Personas que no han concluido sus estudios"))


contgenpob=(data.frame(table(datos_persona$p02,datos_persona$pobreza)))

viv10 <- hchart(contgenpob, "column", hcaes(x=contgenpob$Var2,y=contgenpob$Freq, group=contgenpob$Var1)) %>% 
  hc_title(text ="Barras de Situación Socio-Económica según género")%>%
  hc_xAxis(title=list(text="Situación Socio-Económica"))%>% hc_yAxis(title=list(text="Cantidad de Respuestas")) 



(cont_gen_edu=(data.frame(table(datos_persona$p02,datos_persona$p09))))

viv11 <- hchart(cont_gen_edu, "bar", hcaes(x=cont_gen_edu$Var2,y=cont_gen_edu$Freq, group=cont_gen_edu$Var1)) %>% 
  hc_title(text ="Barras de razón del porque no continuaron sus estudios según género")%>%
  hc_xAxis(title=list(text="Razones"))%>% hc_yAxis(title=list(text="Cantidad de Respuestas")) %>% 
  hc_add_theme(hc_theme_google())


(cont_gen_eduj=(data.frame(table(datos_persona$p02,datos_persona$p10a))))

viv12 <- hchart(cont_gen_eduj, "bar", hcaes(x=cont_gen_eduj$Var2,y=cont_gen_eduj$Freq, group=cont_gen_eduj$Var1)) %>% 
  hc_title(text ="Barras de nivel de estudio según género")%>%
  hc_xAxis(title=list(text="Nivel de estudio"))%>% hc_yAxis(title=list(text="Cantidad de Respuestas")) %>% 
  hc_add_theme(hc_theme_google())



(cont_gen_con=(data.frame(table(datos_persona$p02,datos_persona$condact))))
viv13 <- hchart(cont_gen_con, "bar", hcaes(x =cont_gen_con$Var2,y=cont_gen_con$Freq, group=cont_gen_con$Var1)) %>% 
  hc_title(text ="Barras de condición laboral según género")%>%
  hc_xAxis(title=list(text="Condición laboral"))%>% hc_yAxis(title=list(text="Cantidad de Respuestas")) %>% 
  hc_add_theme(hc_theme_google())

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse,warn.conflicts = FALSE)
library(DT)
library(foreign)
library(shinyjs)
library(dplyr)
library(highcharter)
library(foreign)
library(readxl)
library(tseries)
library(lubridate)
library(tidyr)
library(haven)
library(psych)
library(mnormt)
library(ca)
library(vegan)
library(MASS)
library(plotly)
library(MVN)
library(rsconnect)
library(shinydashboard)
library(shinydashboardPlus)
library(dbplyr)
source('https://raw.githubusercontent.com/JavierRojasC/MultivEspol/master/POBREZA/ac.R')


datos_consumidor<-read.spss("https://github.com/JavierRojasC/MultivEspol/blob/master/POBREZA/enemdu_consumidor_201912%20(1).sav?raw=true",to.data.frame=T)
datos_persona<-read.spss("https://github.com/JavierRojasC/MultivEspol/blob/master/POBREZA/enemdu_persona_201912%20(1).sav?raw=true",to.data.frame=T)
datos_hogar<-read.spss("https://github.com/JavierRojasC/MultivEspol/blob/master/POBREZA/enemdu_viv_hog_201912%20(1).sav?raw=true",to.data.frame=T)

#Wladimir-Madisson
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

mnvdemisdatos <- mvn(datos_sever_ext_pob[,2:6], mvnTest = "mardia")






#Wladimir

datos_incid_ext_pob=data.frame(agrup_datos_emp_subempr$X.1,agrup_datos_emp_subempr$X.2,agrup_datos_emp_subempr$X.5,agrup_datos_emp_subempr$Indice,agrup_datos_emp_subempr$X.10,agrup_datos_emp_subempr$X.13)
colnames(datos_incid_ext_pob)=c("Periodo","Pichincha","Guayas","Azuay","El Oro","Tungurahua")
datos_incid_ext_pob$Guayas=as.numeric(datos_incid_ext_pob$Guayas)
datos_incid_ext_pob$Pichincha=as.numeric(datos_incid_ext_pob$Pichincha)
datos_incid_ext_pob$Azuay=as.numeric(datos_incid_ext_pob$Azuay)
datos_incid_ext_pob$`El Oro`=as.numeric(datos_incid_ext_pob$`El Oro`)
datos_incid_ext_pob$`Tungurahua`=as.numeric(datos_incid_ext_pob$`Tungurahua`)

datos_incid_ext_pob=data.frame(datos_incid_ext_pob %>% group_by(Periodo=year(Periodo)) %>%summarise(mean(`Pichincha`),             
                                                                                                    mean(Guayas), mean(Azuay), mean(`El Oro`),mean(`Tungurahua`)))
colnames(datos_incid_ext_pob)=c("Periodo","Pichincha","Guayas","Azuay","El Oro","Tungurahua")
mnvdemisdatos <- mvn(datos_incid_ext_pob[,2:6], mvnTest = "mardia")









#Kenia-Ashley
#Ashley
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










#Kenia


datos_incid_pob_ciu=data.frame(agrup_datos_pob_ciu$X.1,agrup_datos_pob_ciu$X.2,agrup_datos_pob_ciu$X.5,agrup_datos_pob_ciu$X.7,agrup_datos_pob_ciu$X.10,agrup_datos_pob_ciu$X.13)
colnames(datos_incid_pob_ciu)=c("Periodo","Pichincha","Guayas","Azuay","El Oro","Tungurahua")

datos_incid_pob_ciu$Guayas=as.numeric(datos_incid_pob_ciu$Guayas)
datos_incid_pob_ciu$Pichincha=as.numeric(datos_incid_pob_ciu$Pichincha)
datos_incid_pob_ciu$Azuay=as.numeric(datos_incid_pob_ciu$Azuay)
datos_incid_pob_ciu$`El Oro`=as.numeric(datos_incid_pob_ciu$`El Oro`)
datos_incid_pob_ciu$`Tungurahua`=as.numeric(datos_incid_pob_ciu$`Tungurahua`)

datos_incid_pob_ciu=data.frame(datos_incid_pob_ciu %>% group_by(Periodo=year(Periodo)) %>%summarise(mean(`Pichincha`),             
                                                                                                    mean(Guayas), mean(Azuay), mean(`El Oro`),mean(`Tungurahua`)))
colnames(datos_incid_pob_ciu)=c("Periodo","Pichincha","Guayas","Azuay","El Oro","Tungurahua")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$dat_nac_inc<- DT::renderDataTable({datos_incid_pob_ciu})
    output$dat_nac<- DT::renderDataTable({datos_severidad_pob_ciu})
    output$dat_ext_inc<- DT::renderDataTable({datos_incid_ext_pob})
    output$dat_ext<- DT::renderDataTable({datos_sever_ext_pob})
    
    output$hc2<-renderHighchart({
        (cont_sit_prox=(data.frame(table(datos_consumidor$c19))))
        hchart(cont_sit_prox, "line", hcaes(x =cont_sit_prox$Var1,y=cont_sit_prox$Freq),showInLegend = TRUE,
               size = "5%",center = list("5%", "5%"),startAngle = -100,endAngle  = 100) %>% 
            hc_colors(colors = c('#EB001F', '#64A12D','#008AC5' )) 
    })
    
    
    
    
    output$plot<-renderPlotly({
        pca <- (fit.pca.vari=principal(datos_incid_ext_pob[,2:6],nfactors=3,rotate="none")) 
        scores <- pca$scores
        loads <- pca$loadings
        scale.loads <- 5
        #Gráfico 2D (2 Factores)
        x <- scores[,1]
        y <- scores[,2]
        plt_acp <- plot_ly() %>%
            add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                            colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
        plt_acp<- plt_acp %>% layout(title=" ")
        
        for (k in (1:nrow(loads))){
            x <- c(0, loads[k,1])*scale.loads
            y <- c(0, loads[k,2])*scale.loads
            plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=8),
                                             opacity = 1,name= colnames(datos_incid_ext_pob[,2:6])[k])
        }
    })
    
    output$tabset1Selected <- renderText({input$tabset1})
    
    
    #Madison
    
    output$qq<-renderHighchart({
        if(input$selected804=="PIN"){
            qq_Pinca=data.frame(qqnorm(datos_sever_ext_pob$Pichincha,plot=F))
            hchart(qq_Pinca,type="scatter") %>% 
                hc_add_series(data=list(c(-1.75,0.15),c(1.78,0.8)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Pichincha")
        }else if(input$selected804=="GYE"){
            
            qq_Pinc=data.frame(qqnorm(datos_sever_ext_pob$Guayas,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.75,0.01),c(2,0.7)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Guayas")
            
            
        }else if(input$selected804=="AZU"){
            
            qq_Pinc=data.frame(qqnorm(datos_sever_ext_pob$Azuay,plot=F))
            
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.65,0.015),c(2,0.5)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Azuay")
            
        }else if(input$selected804=="ORO"){
            
            qq_Pinc=data.frame(qqnorm(datos_sever_ext_pob$`El Oro`,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.75,0.15),c(1.78,0.6)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ El ORO") %>% hc_add_theme(hc_theme_google())
            
        }else if(input$selected804=="TUN"){
            
            qq_Pinc=data.frame(qqnorm(datos_sever_ext_pob$Tungurahua,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.7668,0),c(1.768,1.05)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Tungurahua") }
    }) 
    
    
    
    output$es1003<-renderText({
        if(input$selected804=="PIN"){
            "En la presente Gráfica Normal QQ podemos inferir que la severidad de la provincia de Pichincha tiene una distribución normal. "
        }else if(input$selected804=="GYE"){
            "Puesto que la mayoria de puntos se encuentran pegados a la diagonal, se puede diferir que la distribucón es Normal."
            
        }else if(input$selected804=="AZU"){
            "Al observar los puntos se puede denotar como los datos desde el 2007 al 2019 de severidad se encuentran alrededor de la linea QQ por lo cual se infiere que los datos siguen una distribución normal."
            
        }else if(input$selected804=="ORO"){
            "Al denotar los puntos en que se encuentran alrededor de la pendiente QQ, se podria inferir que la severidad del ORO, podrian seguir una distribución normal."
            
        }else if(input$selected804=="TUN"){
            "Los presentes datos en parte se ajustan a la recta QQ, por lo tanto se puede inferir que siguen una distribución normal."
            
        }
    })
    
    #Wladimir QQ
    
    output$tabsetSelected <- renderText({input$tabset2})
    
    
    output$qqw<-renderHighchart({
        if (input$selected801=="PIN"){
            qq_Pinc=data.frame(qqnorm(datos_incid_ext_pob$Pichincha,plot=F))
            
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.75,1.65),c(1.78,3.8)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Pichincha")
        }else if (input$selected801=="GYE"){
            qq_Pinc=data.frame(qqnorm(datos_incid_ext_pob$Guayas,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.75,1.15),c(1.79,5)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Guayas")
            
        }else if (input$selected801=="AZU"){
            
            qq_Pinc=data.frame(qqnorm(datos_incid_ext_pob$Azuay,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.15,0.45),c(1.74,3.25)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Azuay")
            
        }else if (input$selected801=="ORO"){
            
            qq_Pinc=data.frame(qqnorm(datos_incid_ext_pob$`El Oro`,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.75,1.005),c(1.78,7)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ El ORO") %>% hc_add_theme(hc_theme_google())
        }else if (input$selected801=="TUN"){
            
            qq_Pinc=data.frame(qqnorm(datos_incid_ext_pob$Tungurahua,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.7668,1.4),c(1.768,4.95)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Tungurahua") } 
    })
    output$es10001<-renderText({
        if(input$selected801=="PIN"){
            "Los presentes datos de la incidencia de extrema pobreza entre los años 2007 al 2019 siguen una distribución normal puesto que los puntos se ajustan la linea QQ."
        }else if(input$selected801=="GYE"){
            "En los presentes datos de la incidencia de la provincia del Guayas se puede observar como no siguen una distribucion normal, puesto la mayoria de puntos se encuentran alejados de la linea QQ."
            
        }else if(input$selected801=="AZU"){
            "En los presentes datos de la incidencia de extrema pobreza de la provincia de Azuay sigue una distribución normal."
            
        }else if(input$selected801=="ORO"){
            "Se observar que los presentes datos no se ajustan a la recta QQ, por lo cual no sigue una distribución normal."
            
        }else if(input$selected801=="TUN"){
            "Se puede denotar que los datos de la incidencia de la provincia de Tungurahua sigue una distribución normal puesto que se ajustan a la pendiente QQ."
            
        }
    })
    
    output$es1000<-renderText({
        if(input$selected800=="PIN"){
            "Mediante la Gráfica QQ se puede observar como efectivamente los datos de la provincia de Pichincha sigue una distribución normal. "
        }else if(input$selected800=="GYE"){
            "Se puede denotar que la mayoria de puntuaciones QQ de la presente Provincia sigue una distribución normal."
            
        }else if(input$selected800=="AZU"){
            "En los presentes de datos se incidencia de pobreza se puede a simple vista inferir que los puntajes se ajustan a la recta QQ, por lo cual sigue una distribución normal."
            
        }else if(input$selected800=="ORO"){
            "Se observar que los presentes datos no se ajustan a la recta normal QQ, por lo tanto no sigue una distribución normal."
            
        }else if(input$selected800=="TUN"){
            "A simple vista se puede verificar que los datos de incidencia del nivel de pobreza de la provincia de Tungurahua, sigue una distribución normal."
            
        }
    })
    
    #Kenia QQ
    
    output$tabsetSelected <- renderText({input$tabset3})
    
    output$qqk<-renderHighchart({
        if (input$selected800=="PIN"){
            qq_Pinc=data.frame(qqnorm(datos_incid_pob_ciu$Pichincha,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.75,7.15),c(1.78,11.8)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Pichincha")
        }else if (input$selected800=="GYE"){
            
            qq_Pinc=data.frame(qqnorm(datos_incid_pob_ciu$Guayas,plot=F))
            
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.75,8.501),c(1.76,20.7)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Guayas")
        }else if (input$selected800=="AZU"){
            
            qq_Pinc=data.frame(qqnorm(datos_incid_pob_ciu$Azuay,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.75,3.15),c(1.76,10.95)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Azuay") 
        }else if (input$selected800=="ORO"){
            
            qq_Pinc=data.frame(qqnorm(datos_incid_pob_ciu$`El Oro`,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.75,9.15),c(1.78,22.6)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ El ORO") %>% hc_add_theme(hc_theme_google()) 
        }else if (input$selected800=="TUN"){
            
            qq_Pinc=data.frame(qqnorm(datos_incid_pob_ciu$Tungurahua,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.7668,7.55),c(1.768,15.05)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Tungurahua") }
    })
    
    #Ashley QQ
    
    output$tabsetSelected <- renderText({input$tabset4})
    
    output$qqa<-renderHighchart({
        if(input$selected803=="PIN"){
            qq_Pinc=data.frame(qqnorm(datos_severidad_pob_ciu$Pichincha,plot=F))
            
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-2.05,0.65),c(2.08,2.4)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Pichincha")
        }else if(input$selected803=="GYE"){
            qq_Pinc=data.frame(qqnorm(datos_severidad_pob_ciu$Guayas,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-2.75,0.71),c(2,3)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Guayas") 
        }else if(input$selected803=="AZU"){
            qq_Pinc=data.frame(qqnorm(datos_severidad_pob_ciu$Azuay,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-2.065,0.15),c(2,2)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Azuay")
            
        }else if(input$selected803=="ORO"){
            qq_Pinc=data.frame(qqnorm(datos_severidad_pob_ciu$`El Oro`,plot=F))
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-2.075,0.5),c(2.078,3.8)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ El ORO") %>% hc_add_theme(hc_theme_google()) 
            
        }else if(input$selected803=="TUN"){
            
            qq_Pinc=data.frame(qqnorm(datos_sever_ext_pob$Tungurahua,plot=T))
            
            hchart(qq_Pinc,type="scatter") %>% 
                hc_add_series(data=list(c(-1.737668,0.15),c(1.768,0.75)),type="line") %>% 
                hc_plotOptions(line = list(color = "black",marker = list(radius = 0.5,symbol = "circle",lineWidth = 1))) %>% 
                hc_title(text ="Gráfico Normal QQ Tungurahua") 
        }
    })
    output$es1001<-renderText({
        if(input$selected803=="PIN"){
            "Se puede denotar en la presente Gráfico como los datos se ajustan a la recta QQ, por lo tanto sigue una distribución normal."
        }else if(input$selected803=="GYE"){
            "Como se puede observar los datos de severidad del nivel de pobreza, no provienen de una distribución normal."
            
        }else if(input$selected803=="AZU"){
            "En la presente Gráfica QQ se puede denotar como la provincia del Azuay proviene de una distribución normal."
        }else if(input$selected803=="ORO"){
            "En la presente Gráfico de la provincia del Oro se puede observar como los datos de los años del 2007 al 2019, no provienen de una distribución normal."
            
            
        }else if(input$selected803=="TUN"){
            "Mediante la Gráfica normal QQ de la provincia de Tungurahua se puede observar que los datos si provinenen de una distribuión normal."
            
        }
    })
    
    
    
    
    
    output$tabsetSelected <- renderText({input$tabset5}) #Madisomn
    output$tabsetSelected <- renderText({input$tabset6}) #Wladimir
    output$tabsetSelected <- renderText({input$tabset7}) #Kenia
    output$tabsetSelected <- renderText({input$tabset8}) #Ashley
    output$tabsetSelected <- renderText({input$tabset9}) #INEC
    
    
    ####Correlacion
    output$acm<-renderHighchart({
        
        R=cor(datos_sever_ext_pob[,2:6])
        hchart(R)
        
    })
    output$acw<-renderHighchart({
        R=cor(datos_incid_ext_pob[,2:6])
        hchart(R)
        
    })
    output$ack<-renderHighchart({
        R=cor(datos_incid_pob_ciu[,2:6])
        hchart(R)
        
    })
    output$aca<-renderHighchart({
        R=cor(datos_severidad_pob_ciu[,2:6])
        
        hchart(R)
        
    })
    
    #Componentes Principales
    output$cpm<-renderPlotly({
        
        if (input$selected=="ACPS"){
            pca <- (fit.pca.vari=principal(datos_incid_ext_pob[,2:6],nfactors=3,rotate="none")) 
            scores <- pca$scores
            loads <- pca$loadings
            scale.loads <- 5
            
            #Gráfico 2D (2 Factores)
            
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in (1:nrow(loads))){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_ext_pob[,2:6])[k])}
            plt_acp
        }else if (input$selected=="ACPO"){
            
            (fit.pca.vari=principal(datos_sever_ext_pob[,2:6],nfactors=3,rotate="varimax")) 
            
            scores <- fit.pca.vari$scores
            loads <- fit.pca.vari$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            
            #Gráfico 2D (2 Factores)
            
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_sever_ext_pob[,2:6])[k])}
            
            plt_acp
            
        }else if (input$selected=="ACPOC") {
            
            #Rotando de forma oblicua 
            fit.pca.prom=principal(datos_sever_ext_pob[,2:6],nfactors=3,rotate="Promax") 
            
            scores <- fit.pca.prom$scores
            loads <- fit.pca.prom$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_sever_ext_pob[,2:6])[k])
            }
            
            plt_acp}
        
    })
    
    
    #Componentes Principales
    output$cpk<-renderPlotly({
        
        if (input$selected9=="ACPS"){
            pca <- (fit.pca.vari=principal(datos_incid_pob_ciu[,2:6],nfactors=3,rotate="none")) 
            scores <- pca$scores
            loads <- pca$loadings
            scale.loads <- 5
            
            #Gráfico 2D (2 Factores)
            
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in (1:nrow(loads))){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_pob_ciu[,2:6])[k])}
            plt_acp
        }else if (input$selected9=="ACPO"){
            
            (fit.pca.vari=principal(datos_incid_pob_ciu[,2:6],nfactors=3,rotate="varimax")) 
            
            scores <- fit.pca.vari$scores
            loads <- fit.pca.vari$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            
            #Gráfico 2D (2 Factores)
            
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_pob_ciu[,2:6])[k])}
            
            plt_acp
            
        }else if (input$selected9=="ACPOC") {
            
            #Rotando de forma oblicua 
            fit.pca.prom=principal(datos_incid_pob_ciu[,2:6],nfactors=3,rotate="Promax") 
            
            scores <- fit.pca.prom$scores
            loads <- fit.pca.prom$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_pob_ciu[,2:6])[k])
            }
            
            plt_acp}
        
    })
    #Componentes Principales
    output$cpa<-renderPlotly({
        
        if (input$selected15=="ACPS"){
            pca <- (fit.pca.vari=principal(datos_severidad_pob_ciu[,2:6],nfactors=3,rotate="none")) 
            scores <- pca$scores
            loads <- pca$loadings
            scale.loads <- 5
            
            #Gráfico 2D (2 Factores)
            
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in (1:nrow(loads))){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_severidad_pob_ciu[,2:6])[k])}
            plt_acp
        }else if (input$selected15=="ACPO"){
            
            (fit.pca.vari=principal(datos_severidad_pob_ciu[,2:6],nfactors=3,rotate="varimax")) 
            
            scores <- fit.pca.vari$scores
            loads <- fit.pca.vari$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            
            #Gráfico 2D (2 Factores)
            
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_severidad_pob_ciu[,2:6])[k])}
            
            plt_acp
            
        }else if (input$selected15=="ACPOC") {
            
            #Rotando de forma oblicua 
            fit.pca.prom=principal(datos_severidad_pob_ciu[,2:6],nfactors=3,rotate="Promax") 
            
            scores <- fit.pca.prom$scores
            loads <- fit.pca.prom$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_severidad_pob_ciu[,2:6])[k])
            }
            
            plt_acp}
        
    })
    output$cfk<-renderPlotly({
        if (input$selected10=="AFPS"){
            pca <- (fit.pca.vari=fa(datos_incid_pob_ciu[,2:6],nfactors=2,rotate="none")) 
            scores <- pca$scores
            loads <- pca$loadings
            scale.loads <- 5
            
            #Gráfico 2D (2 Factores)
            
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in (1:nrow(loads))){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_pob_ciu[,2:6])[k])}
            plt_acp
        } else if (input$selected10=="AFPO"){
            
            (fit.pca.vari=fa(datos_incid_pob_ciu[,2:6],nfactors=3,rotate="varimax",n.obs=65)) 
            
            scores <- fit.pca.vari$scores
            loads <- fit.pca.vari$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            
            #Gráfico 2D (2 Factores)
            
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_pob_ciu[,2:6])[k])}
            
            plt_acp
            
        }else if (input$selected10=="AFPOC") {
            
            #Rotando de forma oblicua 
            fit.pca.prom=fa(datos_incid_pob_ciu[,2:6],nfactors=3,rotate="Promax",n.obs=65) 
            
            scores <- fit.pca.prom$scores
            loads <- fit.pca.prom$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_pob_ciu[,2:6])[k])
            }
            
            plt_acp}
        
        
        
    })
    
    output$cfa<-renderPlotly({
        if (input$selected16=="AFPS"){
            pca <- (fit.pca.vari=fa(datos_severidad_pob_ciu[,2:6],nfactors=2,rotate="none")) 
            scores <- pca$scores
            loads <- pca$loadings
            scale.loads <- 5
            
            #Gráfico 2D (2 Factores)
            
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in (1:nrow(loads))){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_severidad_pob_ciu[,2:6])[k])}
            plt_acp
        } else if (input$selected16=="AFPO"){
            
            (fit.pca.vari=fa(datos_severidad_pob_ciu[,2:6],nfactors=3,rotate="varimax",n.obs=65)) 
            
            scores <- fit.pca.vari$scores
            loads <- fit.pca.vari$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            
            #Gráfico 2D (2 Factores)
            
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_severidad_pob_ciu[,2:6])[k])}
            
            plt_acp
            
        }else if (input$selected16=="AFPOC") {
            
            #Rotando de forma oblicua 
            fit.pca.prom=fa(datos_severidad_pob_ciu[,2:6],nfactors=3,rotate="Promax",n.obs=65) 
            
            scores <- fit.pca.prom$scores
            loads <- fit.pca.prom$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_severidad_pob_ciu[,2:6])[k])
            }
            
            plt_acp}
        
        
        
    })
    
    
    output$cpw<-renderPlotly({
        if (input$selected5=="ACPO"){
            
            #Rotando: Ordenando variables(Rotacion Ortogonal)
            (fit.pca.vari=principal(datos_incid_ext_pob[,2:6],nfactors=3,rotate="varimax")) 
            
            scores <- fit.pca.vari$scores
            loads <- fit.pca.vari$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            
            #Gráfico 2D (2 Factores)
            
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in (1:nrow(loads))){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_ext_pob[,2:6])[k])
            }
            
            plt_acp   
        }else if (input$selected5=="ACPS"){
            scores <- pca$scores
            loads <- pca$loadings
            scale.loads <- 5
            
            
            #Gráfico 2D (2 Factores)
            
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_ext_pob[,2:6])[k])
            }
            
            plt_acp
            
            
        }else if(input$selected5=="ACPOC"){
            
            
            #Rotando de forma oblicua 
            fit.pca.prom=principal(datos_incid_ext_pob[,2:6],nfactors=3,rotate="Promax") 
            #Se puede observar en los componentes de relacion un poco bajo al anterior, por lo cual diriamos que mejor explican a las variables
            #la rotacion ortogonal.
            
            scores <- fit.pca.prom$scores
            loads <- fit.pca.prom$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_ext_pob[,2:6])[k])
            }
            
            plt_acp
            
            
        }
        
        
        
    })
    
    
    
    
    output$aa<-renderHighchart({
        hchart(datos_incid_ext_pob,"scatter") %>% 
            hc_title(text =" ")%>% 
            hc_xAxis(title=list(text="RC 1"),categories=datos_incid_ext_pob$Periodo)%>% 
            hc_yAxis(title=list(text="RC 2")) %>% hc_add_series(data=datos_incid_ext_pob$Pichincha,type="scatter",name="Pichincha",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_ext_pob$Guayas,type="scatter",name="Guayas",showInLegend = T) %>%
            hc_add_series(data=datos_incid_ext_pob$Azuay,type="scatter",name="Azuay",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_ext_pob$`El Oro`,type="scatter",name="El Oro",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_ext_pob$`Tungurahua`,type="scatter",name="Tungurahua",showInLegend = T) 
        
    })
    
    
    
    
    
    output$bb<-renderHighchart({
        #Gráfico de correlación de las variables involucradas en el análisis (círculo unitario)
        hchart(datos_severidad_pob_ciu,"scatter") %>% 
            hc_title(text =" ")%>% 
            hc_xAxis(title=list(text="RC 1"),categories=datos_severidad_pob_ciu$Fecha)%>% 
            hc_yAxis(title=list(text="RC 2")) %>% 
            hc_add_series(data=datos_severidad_pob_ciu$Pichincha,type="scatter",name="Pichincha",showInLegend = T) %>% 
            hc_add_series(data=datos_severidad_pob_ciu$Guayas,type="scatter",name="Guayas",showInLegend = T) %>%
            hc_add_series(data=datos_severidad_pob_ciu$Azuay,type="scatter",name="Azuay",showInLegend = T) %>% 
            hc_add_series(data=datos_severidad_pob_ciu$`El Oro`,type="scatter",name="`El Oro`",showInLegend = T) %>% 
            hc_add_series(data=datos_severidad_pob_ciu$Tungurahua,type="scatter",name="Tungurahua",showInLegend = T) 
        #hc_plotOptions(line=list(dataLabels=list(enabled=T)))
        
    })
    
    
    
    
    output$cc<-renderHighchart({
        #Gráfico de correlación de las variables involucradas en el análisis (círculo unitario)
        hchart(datos_incid_pob_ciu,"scatter") %>% 
            hc_title(text =" ")%>% 
            hc_xAxis(title=list(text="RC 1"),categories=datos_incid_pob_ciu$Fecha)%>% 
            hc_yAxis(title=list(text="RC 2")) %>% 
            hc_add_series(data=datos_incid_pob_ciu$Pichincha,type="scatter",name="Pichincha",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_pob_ciu$Guayas,type="scatter",name="Guayas",showInLegend = T) %>%
            hc_add_series(data=datos_incid_pob_ciu$Azuay,type="scatter",name="Azuay",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_pob_ciu$`El Oro`,type="scatter",name="`El Oro`",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_pob_ciu$Tungurahua,type="scatter",name="Tungurahua",showInLegend = T) 
        
    })
    
    
    
    output$cfm<-renderPlotly({
        if (input$selected1=="AFPS"){
            pca <- (fit.pca.vari=fa(datos_sever_ext_pob[,2:6],nfactors=2,rotate="none")) 
            scores <- pca$scores
            loads <- pca$loadings
            scale.loads <- 5
            
            #Gráfico 2D (2 Factores)
            
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in (1:nrow(loads))){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_ext_pob[,2:6])[k])}
            plt_acp
        } else if (input$selected1=="AFPO"){
            
            (fit.pca.vari=fa(datos_sever_ext_pob[,2:6],nfactors=3,rotate="varimax",n.obs=65)) 
            
            scores <- fit.pca.vari$scores
            loads <- fit.pca.vari$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            
            #Gráfico 2D (2 Factores)
            
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_sever_ext_pob[,2:6])[k])}
            
            plt_acp
            
        }else if (input$selected1=="AFPOC") {
            
            #Rotando de forma oblicua 
            fit.pca.prom=fa(datos_sever_ext_pob[,2:6],nfactors=3,rotate="Promax",n.obs=65) 
            
            scores <- fit.pca.prom$scores
            loads <- fit.pca.prom$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_sever_ext_pob[,2:6])[k])
            }
            
            plt_acp}
        
        
        
    })
    
    
    
    output$rrmm<-renderPlotly({
        if (input$selected2=="GM"){
            dist=vegdist(datos_sever_ext_pob[,2:6],method="jaccard",diag=T) 
            fit=isoMDS(dist,k=2)
            fit$points
            
            scores <- fit$points
            loads <- datos_sever_ext_pob[,2:6]
            
            scale.loads <- 4
            x <- scores[,1]
            y <- scores[,2]
            plt_af_pro <- plot_ly() %>%
                add_trace(x=x, y=y,type='scatter', mode="markers",
                          marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),text=~paste("Periodo:",datos_sever_ext_pob$Periodo))
            plt_af_pro<- plt_af_pro %>% layout(title=" ")
            
            plt_af_pro 
        }else if (input$selected2=="SH"){
            dist=vegdist(datos_sever_ext_pob[,2:6],method="jaccard",diag=T) 
            fit=isoMDS(dist,k=2)
            #Gr?fico de Sheppard
            shep=Shepard(as.dist(dist),fit$points)
            #plot(shep)
            
            
            x <- shep$x
            y <- shep$y
            plt_shep <- plot_ly() %>%
                add_trace(x=x, y=y,type='scatter', mode="markers",
                          marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7))%>% 
                layout(title=" ")
            plt_shep
            
            
        }
        
    })
    output$rraa<-renderPlotly({
        if (input$selected17=="GM"){
            dist=vegdist(datos_severidad_pob_ciu[,2:6],method="jaccard",diag=T) 
            fit=isoMDS(dist,k=2)
            fit$points
            
            scores <- fit$points
            loads <- datos_severidad_pob_ciu[,2:6]
            
            #Gráfico 2D (2 Factores)
            scale.loads <- 4
            x <- scores[,1]
            y <- scores[,2]
            plt_af_pro <- plot_ly() %>%
                add_trace(x=x, y=y,type='scatter', mode="markers",
                          marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),
                                        opacity = 0.7),text=~paste("A?o:",datos_severidad_pob_ciu$Fecha))
            plt_af_pro<- plt_af_pro %>% layout(title=" ")
            
            plt_af_pro
            
        }else if (input$selected17=="SH"){
            dist=vegdist(datos_severidad_pob_ciu[,2:6],method="jaccard",diag=T) 
            fit=isoMDS(dist,k=2)
            #Gráfico Sheppard
            shep=Shepard(as.dist(dist),fit$points)
            plot(shep)
            
            #Gráfico 2D (2 Factores)
            x <- shep$x
            y <- shep$y
            plt_shep <- plot_ly() %>%
                add_trace(x=x, y=y,type='scatter', mode="markers",
                          marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),
                                        opacity = 0.7))%>%layout(title=" ")
            plt_shep
            
        }
        
    })
    output$rr<-renderPlotly({
        if (input$selected7=="GM"){
            dist=vegdist(datos_incid_ext_pob[,2:6],method="jaccard",diag=T) 
            fit=isoMDS(dist,k=2)
            fit$points
            
            scores <- fit$points
            loads <- datos_incid_ext_pob[,2:6]
            
            scale.loads <- 4
            x <- scores[,1]
            y <- scores[,2]
            plt_af_pro <- plot_ly() %>%
                add_trace(x=x, y=y,type='scatter', mode="markers",
                          marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),text=~paste("Periodo:",datos_incid_ext_pob$Periodo))
            plt_af_pro<- plt_af_pro %>% layout(title=" ")
            
            plt_af_pro 
        }else if (input$selected7=="SH"){
            dist=vegdist(datos_incid_ext_pob[,2:6],method="jaccard",diag=T) 
            fit=isoMDS(dist,k=2)
            #Gráfico de Sheppard
            shep=Shepard(as.dist(dist),fit$points)
            #plot(shep)
            
            
            x <- shep$x
            y <- shep$y
            plt_shep <- plot_ly() %>%
                add_trace(x=x, y=y,type='scatter', mode="markers",
                          marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7))%>% 
                layout(title=" ")
            plt_shep
            
            
        }
        
    })
    
    output$op<-renderPlotly({
        if (input$selected11=="GM"){
            dist=vegdist(datos_incid_pob_ciu[,2:6],method="jaccard",diag=T) 
            fit=isoMDS(dist,k=2)
            fit$points
            
            scores <- fit$points
            loads <- datos_incid_pob_ciu[,2:6]
            
            scale.loads <- 4
            x <- scores[,1]
            y <- scores[,2]
            plt_af_pro <- plot_ly() %>%
                add_trace(x=x, y=y,type='scatter', mode="markers",
                          marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),text=~paste("Periodo:",datos_incid_pob_ciu$Periodo))
            plt_af_pro<- plt_af_pro %>% layout(title=" ")
            
            plt_af_pro 
        }else if (input$selected11=="SH"){
            dist=vegdist(datos_incid_pob_ciu[,2:6],method="jaccard",diag=T) 
            fit=isoMDS(dist,k=2)
            #Gr?fico de Sheppard
            shep=Shepard(as.dist(dist),fit$points)
            #plot(shep)
            
            
            x <- shep$x
            y <- shep$y
            plt_shep <- plot_ly() %>%
                add_trace(x=x, y=y,type='scatter', mode="markers",
                          marker = list(color=y, colorscale = c("#FFE1A1", "#683531"),opacity = 0.7))%>% 
                layout(title=" ")
            plt_shep
            
            
        }
        
    })
    output$cfw<-renderPlotly({
        if (input$selected6=="AFPS"){
            pca <- (fit.pca.vari=fa(datos_incid_ext_pob[,2:6],nfactors=2,rotate="none")) 
            scores <- pca$scores
            loads <- pca$loadings
            scale.loads <- 5
            
            #Gráfico 2D (2 Factores)
            
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in (1:nrow(loads))){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_ext_pob[,2:6])[k])}
            plt_acp
        } else if (input$selected6=="AFPO"){
            
            (fit.pca.vari=fa(datos_incid_ext_pob[,2:6],nfactors=3,rotate="varimax",n.obs=65)) 
            
            scores <- fit.pca.vari$scores
            loads <- fit.pca.vari$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            
            #Gráfico 2D (2 Factores)
            
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_ext_pob[,2:6])[k])}
            
            plt_acp
            
        }else if (input$selected6=="AFPOC") {
            
            #Rotando de forma oblicua 
            fit.pca.prom=fa(datos_incid_ext_pob[,2:6],nfactors=3,rotate="Promax",n.obs=65) 
            
            scores <- fit.pca.prom$scores
            loads <- fit.pca.prom$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_ext_pob[,2:6])[k])
            }
            
            plt_acp}
        
        
        
    })
    output$cfk<-renderPlotly({
        if (input$selected10=="AFPS"){
            pca <- (fit.pca.vari=fa(datos_incid_pob_ciu[,2:6],nfactors=2,rotate="none")) 
            scores <- pca$scores
            loads <- pca$loadings
            scale.loads <- 5
            
            #Gráfico 2D (2 Factores)
            
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in (1:nrow(loads))){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_pob_ciu[,2:6])[k])}
            plt_acp
        } else if (input$selected10=="AFPO"){
            
            (fit.pca.vari=fa(datos_incid_pob_ciu[,2:6],nfactors=3,rotate="varimax",n.obs=65)) 
            
            scores <- fit.pca.vari$scores
            loads <- fit.pca.vari$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            
            #Gráfico 2D (2 Factores)
            
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_pob_ciu[,2:6])[k])}
            
            plt_acp
            
        }else if (input$selected10=="AFPOC") {
            
            #Rotando de forma oblicua 
            fit.pca.prom=fa(datos_incid_pob_ciu[,2:6],nfactors=3,rotate="Promax",n.obs=65) 
            
            scores <- fit.pca.prom$scores
            loads <- fit.pca.prom$loadings
            scale.loads <- 5
            x <- scores[,1]
            y <- scores[,2]
            plt_acp <- plot_ly() %>%
                add_trace(x=x, y=y,type="scatter", mode="markers",marker = list(color=y, 
                                                                                colorscale = c("#FFE1A1", "#683531"),opacity = 0.7),name="Años") 
            plt_acp<- plt_acp %>% layout(title=" ")
            
            for (k in 1:nrow(loads)){
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                plt_acp <- plt_acp %>% add_trace(x=x, y=y,type="scatter", mode="lines",line = list(width=3),
                                                 opacity = 1,name= colnames(datos_incid_pob_ciu[,2:6])[k])
            }
            
            plt_acp}
        
        
        
    })
    output$tt<-renderHighchart({

        
        if (input$selected8=="gc"){
        AC3
            
        } else if (input$selected8=="gf"){
        AC4
            
        }
        
        
    })         
    
    output$ttk<-renderHighchart({

        if (input$selected12=="gc"){
          AC1
            
        } else if (input$selected12=="gf"){
          AC2
            
        }
        
        
    })         
    
    output$tta<-renderHighchart({

        if (input$selected18=="gc"){
            
        AC3
            
        } else if (input$selected18=="gf"){
            #Por Filas (Periodos)
        AC4
            
        }
        
        
    })         
    
    output$zza<-renderHighchart({
        
        hchart(datos_severidad_pob_ciu,"line") %>% 
            hc_title(text ="Serie de Tiempo de la Incidencia de Pobreza por Provincia")%>% 
            hc_xAxis(title=list(text="RC 1"),categories=datos_severidad_pob_ciu$Fecha)%>% 
            hc_yAxis(title=list(text="RC 2")) %>% 
            hc_add_series(data=datos_severidad_pob_ciu$Pichincha,type="line",name="Pichincha",showInLegend = T) %>% 
            hc_add_series(data=datos_severidad_pob_ciu$Guayas,type="line",name="Guayas",showInLegend = T) %>%
            hc_add_series(data=datos_severidad_pob_ciu$Azuay,type="line",name="Azuay",showInLegend = T) %>% 
            hc_add_series(data=datos_severidad_pob_ciu$`El Oro`,type="line",name="`El Oro`",showInLegend = T) %>% 
            hc_add_series(data=datos_severidad_pob_ciu$Tungurahua,type="line",name="Tungurahua",showInLegend = T) 
        #hc_plotOptions(line=list(dataLabels=list(enabled=T)))
        
    })  
    
    output$lla<-renderHighchart({
        
        datos_severidad_pob_media<-t(data.frame(mean(datos_severidad_pob_ciu$Pichincha),
                                                mean(datos_severidad_pob_ciu$Guayas),
                                                mean(datos_severidad_pob_ciu$Azuay),
                                                mean(datos_severidad_pob_ciu$`El Oro`),
                                                mean(datos_severidad_pob_ciu$Tungurahua)))
        rownames(datos_severidad_pob_media)<-c(seq(1:5))
        datos_severidad_pob_media<-data.frame(datos_severidad_pob_media,colnames(datos_severidad_pob_ciu[2:6]))
        colnames(datos_severidad_pob_media)<-c("Severidad","Provincia")
        
        
        hcmap("countries/ec/ec-all")
        mapdata=get_data_from_map(download_map_data("countries/ec/ec-all"))
        data_ecuador<-merge(x = mapdata, y = datos_severidad_pob_media,
                            by.x = "name", by.y = "Provincia",all.x  =F)
        hcmap("countries/ec/ec-all", data = data_ecuador, value = "Severidad",
              joinBy = c("name"),
              dataLabels = list(enabled = TRUE, format = '{point.name}'))
        
        
        
    })
    
    
    
    
    
    
    
    
    
    
    output$zz<-renderHighchart({
        
        hchart(datos_incid_ext_pob,"line") %>% 
            hc_title(text ="Serie de Tiempo de la Incidencia de Extrema Pobreza")%>% 
            hc_xAxis(title=list(text="RC 1"),categories=datos_incid_ext_pob$Periodo)%>% 
            hc_yAxis(title=list(text="RC 2")) %>% hc_add_series(data=datos_incid_ext_pob$Pichincha,type="line",name="Pichincha",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_ext_pob$Guayas,type="line",name="Guayas",showInLegend = T) %>%
            hc_add_series(data=datos_incid_ext_pob$Azuay,type="line",name="Azuay",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_ext_pob$`El Oro`,type="line",name="El Oro",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_ext_pob$`Tungurahua`,type="line",name="Tungurahua",showInLegend = T) 
        #hc_plotOptions(line=list(dataLabels=list(enabled=T)))
        
        
    })  
    output$ll<-renderHighchart({
        
        datos_incid_ext_pob_media<-t(data.frame(mean(datos_incid_ext_pob$Pichincha),mean(datos_incid_ext_pob$Guayas),mean(datos_incid_ext_pob$Azuay),
                                                mean(datos_incid_ext_pob$`El Oro`),mean(datos_incid_ext_pob$`Tungurahua`)))
        rownames(datos_incid_ext_pob_media)<-c(seq(1:5))
        datos_incid_ext_pob_media<-data.frame(datos_incid_ext_pob_media,colnames(datos_incid_ext_pob[2:6]))
        colnames(datos_incid_ext_pob_media)<-c("incididad","Provincia")
        
        
        hcmap("countries/ec/ec-all")
        mapdata=get_data_from_map(download_map_data("countries/ec/ec-all"))
        data_ecuador<-merge(x = mapdata, y = datos_incid_ext_pob_media, by.x = "name", by.y = "Provincia",all.x  =F)
        hcmap("countries/ec/ec-all", data = data_ecuador, value = "incididad",
              joinBy = c("name"),
              dataLabels = list(enabled = TRUE, format = '{point.name}'))
        
        
    })
    output$mml<-renderHighchart({
        #Gráfico de correlación de las variables involucradas en el análisis (círculo unitario)
        hchart(datos_sever_ext_pob,"scatter") %>% 
            hc_title(text =" ")%>% 
            hc_xAxis(title=list(text="RC 1"),categories=datos_sever_ext_pob$Periodo)%>% 
            hc_yAxis(title=list(text="RC 2")) %>% hc_add_series(data=datos_sever_ext_pob$Pichincha,type="scatter",name="Pichincha",showInLegend = T) %>% 
            hc_add_series(data=datos_sever_ext_pob$Guayas,type="scatter",name="Guayas",showInLegend = T) %>%
            hc_add_series(data=datos_sever_ext_pob$Azuay,type="scatter",name="Azuay",showInLegend = T) %>% 
            hc_add_series(data=datos_sever_ext_pob$`El Oro`,type="scatter",name="El Oro",showInLegend = T) %>% 
            hc_add_series(data=datos_sever_ext_pob$`Tungurahua`,type="scatter",name="Tungurahua",showInLegend = T) 
        #hc_plotOptions(line=list(dataLabels=list(enabled=T)))
        
        
        
    })
    
    output$ss<-renderHighchart({
        usr=datos_sever_ext_pob[,-1]
        rownames(usr)=datos_sever_ext_pob$Periodo
        usr=ca(usr)
        if (input$selected3=="gc"){
            
            gr=data.frame(usr$colnames,usr$colcoord[,1],usr$colcoord[,2],usr$colmass)
            colnames(gr)=c("Ciudades","Dim1","Dim2","Size")
            hchart(gr, "bubble",hcaes(x =gr$Dim1,y=gr$Dim2,group=gr$Ciudades,size=gr$Size)) %>% 
                hc_title(text =" ")%>% 
                hc_xAxis(title=list(text="Dimension 1"))%>% hc_yAxis(title=list(text="Dimension 2"))
            
        } else if (input$selected3=="gf"){
            #Por Filas (Periodos)
            grm=data.frame(usr$rownames,usr$rowcoord[,1],usr$rowcoord[,2],usr$rowmass)
            colnames(grm)=c("Periodos","Dim1","Dim2","Size")
            hchart(grm, "bubble",hcaes(x =grm$Dim1,y=grm$Dim2,group=grm$Periodos,size=grm$Size)) %>% 
                hc_title(text =" ")%>% 
                hc_xAxis(title=list(text="Dimension 1"))%>% hc_yAxis(title=list(text="Dimension 2"))
            
        }
        
        
    })  
    output$ss1<-renderHighchart({
        
        hchart(datos_sever_ext_pob,"line") %>% 
            hc_title(text =" ")%>% 
            hc_xAxis(title=list(text="RC 1"),categories=datos_sever_ext_pob$Periodo)%>% 
            hc_yAxis(title=list(text="RC 2")) %>% hc_add_series(data=datos_sever_ext_pob$Pichincha,type="line",name="Pichincha",showInLegend = T) %>% 
            hc_add_series(data=datos_sever_ext_pob$Guayas,type="line",name="Guayas",showInLegend = T) %>%
            hc_add_series(data=datos_sever_ext_pob$Azuay,type="line",name="Azuay",showInLegend = T) %>% 
            hc_add_series(data=datos_sever_ext_pob$`El Oro`,type="line",name="El Oro",showInLegend = T) %>% 
            hc_add_series(data=datos_sever_ext_pob$`Tungurahua`,type="line",name="Tungurahua",showInLegend = T) 
        #hc_plotOptions(line=list(dataLabels=list(enabled=T)))
        
        
    })
    output$ss12<-renderHighchart({
        
        datos_sever_ext_pob_media<-t(data.frame(mean(datos_sever_ext_pob$Pichincha),mean(datos_sever_ext_pob$Guayas),mean(datos_sever_ext_pob$Azuay),
                                                mean(datos_sever_ext_pob$`El Oro`),mean(datos_sever_ext_pob$`Tungurahua`)))
        rownames(datos_sever_ext_pob_media)<-c(seq(1:5))
        datos_sever_ext_pob_media<-data.frame(datos_sever_ext_pob_media,colnames(datos_sever_ext_pob[2:6]))
        colnames(datos_sever_ext_pob_media)<-c("Severidad","Provincia")
        
        
        hcmap("countries/ec/ec-all")
        mapdata=get_data_from_map(download_map_data("countries/ec/ec-all"))
        data_ecuador<-merge(x = mapdata, y = datos_sever_ext_pob_media, by.x = "name", by.y = "Provincia",all.x  =F)
        hcmap("countries/ec/ec-all", data = data_ecuador, value = "Severidad",
              joinBy = c("name"),
              dataLabels = list(enabled = TRUE, format = '{point.name}'))
        
    })
    output$ss12k<-renderHighchart({
        
        datos_incid_pob_media<-t(data.frame(mean(datos_incid_pob_ciu$Pichincha),
                                            mean(datos_incid_pob_ciu$Guayas),
                                            mean(datos_incid_pob_ciu$Azuay),
                                            mean(datos_incid_pob_ciu$`El Oro`),
                                            mean(datos_incid_pob_ciu$Tungurahua)))
        rownames(datos_incid_pob_media)<-c(seq(1:5))
        datos_incid_pob_media<-data.frame(datos_incid_pob_media,colnames(datos_incid_pob_ciu[2:6]))
        colnames(datos_incid_pob_media)<-c("Incidencia","Provincia")
        
        
        hcmap("countries/ec/ec-all")
        mapdata=get_data_from_map(download_map_data("countries/ec/ec-all"))
        data_ecuador<-merge(x = mapdata, y = datos_incid_pob_media,
                            by.x = "name", by.y = "Provincia",all.x  =F)
        hcmap("countries/ec/ec-all", data = data_ecuador, value = "Incidencia",
              joinBy = c("name"),
              dataLabels = list(enabled = TRUE, format = '{point.name}'))
        
    })
    output$llk<-renderHighchart({
        
        hchart(datos_incid_pob_ciu,"line") %>% 
            hc_title(text =" ")%>% 
            hc_xAxis(title=list(text="RC 1"),categories=datos_incid_pob_ciu$Fecha)%>% 
            hc_yAxis(title=list(text="RC 2")) %>% 
            hc_add_series(data=datos_incid_pob_ciu$Pichincha,type="line",name="Pichincha",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_pob_ciu$Guayas,type="line",name="Guayas",showInLegend = T) %>%
            hc_add_series(data=datos_incid_pob_ciu$Azuay,type="line",name="Azuay",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_pob_ciu$`El Oro`,type="line",name="`El Oro`",showInLegend = T) %>% 
            hc_add_series(data=datos_incid_pob_ciu$Tungurahua,type="line",name="Tungurahua",showInLegend = T) 
        #hc_plotOptions(line=list(dataLabels=list(enabled=T)))
        
    })
    output$ki<-renderHighchart({
        
        if (input$selected20=="viv"){
            #Gráficas
            viv1
        } else if (input$selected20=="op3"){
            viv2 
            
        }
        
        
    })
    
    output$ki2<-renderHighchart({
        if (input$selected21=="nivp"){
            viv3
        } else if (input$selected21=="comp2"){
            
            viv4
            
        }else if(input$selected21=="opp"){
            #Pensamiento del Consumidor sobre situaci?n del pa?s los pr?ximos meses
            viv5
            
        }else if(input$selected21=="cod"){
            viv6
            
            
        }
        
        
    }) 
    
    output$ki3<-renderHighchart({
        if(input$selected22=="edu"){
            
        viv7
            
        } else if(input$selected22=="grad"){
        viv8
        }else if(input$selected22=="raz"){
           viv9
        }
        
    })
    output$ki4<-renderHighchart({
        if (input$selected210=="si"){
           viv10
        }else if (input$selected210=="raz"){
            viv11
        }else if (input$selected210=="niv"){
            viv12
            
        }else if (input$selected210=="cod"){
            viv13
        } 
        
    })
    
    output$es<-renderText({
        if (input$selected5=="ACPS"){
            "Se puede notar en el gráfico de componentes principales sin rotación que entre Azuay y Tungurahua existen una correlación o
intensidad fuerte, además se observa que en el 3er cuadrante se encuentra la mayor cantidad de las
puntuaciones, por lo tanto, se ha decidido optar por una rotación para considerar su
comportamiento.
"
        } else if (input$selected5=="ACPO"){
            "En el gráfico con rotación ortogonal las puntuaciones están muchas más
                cercas, se podría decir que están ordenadas ya que existe una mayor intensidad de correlación por
                su ángulo que forman entre variables de las provincias.
                
                
                Se puede observar en los componentes de relación un poco bajo al anterior, por lo cual
                se diría que mejor explican a las variables la rotación ortogonal."
            
            
            
        }else if (input$selected5=="ACPOC"){
            
            "En esta rotación oblicua se obtiene al igual que al anterior gráfico puntuaciones muy
cercanas, sus ángulos muy similares mayormente entre naranja y rojo. Nos quedaríamos
como el análisis de componentes principales con rotación ortogonal el mejor gráfico y
resultados en los cuales se observa cómo se extrae la mayor variabilidad posible."
            
            
            
        }
    })   
    
    output$es1<-renderText({
        if (input$selected=="ACPS"){
            "Como es de conocimiento,  1 - las comunalidades representa esa varianza que no pudo ser explicada por
el factor 1, por lo que como se mencionó anteriormente ante las posibilidades de hacer
rotaciones, se realizaran las mismas con el fin de notar si puede ser explicada una mayor
varianza.
"
        }else if (input$selected=="ACPO"){
            "Se puede notar en el gráfico interactivo que entre Guayas y El Oro existe correlación o mayor
correlación, que, entre Guayas y Pichincha, por ejemplo. Por otro lado, se puede notar que
Guayas explica o proporciona mejor información debido a la longitud de su vector, con
respecto a las otras provincias. "
        }else if (input$selected=="ACPOC"){
            "Algo importante a destacar del gráfico con rotación promax u oblícuo, es que el vector de la provincia
de Pichincha es muy pequeño a cuando no se aplicaba algún tipo de rotación, lo que podría
indicar que aporta muy poca varianza en el plano bidimensional."
        }
        
    })   
    
    
    output$es3<-renderText({
        if (input$selected6=="AFPS"){
            "Se puede denotar como existen ángulos muy agudos, confirmando lo ya
obtenido en las distintas pruebas su intensa correlación entre varias de las variables."
        }else if (input$selected6=="AFPO"){
            "Luego de darle una rotación ortogonal se puede ver como las puntuaciones se asemejan más
entre ellas de lo que ya se veía en el gráfico sin rotación, sin embargo, las varianzas que quedan por
explicar son muy similares a la anterior. Las puntuaciones en el gráfico  de esta rotación se puede ver que aleja los puntos en su
mayoría, y los ángulos se hacen menos agudos ósea posee una menor intensidad de
correlaciones con dicha rotación."
        }else if (input$selected6=="AFPOC"){
            "Se puede ver que el gráfico tiene mejor posición de agrupación de las puntuaciones, sin
embargo, sigue siendo mejor el análisis sin rotación ya que el análisis factorial tiene como
objetivo expresar cada factor común en un factor único y expresar una variabilidad en común."
        }
    }) 
    
    output$es2<-renderText({
        if (input$selected1=="AFPS"){
            "Es posible notar que existen ángulos muy agudos, permitiendo decir que existe correlación entre
ellas, pero por supuesto, en diferente grado para las variables. Se puede observar también que para
la provincia de Pichincha no proporciona mucha información en el gráfico bidimensional."
        }else if (input$selected1=="AFPO"){
            "Se puede notar que las cargas se encuentran en su mayoría en el tercer cuadrante, y que los
vectores presentan ángulos más agudos, que explicaría correlación más significativa."
        }else if (input$selected1=="AFPOC"){
            "Finalmente aplicando rotación oblicua, se observa que las variables presentan ángulos más
abiertos que indican correlaciones no tan significativas."
        }
    })
    
    
    output$es5<-renderText({
        if (input$selected2=="GM"){
            "Utilizando el método de distancia Jaccard, aconsejable para saber las relaciones entre
variables, se puede ver que existe una mayor distancia entre los años 2007 y 2012 en
cuanto a indicadores de severidad de extrema pobreza. "
        }else if (input$selected2=="SH"){
            "El diagrama de Shepard ofrece la posibilidad de detectar cualquier anomalía. La
configuración estará bien ajustada cuando el diagrama refleje una función creciente. En este
caso se puede visualizar de esta manera, pero las similaridades se encuentran muy dispersas. "
        }
    })
    
    output$es4<-renderText({
        if (input$selected7=="GM"){
            "Al realizar los ajustes se obtiene como resultado puntos los cuales significan las ubicaciones o coordenadas principales en el
plano bidimensional que utilizamos k=2.
En el análisis multidimensional se usó el método de distancia Jaccard, aconsejable para saber
las relaciones entre variables, en la se puede observar como en su mayoría se relacionan
los años siendo los más distantes los periodos 2018-2019 en la parte inferior, y en la parte
superior 2017, 2012, 2016.
El estress de un 7% denota que no es adecuado para determinar que representa buena la representación."
        }else if (input$selected7=="SH"){
            "En el gráfico de Shepard, si los puntos dibujan una curva creciente se considera que la
representación es buena, ya que se puede decir que considera bien la preordenación.
Como se puede observar en el gráfico, este dibuja un modelo lineal y no una curva, por lo tanto,
se vuelve a afirmar que la representación no es buena."
        }
    })
    
    
    #  output$es1000<-renderText({
    #   if (input$selected7==""){
    #       ""
    # }else if(input$selected7==""){
    #     ""
    #  }
    # })   
    
    output$es7<-renderText({
        if (input$selected8=="gf"){
            "El análisis de correspondencia del gráfico  muestra variabilidad en dichos periodos
analizados, donde se denota que en el 2015 en las 5 principales provincias, se tuvo la menor
incidencia en ese intervalo de periodos, en el 2007 fue donde se tuvo la mayor incidencia de
pobreza, también se puede observar como se correlaciona la incidencia entre los años
2013, 2011, 2014 y 2009, donde se podría decir que se situaría la media del índice de pobreza
en esos periodos de las 5 principales provincias del Ecuador.
Se decidió hacer un análisis de correspondencia a las variables categóricas Pobreza y
Extrema Pobreza de la población muestral que obtuvimos en la encuesta del INEC para tener
una mayor apreciación de lo ya mencionado."
        }else if(input$selected8=="gc"){
            "En análisis de correspondencia se denota como Azuay es explicada por la
dimensión 2 y existe una menor incidencia en dicha Ciudad, una similar incidencia de
extrema pobreza posee Guayas y Pichincha, sin embargo, El Oro siendo una provincia con
una población mucho menor que dichas ciudades tiene mayor incidencia de extrema pobreza."
        }
    }) 
    
    output$es15<-renderText({
        if (input$selected3=="gf"){
            "En el análisis de correspondencia por filas nos denotan la variabilidad en los periodos
trabajados, donde, por el contrario, se observa que en el 2012 en las 5 principales provincias
existió una menor severidad, mientras que de igual manera en el 2007 fue donde se tuvo la
mayor severidad de extrema pobreza.
Se decidió hacer un análisis de correspondencia a las variables categóricas Pobreza y
Extrema pobreza de la población muestral que obtuvimos en la encuesta del INEC para tener
una mayor apreciación de lo ya mencionado.
"
        }else if(input$selected3=="gc"){
            "En el análisis de correspondencia podemos denotar como Azuay puede existir una menor
severidad, es decir, no es tanta la profundidad de pobreza, dentro de pobreza, comparándola
con las otras variables. Por otro lado, Pichincha y Tungurahua presentan severidades muy
similares."
        }
    }) 
    
    
    output$es8<-renderText({
        if (input$selected12=="gf"){
            "Se nota que el año con mayor incidencia en las provincias estudiadas es 2007 seguido del
año 2009 y el año 2010, estos 3 son los más incidentes en pobreza. Los años con menor
incidencia en pobreza son 2012, 2015 y 2019. "
        }else if(input$selected12=="gc"){
            "Se puede notar que en la provincia de Azuay se concentra menor cantidad de severidad de
pobreza desde el 2007 al 2019. En la provincia de El Oro es donde la concentración de
severidad de pobreza es mayor, seguido de este la provincia del Guayas.
La Provincia de Pichincha es la segunda provincia con menor severidad de pobreza. Se puede
notar que las dos provincias de la sierra ecuatoriana son las que presentan la mayor severidad
de pobreza. "
        }
    }) 
    
    output$es9<-renderText({
        if (input$selected18=="gf"){
            "En el siguiente gráfico se puede notar que el año con mayor incidencia en las provincias
estudiadas es 2007 seguido del año 2009 y el año 2010, estos 3 son los más incidentes en
pobreza. Los años con menor incidencia en pobreza son 2012, 2015 y 2019."
        }else if(input$selected18=="gc"){
            "En el siguiente gráfico se puede notar que en la provincia de Azuay se concentra menor
cantidad de incidencia de pobreza desde el 2007 al 2019. En la provincia de El Oro es donde
la concentración de incidencia de pobreza es mayor, sobrepasando los 14 puntos desde el
año 2007 al 2019. Seguido de este la provincia del Guayas con 13 puntos en incidencia de
pobreza desde el año 2007 al año 2019.
La Provincia de Pichincha es la segunda provincia con menor incidencia de pobreza. Se
puede notar que las dos provincias de la sierra ecuatoriana son las que presentan la mayor
incidencia de pobreza. "
        }
    }) 
    
    output$es200<-renderText({
        if (input$selected20=="viv"){
            "Se puede denotar en los diagrama de barras que tanto en el área urbana y rural viven una mayor cantidad
de personas en casa o villa, sin embargo, existe una mayor diferencia con el tipo de vivienda
departamento.
"
        }else if(input$selected20=="op3"){
            " Se observa una comparación entre el área y la opinión publica de cómo será su
situación en los posteriores 3 meses, donde la mayoría del área tanto rural y urbana contestaron en
apoyo al nivel “Igual” y en el nivel más bajo su contestación fue “Peor”, donde en otras palabras,
                 existe menor respuesta que a que la situación mejore y más apoyo a considerar que se estará en la misma
                 situación.
                 "
        }
    }) 
    
    output$es201<-renderText({
        if (input$selected21=="nivp"){
            " Se puede observar que existe una mayor incidencia de extrema pobreza
en Pichincha, Quito.
"
        }else if(input$selected21=="opp"){
            " En el gráfico se puede denotar el pensamiento de los encuestados sobre la situación del país en los
próximos meses, donde sus respuestas se disputan entre una situación “Igual” o “Peor” que la actual. 
                 "
        }else if(input$selected21=="comp2"){
            " Comparando las variables de Pobreza e Indigencia en las principales ciudades en el gráfico se observa como existe un
mayor número de pobreza en dichas provincias, llevándose el primer lugar Guayaquil, sin embargo,
en extrema pobreza se puede ver que resalta a Ambato.

                 "}else if(input$selected21=="cod"){
                     "La presente Gráfica muestra una comparacion entre los niveles pobre y no pobre con respecto a la condicion laboral que se encuentran las mencionados categorias"  
                 }
    }) 
    
    
    output$es203<-renderText({
        if (input$selected22=="edu"){
            "En el diagrama de barras, es posible notar que uno de los factores que influye mucho en la pobreza o
extrema pobreza es el nivel de educación, donde se nota que la mayoría de las personas en la
muestra de estudio ha concluido su nivel de estudio secundario, en cambio, en la región rural
terminan llegando solo hasta primaria.
Se puede denotar también que un mayor número de personas en el área urbana sigue sus estudios
hasta nivel de postgrado en comparación con el área urbana.

"
        }else if(input$selected22=="grad"){
            " En el análisis de nivel de educación el cual se ha escogido para denotar el alto
índice de pobreza, se analizará el máximo nivel al que la mayoría de los
encuestados han llegado, donde su mayor nivel en Primeria 6to año (conclusión de Primaria) de
ahí los sigue finalizando la secundaria (6to año), así mismo de forma contraria tenemos una
menor cantidad que ha finalizado su Postgrado.
                 "
        }else if(input$selected22=="raz"){
            " Profundizando este análisis, se nota que de la muestra de los encuestados, casi 12.000 personas
no concluyen sus estudios por el factor de trabajar, seguramente por la necesidad de ingresos.
Mientras que un poco más de 8.000 no los concluye por la edad, seguido de la falta de recursos
económicos con casi 6.000 personas.
Por lo contrario, entre los factores menos influyen a que no concluyan sus estudios son el fracaso
escolar, por asistir a nivelación SENESCYT, porque la familia no le permite estudiar, por
motivos de embarazo y por temor a sus compañeros.
                 "
        }
    }) 
    
    output$es300<-renderText({
        if (input$selected9=="ACPS"){
            "En el análisis de componentes principales sin rotación se puede denotar como las puntuaciones se estan alejando de las cargas, por lo tanto, podría indicar que haría falta
                    realizar una rotación para que haya una mayor correlación entre variables, lo cual es el objetivo."
        }else if (input$selected9=="ACPO"){
            "En este biplot se observar que los ángulos estan más agudos, lo cual indicaría que hay una fuerte correlación entre variables con la presente rotación."
        }else if (input$selected9=="ACPOC"){
            "En este biplot con rotación oblicua se observa como las puntuaciones se alejan de las cargas, así mismo los ángulos dejan de ser agudos, por lo cual no sea recomendable la presente rotación para una adecuada representación de la correlación entre variables. "
        }
        
    })          
    
    output$es301<-renderText({
        if (input$selected15=="ACPS"){
            "En este biplot sin rotación de la severidad en la pobreza se puede observar como las puntuaciones se están encontrando pegadas, lo cual es muy bueno ya que se tendría una mejor manera de 
                   encontrar una explicación sobre la varianza, también se puede denotar como los ángulos se encuentran agudos entre las cargas, lo cual indacaría una correlación intensa."
        }else if (input$selected15=="ACPO"){
            "En este biplor de componentes principales se puede observar como las puntuaciones se encuentran más pegadas, los ángulos más agudos, se podría decir que es una perfecta rotación que explicaría la correlación entre variables."
        }else if (input$selected15=="ACPOC"){
            "En el biplot de rotación oblicua se puede observar que no favoreció mucho puesto que se encuentra poca relación entre variables y puntuaciones más distintas."
        }
        
    })          
    
    output$es250<-renderText({
        if (input$selected10=="AFPS"){
            "Se puede denotar en este biplot de análisis factorial como las puntuaciones están muy distantes que las cargas, por lo tanto esta representación gráfica
                       no nos dice mucho sobre las correlaciones entre variables."
        }else if (input$selected10=="AFPO"){
            "Al realizar la rotacion ortogonal se puede denotar que los ángulos entre las cargas se encuentran más agudos, lo que indicaría que existe una intensa correlación entre variables,
                       así mismo, las puntuaciones se hallan con una cercanía entre ellas."
        }else if (input$selected10=="AFPOC"){
            "Es posible notar la rotación oblicua como las cargas de cada variable tiene ángulos ya no tan agudos, lo cual no nos explicaría su posible correlación entre las variables."
        }
    })
    
    output$es260<-renderText({
        if (input$selected16=="AFPS"){
            "En el presente biplot se puede observar como las puntuaciones se encuentran un poco alejadas de las cargas, por lo tanto, indicaría que el presente gráfico no ayuda a explicar la correlación entre variables."
        }else if (input$selected16=="AFPO"){
            "De forma distinta al anterior, al tener una rotación ortogonal se puede observar como se agrupan mayormente las puntuaciones y las cargas, también, como las cargas tienen un ángulo más agudo significaría una intensa correlación entre variables."
        }else if (input$selected16=="AFPOC"){
            "En este biplot se puede observar como al realizar una rotación oblicua se alejan tanto las cargas como las puntuaciones, lo que significaría que no representa muy bien esta rotación las correlaciones entre las variables estudiadas."
        }
    })
    
    output$es400<-renderText({
        if (input$selected11=="GM"){
            "Se puede observar en este biplot como las puntuaciones de los años de la incidencia de pobreza se encuentran mejor explicadas por los primeros dos cuadrantes."
        }else if (input$selected11=="SH"){
            "En el Gráfico de Sheppard se puede observar como sigue una forma de pendiente lo cual indicaría que sí se representa de forma adecuada las variables."
        }
    })
    output$es401<-renderText({
        if (input$selected17=="GM"){
            "En el biplot multidimensional se puede observar el comportamiento entre los años de la severidad de la pobreza en las principales 5 provincias del Ecuador."
        }else if (input$selected17=="SH"){
            "En el Gráfico de Sheppard se puede observar como sigue una forma de pendiente lo cual indicaría que sí se representan de forma adecuada las variables."
        }
    })
    
    
    output$es2001<-renderText({
        if (input$selected210=="si"){
            "El presente gráfico de barras muestra como en su mayoría existen más mujeres pobres que hombres, y así mismo, más mujeres que no consideran pobres que hombres en las 5 principales provincias del Ecuador."
        }else if (input$selected210=="raz"){
            "En la gráfica se puede denotar como existe un mayor número de hombres que han dejado sus estudios por trabajo, así mismo que existe un mayor número de mujeres con respecto a hombres que han decidido no continuar con sus estudios ya que no les interesa. Por otro lado, en la categoría edad se puede observar que las mujeres poseen en mayor cantidad a esta como una de las razones para dejar de estudiar."
        }else if (input$selected210=="niv"){
            "En la presente gráfica se denota como existe mayor número de mujeres que paralizan sus estudios en primaria y secundaria, así mismo de forma más favorable existe según lo muestreado una mayor cantidad de mujeres que concluyen sus estudios con un nivel superior universitario y de post-grado en comparación a los hombres."
        }else if (input$selected210=="cod"){
            "Mediante la base de datos de la encuesta ENEMDU se construyó la presente gráfica, donde muestra alarmantemente como existe una mayor cantidad de mujeres en la categoría de población económicamente inactiva con respecto a los hombres y así mismo en la categoría de empleo no remunerado."} 
        
    })
})

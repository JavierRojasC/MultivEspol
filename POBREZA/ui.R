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


library(shiny)

# Define UI for application that draws a histogram
shinyUI(dashboardPagePlus(title="Proyecto Multivariante",
                          
                          skin = "black",
                          dashboardHeaderPlus(title = "Análisis de la Pobreza y Extrema Pobreza",titleWidth = 450),
                          
                          dashboardSidebar( 
                              
                              sidebarMenu(
                                  menuItem("Introducción",tabName = "intro",icon = icon("fas fa-angle-double-right"),selected = TRUE),
                                  menuItem("Pobreza ", startExpanded = T,selected = TRUE,tabName = "nac", icon = icon("fas fa-angle-double-right"),
                                           
                                           
                                           menuItem("Incidencia",startExpanded = T,tabName = "inc",icon = icon("fas fa-angle-right"),
                                                    
                                                    menuSubItem("Data",tabName="dat1",icon=icon("fas fa-chart-pie")),
                                                    menuSubItem("Análisis Multivariante",tabName="inc3",icon=icon("far fa-chart-bar"))),
                                           
                                           menuItem("Severidad",startExpanded = T,tabName = "sev",icon = icon("fas fa-angle-right"),
                                                    
                                                    menuSubItem("Data",tabName="dat2",icon=icon("fas fa-chart-pie")),
                                                    menuSubItem("Análisis Multivariante",tabName="sev3",icon=icon("far fa-chart-bar")))),
                                  
                                  menuItem("Extrema Pobreza",startExpanded = T, tabName = "ext", icon = icon("fas fa-angle-double-right"),
                                           
                                           menuItem("Incidencia",startExpanded = T,tabName = "inc4",icon = icon("fas fa-angle-right"),
                                                    
                                                    menuSubItem("Data",tabName="dat3",icon=icon("fas fa-chart-pie")),
                                                    menuSubItem("Análisis Multivariante",tabName="inc6",icon=icon("far fa-chart-bar"))),
                                           
                                           menuItem("Severidad",startExpanded = T,tabName = "sev1",icon = icon("fas fa-angle-right"),
                                                    
                                                    menuSubItem("Data",tabName="dat4",icon=icon("fas fa-chart-pie")),
                                                    menuSubItem("Análisis Multivariante",tabName="sev5",icon=icon("far fa-chart-bar")))),
                                  
                                  menuItem("Preguntas de Interés",startExpanded = T, tabName = "prg", icon = icon("fas fa-angle-double-right"),
                                           
                                           menuItem("ENEMDU",startExpanded = T,tabName = "ene",icon = icon("fas fa-check"),
                                                    
                                                    menuSubItem("Gráficos",tabName="grf",icon=icon("fas fa-chart-bar")))),
                                  menuItem("Conclusiones",tabName = "conclu",icon = icon("fas fa-angle-double-right")))),
                          
                          
                          dashboardBody(
                              tabItems(
                                  
                                  
                                  
                                  tabItem(tabName = "conclu",fluidRow(
                                      tabBox(
                                          title = " ",
                                          id = "tabset10", height = "750px",width ="50%" ,
                                          tabPanel("Conclusiones",
                                                   fluidPage(fluidRow(box(title=" ","En el presente proyecto al efectuar el análisis descriptivo, bivariante y multivariante, se pudo denotar que en los últimos 13
años el El Oro aún siendo una provincia con la menor superficie y población de las analizadas
posee la mayor incidencia de extrema pobreza a comparación de las otras 4 provincias. Así mismo,
tanto Guayas, Pichincha y Tungurahua poseen un porcentaje de incidencia similar a la media, en
el caso de Azuay se podría concluir que es una provincia que en los últimos años ha ido
decreciendo su incidencia, por lo tanto este análisis se da como una antesala para continuar con
los factores o causas que Azuaya cada años tenga un menor número de incidencia de extrema
pobreza en su población.",br(),br(),
                                                                          "De igual manera, un punto importante a analizar es como desde el 2015, poseyendo la menor
incidencia de pobreza extrema en ese periodo de tiempo, el país ha vuelto incrementar su
incidencia de extrema pobreza como se denotó en el análisis de correspondencia. Gracias a las
preguntas de la encuesta del INEC que se analizó, se pudo denotar que el mayor factor de la
extrema pobreza es el nivel de educación.",br(),br(),
                                                                          "Sin embargo, esta asociación que se ha hecho puede ser tomada como una antesala a un futuro
análisis de dicha información, ya que en la tabla de causas que dejen los estudios o su nivel de
educación se obtuvo como resultado que fue por el deseo y preferencia de trabajar.",br(),br(),
                                                                          "Asimilando estos resultados, con el análisis de correspondencia se podría incluir como factor
que la mala administración pública en los últimos gobiernos ha logrado un incremento de
incidencia de extrema pobreza, y el nivel de educación solo sería una variable de confusión o
mediadora.",solidHeader = T,width = 12,align="justify"))),align="center")))),
                                  
                                  
                                  
                                  
                                  
                                  tabItem(tabName = "intro",fluidRow(
                                      tabBox(
                                          #title = "Introducción",
                                          id = "tabset10", height = "750px",width ="50%" ,
                                          tabPanel("Portada",
                                                   fluidPage(fluidPage(img(src = "https://github.com/JavierRojasC/MultivEspol/blob/master/POBREZA/espol.png?raw=true", height = 80, width = 300),align="center"),h3("ESCUELA SUPERIOR POLITÉCNICA DEL LITORAL",align="center"),br(),
                                                             h4("Análisis de la Pobreza y Extrema Pobreza de las principales provincias del Ecuador",br(),br(),"FACULTAD DE CIENCIAS NATURALES Y MATEMÁTICAS",
                                                                br(),br(),"Estadística Multivariante",br(),br(),"Diaz Romero Samuel Ismael",br(),br(),"Ortiz Goya Kenia Sintike",br(),br(),"Padilla Navarro Ashley Lissette",br(),br(),
                                                                "Sanyer Mosquera Wladimir Alejandro",br(),br(),"Soledispa Soriano Madison Madeline",br(),br(),"Carrera:",br(),br(),"Estadística",br(),br(),
                                                                "Dirigido por:",br(),br(),"Dr. Johny Javier Pambabay Calero",br(),br(),"GUAYAQUIL – ECUADOR ",
                                                                
                                                                
                                                                align="center"))),
                                          
                                          tabPanel("Introducción",
                                                   fluidPage(fluidRow(box(title=" ","Este proyecto pretende estudiar y mostrar el comportamiento de los datos obtenidos en la
Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU) realizada en Ecuador en
el año 2019 del mes de diciembre por el Instituto Nacional de Estadística y Censos (INEC).
Se realizará un análisis estadístico, el cual comprende de análisis descriptivo de las variables,
análisis inferencial, y un análisis multivariante",br(),br(),"Para realizar todos los análisis, los indicadores que se tomarán en cuenta son: ",br(),br(),
                                                                          "- Incidencias de pobreza: Variable que determina el cociente total de la
población muestral pobre y la población total muestral.",br(),br(),"- Severidad de pobreza: Indicador que mide la profundidad de la pobreza dentro de la
pobreza.",br(),br(),"Al utilizar las variables tanto de incidencia como severidad, se hará un estudio de la pobreza
y extrema pobreza del país, el cual se podrá visualizar por las cinco principales provincias
del Ecuador, siendo estas Guayas, Pichincha, Azuay, El Oro y Tungurahua.",br(),br(),"La población objetivo para efectos de esta encuesta realizada por el INEC, fueron las personas
que habitan en las cinco ciudades principales de Ecuador previamente mencionadas, de esta
población objetivo se trabajó con una muestra de 59.208 personas.",solidHeader = T,width = 12,align="justify"))))))),
                                  
                                  tabItem(h3("Data de la Incidencia de Pobreza en las 5 principales provincias del Ecuador"),br(),tabName = "dat1",fluidRow((DTOutput("dat_nac_inc")))),
                                  tabItem(h3("Data de la Severidad de Pobreza en las 5 principales provincias del Ecuador",align="right"),br(),tabName = "dat2",fluidRow((DTOutput("dat_nac")))),
                                  tabItem(h3("Data de la Severidad de Pobreza en las 5 principales provincias del Ecuador"),br(),tabName = "dat4",fluidRow((DTOutput("dat_ext")))),
                                  tabItem(h3("Data de la Incidencia de Extrema Pobreza en las 5 principales provincias del Ecuador"),br(),tabName = "dat3",fluidRow((DTOutput("dat_ext_inc")))),
                                  
                                  
                                  
                                  
                                  
                                  
                                  #INEC
                                  
                                  tabItem(tabName = "grf",
                                          h2("Análisis Preguntas ENEMDU"),fluidRow(
                                              tabBox(
                                                  title = " ",
                                                  id = "tabset9", height = "750px",width ="50%" ,
                                                  tabPanel("Segun área",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected20",
                                                                                                                                       label = ("Seleccione el tipo comparación que desee observar"),choices = c("Comparación tipo de vivienda"="viv","Opinión del encuestado en los próximos meses por área"="op3")))),
                                                                     fluidRow(column(width=12,box(highchartOutput("ki")),box(title="Interpretación",status="primary",textOutput("es200"),solidHeader = T,width = 4,align="justify"))))),
                                                  
                                                  tabPanel("Nivel de pobreza",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected21",
                                                                                                                                       label = ("Seleccione el tipo comparación que desee observar"),choices = c("Pobreza según ciudades"="nivp","Comparativa entre pobreza e indigencia"="comp2","Opinión nivel de pobreza futura"="opp","Condición laboral"="cod"))),
                                                                              fluidRow(column(width=12,box(highchartOutput("ki2")),box(title="Interpretación",status="primary",textOutput("es201"),solidHeader = T,width = 4,align="justify")))))),
                                                  
                                                  tabPanel("Género",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected210",
                                                                                                                                       label = ("Seleccione el tipo comparación que desee observar"),choices = c("Situación Socio-Económica"="si","Razón del porque no continuaron sus estudios"="raz","Nivel de estudio"="niv","Condición laboral"="cod"))),
                                                                              fluidRow(column(width=12,box(highchartOutput("ki4")),box(title="Interpretación",status="primary",textOutput("es2001"),solidHeader = T,width = 4,align="justify")))))),
                                                  
                                                  
                                                  tabPanel("Comparación factor educación",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected22",
                                                                                                                                       label = ("Seleccione el tipo comparación que desee observar"),choices = c("Comparación del nivel de educación por área"="edu","Comparación del grado alcanzado"="grad","Razones para no concluir estudios"="raz")))),
                                                                     fluidRow(column(width=12,box(highchartOutput("ki3")),box(title="Interpretación",status="primary",textOutput("es203"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  
                                                  
                                              ))),
                                  
                                  
                                  
                                  
                                  tabItem(tabName = "sev5",
                                          h2("Análisis Multivariante de la Severidad de Extrema Pobreza"),fluidRow(
                                              tabBox(
                                                  #title = "Análisis Multivariante",
                                                  id = "tabset5", height = "750px",width ="50%" ,
                                                  tabPanel("Análisis Bivariante",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected804",
                                                                                                                                       label = ("Seleccione la provincia"),choices = c("Pichincha"="PIN","Guayas"="GYE","El Oro"="ORO","Azuay"="AZU","Tungurahua"="TUN")))),
                                                                     fluidRow(column(width=12,box(highchartOutput("qq")),
                                                                                     box(title="Interpretación",status="primary",textOutput("es1003"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  
                                                  ,tabPanel("Correlación",fluidRow(column(width=12,box(highchartOutput("acm")),
                                                                                          box(title="Interpretación",status="primary","En el diagrama de correlación se puede notar que existen correlaciones entre las variables de
severidad entre las principales provincias estudiadas." ,br(),br(),"Se puede observar que, entre las
provincias de Azuay y Guayas, además de El Oro y Guayas existe una correlación fuerte de
0.81 y 0.84 respectivamente, en comparación a las demás variables, en cuanto a las
profundidades de pobreza dentro de la pobreza.",solidHeader = T,width = 4,align="justify")))),
                                                  
                                                  tabPanel("Análisis de Componentes Principales",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected",
                                                                                                                                       label = ("Seleccione el tipo de rotación que desea observar"),choices = c("ACP Sin Rotación"="ACPS","ACP con Rotación Ortongal"="ACPO","ACP con Rotación Oblicua"="ACPOC")))),
                                                                     fluidRow(column(width=12,box(plotlyOutput("cpm")),box(title="Interpretación",status="primary",textOutput("es1"),solidHeader = T,width = 4,align="justify"))))),
                                                  
                                                  
                                                  tabPanel("Análisis de Factorial",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected1",
                                                                                                                                       label = "Seleccione el tipo de rotación que desea observar",choices = c("AF Sin Rotación"="AFPS","AF con Rotación Ortongal"="AFPO","AF con Rotación Oblicua"="AFPOC")))),
                                                                     fluidRow(column(width=12,box(plotlyOutput("cfm")),box(title="Interpretación",status ="primary",textOutput("es2"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  ,tabPanel("Diagrama de Dispersión",fluidRow(column(width=12,box(highchartOutput("mml")),
                                                                                                     box(title="Interpretación",status="primary"," En el diagrama de dispersión se puede observar el comportamiento de los datos en cada año desde el 2007
al 2019.",solidHeader = T,width = 4,align="justify"))))                                               
                                                  
                                                  #tabPanel("Correlacion", "First tab content",fluidRow(column(width=12,box(highchartOutput("ack")),
                                                  #  box(title="Interpretacion",status="primary",br(),"buenas tardes señores",solidHeader = T,width = 4,align="center")))) 
                                                  
                                                  
                                                  ,tabPanel("Análisis Multidemensional",
                                                            fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected2",
                                                                                                                                        label = "Seleccione el tipo de rotación que desea observar",choices = c("Gráfico Multidimensional"="GM","Gráfico de Sheppard"="SH")))),
                                                                      fluidRow(column(width=12,box(plotlyOutput("rrmm")),box(title="Interpretación",status = "primary",textOutput("es5"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  ,tabPanel("Análisis de Correspondencia ",
                                                            fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected3",
                                                                                                                                        label = "Seleccione el contenido que desea observar",choices = c("Por años"="gf","Por ciudades - provincias"="gc")))),
                                                                      fluidRow(column(width=12,box(highchartOutput("ss")),box(title="Interpretación",status = "primary",textOutput("es15"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  
                                                  
                                                  ,tabPanel("Serie de Tiempo ", fluidRow(column(width=12,box(highchartOutput("ss1")),
                                                                                                box(title="Interpretación",status="primary","Esta serie de tiempo muestra como ha incrementado o no los niveles de severidad en las distintas
provincias estudias donde se puede notar que en Tungurahua el grado de severidad
incrementó bruscamente entre el 2015 y 2017, por el contrario, a El Oro que tuvo una
disminución del 2011 al 2012, pero el nivel de severidad aumentó de forma medianamente brusca para
el siguiente año.",br(),br(),"Cabe mencionar, que tal como se reflejó en la incidencia, para Azuay, este nivel de severidad
ha ido disminuyendo a través de los años.",solidHeader = T,width = 4,align="justify")))) 
                                                  
                                                  
                                                  ,tabPanel("Mapa del Ecuador ", fluidRow(column(width=12,box(highchartOutput("ss12")),
                                                                                                 box(title="Interpretación",status="primary","En mapa del Ecuador proyectado se puede observar que estos niveles de severidad se
encuentran de manera más fuerte en las provincias de Pichincha y Tungurahua, mientras que
para Azuay todo lo contrario.",solidHeader = T,width = 4,align="justify"))))           
                                                  
                                                  
                                                  
                                                  
                                                  
                                              ))),
                                  
                                  tabItem(tabName = "inc6",
                                          h2("Análisis Multivariante de la Incidencia de Extrema Pobreza"),fluidRow(
                                              tabBox(
                                                  #title = "Análisis Multivariante",
                                                  id = "tabset6", height = "750px",width ="50%" ,
                                                  
                                                  tabPanel("Análisis Bivariante",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected801",
                                                                                                                                       label = ("Seleccione la provincia"),choices = c("Pichincha"="PIN","Guayas"="GYE","El Oro"="ORO","Azuay"="AZU","Tungurahua"="TUN")))),
                                                                     fluidRow(column(width=12,box(highchartOutput("qqw")),
                                                                                     box(title="Interpretación",status="primary",textOutput("es10001"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  
                                                  
                                                  ,tabPanel("Correlación", fluidRow(column(width=12,box(highchartOutput("acw")),
                                                                                           box(title="Interpretación",status="primary","En primer lugar es importante recalcar que en esta sección se hará referencia a la incidencia de extrema pobreza de las 5 principales 
                                                                                   provincias del Ecuador." ,br(),br(),"El análisis descriptivo denota el comportamiento de las
variables con una incidencia promedio de 3.35 en El Oro, lo cual es un hallazgo que causa sorpresa
ya que la media de las dos principales ciudades con mayor población (Guayas y Pichincha)
tiene una menor incidencia.", br(), br(),"En el diagrama correlación se obtiene que existe una relación entre las variables de la
incidencia entre provincias principales en Ecuador. Se observa que la variable menor
correlación se encuentra entre Pichincha y Azuay con 0.33, sin embargo, Pichincha y
Tungurahua tiene el mismo valor.",solidHeader = T,width = 4,align="justify")))),
                                                  
                                                  tabPanel("Análisis de Componentes Principales",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected5",
                                                                                                                                       label = ("Seleccione el tipo de rotación que desea observar"),choices = c("ACP Sin Rotación"="ACPS","ACP con Rotación Ortongal"="ACPO","ACP con Rotación Oblicua"="ACPOC")))),
                                                                     fluidRow(column(width=12,box(plotlyOutput("cpw")),box(title="Interpretación",status="primary",textOutput("es"),solidHeader = T,width = 4,align="justify"))))),
                                                  
                                                  
                                                  tabPanel("Análisis de Factorial",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected6",
                                                                                                                                       label = "Seleccione el tipo de rotación que desea observar",choices = c("AF Sin Rotación"="AFPS","AF con Rotación Ortongal"="AFPO","AF con Rotación Oblicua"="AFPOC")))),
                                                                     fluidRow(column(width=12,box(plotlyOutput("cfw")),box(title="Interpretación",status ="primary",textOutput("es3"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  ,tabPanel("Diagrama de Dispersión",fluidRow(column(width=12,box(highchartOutput("aa")),
                                                                                                     box(title="Interpretación",status="primary"," En el diagrama de dispersión se puede observar el comportamiento de los datos en cada año desde el 2007
al 2019.",solidHeader = T,width = 4,align="justify"))))                                               
                                                  
                                                  
                                                  
                                                  ,tabPanel("Análisis Multidemensional",
                                                            fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected7",
                                                                                                                                        label = "Seleccione el tipo que desea observar",choices = c("Gráfico Multidimensional"="GM","Gráfico de Sheppard"="SH")))),
                                                                      fluidRow(column(width=12,box(plotlyOutput("rr")),box(title="Interpretación",status = "primary",textOutput("es4"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  ,tabPanel("Análisis de Correspondencia ", 
                                                            fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected8",
                                                                                                                                        label = "Seleccione el contenido que desea observar",choices = c("Por años"="gf","Por ciudades - provincias"="gc")))),
                                                                      fluidRow(column(width=12,box(highchartOutput("tt")),box(title="Interpretación",status = "primary",textOutput("es7"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  
                                                  
                                                  ,tabPanel("Serie de Tiempo ",fluidRow(column(width=12,box(highchartOutput("zz")),
                                                                                               box(title="Interpretación",status="primary","La serie de tiempo indica como El Oro en el 2009 ha tenido un incremento
brusco de pobreza y en el 2011 - 2013 un decrecimiento brusco, al contrario la incidencia en
Azuay ha venido a la decreciendo entre todos estos años, este tema es de interés
puesto que puede ser de antesala para que provincias como El Oro, puedan obtener resultados
como los que ha tenido Azuay, cabe recalcar que las demás provincias a simple vista se
pueden denotar que tienen siguen una serie estacionario, lo que significaría una media y
varianza constante.",solidHeader = T,width = 4,align="justify")))) 
                                                  
                                                  
                                                  ,tabPanel("Mapa del Ecuador " ,fluidRow(column(width=12,box(highchartOutput("ll")),
                                                                                                 box(title="Interpretación",status="primary","En el gráfico se observa como Azuay entre dichos años, ha tenido una menor
incidencia, confirmando los hallazdos en el análisis multivariante.", br(), br(),  "Así mismo,
Pichincha y Guayas se mantienen en similares números y al contrario El Oro, es el de mayor
incremento, dato alarmante por cantidad población y menor superficie.",solidHeader = T,width = 4,align="justify"))))           
                                                  
                                                  
                                                  
                                                  
                                                  
                                              ))),           
                                  
                                  
                                  
                                  
                                  
                                  
                                  tabItem(tabName = "inc3",
                                          h2("Análisis Multivariante de la Incidencia de Pobreza"),fluidRow(
                                              tabBox(
                                                  #title = "Analisis Multivariante",
                                                  id = "tabset7", height = "750px",width ="50%" ,
                                                  
                                                  
                                                  
                                                  tabPanel("Análisis Bivariante",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected800",
                                                                                                                                       label = ("Seleccione la provincia"),choices = c("Pichincha"="PIN","Guayas"="GYE","El Oro"="ORO","Azuay"="AZU","Tungurahua"="TUN")))),
                                                                     fluidRow(column(width=12,box(highchartOutput("qqk")),
                                                                                     box(title="Interpretación",status="primary",textOutput("es1000"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  ,tabPanel("Correlación",fluidRow(column(width=12,box(highchartOutput("ack")),
                                                                                          box(title="Interpretación",status="primary","Se puede denotar en el diagrama de correlación como El Oro con Tunguarahua son las variables que mayormente están correlacionadas, así mismo en 2do lugar se encuentra Guayas y El Oro, lo significaría que tienen valores muy cercanos en la incidencia de pobreza en dichas provincias.",solidHeader = T,width = 4,align="justify")))),
                                                  
                                                  tabPanel("Análisis de Componentes Principales",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected9",
                                                                                                                                       label = ("Seleccione el tipo de rotación que desea observar"),choices = c("ACP Sin Rotación"="ACPS","ACP con Rotación Ortongal"="ACPO","ACP con Rotación Oblicua"="ACPOC")))),
                                                                     fluidRow(column(width=12,box(plotlyOutput("cpk")),box(title="Interpretación",status="primary",textOutput("es300"),solidHeader = T,width = 4,align="justify"))))),
                                                  
                                                  
                                                  tabPanel("Análisis de Factorial",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected10",
                                                                                                                                       label = "Seleccione el tipo de rotación que desea observar",choices = c("AF Sin Rotación"="AFPS","AF con Rotación Ortongal"="AFPO","AF con Rotación Oblicua"="AFPOC")))),
                                                                     fluidRow(column(width=12,box(plotlyOutput("cfk")),box(title="Interpretación",status ="primary",textOutput("es250"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  ,tabPanel("Diagrama de Dispersión", fluidRow(column(width=12,box(highchartOutput("cc")),
                                                                                                      box(title="Interpretación",status="primary"," En el diagrama de dispersión se puede observar el comportamiento de los datos en cada año desde el 2007
al 2019.",solidHeader = T,width = 4,align="justify"))))                                               
                                                  
                                                  
                                                  
                                                  ,tabPanel("Análisis Multidemensional",
                                                            fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected11",
                                                                                                                                        label = "Seleccione el tipo de rotación que desea observar",choices = c("Gráfico Multidimensional"="GM","Gráfico de Sheppard"="SH")))),
                                                                      fluidRow(column(width=12,box(plotlyOutput("op")),box(title="Interpretación",status = "primary",textOutput("es400"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  ,tabPanel("Análisis de Correspondencia ",
                                                            fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected12",
                                                                                                                                        label = "Seleccione el contenido que desea observar",
                                                                                                                                        choices = c("Por años"="gf","Por ciudades - provincias"="gc")))),
                                                                      fluidRow(column(width=12,box(highchartOutput("ttk")),box(title="Interpretación",status = "primary",textOutput("es8"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  
                                                  
                                                  ,tabPanel("Serie de Tiempo ", fluidRow(column(width=12,box(highchartOutput("llk")),
                                                                                                box(title="Interpretación",status="primary","Las tendencias anuales de las provincias en algunas de estas tienen cambios bruscos,
empezando por describir la serie temporal de la provincia de Pichincha, esta inicia con una
incidencia de pobreza un poco mayor que 10, en el año 2008 la incidencia de pobreza
disminuyó notablemente a 9, y en el 2009, tuvo un crecimiento sobrepasando los niveles
del 2007, en el año 2010 llegó a su punto máximo de incidencia de pobreza a 12 puntos, en
el año 2019 llego a tener 10 puntos.", br(), br(), 
                                                                                                    "La provincia de El Oro al año 2007 tuvo una incidencia de pobreza de 24.5 siendo esta la
provincia con mayor incidencia entre las cinco provincias estudiadas, terminando en 2019
con una incidencia de 10.2 de pobreza. En Azuay, es notablemente que ha tenido menor incidencia en pobreza, teniendo solamente como incidencia
máxima en 11.", br(), br(),  "No se han realizado pronósticos al año 2020, debido a que sería realizar
un estudio erróneo ya que el año 2020 ha sido un caso particular y no sería lo más
beneficioso para el estudio, puesto que el aumento de pobreza en el país
desafortunadamente aumentó considerablemente.",solidHeader = T,width = 4,align="justify")))) 
                                                  
                                                  
                                                  ,tabPanel("Mapa del Ecuador ",fluidRow(column(width=12,box(highchartOutput("ss12k")),
                                                                                                box(title="Interpretación",status="primary","En el mapa del Ecuador se puede claramente apreciar la incidencia por provincia de
pobreza, mostrando así con el azul con más intensidad las provincias con mayor
incidencia de pobreza y con color azul opaco, las provincias con menor incidencia de
pobreza desde el año 2007 al año.", br(), br(),
                                                                                                    "Este gráfico se realizó con la finalidad de representar en las provincias estudiadas dicha incidencia en la pobreza.",solidHeader = T,width = 4,align="justify"))))           
                                                  
                                                  
                                                  
                                                  
                                                  
                                              ))),           
                                  tabItem(tabName = "sev3",
                                          h2("Análisis Multivariante de la Severidad de Pobreza"),fluidRow(
                                              tabBox(
                                                  #title = "Analisis Multivariante",
                                                  id = "tabset8", height = "750px",width ="50%" ,
                                                  tabPanel("Análisis Bivariante",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected803",
                                                                                                                                       label = ("Seleccione la provincia"),choices = c("Pichincha"="PIN","Guayas"="GYE","El Oro"="ORO","Azuay"="AZU","Tungurahua"="TUN")))),
                                                                     fluidRow(column(width=12,box(highchartOutput("qqa")),
                                                                                     box(title="Interpretación",status="primary",textOutput("es1001"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  
                                                  
                                                  
                                                  ,tabPanel("Correlación",fluidRow(column(width=12,box(highchartOutput("aca")),
                                                                                          box(title="Interpretación",status="primary","En este diagrama se puede observar como en el centro se encuentra una fuerte correlación entre variables, por ejemplo, entre Guayas y Azuay. Así mismo entre Tunguarahua y Guayas, se puede denotar por 
                                                                              el color más intenso donde se encuentra mayor correlacion y por el color menos intenso donde existe menor correlación entre las variables. ",solidHeader = T,width = 4,align="justify")))),
                                                  
                                                  tabPanel("Análisis de Componentes Principales",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected15",
                                                                                                                                       label = ("Seleccione el tipo de rotación que desea observar"),choices = c("ACP Sin Rotación"="ACPS","ACP con Rotación Ortongal"="ACPO","ACP con Rotación Oblicua"="ACPOC")))),
                                                                     fluidRow(column(width=12,box(plotlyOutput("cpa")),box(title="Interpretación",status="primary",textOutput("es301"),solidHeader = T,width = 4,align="justify"))))),
                                                  
                                                  
                                                  tabPanel("Análisis de Factorial",
                                                           fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected16",
                                                                                                                                       label = "Seleccione el tipo de rotación que desea observar",choices = c("AF Sin Rotación"="AFPS","AF con Rotación Ortongal"="AFPO","AF con Rotación Oblicua"="AFPOC")))),
                                                                     fluidRow(column(width=12,box(plotlyOutput("cfa")),box(title="Interpretación",status ="primary",textOutput("es260"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  ,tabPanel("Diagrama de Dispersión",fluidRow(column(width=12,box(highchartOutput("bb")),
                                                                                                     box(title="Interpretación",status="primary"," En el diagrama de dispersión se puede observar el comportamiento de los datos en cada año desde el 2007
al 2019.",solidHeader = T,width = 4,align="justify"))))                                               
                                                  
                                                  
                                                  
                                                  ,tabPanel("Análisis Multidemensional",
                                                            fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected17",
                                                                                                                                        label = "Seleccione el tipo de rotación que desea observar",choices = c("Gráfico Multidimensional"="GM","Gráfico de Sheppard"="SH")))),
                                                                      fluidRow(column(width=12,box(plotlyOutput("rraa")),box(title="Interpretación",status = "primary",textOutput("es401"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  ,tabPanel("Análisis de Correspondencia ",
                                                            fluidPage(fluidRow(column(2, offset = 0, style = 'padding:1px;',selectInput(inputId = "selected18",
                                                                                                                                        label = "Seleccione el contenido que desea observar",choices = c("Por años"="gf","Por ciudades - provincias"="gc")))),
                                                                      fluidRow(column(width=12,box(highchartOutput("tta")),box(title="Interpretación",status = "primary",textOutput("es9"),solidHeader = T,width = 4,align="justify")))))
                                                  
                                                  
                                                  
                                                  ,tabPanel("Serie de Tiempo ",fluidRow(column(width=12,box(highchartOutput("zza")),
                                                                                               box(title="Interpretación",status="primary","Las tendencias anuales de las provincias en algunas de estas tienen cambios bruscos,
empezando por describir la serie temporal de la provincia del Oro, que inicia con una severidad
de casi 4, en el año 2008 la severidad de pobreza disminuyó notablemente a casi 2, mientras que en el 2009
tuvo un leve crecimiento. En el año 2010 llegó a un punto alto de severidad de pobreza
siendo de 3 puntos, además de notarse cambio decreciente bastante considerable en el año 2012 con una severidad
que llegó aproximadamente a 0.5." , br(), br(), "En el año 2019 llegó a tener aproximadamente 1.9 puntos, siendo la provincia que llegó a tener el punto máximo de severidad seguida de la Provincia, Tungurahua, Guayas y Azuay.",solidHeader = T,width = 4,align="justify")))) 
                                                  
                                                  
                                                  ,tabPanel("Mapa del Ecuador ",fluidRow(column(width=12,box(highchartOutput("lla")),
                                                                                                box(title="Interpretación",status="primary","En el mapa del Ecuador se puede claramente apreciar la severidad por provincia de pobreza,
mostrando así con el azul con más intensidad las provincias con mayor severidad de pobreza
y con color azul opaco, las provincias con menor severidad de pobreza desde el año 2007 al
año.", br(), br(), "Este gráfico se realizó con la finalidad de representar en las provincias estudiadas y
poder apreciar de una forma cultural lo que se ha venido queriendo dar a conocer durante el
estudio de este conjunto de datos.",solidHeader = T,width = 4,align="justify"))))           
                                                  
                                                  
                                                  
                                                  
                                                  
                                              )))           
                                  
                                  
                                  
                                  
                              ))
)





)

#################PAgina de Metodos Descriptivos###############3


tabPanel( div( id="textoIconos",
  
              div( id="imgIconos",
                   tags$img(src ='www/iconos2/descriptivos.png', style= "width: 50px; margin-top: 0px; margin-bottom: 5px; height: 30px;")),
              div( 
                  "DESCRIPTIVOS" )
              ), value = "Tab 2",
           sidebarPanel( style =paste0(" height: 620px; overflow-y: auto; "),  width = 6,
                    
                         
                         fluidRow(
                           column(
                             #tags$b("VARIABLES"),
                             width = 6,
                             div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                 uiOutput("bucket_1_Descriptiva"))
                           ), 
                           column(width = 5,
                                  div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                      fluidRow(
                                        
                                        tags$div(id = 'placeholderDescriptivaDependiente')
                                      )),
                                  div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em", 
                                      fluidRow(width = 4,
                                               tags$div(id = 'placeholderDescriptivaAgrupamiento')
                                               # uiOutput("bucket_3_Ttest")
                                      ))
                           )
                         ),#fin fluid row
                      
                      uiOutput("checkbox"),
                      checkboxInput("check_tabla_Frecuencia", tags$div(tags$span(strong("Tablas de Frecuencia"),style = paste0('color: black;')),
                                                                       tags$span(class = "fa-stack",icon("chart-bar","fa-stack-1x",lib = "font-awesome"), style="color:grey")),FALSE),
                      actionButton("menuGraficas", "Gr\u00e1ficas"),
                      
                      div(
                        fluidRow(
                          column(6,div(id="bloqueDescriptivasPersonalizadas",
                                 div(id= "tituloGraficasPersonalizadas", tags$div(tags$span(strong("Gr\u00e1ficas Personalizadas"),style = "color: black;"))),
                                 
                                 #div(style="display: inline-block;vertical-align:top; width: 10px; " , id= "br_Descriptivos_Boxplot"),
                                 div(style="display: inline-block;vertical-align:top;  ", id ="checkBoxplotsEnable",checkboxInput("checkBoxplotsEnables", "Boxplots", FALSE)),
                                 
                                 div( style = "width: 10px;",id= "br_Descriptivos_Boxplot2"),
                                 
                                 div( style = "display: inline-block; vertical-align:top; width: 20px; margin-top: -20px ",id= "br_Descriptivos_Boxplot3"),
                                 div( style="display: inline-block;vertical-align:top;  ",  id = "Br_bloqueGraficaBoxplot" ,
                                      
                                      div(style="margin-top: -20px", id ="checkBoxplotElement",checkboxInput("checkBoxplotElements", "Element Boxplot", FALSE)),
                                      div(style="margin-top: -15px", id ="checkBoxplotViolin",checkboxInput("checkBoxplotViolins", "Violin Boxplot", FALSE)),
                                      div( style="margin-top: -15px", id ="checkBoxplotJitter",checkboxInput("checkBoxplotJitters", "Jitter Boxplot", FALSE))
                                 ),#Fin Div engloba opciones Boxplot 
                                 
                             ############################    ############ CHECKBOX GRAFICO DISOERSION  #################################
                                 div( id = "br_Descriptivos_Dispersion0"),
                                # div(style="display: inline-block;vertical-align:top; width: 10px; " , id= "br_Descriptivos_Dispersion"),
                                 div(style="display: inline-block;vertical-align:top; ", id ="checkDispersionEnable",checkboxInput("checkDispersionEnables" ,"Graficos Dispersi\u00f3n",FALSE)),
                                 
                                 div( style = "width: 10px;",id= "br_Descriptivos_Dispersion2"),
                                 
                                 div( style = "display: inline-block;vertical-align:top; width: 20px; ",id= "br_Descriptivos_Dispersion3 "),
                                 div( style = "display: inline-block;vertical-align:top;  ",id= "radioButtonplotEncima", radioButtons("radioButtonplotEncimas","Sobre el grafico de Disapersi\u00f3n", c("Densidad"="den","Histograma"="Hist","Ninguno"="Ning."),selected = "Ning.")),

                                 div( style = "width: 10px;",id= "br_Descriptivos_Dispersion4"),

                                 div(style="display: inline-block;vertical-align:top; width: 20px; " , id= "br_Descriptivos_Dispersion5"),
                                 div(style="display: inline-block;vertical-align:top;  ", id ="radioButtonplotDerecho",radioButtons("radioButtonplotDerechos","Derecha del grafico de dispersi\u00f3n", c("Densidad"="den","Histograma"="Hist","Ninguno"="Ning."),selected = "Ning.")),

                                 #######LInea de Regresion
                                 div( id= "br_Descriptivos_Dispersion6"),

                                 div(style="display: inline-block;vertical-align:top; width: 20px; " , id= "br_Descriptivos_Dispersion7"),
                                 div(style="display: inline-block;vertical-align:top;  ", id ="checkDispersionRegresionEnable",checkboxInput("checkDispersionRegresionEnables" ," Regresi\u00f3n Lineal", FALSE)),

                                 div( style = "width: 10px;",id= "br_Descriptivos_Dispersion8"),

                                 div(style="display: inline-block;vertical-align:top; width: 30px; " , id= "br_Descriptivos_Dispersion9"),
                                 div(style="display: inline-block;vertical-align:top; margin-top: -25px ", id ="radioButtonplotRegresion",radioButtons("radioButtonplotRegresions","", c("Smooth"="Smooth","Lineal"="Lineal"),selected = "Smooth")),

                                 div( style = "width: 10px;",id= "br_Descriptivos_Dispersion10"),

                                 div(style="display: inline-block;vertical-align:top; width: 30px; " , id= "br_Descriptivos_Dispersion11"),
                                 div(style="display: inline-block;vertical-align:top;  margin-top: -20px  " , id ="checkDispersionRegresionIntervalo",checkboxInput("checkDispersionRegresionIntervalos" ," Intervalo Confidencia", FALSE)),
                                 div(style="display: inline-block;vertical-align:top; width: 50px; margin-top: -20px " , id ="txtInputDispersionRegresionIntervalo", textInput("txtInputDispersionRegresionIntervalos",label= NULL,value = "95")),
                                 div( style="display: inline-block;vertical-align:top; width: 2px;margin-top: -20px   ", id ="txtDispersionRegresionIntervalos",h4("%"))

                          ) # fin bloque personalizada
                          ),
                          column(6,div( id="bloqueDescriptivasBasicas",
                                 div(id= "tituloGraficasBasicas",tags$div(tags$span(strong("Gr\u00e1ficas Personalizadas"),style = "color: black;"))),
                                # div(style="display: inline-block;vertical-align:top; width: 10px; " , id= "br_Descriptivos_Grafica"),
                                 div(style="display: inline-block;vertical-align:top;  ", id = "BloqueGraficaBasica" ,

                                     div( id ="checkDistribucion",checkboxInput("checkDistribucions", "Gr\u00e1fica Distribuci\u00f3n", FALSE)),

                                     div( style="display: inline-block;vertical-align:top; width: 10px; margin-top: -20px " , id ="br_Descriptivos_Grafica_Densidad"),
                                     div( style="display: inline-block;vertical-align:top; margin-top: -20px ", id ="checkDensidad",checkboxInput("checkDensidads", "Grafica Densidad", FALSE)),
                                     div( style=" margin-top: -20px ", id ="checkCorrelacion",checkboxInput("checkCorrelacions", "Grafica Correlaci\u00f3n", FALSE)),
                                     div( style=" margin-top: -15px ", id ="checkPastel",checkboxInput("checkPastels", "Grafico Pastel", FALSE)),
                                     div( style="margin-top: -15px ", id ="checkQQ",checkboxInput("checkQQs", "Grafica Q_Q", FALSE)))
                          
                                 ) #Fin bloque div de Basicas

                           )
                          
                        )), #fin del div
                      
                      actionButton("menuEstadistica", "Estadística"),
                      
                      fluidRow(

                        column(6,div(id="bloqueValoresPorcentilesColumna1",

                               tags$div(tags$span(strong("Valores Porcentiles"),style = "color: black;")),
                               

                              # div( style="display: inline-block;vertical-align:top; width: 20px; " ,   id= "br_Descriptiva_Estadistica1"),
                               div( style="display: inline-block;vertical-align:top;  ", id ="checkInputDescriptivoEstadisticaQuartiles",checkboxInput("checkInputDescriptivoEstadisticaQuartiless", "Quartil", FALSE)),

                               div( style = "width: 10px;",id= "br_Descriptiva_Estadistica2"),

                             #  div(style="display: inline-block;vertical-align:top; width: 20px; " , id= "br_Descriptiva_Estadistica3"),
                               div(style="display: inline-block;vertical-align:top; margin-top: -20px ", id ="checkInputDescriptivaEstadisticaPuntoCorte",checkboxInput("checkInputDescriptivaEstadisticaPuntoCortes" ," Puntos de Corte : ")),
                               div(style="display: inline-block;vertical-align:top; width: 50px; margin-top: -12px " , id ="txtInputDescriptivaEstadisticaPuntoCortes", textInput("txtInputDescriptivaEstadisticaPuntoCortess",label= NULL,value = "4")),
                               div( style="display: inline-block;vertical-align:top; width: 2px;margin-top: -12px   ", id ="txtDescriptivaEstadisticaPuntoCortes",h5("grupos")),

                               div( style = "width: 10px;",id= "br_Descriptiva_Estadistica4"),

                            #   div(style="display: inline-block;vertical-align:top; width: 20px; " , id= "br_Descriptiva_Estadistica5"),
                               div(style="display: inline-block;vertical-align:top; margin-top: -18px ", id ="checkDescriptivaEstadisticaPercentiles",checkboxInput("checkInputDescriptivaEstadisticaPercentiless" ," Percentiles : ")),
                               div(style="display: inline-block;vertical-align:top; width: 50px; margin-top: -8px; " , id ="txtInputDescriptivaEstadisticaPercentiles", textInput("txtInputDescriptivaEstadisticaPercentiless",label= NULL,value = "5")),
                               

                               tags$div(tags$span(strong("Dispersi\u00f3n"),style = "color: black;")),
                               
                               fluidRow(
                                 column(6,
                                        
                                        #div( style="display: inline-block;vertical-align:top; width: 20px; " ,   id= "br_Descriptiva_DispersionValores"),
                                        div( style="display: inline-block;vertical-align:top;  ", id ="checkInputDescriptivoDispersionSEmean",checkboxInput("checkInputDescriptivoDispersionSEmeans", "S.E. mean", FALSE)),
                                        
                                        div( style = "width: 10px;",id= "br_Descriptiva_DispersionValores1"),
                                       # div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -20px " ,   id= "br_Descriptiva_DispersionValores2"),
                                        div( style="display: inline-block;vertical-align:top; margin-top: -20px ", id ="checkInputDescriptivoDispersionMAD",checkboxInput("checkInputDescriptivoDispersionMADs", "MAD", FALSE)),
                                        
                                        div( style = "width: 10px;",id= "br_Descriptiva_DispersionValores3"),
                                       # div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -20px " ,   id= "br_Descriptiva_DispersionValores4"),
                                        div( style="display: inline-block;vertical-align:top; margin-top: -20px ", id ="checkInputDescriptivoDispersionIQR",checkboxInput("checkInputDescriptivoDispersionIQRs", "IQR", FALSE)),
                                        
                                        div( style = "width: 10px;",id= "br_Descriptiva_DispersionValores5"),
                                      #  div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -20px " ,   id= "br_Descriptiva_DispersionValores6"),
                                        div( style="display: inline-block;vertical-align:top; margin-top: -20px ", id ="checkInputDescriptivoDispersionRange",checkboxInput("checkInputDescriptivoDispersionRanges", "Range", FALSE)),
                                        
                                        
                                        div( style = "width: 10px;",id= "br_Descriptiva_DispersionValores51"),
                                      #  div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -20px " ,   id= "br_Descriptiva_DispersionValores61"),
                                        div( style="display: inline-block;vertical-align:top; margin-top: -20px ", id ="checkInputDescriptivoDispersionMaximo",checkboxInput("checkInputDescriptivoDispersionMaximos", "Maximo", TRUE)),
                                        
                                        ), #Fin columna 1 Valores de Disperson
                                 column(6,
                                        
                                        #div( style="display: inline-block;vertical-align:top; width: 20px; " ,   id= "br_Descriptiva_DispersionValores7"),
                                        div( id ="checkInputDescriptivoDispersionDesviacionStd",checkboxInput("checkInputDescriptivoDispersionDesviacionStds", "Desviacion Std.", TRUE)),
                                        
                                        #div( style = "width: 10px;",id= "br_Descriptiva_DispersionValores8"),
                                        
                                       # div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -25px " ,   id= "br_Descriptiva_DispersionValores9"),
                                        div( style=" margin-top: -10px ", id ="checkInputDescriptivoDispersionMADRobusto",checkboxInput("checkInputDescriptivoDispersionMADRobustos", "MAD Robusto", FALSE)),
                                        div( style=" margin-top: -10px ", id ="checkInputDescriptivoDispersionVarianza",checkboxInput("checkInputDescriptivoDispersionVarianzas", "Varianza", FALSE)),
                                        div( style="margin-top: -10px ", id ="checkInputDescriptivoDispersionMinimos",checkboxInput("checkInputDescriptivoDispersionMinimos", "Minimo", TRUE)),

                                        )#Fin columna 2 Valores de Disperson
                               )

                        ) # Fin div bloque
                        ), # fin columna 1
                        column(6, div(id="bloqueValoresPorcentilesColumna2",
                               
                               tags$div(tags$span(strong("Tendencia Central"),style = "color: black;")),        
                               #h5("Tendencia Central"),

                               div(  id ="checkInputDescriptivoEstadisticaTendenciCentralMedia",checkboxInput("checkInputDescriptivoEstadisticaTendenciCentralMedias", "Media", TRUE)),
                               div( style ="margin-top: -15px;", id ="checkInputDescriptivoEstadisticaTendenciCentralMediana",checkboxInput("checkInputDescriptivoEstadisticaTendenciCentralMedianas", "Mediana", FALSE)),
                               div( style ="margin-top: -15px;", id ="checkInputDescriptivoEstadisticaTendenciCentralModa",checkboxInput("checkInputDescriptivoEstadisticaTendenciCentralModas", "Moda", FALSE)),
                               div(style ="margin-top: -15px;", id ="checkInputDescriptivoEstadisticaTendenciCentralSuma",checkboxInput("checkInputDescriptivoEstadisticaTendenciCentralSumas", "Suma", FALSE)),
                        )#Fin columna 2

                        )  #fin columna 2

                      ),  # fin FluidROW de menu sidebr estadisticas

         ), #fin del sidebar panel Descriptivas
         
         mainPanel(style =paste0("background-color: transparent; height: 585px; overflow-y: auto; "),width = 6,
                   
                   tags$div(tags$span(strong(h3("Estadísticas Descriptivas")),style = "color: black;")),
                   #p("Por favor arrastre las variables al cajon adecuado, en la primera pagina se puede identificar el tiplo de variable "),
                 
                   # div( id="botonDescargarDescriptivo",downloadButton("descargaTablaDescriptivos","",
                   #                                                    style = "color: red;
                   #                                                    background-color: white;
                   #                                                    height: 35px;
                   #                                                    width: 35px;
                   #                                                    background-image:none
                   #                                                    "
                   #                                                    ))
                        
                        
                        
                   gt::gt_output("tablaDescriptivos"),
                  
              
                   #Condiciones para Graficas Basicas
                   conditionalPanel("input.check_tabla_Frecuencia == true", h2("Tabla de Frecuencia"), gt::gt_output("tablaDescriptivos_Frecuencias")),
                   conditionalPanel("input.checkDistribucions == true", h2("Grafica de Distribucion"),plotOutput("graficaDistribucion")),
                   conditionalPanel("input.checkCorrelacions == true", h2("Grafica de Correlacion"), plotOutput("graficaCorrelacion")),
                   conditionalPanel("input.checkPastels == true", h2("Grafica de Pastel"),plotOutput("graficaPie")),
                   conditionalPanel("input.checkQQs == true", h2("Grafica Q-Q"), plotOutput("graficaQQ")),
                   # COndiciones para Grafica Boxplot
                   conditionalPanel("input.checkDispersionEnables == true", h2("Grafica Dispersion"), plotOutput("DescriptivosDispersion")),
                  # conditionalPanel("input.checkBoxplotElements == true || input.checkBoxplotJitters == true || input.checkBoxplotViolins == true", h2("Boxplots"), plotOutput("DescriptivosBoxplots"))
                  conditionalPanel("input.checkBoxplotsEnables == true", h2("Boxplots"), plotOutput("DescriptivosBoxplots"))
                  
                  

         )#fin del main panel
)
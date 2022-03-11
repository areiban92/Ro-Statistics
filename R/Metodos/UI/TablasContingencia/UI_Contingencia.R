################Tablas  Contingencia

tabPanel(
  div(  id="textoIconos",
        tags$img(src ='www/iconos2/contingencia.png', style= "width: 50px; margin-top: 0px; margin-bottom: 5px; height: 34px"),
  div(
      "CONTINGENCIA")
  
  ),value = "Tab 4",
         sidebarPanel( style =paste0(" height: 650px; overflow-y: auto; "), width = 6, 

                       fluidRow(
                         column( width = 6,
                       
                                 div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                               uiOutput("bucket_1_Tcontingencia"))
                           ),
                         column(width = 5,
                                div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                    fluidRow(
                                      #tags$b("VARIABLES2"),
                                      tags$div(id = 'placeholderContingenciaDependiente')
                                      
                                      #uiOutput("bucket_2_Tcontingencia")
                                    )),
                                div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em",
                                    fluidRow(width = 4,
                                             
                                             tags$div(id = 'placeholderContingenciaAgrupamiento')
                                             
                                             #uiOutput("bucket_3_Tcontingencia")
                                    ))
                         )
                       ),#fin fluid row
                       
                       actionButton("menuTestContingencia", " Test "),
                       fluidRow(  div(id="bloqueTestContingencia",
                         
                         column(3,
                                
                                tags$div(tags$span(strong(("Test")),style = "color: black;")),
                               
                                    
                                    div( id ="chiCuadradoContingenciaEnable",checkboxInput("chiCuadradoContingenciaEnables", HTML(paste0("X",tags$sup("2"))), FALSE)),
                                    div( style=" margin-top: -10px;", id ="fisherTestContingenciaEnable",checkboxInput("fisherTestContingenciaEnables", "Fisher Test", FALSE))
                                    
                                
                                
                                ),
                         column(width = 4,
                                
                                tags$div(tags$span(strong(("Hip\u00d3tesis")),style = "color: black;")),
                                
                                
                              #  div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -10px; " ,   id= "br_Contingencia_Hipotesis1"),
                                
                                div( style="display: inline-block;vertical-align:top; margin-top: -10px;", id ="br_Contingencia_Hipotesis2", 
                                     div( id= "radioContingenciaHypotesis",radioButtons("radioInputContingenciaHypotesis", "",
                                                                                c("Grupo 1  != Grupo 2"="hipotesis_Igual",
                                                                                  "Grupo 1 > Grupo 2"="hipotesis_Mayor",
                                                                                  "Grupo 1 < Grupo 2"="hipotesis_Menor"
                                                                                ),
                                                                                selected = "hipotesis_Igual"))) #Fin radioButons

                         ), #fin columna2
                         column(width = 5,
                                
                                tags$div(tags$span(strong(("Estadística")),style = "color: black;")),
                                
                                div(
                                  div( id ="check_Contingencia_Parametro_Ubicacion",checkboxInput("checkInput_Contingencia_Parametro_Ubicacion", "Parametro Ubicaci\u00f3n", FALSE)) ,
                                  div( style="display: inline-block;vertical-align:top; width: 10px;", id ="br_Contingencia_Intervalo_Confidencia"),
                                  div( style="display: inline-block;vertical-align:top; width: 85px; margin-top: -20px; ", id ="check_Contingencia_Intervalo_Confidencia",checkboxInput("checkInput_Contingencia_Intervalo_Confidencia", "Int Confid", FALSE)),
                                  div( style="display: inline-block;vertical-align:top; width: 45px; margin-top: -10px; ", id ="input_Contingencia_Intervalo_Confidencia",textInput("txtInput_Contingencia_Intervalo_Confidencia",label= NULL,value = "95"))
                                  
                                )
                         )      
                         )),  #Fin de fluidRow
                         
                       actionButton("menuPorcentajesContingencia", " Porcentajes "),
                       fluidRow( div( id= "bloquePorcentajesContingencia",  
                           column(6,
                                  
                                  tags$div(tags$span(strong((" Porcentajes")),style = "color: black;")),
                                  
                                 # div( style=" width: 10px;", id ="br_Contingencia_Porcentajes1"),

                                   div( style="display: inline-block;",

                                    div(  id ="check_Contingencia_PorcentajeFilas", checkboxInput("checkInput_Contingencia_PorcentajeFilas","Filas",FALSE)),
                                    div( style = " margin-top: -15px; ", id ="check_Contingencia_PorcentajeFilas", checkboxInput("checkInput_Contingencia_PorcentajeColumnas","Columnas",FALSE)),

                                  )

                                  ), #Fin de columnas de procentajes
                           column(6,
                                  
                                  tags$div(tags$span(strong((" Conteo:")),style = "color: black;")),
                                  
                                  div( checkboxInput("checkInput_Contingencia_Conteo","Esperado",FALSE))
         
                                  )
                              ) # fin del DIV
                           ) # Fin Fluid Row
                       ),
         mainPanel(style =paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6, 
           
                   tags$div(tags$span(strong(h3("An\u00e1lisis Tablas Contingencia")),style = "color: black;")),
           
                            gt::gt_output("tablaContingencia"),
           conditionalPanel("input.chiCuadradoContingenciaEnables == true", h2("Test Chi Cuadrado"),
                            
                            gt::gt_output("tablaChiCUadradoContingencia")),
           conditionalPanel("input.fisherTestContingenciaEnables == true", h2("Test Fisher "),
                            
                            gt::gt_output("tablaFisherContingencia")),
           
           conditionalPanel("input.checkInput_Contingencia_Conteo == true", h2("Conteo Esperado"),
                            
                            gt::gt_output("tablaConteoEsperado"))
           
           
           )
)

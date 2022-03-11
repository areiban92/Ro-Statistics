tabPanel("Regresi\u00f3n Binomial",value = "RegresionBinomial",
         
         
         sidebarPanel(style =paste0(" height: 650px; overflow-y: auto; "), width = 6,
                      
                      
                      fluidRow(
                        column(

                          width = 6,
                          div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                              uiOutput("bucket_1_RegresionBinomial"))
                        ), 
                        column(width = 5,
                               div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                   fluidRow(
                                     
                                     tags$div(id = 'placeholderRegresionBinomialDependiente')
                                   )),
                               div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em", 
                                   fluidRow(width = 4,
                                            tags$div(id = 'placeholderRegresionBinomialAgrupamiento')
                                            
                                   ))
                        )
                      ),#fin fluid row
                      
                      fluidRow(
                          column(6,
                                 
                                 div(style=" vertical-align:top;  ", id ="efectosEnable_RegresionBinomial",checkboxInput("check_Input_efectos_RegresionBinomial", "Efectos fijos y aleatorios", FALSE)),
                          )
                     #   column(6,

                       # )
                      )
                      
         ),
         
         mainPanel(style =paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6,
                   
                   h3("An\u00e1lisis Regresion Binomial"),
                   gt::gt_output("tablaRegresionBinomialResumenR"),
                   br(),
                   gt::gt_output("tablaRegresionBinomial")
             #      conditionalPanel("input.check_Input_efectos_RegresionBinomial == true  ", h3("Efectos Fijos y Aleatorios"), gt::gt_output("tablaEfectosFijos"),gt_output("tablaRegresionBinomialEfectosAleatorios"))
    
         ) # Fin de main panel
)

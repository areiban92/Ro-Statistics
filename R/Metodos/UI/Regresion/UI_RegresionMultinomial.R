tabPanel("Regresi\u00f3n Multinomial",value = "RegresionMultinomial",
         
         
         sidebarPanel(style =paste0(" height: 650px; overflow-y: auto; "), width = 6,
                      
                      
                      fluidRow(
                        column(
                          #tags$b("VARIABLES"),
                          width = 6,
                          div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                              uiOutput("bucket_1_RegresionMultinomial"))
                        ), 
                        column(width = 5,
                               div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                   fluidRow(
                                     
                                     tags$div(id = 'placeholderRegresionMultinomialDependiente')
                                   )),
                               div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em", 
                                   fluidRow(width = 4,
                                            tags$div(id = 'placeholderRegresionMultinomialAgrupamiento')
                                            
                                   ))
                        )
                      ),#fin fluid row
                      fluidRow(
                        
                        
                        actionButton("nivelReferenciaRegresionMultinomial", "Niveles de Referencia"),
                      ),
                      fluidRow(
                        column(6,
                               h5("Seleccione la Variable :")
                               
                               ),
                        column(6,
                               
                               uiOutput("nivelRefenciaRegresionMultinomial")
                               
                               )
                        
                      )
         ),
         
         mainPanel(style =paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6,
                   
                   h3("An\u00e1lisis Regresion Multinomial"),
                   gt::gt_output("tablaRegresionMultinomialResumenR"),
                   br(),
                   gt::gt_output("tablaRegresionMultinomial")
                   
         ) # Fin de main panel
)

tabPanel("Regresi\u00f3n Lineal",value = "RegresionLineal",
         sidebarPanel(style =paste0(" height: 650px; overflow-y: auto; "), width = 6,
                      
                      
                      fluidRow(
                        column(
                          #tags$b("VARIABLES"),
                          width = 6,
                          div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                              uiOutput("bucket_1_RegresionLineal"))
                        ), 
                        column(width = 5,
                               div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                   fluidRow(
                                     
                                     tags$div(id = 'placeholderRegresionLinealDependiente')
                                   )),
                               div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em", 
                                   fluidRow(width = 4,
                                            tags$div(id = 'placeholderRegresionLinealAgrupamiento')
                                            
                                   ))
                        )
                      )#fin fluid row
         ),
         
         mainPanel(style =paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6,
                   
                   h3("An\u00e1lisis Regresion Lineal"),
                   gt::gt_output("tablaRegresionLinealResumenR"),
                   br(),
                   gt::gt_output("tablaRegresionLineal")
                   
                   
         ) # Fin de main panel
)


tabPanel("Correlaci\u00f3n ",value = "RegresionCorrelacion",
          sidebarPanel( width = 6, 
#                        
                       fluidRow(
                         column( width = 6,
                                 
                                 div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                     uiOutput("bucket_1_Correlacion"))
                                 ),
                         column(width = 5,
                                div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                    fluidRow(
                                      #tags$b("VARIABLES2"),
                                      #uiOutput("bucket_2_Correlacion")
                                      tags$div(id = 'placeholderCorrelacionDependiente')
                                    )),
                                div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em",
                                    fluidRow(width = 4,
                                             
                                            # uiOutput("bucket_3_Correlacion")
                                    ))
                         )
                       ),#fin fluid row
                          actionButton("menuTestCorrelacion", " Test "),
                       fluidRow( div (id ="bloqueTestCorrelacion",

                         column(6,
                                
                                tags$div(tags$span(strong(("Coeficiente de Correlaci\u00d3n")),style = "color: black;")),
                                
                                
                                 #   div( style="display: inline-block;vertical-align:top; width: 20px; " ,   id= "br_Correlacion_Tipo0"),
                                    
                                    div(  style="display: inline-block;vertical-align:top;" ,  id="bloqueEstadisticaCorrelacion",
                                
                                    div( id ="check_Correlacion_Pearson",checkboxInput("checkInput_Correlacion_Pearson", "Pearson's r", FALSE)),
                                    div(  style ="margin-top: -15px;", id ="check_Correlacion_Spearman",checkboxInput("checkInput_Correlacion_Spearman", "Spearman’s rho", FALSE)),
                                    div(  style ="margin-top: -15px;",  id ="check_Correlacion_Kendall",checkboxInput("checkInput_Correlacion_Kendall", "Kendall’s tau", FALSE))
                                    

                                    
                                    
                                    # div( style="display: inline-block;vertical-align:top; width: 20px; " ,   id= "br_Ttest_Tipo0"),
                                    # div(  style="display: inline-block;vertical-align:top;  " , id= "br_Ttest_Tipo1",
                                    #       div(  id ="check_Ttest_Student",checkboxInput("checkInput_Ttest_Student", "Student", FALSE)),
                                    #       div(  style ="margin-top: -15px;", id ="check_Ttest_Welch",checkboxInput("checkInput_Ttest_Welch", "Welch", FALSE)),
                                    #       div(  style ="margin-top: -15px;",  id ="check_Ttest_Mann_Whitney",checkboxInput("checkInput_Ttest_Mann_Whitney", "Mann-Whitney", FALSE))
                                    # ), #fin del div
                                    
                                ) # Fin del div

                         ), # Fin columna 1
                         column(6,
                                
                                tags$div(tags$span(strong(("Hip\u00f3tesis")),style = "color: black;")),
                                
                            #    div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -10px; " ,   id= "br_Correlacion_Hipotesis1"),
                                div( style="display: inline-block;vertical-align:top; margin-top: -10px;", id ="br_Correlacion_Hipotesis2", 
                                     div(id= "radioCorrelaciontHypotesis",radioButtons("radioInputCorrelacionHypotesis", "",
                                                                                c("Correlaci\u00f3n"="correlacion",
                                                                                  "Correlaci\u00f3n Positiva"="correlacion_Positiva",
                                                                                  "Correlaci\u00f3n Negativa"="correlacion_Negativa"
                                                                                ),
                                                                                selected = "correlacion"))) #Fin radioButons
                         ), #fin columna2
                      ), #Fin del DIV
                      ), #Fin Fluid row
            
              actionButton("menuOpcionesCorrelacion", " Opciones "),
            fluidRow( div( id= "bloqueOpcionesCorrelacion",
              
              column(6,
                     
                     tags$div(tags$span(strong(("Opciones")),style = "color: black;")),
                     
                     div(
                       
                      # div( style="display: inline-block;vertical-align:top; width: 10px;", id ="br_Correlacion_Significancia"),
                       div( style="display: inline-block;vertical-align:top; margin-top: -10px;", id ="br_bloque_Correlacion_Opciones", 
                       div(id ="check_Correlacion_Significancia",checkboxInput("checkInput_Correlacion_Significancia", "Calcular Significancia", FALSE)),
                       
                       div( style= "margin-top: -15px;",  id ="check_Correlacion_Estadistica",checkboxInput("checkInput_Correlacion_Estadistica", "Descriptiva", FALSE)),
                       div( style= "margin-top: -15px;",  id ="check_Correlacion_MapaCalor",checkboxInput("checkInput_Correlacion_MapaCalor", "Mapa de Calor", FALSE))
                       ) #div de fin de bloque de opciones
                     )  # FIN DEL DIV GENERAL
                     ),
              column(6,

                     )

            )# fin del div          
            )
          ),
          mainPanel(style =paste0("background-color: transparent; height: 565px; overflow-y: auto; "), width = 6, 
#                    
                   h3("An\u00e1lisis Correlaci\u00f3n"),
                  conditionalPanel("input.checkInput_Correlacion_Pearson == true || input.checkInput_Correlacion_Spearman == true || input.checkInput_Correlacion_Kendall == true", h3("Correlacion Tabla"),
                                   
                                   gt::gt_output("tablaCorrelacion")),
                  conditionalPanel("input.checkInput_Correlacion_MapaCalor == true", h2("Mapa de Calor"),plotOutput("mapaCalor")),
                  conditionalPanel("input.checkInput_Correlacion_Estadistica == true", h2("Estadistica"),plotOutput("estadisticaCorrelacion"))
#                    
#                    



#                    
          )
#          
 )

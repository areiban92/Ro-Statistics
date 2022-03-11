#pagina Ttest


tabPanel(div(  id="textoIconos",
               
  div(  id="imgIconos",           
        tags$img(src = 'www/iconos2/ttest.png' , style= "width: 50px; margin-top: 0px; margin-bottom: 5px;  height: 30px")),
  
  div(" PRUEBAS - T " )
  
  ),value = "Tab 3",
         sidebarPanel(style =paste0(" height: 650px; overflow-y: auto; "), width = 6,
                     
                    
                      fluidRow(
                        column(
                          
                          width = 6,
                          div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                              uiOutput("bucket_1_Ttest"))
                          ), 
                        column(width = 5,
                               div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                   fluidRow(
                                     
                                     tags$div(id = 'placeholderTtestDependiente')
                                   )),
                               div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em", 
                                   fluidRow(width = 4,
                                            tags$div(id = 'placeholderTtestAgrupamiento')
                                           
                                   ))
                        )
                      ),#fin fluid row
                      
                      actionButton("menutestTtest", "Test"),
                      
                      fluidRow( div ( id ="bloqueTestTtest",
                        
                        column(width = 4,
                               
                               tags$div(tags$span(strong(("Test: ")),style = "color: black;")),
                              
                               
                             #  div( style="display: inline-block;vertical-align:top; width: 20px; " ,   id= "br_Ttest_Tipo0"),
                               div(  style="display: inline-block;vertical-align:top;  " , id= "br_Ttest_Tipo1",
                                 div(  id ="check_Ttest_Student",checkboxInput("checkInput_Ttest_Student", "Student", FALSE)),
                                 div(  style ="margin-top: -15px;", id ="check_Ttest_Welch",checkboxInput("checkInput_Ttest_Welch", "Welch", FALSE)),
                                 div(  style ="margin-top: -15px;",  id ="check_Ttest_Mann_Whitney",checkboxInput("checkInput_Ttest_Mann_Whitney", "Mann-Whitney", FALSE))
                               ), #fin del div
                               
                        ), #fin columna1
                        column(width = 4,
                              
                               tags$div(tags$span(strong(("Hip\u00f3tesis: ")),style = "color: black;")),
                              
                               
                              # div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -10px; " ,   id= "br_Ttest_Hipotesis1"),
                               div( style="display: inline-block;vertical-align:top; margin-top: -10px;", id ="br_Ttest_Hipotesis2", 
                                     div(id= "radioTtestHypotesis",radioButtons("radioInputTtestHypotesis", "",
                                            c("Grupo1 != Grupo 2"="hipotesis_Igual",
                                              "Grupo 1 > Grupo 2"="hipotesis_Mayor",
                                              "Grupo 1 < Grupo 2"="hipotesis_Menor"
                                            ),
                                            selected = "hipotesis_Igual"))) #Fin radioButons
                               
                               
                        ), #fin columna2
                        
                        column(width = 4,
                               tags$div(tags$span(strong(("Estadística:  ")),style = "color: black;")),
                             
                               div(
                                 div( id ="check_Ttest_Parametro_Ubicacion",checkboxInput("checkInput_Ttest_Parametro_Ubicacion", "Parametro Ubicaci\u00f3n", FALSE)) ,
                                 
                                 
                                 div( style="display: inline-block;vertical-align:top; width: 10px;", id ="br_Ttest_Intervalo_Confidencia"),
                                 div( style="display: inline-block;vertical-align:top; width: 85px; margin-top: -20px; ", id ="check_Ttest_Intervalo_Confidencia",checkboxInput("checkInput_Ttest_Intervalo_Confidencia", "Int Confid", FALSE)),
                                 div( style="display: inline-block;vertical-align:top; width: 45px; margin-top: -10px; ", id ="input_Ttest_Intervalo_Confidencia",textInput("txtInput_Ttest_Intervalo_Confidencia",label= NULL,value = "95")),
                                 
                                 div( style= "margin-top: -25px;",  id ="check_Ttest_Estadistica",checkboxInput("checkInput_Ttest_Estadistica", "Descriptiva", FALSE))
                                 
                               )
                               
                        )#Fin de column3
                        
                      ) #Fin del  DIV
                      ),#Fin fluidRow
                      
                      actionButton("menuSuposicionesTtest", "Verificaci\u00f3n de Suposiciones"),
                      fluidRow( div( id="bloqueSuposicionesTtest",
                        
                        column(width = 8,
                               tags$div(tags$span(strong(("Verificaci\u00f3n de Suposiciones")),style = "color: black;")),
                            
                               
                               div(
                                 div(  id ="check_Ttest_Suposiciones_Normalidad",checkboxInput("checkInput_Ttest_Suposiciones_Normalidad", "Normalidad", FALSE)),
                                 div(style= "margin-top: -15px;", id ="check_Ttest_Suposiciones_VarianzasIguales",checkboxInput("checkInput_Ttest_Suposiciones_VarianzasIguales", "Igualdad de Varianza", FALSE))
                               ), #fin del div
                               
                        ) #fin columna1
                      )# fin del DIV
                      )#Fin de FluidRow
                      
                      
         ),
         
         mainPanel(style =paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6,
                   
                   h3("An\u00e1lisis T-Test"),
                   
                   conditionalPanel("input.checkInput_Ttest_Student == true || input.checkInput_Ttest_Mann_Whitney == true || input.checkInput_Ttest_Welch == true", h3("Test de Independencia T-Test"), verbatimTextOutput("txt_Ttest_Independencia_Student"),
                                    gt::gt_output("ttesttabla")
                                    ),
                   
                   #check de Validacion de Suposicion
                   conditionalPanel("input.checkInput_Ttest_Suposiciones_Normalidad == true || input.checkInput_Ttest_Suposiciones_VarianzasIguales == true ", h3("Verificaci\u00d3n de Suposiciones"),verbatimTextOutput("txt_Ttest_Independencia_Suposiciones")),
                   
                   conditionalPanel("input.checkInput_Ttest_Suposiciones_Normalidad == true ",gt::gt_output("table_tTest_VerifSupos_Normalidad")),
                           
                   conditionalPanel("input.checkInput_Ttest_Suposiciones_VarianzasIguales == true ",gt::gt_output("table_tTest_VerifSupos_VarianzasIguales")),
                   conditionalPanel("input.checkInput_Ttest_Estadistica == true ", gt::gt_output("estadisticasTtest"))
                   
                   
                   # gt::gt_output("estadisticasTtest"), 
                   #  conditionalPanel("input.checkInput_Ttest_Mann_Whitney == true", h3("Test de Independencia T-Testss"),verbatimTextOutput("txt_Ttest_Independencia_Mann"),gt::gt_output("tStudent")),
                   #  conditionalPanel("input.checkInput_Ttest_Welch == true", h3("Test de Independencia T-Test"), verbatimTextOutput("txt_Ttest_Independencia_Tipo_Welch"),gt::gt_output("tStudent")),
                   
                   #             conditionalPanel("input.checkInput_Ttest_Mann_Whitney == true", h3("Test de Independencia T-Testss"),verbatimTextOutput("txt_Ttest_Independencia_Mann"),gt::gt_output("tMann")),
                   #             conditionalPanel("input.checkInput_Ttest_Welch == true", h3("Test de Independencia T-Test"), verbatimTextOutput("txt_Ttest_Independencia_Tipo_Welch"),gt::gt_output("tWelch")),
                   
         ) # fin main panel
         
)

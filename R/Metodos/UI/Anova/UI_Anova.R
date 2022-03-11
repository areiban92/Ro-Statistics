##UI_Anova

tabPanel(div(  id="textoIconos",
               
               div(  id="imgIconos",           
                     tags$img(src = 'www/iconos2/ttest.png' , style= "width: 50px; margin-top: 0px; margin-bottom: 5px;  height: 30px")),
               
               div("ANOVA TEST" )
               
        ),value = "Tab 7",
sidebarPanel(style =paste0(" height: 650px; overflow-y: auto; "), width = 6,

             fluidRow(
               column(
              
                 width = 6,
                 div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                     uiOutput("bucket_1_Anova"))
               ), 
               column(width = 5,
                      div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                          fluidRow(
                            
                            tags$div(id = 'placeholderAnovaDependiente')
                          )),
                      div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em", 
                          fluidRow(width = 4,
                                   tags$div(id = 'placeholderAnovaAgrupamiento')
                          ))
               )
             ),#fin fluid row
             
             actionButton("menuPostHocAnova", "Pruebas Post Hoc"),
             fluidRow( div( id="bloquePostHocAnova",
               
               column(6,
                      div(id= "tituloAumiendoIguales", tags$div(tags$span(strong("Asumiendo que son Iguales"),style = "color: black;"))),
                      div(style="vertical-align:top;  ", id ="checkTurkeyEnable_Anova",checkboxInput("check_Input_Turkey_Anova", "Turkey", FALSE)),
                      div(style="vertical-align:top;  ", id ="checkBonferroniEnable_Anova",checkboxInput("check_Input_Bonferroni_Anova", "Bonferroni", FALSE)),
                      
                      ),# Fin  de Columna 
               column(6,
                      div(id= "tituloGraficasPersonalizadas", tags$div(tags$span(strong("Asumiendo que son diferentes"),style = "color: black;"))),
                      div(style="display: inline-block;vertical-align:top;  ", id ="checkGamesHowellEnable_Anova",checkboxInput("check_Input_GamesHowell_Anova", "Games Howell", FALSE)),
               
                  )
               
             ) # fin del div
             ), # fin de fluid Row
             actionButton("menuOpcionesAnova", "Opciones"),
             # 
             fluidRow(div( id="bloqueOpcionesAnova",

               column(6,
                 div(style=" vertical-align:top;  ", id ="estadisticosAnovaEnable_Anova",checkboxInput("check_Input_estadisticos_Anova", "Estadisticos", FALSE)),
              
                 div(style=" vertical-align:top;  ", id ="homogenidadAnovaEnable_Anova",checkboxInput("check_Input_homogeniedad_Anova", "Homogenidad de Varianzas", FALSE)),
               
               ),
               column(6,
                      
                  div(style="vertical-align:top;  ", id ="brownAnovaEnable_Anova",checkboxInput("check_Input_brown_Anova", "Brown-Forsythe", FALSE)),
                  div(style="vertical-align:top;  ", id ="welchAnovaEnable_Anova",checkboxInput("check_Input_welch_Anova", "Welch", FALSE)) 
                      )
             )# fin del div
             )# Fin de fluidRow
             
        ),# fIN DE SIDEBAR
mainPanel(style = paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6,
          
          h3("An\u00e1lisis ANOVA un Factor"),
          
          gt::gt_output("tablaAnova"),
          conditionalPanel("input.check_Input_estadisticos_Anova == true ", h3("Descriptivas Anova"), gt::gt_output("tablaAnovaDescriptivas"), h5(" Gr\u00e1fica Descriptivas "), plotOutput("boxplotsAnova")),
          conditionalPanel("input.check_Input_Turkey_Anova == true || input.check_Input_Bonferroni_Anova == true ", h3("Post Hoc"), gt::gt_output("tablaAnovaPostHocIguales")),
          conditionalPanel("input.check_Input_homogeniedad_Anova == true ", h3("Homogeniedad de Varianza"),gt::gt_output("tablaAnovaHomogeniedad")),
          conditionalPanel("input.check_Input_GamesHowell_Anova == true  ", h3("Post Hoc"), gt::gt_output("tablaAnovaPostHocDesiguales")),
          conditionalPanel("input.check_Input_welch_Anova == true  ", h3("Anova Welch"), gt::gt_output("tablaAnovaWelch")),
          conditionalPanel("input.check_Input_brown_Anova == true  ", h3("Anova Brown-Forsythe"), gt::gt_output("tablaAnovaBrown"))
          

          
          )
)


tabPanel("Acerca de Ro-Statistics ", value = "Acerca de Ro-Statistics",
         
         
         shinyWidgets::setBackgroundImage( src = "www/test3.jpg",shinydashboard = FALSE),

         fluidRow(
           div(tags$img(src = "www/padlabSinFondo.png",height = 100, width = 600) , style="text-align: center; padding-top: 100px;")
         ),
         
         fluidRow(
           
           column(4, 
                  #  div(img(src = "descriptivo.png",height = 140, width = 140) , style="text-align: center; padding-top: 100px;") 
           ),
           column(8, 
                  br(),
                  # div("Copyright 2021-2021 ", style = "color:black; font-size: 20px; padding-top: 40px;  "),
                  
                  
                  column(2,
                         
                         div( style="display: inline-block;vertical-align:top; font-size: 16px; color: red; ","Versi\u00f3n: " ),
                         br(),
                         div( style="display: inline-block;vertical-align:top; font-size: 16px; color: red; ","Construido: "),
                         br(),
                         div( style="display: inline-block;vertical-align:top; font-size: 16px; color: red; ","Recurso: " ),
                         br(),
                         div( style="display: inline-block;vertical-align:top; font-size: 16px; color: red; ","Cita: " ),
                  ),
                  column(4,
                         
                         div( style="display: inline-block;vertical-align:top; font-size: 16px;", "00.0.900  **Beta 2"),
                         br(),  
                         
                         div( style="display: inline-block;vertical-align:top; font-size: 16px;","20 de Febrero 2022  " ),
                         br(),
                         
                         div( style="display: inline-block;vertical-align:top; font-size: 16px; ",a("https://rostatictictest.shinyapps.io/RoStatistics/ ", href="https://rostatictictest.shinyapps.io/Ucacue/ ") ),
                         br(),
                         div( style="display: inline-block;vertical-align:top; font-size: 16px; "," Ro-Statistics 00.0.900 (2021). Ro-Statistics (version 00.0.900) [Computer software].  "),
                  ), #Finde columa2
                  
                  
           ) #Finde columna de texto
         ), #Fin de fluidrow
         div(   style="display: inline-block;vertical-align:top; padding-top: 300px;", p("Este programa se proporciona TAL CUAL, 
                                   SIN GARANTÍA DE NINGÚN TIPO, INCLUYENDO LA GARANTÍA DE DISEÑO, 
                                   COMERCIABILIDAD Y APTITUD PARA UN PROP\u00d3SITO PARTICULAR"))
)
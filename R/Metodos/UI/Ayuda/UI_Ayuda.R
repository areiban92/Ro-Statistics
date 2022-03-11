tabPanel(div(  id="textoIconos",
  
          div( id="imgIconos",
                 img(src='iconos2/info.png', style= "width: 50px; margin-top: 0px; margin-bottom: 5px;  height: 30px")),
          div( 
            
            "INFORMACI\u00d3N" ) ,
  ),value = "Tab 6",
         sidebarPanel( width = 6, 
                      
                       radioButtons(inputId = "ayudaMenu", 
                                    label = "Tabs", 
                                    choices = c("Descriptivos","T-Test","Contingencia","ANOVA","Regresi\u00f3n")
                                    )
                   
             
         ),
         mainPanel(width = 6, 
                
                   h1("AYUDA"),
         conditionalPanel("input.ayudaMenu == 'Descriptivos'", uiOutput('descriptivos_ayuda')),
         conditionalPanel("input.ayudaMenu == 'T-Test'", uiOutput('ttest_ayuda')),
         conditionalPanel("input.ayudaMenu == 'Correlaci\u00f3n'", uiOutput('correlacion_ayuda')),
         conditionalPanel("input.ayudaMenu == 'Contingencia'", uiOutput('contingencia_ayuda')),
)
)

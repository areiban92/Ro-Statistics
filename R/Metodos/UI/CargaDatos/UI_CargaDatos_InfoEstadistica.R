tabPanel("Informaci\u00f3n Estadistica", value = "Info Estadistica",
         sidebarPanel( width = 4, 
                       
                       radioButtons(inputId = "ayudaMenu", 
                                    label = "Tabs", 
                                    choices = c("Descriptivos","T-Test","Contingencia","ANOVA","Regresi\u00f3n")
                       )
                       
                       
         ),
         mainPanel(width = 8, 
                   
                   h1("AYUDA"),
                   conditionalPanel("input.ayudaMenu == 'Descriptivos'", uiOutput('descriptivos_ayuda')),
                   conditionalPanel("input.ayudaMenu == 'T-Test'", uiOutput('ttest_ayuda')),
                   conditionalPanel("input.ayudaMenu == 'Correlaci\u00f3n'", uiOutput('correlacion_ayuda')),
                   conditionalPanel("input.ayudaMenu == 'Contingencia'", uiOutput('contingencia_ayuda')),
         )
)
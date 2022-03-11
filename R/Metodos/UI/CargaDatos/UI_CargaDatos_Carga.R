tabPanel("Carga de Datos   ",value = "Tab 1",
         sidebarPanel( width = 3,
                       
                fluidRow(
                   tags$head(
                     tags$style(HTML("#checkboxDescripcionVariable  {max-height: 200px}"))
                     #                        #tags$style(HTML(".bucket-list-container {min-height: 50px;height: 200px;},rank-list-item"))
                   ),
                   fileInput('file', 'Eliga la Base de Datos ',
                             accept= c('application/vnd.ms-excel',
                                       'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                       '.xls',
                                       '.xlsx','.csv','.sav'))
                   ), # fin fluid row1
        
                fluidRow(
                  
                     uiOutput("checkboxDescripcionVariable")
                   ), # fin fluid row1
                fluidRow(
                  actionButton("guardar", "Guardar")
                ),#fin del row 2
                 fluidRow(
                   #uiOutput("unique")
                     verbatimTextOutput("unique")
                   
                  )# fin fluid row3
                
                       
                        
         ),
        mainPanel(width = 9,
                   
                       
                                  
                       
        tabsetPanel(type = "tabs",   
            tabPanel("Tabla",  
                     DT::dataTableOutput('tablaSeleccion'),
                    
                     div(style = 'width:100%;margin:auto',DT::dataTableOutput('tablaPrincipal')),
                     tags$head(
                             tags$script('
                        Shiny.addCustomMessageHandler("unbinding_table_elements", function(x) {
                        Shiny.unbindAll($(document.getElementById(x)).find(".dataTable"));
                        });'
                             ))
                    
                     )
           
       
        )
        )
        
        
        
        
        
        
     #    ), # fin sidebarPanel
       #  mainPanel(width = 6,
                 #  fluidRow(
                  #   column(width = 8,h3("Tabla de Datos")),
                     # column(width = 4,h3("Tipo de Variable"))), # fin fluid ROw
                  #   fluidRow(
                  #     column(width = 8,DT::dataTableOutput('tablaPrincipal')
                              #)
                       
                       
                    # )),# fin fluid row
                   
                  # h1("Tabla de Datos Mutada"),
                   #DT::dataTableOutput('tablaPrincipalMutada'),
                   #rHandsontableOutput("rtablaTest"),
                   
                 #  verbatimTextOutput('rtablaTest1')
                   
         #) # fin main panel
)
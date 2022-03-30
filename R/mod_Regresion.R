#' Regresion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Regresion_ui <- function(id){
  ns <- NS(id)

  navbarMenu( div(  id= ns("textoIconos"),
                    
                    div(  id=ns("imgIconos"),           
                          tags$img(src ='www/iconos2/ttest.png', style= "width: 50px; margin-top: 0px; margin-bottom: 5px;  height: 30px")),
                    
                    div("REGRESI\u00d3N" )
                    
  ),
  # ################## UI CORRELACION ################

  tabPanel("Correlaci\u00f3n ",value = "RegresionCorrelacion",
           sidebarPanel( width = 6,
                         #
                         fluidRow(
                           column( width = 6,

                                   div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                       uiOutput(ns("bucket_1_Correlacion")))
                           ),
                           column(width = 5,
                                  div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                      fluidRow(
                                        #tags$b("VARIABLES2"),
                                        #uiOutput("bucket_2_Correlacion")
                                        tags$div(id = ns('placeholderCorrelacionDependiente'))
                                      )),
                                  div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em",
                                      fluidRow(width = 4,


                                      ))
                           )
                         ),#fin fluid row
                         actionButton(ns("menuTestCorrelacion"), " Test "),
                         fluidRow( div (id = ns("bloqueTestCorrelacion"),

                                        column(6,

                                               tags$div(tags$span(strong(("Coeficiente de Correlaci\u00d3n")),style = "color: black;")),


                                               #   div( style="display: inline-block;vertical-align:top; width: 20px; " ,   id= "br_Correlacion_Tipo0"),

                                               div(  style="display: inline-block;vertical-align:top;" ,  id=ns("bloqueEstadisticaCorrelacion"),

                                                     div( id =ns("check_Correlacion_Pearson"),checkboxInput(ns("checkInput_Correlacion_Pearson"), "Pearson's r", FALSE)),
                                                     div(  style ="margin-top: -15px;", id =ns("check_Correlacion_Spearman"),checkboxInput(ns("checkInput_Correlacion_Spearman"), "Spearman’s rho", FALSE)),
                                                     div(  style ="margin-top: -15px;",  id =ns("check_Correlacion_Kendall"),checkboxInput(ns("checkInput_Correlacion_Kendall"), "Kendall’s tau", FALSE))


                                               ) # Fin del div

                                        ), # Fin columna 1
                                        column(6,

                                               tags$div(tags$span(strong(("Hip\u00f3tesis")),style = "color: black;")),


                                               div( style="display: inline-block;vertical-align:top; margin-top: -10px;", id = ns("br_Correlacion_Hipotesis2"),
                                                    div(id= ns("radioCorrelaciontHypotesis"),radioButtons(ns("radioInputCorrelacionHypotesis"), "",
                                                                                                      c("Correlaci\u00f3n"="correlacion",
                                                                                                        "Correlaci\u00f3n Positiva"="correlacion_Positiva",
                                                                                                        "Correlaci\u00f3n Negativa"="correlacion_Negativa"
                                                                                                      ),
                                                                                                      selected = "correlacion"))) #Fin radioButons
                                        ), #fin columna2
                         ), #Fin del DIV
                         ), #Fin Fluid row

                         actionButton(ns("menuOpcionesCorrelacion"), " Opciones "),
                         fluidRow( div( id= ns("bloqueOpcionesCorrelacion"),

                                        column(6,

                                               tags$div(tags$span(strong(("Opciones")),style = "color: black;")),

                                               div(

                                                 # div( style="display: inline-block;vertical-align:top; width: 10px;", id ="br_Correlacion_Significancia"),
                                                 div( style="display: inline-block;vertical-align:top; margin-top: -10px;", id =ns("br_bloque_Correlacion_Opciones"),
                                                      div(id = ns("check_Correlacion_Significancia"),checkboxInput(ns("checkInput_Correlacion_Significancia"), "Calcular Significancia", FALSE)),

                                                      div( style= "margin-top: -15px;",  id = ns("check_Correlacion_Estadistica"),checkboxInput(ns("checkInput_Correlacion_Estadistica"), "Descriptiva", FALSE)),
                                                      div( style= "margin-top: -15px;",  id = ns("check_Correlacion_MapaCalor"),checkboxInput(ns("checkInput_Correlacion_MapaCalor"), "Mapa de Calor", FALSE))
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
                     conditionalPanel("input.checkInput_Correlacion_Pearson == true || input.checkInput_Correlacion_Spearman == true || input.checkInput_Correlacion_Kendall == true",ns=ns, h3("Correlacion Tabla"),

                                      gt::gt_output(ns("tablaCorrelacion"))),
                     conditionalPanel("input.checkInput_Correlacion_MapaCalor == true", ns=ns, h2("Mapa de Calor"),plotOutput(ns("mapaCalor"))),
                     conditionalPanel("input.checkInput_Correlacion_Estadistica == true", ns=ns, h2("Estadistica"),plotOutput(ns("estadisticaCorrelacion")))

           )

  ),
  # # ###################################### REGRESION LINEAL ################

  tabPanel("Regresi\u00f3n Lineal",value = "RegresionLineal",
           sidebarPanel(style =paste0(" height: 650px; overflow-y: auto; "), width = 6,


                        fluidRow(
                          column(
                            #tags$b("VARIABLES"),
                            width = 6,
                            div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                uiOutput( ns("bucket_1_RegresionLineal")))
                          ),
                          column(width = 5,
                                 div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                     fluidRow(

                                       tags$div(id = ns('placeholderRegresionLinealDependiente'))
                                     )),
                                 div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em",
                                     fluidRow(width = 4,
                                              tags$div(id = ns('placeholderRegresionLinealAgrupamiento'))

                                     ))
                          )
                        )#fin fluid row
           ),

           mainPanel(style =paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6,

                     h3("An\u00e1lisis Regresion Lineal"),
                     gt::gt_output(ns("tablaRegresionLinealResumenR")),
                     br(),
                     gt::gt_output(ns("tablaRegresionLineal"))


           ) # Fin de main panel
  ),


  ########################### REGRESION BINOMIAL ######################

  tabPanel("Regresi\u00f3n Binomial",value = "RegresionBinomial",


           sidebarPanel(style =paste0(" height: 650px; overflow-y: auto; "), width = 6,


                        fluidRow(
                          column(

                            width = 6,
                            div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                uiOutput(ns("bucket_1_RegresionBinomial")))
                          ),
                          column(width = 5,
                                 div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                     fluidRow(

                                       tags$div(id = ns('placeholderRegresionBinomialDependiente'))
                                     )),
                                 div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em",
                                     fluidRow(width = 4,
                                              tags$div(id = ns('placeholderRegresionBinomialAgrupamiento'))

                                     ))
                          )
                        ),#fin fluid row

                        fluidRow(
                          column(6,

                                 div(style=" vertical-align:top;  ", id = ns("efectosEnable_RegresionBinomial"),checkboxInput(ns("check_Input_efectos_RegresionBinomial"), "Efectos fijos y aleatorios", FALSE)),
                          )

                        )

           ),

           mainPanel(style =paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6,

                     h3("An\u00e1lisis Regresion Binomial"),
                     gt::gt_output(ns("tablaRegresionBinomialResumenR")),
                     br(),
                     gt::gt_output(ns("tablaRegresionBinomial"))


           ) # Fin de main panel
  ),

  # # ################################----REGRESION MULTINOMIAL ###########


  tabPanel("Regresi\u00f3n Multinomial",value = "RegresionMultinomial",


           sidebarPanel(style =paste0(" height: 650px; overflow-y: auto; "), width = 6,


                        fluidRow(
                          column(
                            #tags$b("VARIABLES"),
                            width = 6,
                            div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                uiOutput(ns("bucket_1_RegresionMultinomial")))
                          ),
                          column(width = 5,
                                 div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                     fluidRow(

                                       tags$div(id = ns('placeholderRegresionMultinomialDependiente'))
                                     )),
                                 div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em",
                                     fluidRow(width = 4,
                                              tags$div(id = ns('placeholderRegresionMultinomialAgrupamiento'))

                                     ))
                          )
                        ),#fin fluid row
                        fluidRow(


                          actionButton(ns("nivelReferenciaRegresionMultinomial"), "Niveles de Referencia"),
                        ),
                        fluidRow(
                          column(6,
                                 h5("Seleccione la Variable :")

                          ),
                          column(6,

                                 uiOutput(ns("nivelRefenciaRegresionMultinomial"))

                          )

                        )
           ),

           mainPanel(style =paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6,

                     h3("An\u00e1lisis Regresion Multinomial"),
                     gt::gt_output(ns("tablaRegresionMultinomialResumenR")),
                     br(),
                     gt::gt_output(ns("tablaRegresionMultinomial"))

           ) # Fin de main panel
  ) # FIn de TABPANEL

  )


  
  
  }
    
#' Regresion Server Functions
#'
#' @noRd 
mod_Regresion_server <- function(id,namesClasified, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    banderaCorrelacion <- FALSE
    banderaRegresionBinomial <- FALSE
    banderaRegresionLineal <- FALSE
    banderaRegresionMultinomial <- FALSE
    
    descargaTablasCorrelacion <- reactiveValues(  tabla_Correlacion = NULL )
    
    descargaTablasRegresion <- reactiveValues(  tabla_TregresionBinomial = NULL,tabla_TregresionBinomialResumenR = NULL,
                                                tabla_TregresionLineal = NULL,tabla_TregresionLinealResumenR = NULL,
                                                tabla_TregresionMultinomial = NULL,tabla_TregresionMultinomialResumenR = NULL )
    

  #####################  INPUTS  #####################  
    radioCorrelacionHypoValue <- reactive({input$radioInputCorrelacionHypotesis})
    
    checkCorrelacionTipoPearson <- reactive({input$checkInput_Correlacion_Pearson})
    checkCorrelacionTipoSpearman <- reactive({input$checkInput_Correlacion_Spearman})
    checkCorrelacionTipoKendall <- reactive({input$checkInput_Correlacion_Kendall})
    
    checkCorrelacion_Significancia<- reactive({ input$checkInput_Correlacion_Significancia})
    
    checkCorrelacion_Estadisticas <- reactive ({input$checkInput_Correlacion_Estadistica })
    checkCorrelacion_MapaCalor <- reactive ({input$checkInput_Correlacion_MapaCalor })
    
    
 ###################### SELECCION DE CORRELACON #####################
    
    
    df_TCorrelacion_Seleccion_Agrupamiento <- reactive({
      
      req(input$rank_list_TCorrelacion_Agrupamiento)
      df_TCorrelacion_Seleccion_Agrupamiento<- r$valuesmydata %>% dplyr::select(input$rank_list_TCorrelacion_Agrupamiento)
      
    })
    
    df_TCorrelacion_Seleccion_Dependiente <- reactive({
      
      req(input$rank_list_TCorrelacion_Dependiente)
      df_TCorrelacion_Seleccion_Dependiente<- r$valuesmydata %>% dplyr::select(input$rank_list_TCorrelacion_Dependiente)
      
    })
    
    output$bucket_1_Correlacion <- renderUI({
      
      sortable::bucket_list(
        header =  HTML(as.character(  tags$div(tags$span(strong("Entrada"),style = "color: black;")) )), 
        group_name = "bucket_list_TCorrelacion",
        orientation = "horizontal",
        class = c("default-sortable", "custom-sortable"), # add custom style
        
        
        sortable::add_rank_list(
          text = "",
          labels = namesClasified(),
          input_id = ns("rank_list_TCorrelacion_Entrada"),
          css_id = "rank_TCorrelacion_Entrada"
          
        ))
    })
    
    if (banderaCorrelacion==FALSE){
      
      banderaCorrelacion=TRUE
    
      
      insertUI(
        selector=paste0("#",ns("placeholderCorrelacionDependiente")),
        where="beforeBegin",
        ui = tags$div(id = ns("correlacionBoxDependiente"),
                      
                      sortable::bucket_list(
                        header = "Condicion",
                        group_name = "bucket_list_TCorrelacion",
                        orientation = "horizontal",
                        
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_TCorrelacion_Dependiente"),
                          css_id = "rank_TCorrelacion_Dependiente"
                        ))
                      
                      
                      
                      
        ))
      
    }
    
    
    observeEvent( r$inputGuardar,{
      
      golem::print_dev("REGRESIONSSSSSSSSSS")
      
      
      removeUI(
        selector =  paste0("#",ns("correlacionBoxDependiente"))
      )
      
      
      
     insertUI(selector=paste0("#",ns("placeholderCorrelacionDependiente")),
     where='beforeBegin',
     ui =  tags$div(id =ns("correlacionBoxDependiente"),
        sortable::bucket_list(
        header =  HTML(as.character( tags$div(tags$span(strong('Variables : '),style = paste0('color: black;')),
        tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),
        group_name = "bucket_list_TCorrelacion",
        orientation = 'horizontal',
        class = c('default-sortable', "custom-sortableDependiente"),
        sortable::add_rank_list(text = '',labels = NULL,
        input_id =ns("rank_list_TCorrelacion_Dependiente"),
        css_id ="rank_TCorrelacion_Dependiente"))))
      
      
      
      
    
     
    })
    
    
    
 ###################### SELECCION DE REGRESION LINEAL ###############
    df_RegresionLineal_Seleccion_Agrupamiento <- reactive({
      
      req(input$rank_list_RegresionLineal_Agrupamiento)
      df_RegresionLineal_Seleccion_Agrupamiento<- r$valuesmydata %>% dplyr::select(input$rank_list_RegresionLineal_Agrupamiento)
      
    })
    
    df_RegresionLineal_Seleccion_Dependiente <- reactive({
      
      req(input$rank_list_RegresionLineal_Dependiente)
      df_RegresionLineal_Seleccion_Dependiente<- r$valuesmydata %>% dplyr::select(input$rank_list_RegresionLineal_Dependiente)
      
    })
    
    output$bucket_1_RegresionLineal <- renderUI({
      
      sortable::bucket_list(
        header =  HTML(as.character(  tags$div(tags$span(strong("Entrada"),style = "color: black;")) )),    
        group_name = "bucket_list_RegresionLineal",
        orientation = "horizontal",
        class = c("default-sortable", "custom-sortable"), # add custom style
        
        sortable::add_rank_list(
          text = "",
          labels = namesClasified(),
          input_id = ns("rank_list_RegresionLineal_Entrada"),
          css_id = "rank_RegresionLineal_Entrada"
          
        ))
    })
    
    if (banderaRegresionLineal == FALSE) {
      banderaRegresionLineal = TRUE
      
      insertUI(
        selector=paste0("#",ns("placeholderRegresionLinealDependiente")),
        where="beforeBegin",
        ui = tags$div(id = ns("regresionLinealBoxDependiente"),
                      sortable::bucket_list(
                        header = HTML(as.character(   tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                               tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                        group_name = "bucket_list_RegresionLineal",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_RegresionLineal_Dependiente"),
                          css_id = "rank_RegresionLineal_Dependiente"
                        ))
        ))
      
      insertUI(
        selector=paste0("#",ns("placeholderRegresionLinealAgrupamiento")),
        where="beforeBegin",
        ui = tags$div(id = ns("regresionLinealBoxAgrupamiento"),
                      sortable::bucket_list(
                        header =  HTML(as.character(  tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                               tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                        
                        group_name = "bucket_list_RegresionLineal",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_RegresionLineal_Agrupamiento"),
                          css_id = "rank_RegresionLineal_Agrupamiento"
                        ))
        ))
      
    }
    
    
    
    observeEvent( r$inputGuardar,{
      
      golem::print_dev("REGRESIONSSSSSSSSSS LINEAL")
      
      
      removeUI(
        selector =  paste0("#",ns("regresionLinealBoxDependiente"))
      )
      
      removeUI(
        selector =  paste0("#",ns("regresionLinealBoxAgrupamiento"))
      )
      
      
      insertUI(selector=paste0("#",ns("placeholderRegresionLinealDependiente")),
               where='beforeBegin',
               ui =  tags$div(id =ns("regresionLinealBoxDependiente"),
                              sortable::bucket_list(
                                header =  HTML(as.character( tags$div(tags$span(strong('Variables : '),style = paste0('color: black;')),
                                                                      tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),
                                group_name = "bucket_list_RegresionLineal",
                                orientation = 'horizontal',
                                class = c('default-sortable', "custom-sortableDependiente"),
                                sortable::add_rank_list(text = '',labels = NULL,
                                input_id =ns("rank_list_RegresionLineal_Dependiente"),
                                css_id ="rank_RegresionLineal_Dependiente"))))
      
      
      insertUI(selector=paste0("#",ns("placeholderRegresionLinealAgrupamiento")),
      where='beforeBegin',
      ui =  tags$div(id =ns("regresionLinealBoxAgrupamiento"),
      sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Covariante Numerico : '),style = paste0('color: black;')),
              tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),
              group_name = "bucket_list_RegresionLineal",
              orientation = 'horizontal',
              class = c('default-sortable', "custom-sortableAgrupamiento"),
              
              sortable::add_rank_list(text = '',labels = NULL,
              input_id =ns("rank_list_RegresionLineal_Agrupamiento"),
              css_id ="rank_RegresionLineal_Agrupamiento"))))
      
      
      
      
    })
    
    
    
    
    
 ########################### SELECCION DE REGRESION BINOMIAL ############
    

    df_RegresionBinomial_Seleccion_Agrupamiento <- reactive({
      
      req(input$rank_list_RegresionBinomial_Agrupamiento)
      df_RegresionBinomial_Seleccion_Agrupamiento <- r$valuesmydata %>% dplyr::select(input$rank_list_RegresionBinomial_Agrupamiento)
      
    })
    
    df_RegresionBinomial_Seleccion_Dependiente <- reactive({
      
      req(input$rank_list_RegresionBinomial_Dependiente)
      df_RegresionBinomial_Seleccion_Dependiente<- r$valuesmydata %>% dplyr::select(input$rank_list_RegresionBinomial_Dependiente)
      
    })
    
    output$bucket_1_RegresionBinomial <- renderUI({
      
      sortable::bucket_list(
        header = HTML(as.character(   tags$div(tags$span(strong("Entrada"),style = "color: black;")) )),    
        group_name = "bucket_list_RegresionBinomial",
        orientation = "horizontal",
        class = c("default-sortable", "custom-sortable"), # add custom style
        
        sortable::add_rank_list(
          text = "",
          labels = namesClasified(),
          input_id = ns("rank_list_RegresionBinomial_Entrada"),
          css_id = "rank_RegresionBinomial_Entrada"
          
        ))
    })
    
    if (banderaRegresionBinomial == FALSE) {
      banderaRegresionBinomial = TRUE
      
      insertUI(
        selector=paste0("#",ns("placeholderRegresionBinomialDependiente")),
        where="beforeBegin",
        ui = tags$div(id = ns("regresionBinomialBoxDependiente"),
                      sortable::bucket_list(
                        header = HTML(as.character(  tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                              tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                        group_name = "bucket_list_RegresionBinomial",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_RegresionBinomial_Dependiente"),
                          css_id = "rank_RegresionBinomial_Dependiente"
                        ))
        ))
      
      insertUI(
        selector= paste0("#",ns("placeholderRegresionBinomialAgrupamiento")),
        where="beforeBegin",
        ui = tags$div(id = ns("regresionBinomialBoxAgrupamiento"),
                      sortable::bucket_list(
                        header = HTML(as.character(  tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                              tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                        
                        group_name = "bucket_list_RegresionBinomial",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_RegresionBinomial_Agrupamiento"),
                          css_id = "rank_RegresionBinomial_Agrupamiento"
                        ))
        ))
      
    }
   
    
    observeEvent( r$inputGuardar,{
      
      golem::print_dev("REGRESIONSSSSSSSSSS Binomial")
      
      
      removeUI(
        selector =  paste0("#",ns("regresionBinomialBoxDependiente"))
      )
      
      removeUI(
        selector =  paste0("#",ns("regresionBinomialBoxAgrupamiento"))
      )
      
      
      insertUI(selector=paste0("#",ns("placeholderRegresionBinomialDependiente")),
      where='beforeBegin',
      ui =  tags$div(id = ns("regresionBinomialBoxDependiente"),
      sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Variable Dependiente  : '),style = paste0('color: black;')),
          tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),
          group_name = "bucket_list_RegresionBinomial",
          orientation = 'horizontal',
          class = c('default-sortable', "custom-sortableDependiente"),
          sortable::add_rank_list(text = '',labels = NULL,
          input_id = ns("rank_list_RegresionBinomial_Dependiente"),
          css_id ="rank_RegresionBinomial_Dependiente" ))))
      
      
      insertUI(selector=paste0("#",ns("placeholderRegresionBinomialAgrupamiento")),
      where='beforeBegin',
      ui =  tags$div(id = ns("regresionBinomialBoxAgrupamiento"),
          sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Covariante Numerico : '),style = paste0('color: black;')),
          tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),
          group_name = "bucket_list_RegresionBinomial",
          orientation = 'horizontal',
          class = c('default-sortable', "custom-sortableAgrupamiento"),
          sortable::add_rank_list(text = '',labels = NULL,
          input_id =ns("rank_list_RegresionBinomial_Agrupamiento") ,
          css_id ="rank_RegresionBinomial_Agrupamiento" ))))
      
      
      
      
    })
    
    
    
  #################### SELECION DE MULTINOMIAL  ##################
    
    
    df_RegresionMultinomial_Seleccion_Agrupamiento <- reactive({
      
      req(input$rank_list_RegresionMultinomial_Agrupamiento)
      df_RegresionMultinomial_Seleccion_Agrupamiento<- r$valuesmydata %>% dplyr::select(input$rank_list_RegresionMultinomial_Agrupamiento)
      
    })
    
    df_RegresionMultinomial_Seleccion_Dependiente <- reactive({
      
      req(input$rank_list_RegresionMultinomial_Dependiente)
      df_RegresionMultinomial_Seleccion_Dependiente<- r$valuesmydata %>% dplyr::select(input$rank_list_RegresionMultinomial_Dependiente)
      
    })
    
    output$bucket_1_RegresionMultinomial <- renderUI({
      
      sortable::bucket_list(
        header = HTML(as.character(   tags$div(tags$span(strong("Entrada"),style = "color: black;")) )),    
        group_name = "bucket_list_RegresionMultinomial",
        orientation = "horizontal",
        class = c("default-sortable", "custom-sortable"), # add custom style
        
        sortable::add_rank_list(
          text = "",
          labels = namesClasified(),
          input_id = ns("rank_list_RegresionMultinomial_Entrada"),
          css_id = "rank_RegresionMultinomial_Entrada"
          
        ))
    })
    
    if (banderaRegresionMultinomial == FALSE) {
      banderaRegresionMultinomial = TRUE
      
      insertUI(
        selector=paste0("#",ns("placeholderRegresionMultinomialDependiente")),
        where="beforeBegin",
        ui = tags$div(id = ns("regresionMultinomialBoxDependiente"),
                      sortable::bucket_list(
                        header = HTML(as.character(  tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                              tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                        group_name = "bucket_list_RegresionMultinomial",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_RegresionMultinomial_Dependiente"),
                          css_id = "rank_RegresionMultinomial_Dependiente"
                        ))
        ))
      
      insertUI(
        selector=paste0("#",ns("placeholderRegresionMultinomialAgrupamiento")),
        where="beforeBegin",
        ui = tags$div(id = ns("regresionMultinomialBoxAgrupamiento"),
                      sortable::bucket_list(
                        header =  HTML(as.character(  tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                               tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                        
                        group_name = "bucket_list_RegresionMultinomial",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_RegresionMultinomial_Agrupamiento"),
                          css_id = "rank_RegresionMultinomial_Agrupamiento"
                        ))
        ))
      
    }
    
    
    observeEvent( r$inputGuardar,{
      
      golem::print_dev("REGRESIONSSSSSSSSSS Multinomial")
      
      
      removeUI(
        selector =  paste0("#",ns("regresionMultinomialBoxDependiente"))
      )
      
      removeUI(
        selector =  paste0("#",ns("regresionMultinomialBoxAgrupamiento"))
      )
      
      
      insertUI(selector=paste0("#",ns("placeholderRegresionMultinomialDependiente")),
               where='beforeBegin',
               ui =  tags$div(id = ns("regresionMultinomialBoxDependiente"),
                              sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Variable Dependiente  : '),style = paste0('color: black;')),
                                                                                          tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),
                                                    group_name = "bucket_list_RegresionMultinomial",
                                                    orientation = 'horizontal',
                                                    class = c('default-sortable', "custom-sortableDependiente"),
                                                    sortable::add_rank_list(text = '',labels = NULL,
                                                    input_id = ns("rank_list_RegresionMultinomial_Dependiente"),
                                                    css_id ="rank_RegresionMultinomial_Dependiente" ))))
      
      
      insertUI(selector=paste0("#",ns("placeholderRegresionMultinomialAgrupamiento")),
               where='beforeBegin',
               ui =  tags$div(id = ns("regresionMultinomialBoxAgrupamiento"),
                              sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Covariante Numerico : '),style = paste0('color: black;')),
                                                                                          tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),
                                                    group_name = "bucket_list_RegresionMultinomial",
                                                    orientation = 'horizontal',
                                                    class = c('default-sortable', "custom-sortableAgrupamiento"),
                                                    sortable::add_rank_list(text = '',labels = NULL,
                                                    input_id =ns("rank_list_RegresionMultinomial_Agrupamiento") ,
                                                    css_id ="rank_RegresionMultinomial_Agrupamiento" ))))
      
      
      
      
    })
    
    
  ####################### CORRELACION #############
    
    descargaUInameCorrelacion <- c("descargaCorrelacion")
    
    uiCorrelacionRadio  <- c("uiRadioCorrelacion")
    
    nombregtWidgetCorrelacion  <- c("tablaCorrelacion")
    
    table_calculoTCorrelacion <- reactive({
      
      calculoTCorrelacion(
        session,r$valuesmydata,df_TCorrelacion_Seleccion_Dependiente(),df_TCorrelacion_Seleccion_Agrupamiento(),
        checkCorrelacionTipoPearson(),checkCorrelacionTipoSpearman(),checkCorrelacionTipoKendall(),
        radioCorrelacionHypoValue(),checkCorrelacion_Significancia(),checkCorrelacion_Estadisticas()
      )
      
      
      
    })
    
    output$mapaCalor <- renderPlot({
      
      
      
      if(checkCorrelacion_MapaCalor()==TRUE) {
        if (length(df_TCorrelacion_Seleccion_Dependiente()) >= 2){
          
          print_dev("si ok plot de calor")
          
          col<- colorRampPalette(c("blue", "white", "red"))(20)
          heatmap(x = calculoMapadeCalor_Correlacion(session,r$valuesmydata,df_TCorrelacion_Seleccion_Dependiente(),
                                                     df_TCorrelacion_Seleccion_Agrupamiento(),checkCorrelacion_MapaCalor()
          ), col = col, symm = TRUE)
          
        } else {
          
          shinyalert::shinyalert("Oops!", "Se necesita al menos dos variables para gr\u00e1fica de calor", type = "error",size = "xs")
          
        }
      }
      
    }, bg="transparent")
    
    output$estadisticaCorrelacion <- renderPlot({
      
      if(checkCorrelacion_Estadisticas()==TRUE){
        
        PerformanceAnalytics::chart.Correlation(calculoEstadistica_Correlacion(session,r$valuesmydata,df_TCorrelacion_Seleccion_Dependiente(),df_TCorrelacion_Seleccion_Agrupamiento(),
                                                                               checkCorrelacion_Estadisticas()),  method = c("pearson", "kendall", "spearman"),histogram=TRUE, pch=19)
      }
    }, bg="transparent")
    
    output$tablaCorrelacion <- gt::render_gt({
      
      descargaTablasCorrelacion$tabla_Correlacion <<- apaCorrelacion(table_calculoTCorrelacion()[[1]],
                                                                     "Correlacion",
                                                                     table_calculoTCorrelacion()[[2]],"correlacion_tabla")
    })
    

    lapply(1:2, function(i) {
      
      shinyjs::onevent("dblclick",nombregtWidgetCorrelacion[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                                                                                       html = TRUE, text = tagList( radioButtons(ns(uiCorrelacionRadio[i]), "", 
                                                                                                                                 choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                                                                                                                    downloadButton(ns(descargaUInameCorrelacion[i]),"Descargar" ))))
    })
    
    output$descargaCorrelacion <- downloadHandler (
      
      filename = function() {
        paste0("correlacion", input$uiRadioCorrelacion)
      },
      
      content = function(file) {
        if(input$uiRadioCorrelacion == ".rtf") {
          descargaTablasCorrelacion$tabla_Correlacion %>% gt::gtsave("correlacion.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/correlacion.rtf"), file )
        } else if (input$uiRadioCorrelacion == ".tex") {
          descargaTablasCorrelacion$tabla_Correlacion %>% gt::gtsave("correlacion.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/correlacion.tex"), file )
          
        }}
    )
    
    
    observe({
      
      if(input$menuTestCorrelacion %% 2 == 0){
        
        shinyjs::hide(id = "bloqueTestCorrelacion")
      }
      else{
        
        shinyjs::show(id = "bloqueTestCorrelacion")
      }
      
    })
    
    observe({
      
      if(input$menuOpcionesCorrelacion %% 2 == 0){
        
        shinyjs::hide(id = "bloqueOpcionesCorrelacion")
      }
      else{
        
        shinyjs::show(id = "bloqueOpcionesCorrelacion")
      }
      
    })
    
    

    
    
    
    
  ################### REGRESION LINEAL ##################
    
    descargaUInameRegresionLineal <- c("descargaRegresionLineal","descargaRegresionLinealResumen")
    uiRegresionLinealRadio  <- c("uiRadioRegresionLineal","uiRadioRegresionLinealResumen")
    nombregtWidgetRegresionLineal  <- c("tablaRegresionLineal","tablaRegresionLinealResumenR")
    
    
    table_calculo_RegresionLineal <- reactive({
      
      calculoTRegresionLineal(session, r$valuesmydata  ,
                              df_RegresionLineal_Seleccion_Agrupamiento(),
                              
                              df_RegresionLineal_Seleccion_Dependiente()   )
      
    })
    
    output$tablaRegresionLineal <- gt::render_gt({
      
      descargaTablasRegresion$tabla_TregresionLineal <<- apaRegresion(table_calculo_RegresionLineal()[[1]],
                                                                      "Prueba regresionLineal",
                                                                      table_calculo_RegresionLineal()[[2]],"regresionLineal_Tabla")
    })
    
    output$tablaRegresionLinealResumenR <- gt::render_gt({
      
      descargaTablasRegresion$tabla_TregresionLinealResumenR <<- apaRegresion(table_calculo_RegresionLineal()[[4]],
                                                                              "Regresion Lineal R summary",
                                                                              table_calculo_RegresionLineal()[[2]],"regresionLinealResumenR_Tabla")
    })
    
    
    lapply(1:2, function(i) {
      
      shinyjs::onevent("dblclick",nombregtWidgetRegresionLineal[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                                                                                           html = TRUE, text = tagList( radioButtons(ns(uiRegresionLinealRadio[i]), "", 
                                                                                                                                     choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                                                                                                                        downloadButton(ns(descargaUInameRegresionLineal[i]),"Descargar" ))))
    })
    
    
    
    output$descargaRegresionLineal <- downloadHandler (
      
      filename = function() {
        paste0("regresionLineal", input$uiRadioRegresionLineal)
      },
      
      content = function(file) {
        if(input$uiRadioRegresionLineal == ".rtf") {
          descargaTablasRegresion$tabla_TregresionLineal %>% gt::gtsave("regresionLineal.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionLineal.rtf"), file )
        } else if (input$uiRadioRegresionLineal == ".tex") {
          descargaTablasRegresion$tabla_TregresionLineal %>% gt::gtsave("regresionLineal.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionLineal.tex"), file )
          
        }}
    )
    
    
    output$descargaRegresionLinealResumen <- downloadHandler (
      
      filename = function() {
        paste0("regresionLinealResumen", input$uiRadioRegresionLinealResumen)
      },
      
      content = function(file) {
        if(input$uiRadioRegresionLinealResumen == ".rtf") {
          descargaTablasRegresion$tabla_TregresionLinealResumenR %>% gt::gtsave("regresionLinealResumen.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionLinealResumen.rtf"), file )
        } else if (input$uiRadioRegresionLinealResumen == ".tex") {
          descargaTablasRegresion$tabla_TregresionLinealResumenR %>% gt::gtsave("regresionLinealResumen.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionLinealResumen.tex"), file )
          
        }}
    )
    
    
    
    
    
    
    
    
  ##################  REGRESION BINOMIAL #########
    
    
    table_calculo_RegresionBinomial <- reactive({
      
      calculoTRegresionBinomial(session,r$valuesmydata,
                                df_RegresionBinomial_Seleccion_Agrupamiento(),
                                
                                df_RegresionBinomial_Seleccion_Dependiente()   )
      
    })
    
    
    output$tablaRegresionBinomial <- gt::render_gt({
      
      descargaTablasRegresion$tabla_TregresionBinomial <<- apaRegresion(table_calculo_RegresionBinomial()[[1]],
                                                                        "Prueba regresionBinomial",
                                                                        table_calculo_RegresionBinomial()[[2]],"regresionBinomial_Tabla")
    })
    
    
    output$tablaRegresionBinomialResumenR <- gt::render_gt({
      
      descargaTablasRegresion$tabla_TregresionBinomialResumenR <<- apaRegresion(table_calculo_RegresionBinomial()[[4]],
                                                                                "Regresion Binomial R summary",
                                                                                table_calculo_RegresionBinomial()[[2]],"regresionBinomialResumenR_Tabla")
    })
    
    
    descargaUInameRegresionBinomial <- c("descargaRegresionBinomial","descargaRegresionBinomialResumen")
    
    uiRegresionBinomialRadio  <- c("uiRadioRegresionBinomial","uiRadioRegresionBinomialResumen")
    
    nombregtWidgetRegresionBinomial  <- c("tablaRegresionBinomial","tablaRegresionBinomialResumenR")
    
    
    lapply(1:2, function(i) {
      
      shinyjs::onevent("dblclick",nombregtWidgetRegresionBinomial[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                                                                                             html = TRUE, text = tagList( radioButtons(ns(uiRegresionBinomialRadio[i]), "", 
                                                                                                                                       choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                                                                                                                          downloadButton(ns(descargaUInameRegresionBinomial[i]),"Descargar" ))))
    })
    
    
    
    output$descargaRegresionBinomial <- downloadHandler (
      
      filename = function() {
        paste0("regresionBinomial", input$uiRadioRegresionBinomial)
      },
      
      content = function(file) {
        if(input$uiRadioRegresionBinomial == ".rtf") {
          descargaTablasRegresion$tabla_TregresionBinomial %>% gt::gtsave("regresionBinomial.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionBinomial.rtf"), file )
        } else if (input$uiRadioRegresionBinomial == ".tex") {
          descargaTablasRegresion$tabla_TregresionBinomial %>% gt::gtsave("regresionBinomial.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionBinomial.tex"), file )
          
        }}
    )
    
    
    output$descargaRegresionBinomialResumen <- downloadHandler (
      
      filename = function() {
        paste0("regresionBinomialResumen", input$uiRadioRegresionBinomialResumen)
      },
      
      content = function(file) {
        if(input$uiRadioRegresionBinomialResumen == ".rtf") {
          descargaTablasRegresion$tabla_TregresionBinomialResumenR %>% gt::gtsave("regresionBinomialResumen.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionBinomialResumen.rtf"), file )
        } else if (input$uiRadioRegresionBinomialResumen == ".tex") {
          descargaTablasRegresion$tabla_TregresionBinomialResumenR %>% gt::gtsave("regresionBinomialResumen.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionBinomialResumen.tex"), file )
          
        }}
    )
    
    
    
    
    
    
  ################### REGRESION MULTINOMIAL ###############
    
    descargaUInameRegresionMultinomial <- c("descargaRegresionMultinomial","descargaRegresionMultinomialResumen")
    
    uiRegresionMultinomialRadio  <- c("uiRadioRegresionMultinomial","uiRadioRegresionMultinomialResumen")
    
    nombregtWidgetRegresionMultinomial  <- c("tablaRegresionMultinomial","tablaRegresionMultinomialResumenR")
    
    
    table_calculo_RegresionMultinomial <- reactive({
      
      calculoTRegresionMultinomial(session, r$valuesmydata,
                                   df_RegresionMultinomial_Seleccion_Agrupamiento(),
                                   
                                   df_RegresionMultinomial_Seleccion_Dependiente(), input$inputNivelReferencia   )
      
    })
    
    output$nivelRefenciaRegresionMultinomial <- renderUI({
      
      
      selectInput(
        inputId= ns("inputNivelReferencia"),
        label= "Variable de Referencia ",
        choices= sapply(df_RegresionMultinomial_Seleccion_Dependiente(), levels)
        
      )
      
      
    })
    
    
    output$tablaRegresionMultinomial <- gt::render_gt({
      
      descargaTablasRegresion$tabla_TregresionMultinomial <<- apaRegresion(table_calculo_RegresionMultinomial()[[1]],
                                                                           "Prueba regresionMultinomial",
                                                                           table_calculo_RegresionMultinomial()[[2]],"regresionMultinomial_Tabla")
    })
    
    
    output$tablaRegresionMultinomialResumenR <- gt::render_gt({
      
      descargaTablasRegresion$tabla_TregresionMultinomialResumenR <<- apaRegresion(table_calculo_RegresionMultinomial()[[4]],
                                                                                   "Regresion Multinomial R summary",
                                                                                   table_calculo_RegresionMultinomial()[[2]],"regresionMultinomialResumenR_Tabla")
    })
    
    
    lapply(1:2, function(i) {
      
      shinyjs::onevent("dblclick",nombregtWidgetRegresionMultinomial[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                                                                                                html = TRUE, text = tagList( radioButtons(ns(uiRegresionMultinomialRadio[i]), "", 
                                                                                                                                          choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                                                                                                                             downloadButton(ns(descargaUInameRegresionMultinomial[i]),"Descargar" ))))
    })
    
    
    
    output$descargaRegresionMultinomial <- downloadHandler (
      
      filename = function() {
        paste0("regresionMultinomial", input$uiRadioRegresionMultinomial)
      },
      
      content = function(file) {
        if(input$uiRadioRegresionMultinomial == ".rtf") {
          descargaTablasRegresion$tabla_TregresionMultinomial %>% gt::gtsave("regresionMultinomial.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionMultinomial.rtf"), file )
        } else if (input$uiRadioRegresionMultinomial == ".tex") {
          descargaTablasRegresion$tabla_TregresionMultinomial %>% gt::gtsave("regresionMultinomial.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionMultinomial.tex"), file )
          
        }}
    )
    
    
    output$descargaRegresionMultinomialResumen <- downloadHandler (
      
      filename = function() {
        paste0("regresionMultinomialResumen", input$uiRadioRegresionMultinomialResumen)
      },
      
      content = function(file) {
        if(input$uiRadioRegresionMultinomialResumen == ".rtf") {
          descargaTablasRegresion$tabla_TregresionMultinomialResumenR %>% gt::gtsave("regresionMultinomialResumen.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionMultinomialResumen.rtf"), file )
        } else if (input$uiRadioRegresionMultinomialResumen == ".tex") {
          descargaTablasRegresion$tabla_TregresionMultinomialResumenR %>% gt::gtsave("regresionMultinomialResumen.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/regresionMultinomialResumen.tex"), file )
          
        }}
    )
    
    
    
    
    
      })

}
    
## To be copied in the UI
# mod_Regresion_ui("Regresion_ui_1")
    
## To be copied in the server
# mod_Regresion_server("Regresion_ui_1")

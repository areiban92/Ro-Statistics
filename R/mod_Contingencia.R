#' Contingencia UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Contingencia_ui <- function(id){
  ns <- NS(id)
  
  
  tabPanel( div(  id= ns("textoIconos"),
            div(  id= ns("imgIconos"),  
              tags$img(src ='www/iconos2/contingencia.png', style= "width: 50px; margin-top: 0px; margin-bottom: 5px; height: 30px")),
          div(
            "CONTINGENCIA")
          
    ),value = "Tab 4",
    sidebarPanel( style =paste0(" height: 650px; overflow-y: auto; "), width = 6, 
                  
                  fluidRow(
                    column( width = 6,
                            
                            div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                                uiOutput(ns("bucket_1_Tcontingencia")))
                    ),
                    column(width = 5,
                           div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                               fluidRow(
                                 #tags$b("VARIABLES2"),
                                 tags$div(id = ns('placeholderContingenciaDependiente'))
                                 
                               )),
                           div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em",
                               fluidRow(width = 4,
                                        
                                        tags$div(id = ns('placeholderContingenciaAgrupamiento'))
                                        
                                      
                               ))
                    )
                  ),#fin fluid row
                  
                  actionButton(ns("menuTestContingencia"), " Test "),
                  fluidRow(  div(id= ns("bloqueTestContingencia"),
                                 
                                 column(3,
                                        
                                        tags$div(tags$span(strong(("Test")),style = "color: black;")),
                                        
                                        
                                        div( id = ns("chiCuadradoContingenciaEnable"),checkboxInput(ns("chiCuadradoContingenciaEnables"), HTML(paste0("X",tags$sup("2"))), FALSE)),
                                        div( style=" margin-top: -10px;", id = ns("fisherTestContingenciaEnable"),checkboxInput(ns("fisherTestContingenciaEnables"), "Fisher Test", FALSE))
                                        
                                        
                                        
                                 ),
                                 column(width = 4,
                                        
                                        tags$div(tags$span(strong(("Hip\u00d3tesis")),style = "color: black;")),
                                        
                                        
                                        #  div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -10px; " ,   id= "br_Contingencia_Hipotesis1"),
                                        
                                        div( style="display: inline-block;vertical-align:top; margin-top: -10px;", id ="br_Contingencia_Hipotesis2", 
                                             div( id= ns("radioContingenciaHypotesis"),radioButtons(ns( "radioInputContingenciaHypotesis"), "",
                                                                                                c("Grupo 1  != Grupo 2"="hipotesis_Igual",
                                                                                                  "Grupo 1 > Grupo 2"="hipotesis_Mayor",
                                                                                                  "Grupo 1 < Grupo 2"="hipotesis_Menor"
                                                                                                ),
                                                                                                selected = "hipotesis_Igual"))) #Fin radioButons
                                        
                                 ), #fin columna2
                                 column(width = 5,
                                        
                                        tags$div(tags$span(strong(("Estadística")),style = "color: black;")),
                                        
                                        div(
                                          div( id =ns("check_Contingencia_Parametro_Ubicacion"),checkboxInput( ns("checkInput_Contingencia_Parametro_Ubicacion"), "Parametro Ubicaci\u00f3n", FALSE)) ,
                                          div( style="display: inline-block;vertical-align:top; width: 10px;", id = ns("br_Contingencia_Intervalo_Confidencia")),
                                          div( style="display: inline-block;vertical-align:top; width: 85px; margin-top: -20px; ", id = ns("check_Contingencia_Intervalo_Confidencia"),checkboxInput(ns("checkInput_Contingencia_Intervalo_Confidencia"), "Int Confid", FALSE)),
                                          div( style="display: inline-block;vertical-align:top; width: 45px; margin-top: -10px; ", id = ns("input_Contingencia_Intervalo_Confidencia"),textInput(ns("txtInput_Contingencia_Intervalo_Confidencia"),label= NULL,value = "95"))
                                          
                                        )
                                 )      
                  )),  #Fin de fluidRow
                  
                  actionButton( ns("menuPorcentajesContingencia"), " Porcentajes "),
                  fluidRow( div( id= ns("bloquePorcentajesContingencia"),  
                                 column(6,
                                        
                                        tags$div(tags$span(strong((" Porcentajes")),style = "color: black;")),
                                        
                                        # div( style=" width: 10px;", id ="br_Contingencia_Porcentajes1"),
                                        
                                        div( style="display: inline-block;",
                                             
                                             div(  id = ns( "check_Contingencia_PorcentajeFilas"), checkboxInput( ns("checkInput_Contingencia_PorcentajeFilas"),"Filas",FALSE)),
                                             div( style = " margin-top: -15px; ", id = ns("check_Contingencia_PorcentajeFilas"), checkboxInput( ns("checkInput_Contingencia_PorcentajeColumnas"),"Columnas",FALSE)),
                                             
                                        )
                                        
                                 ), #Fin de columnas de procentajes
                                 column(6,
                                        
                                        tags$div(tags$span(strong((" Conteo:")),style = "color: black;")),
                                        
                                        div( checkboxInput( ns("checkInput_Contingencia_Conteo"),"Esperado",FALSE))
                                        
                                 )
                  ) # fin del DIV
                  ) # Fin Fluid Row
    ),
    mainPanel(style =paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6, 
              
              tags$div(tags$span(strong(h3("An\u00e1lisis Tablas Contingencia")),style = "color: black;")),
              
              gt::gt_output( ns("tablaContingencia") ),
              conditionalPanel("input.chiCuadradoContingenciaEnables == true",ns=ns, h2("Test Chi Cuadrado"),
                               
                               gt::gt_output( ns("tablaChiCUadradoContingencia") )),
              conditionalPanel("input.fisherTestContingenciaEnables == true", ns=ns,h2("Test Fisher "),
                               
                               gt::gt_output( ns("tablaFisherContingencia") )),
              
              conditionalPanel("input.checkInput_Contingencia_Conteo == true",ns=ns, h2("Conteo Esperado"),
                               
                               gt::gt_output( ns("tablaConteoEsperado") ))
              
              
    )
  )
  
  
  
  
  
  
  
  
}
    
#' Contingencia Server Functions
#'
#' @noRd 
mod_Contingencia_server <- function(id,namesClasified, r = r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    banderaContingencia <- FALSE
    namesClasified <- NULL
    
    ##################################  TABLAS DE CONTINGENCIA  ################################
    
    radioContingenciaFisher_HypoValue <- reactive({input$radioInputContingenciaHypotesis})
    checkContingenciaTipoFisher <- reactive ({input$fisherTestContingenciaEnables})
    txtContingenciaFisher_IntervaloConfidencia <- reactive({ input$txtInput_Contingencia_Intervalo_Confidencia})
    checkContingenciaPorcentajeFila <- reactive({  input$checkInput_Contingencia_PorcentajeFilas    })
    checkContingenciaPorcentajeColumna <- reactive({  input$checkInput_Contingencia_PorcentajeColumnas    })
    checkContingenciaConteoEsperado <- reactive({  input$checkInput_Contingencia_Conteo    })
    
    descargaTablasContingencia <-  reactiveValues(tabla_Tcontingencia = NULL,
                                                  tabla_Tcontingencia_Fisher = NULL,
                                                  tabla_Tcontingencia_Chicuadrado = NULL,
                                                  table_calculoEsperadoFinal = NULL)
    
    
    
    ############################################  OBSERVER CONTINGENCIA ###############
    observeEvent(input$fisherTestContingenciaEnables, {
      
      shinyjs::toggleState ("radioContingenciaHypotesis")   
      shinyjs::toggleState ("checkInput_Contingencia_Parametro_Ubicacion")
      shinyjs::toggleState ("checkInput_Contingencia_Intervalo_Confidencia")
      shinyjs::toggleState ("txtInput_Contingencia_Intervalo_Confidencia")
      
    })
    
    observe({
      
      if(input$menuTestContingencia %% 2 == 0){
        
        shinyjs::hide(id = "bloqueTestContingencia")
      }
      else{
        
        shinyjs::show(id = "bloqueTestContingencia")
      }
      
    })
    
    observe({
      
      if(input$menuPorcentajesContingencia %% 2 == 0){
        
        shinyjs::hide(id = "bloquePorcentajesContingencia")
      }
      else{
        
        shinyjs::show(id = "bloquePorcentajesContingencia")
      }
      
    })
    
    ##################################### CONTINGENCIA #############
    
    table_calculoTcontingencia <- reactive({
      
      calculoTcontingencia(session,values$mydata,df_Tcontingencia_Seleccion_Dependiente(), df_Tcontingencia_Seleccion_Agrupamiento(),
                           checkContingenciaPorcentajeFila(),
                           checkContingenciaPorcentajeColumna() )
      
    })
    
    table_calculoEsperado <- reactive({
      
      calculoConteoEsperado( session,values$mydata,df_Tcontingencia_Seleccion_Dependiente(), df_Tcontingencia_Seleccion_Agrupamiento(),
                             checkContingenciaConteoEsperado() )
      
      
    })
    
    table_calculoFisherContingencia <- reactive({
      
      calculoFisherContingencia(session,values$mydata,df_Tcontingencia_Seleccion_Agrupamiento(), df_Tcontingencia_Seleccion_Dependiente(),
                                radioContingenciaFisher_HypoValue(),
                                txtContingenciaFisher_IntervaloConfidencia())
      
    })
    
    table_estadisticaCalculoContingencia <- reactive({
      
      estadisticaCalculoContingencia(session,values$mydata,
                                     df_Tcontingencia_Seleccion_Dependiente(), df_Tcontingencia_Seleccion_Agrupamiento())
      
      
    })
    
########################################## CONTINGENCIA TABLAS
    
    
    output$tablaContingencia <- gt::render_gt({
      
      descargaTablasContingencia$tabla_Tcontingencia <- apaContingencia(table_calculoTcontingencia()[[1]],
                                                                        "Tabla de Contingencia",
                                                                        table_calculoTcontingencia()[[2]],"contingencia_Tabla")
    })
    
    output$tablaFisherContingencia <- gt::render_gt({
      
      
      descargaTablasContingencia$tabla_Tcontingencia_Fisher <- apaContingenciaFisher(table_calculoFisherContingencia()[[1]],
                                                                                     " Prueba de Fisher",
                                                                                     table_calculoFisherContingencia()[[2]],"contingenciaFisher_tabla")
    })
    
    output$tablaChiCUadradoContingencia <- gt::render_gt({
      
      
      descargaTablasContingencia$tabla_Tcontingencia_Chicuadrado <- apaContingenciaCuadrado(table_estadisticaCalculoContingencia()[[1]],
                                                                                            "Prueba Chi-Cuadrado",
                                                                                            table_estadisticaCalculoContingencia()[[2]],"contingenciaChi_tabla")
    })
    
    output$tablaConteoEsperado <- gt::render_gt({
      
      
      descargaTablasContingencia$table_calculoEsperadoFinal <- apaContingencia(table_calculoEsperado()[[1]],
                                                                               "Tabla Conteo Esperado",
                                                                               table_calculoEsperado()[[2]],"contingencia_ConteoTabla")
    })
    
    
    descargaUInameContingencia <- c("descargaTcontingencia","descargaTcontingenciaChicuadrado",
                                    "descargaTcontingenciaFisher","descargaTcontingenciaEsperado" )
    
    uiContingenciaRadio <- c("uiRadioContingencia","uiRadioContingenciaChicuadrado",
                             "uiRadioContingenciaFisher","uiRadioContingenciaEsperado")
    
    nombregtWidgetContingencia <- c("tablaContingencia","tablaChiCUadradoContingencia",
                                    "tablaFisherContingencia","tablaConteoEsperado")
    
    
    lapply(1:4, function(i) {
      
      
      shinyjs::onevent("dblclick",nombregtWidgetContingencia[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                                                                                        html = TRUE, text = tagList( radioButtons(uiContingenciaRadio[i], "", 
                                                                                                                                  choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                                                                                                                     downloadButton(descargaUInameContingencia[i],"Descargar" ))))
    })
    
    
    
    output$descargaTcontingencia <- downloadHandler (
      
      filename = function() {
        paste0("contingencia", input$uiRadioContingencia)
      },
      
      content = function(file) {
        if(input$uiRadioContingencia == ".rtf") {
          descargaTablasContingencia$tabla_Tcontingencia %>% gt::gtsave("contingencia.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/contingencia.rtf"), file )
        } else if (input$uiRadioContingencia == ".tex") {
          descargaTablasContingencia$tabla_Tcontingencia %>% gt::gtsave("contingencia.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/contingencia.tex"), file )
          
        }}
    )
    
    
    
    output$descargaTcontingenciaChicuadrado <- downloadHandler (
      
      filename = function() {
        paste0("tablaContingenciaChicuadrado", input$uiRadioContingenciaChicuadrado)
      },
      
      content = function(file) {
        if(input$uiRadioContingenciaChicuadrado == ".rtf") {
          descargaTablasContingencia$tabla_Tcontingencia_Chicuadrado %>% gt::gtsave("tablaContingenciaChicuadrado.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/tablaContingenciaChicuadrado.rtf"), file )
        } else if (input$uiRadioContingenciaChicuadrado == ".tex") {
          descargaTablasContingencia$tabla_Tcontingencia_Chicuadrado %>% gt::gtsave("tablaContingenciaChicuadrado.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/tablaContingenciaChicuadrado.tex"), file )
          
        }}
    )
    
    
    
    
    output$descargaTcontingenciaFisher <- downloadHandler (
      
      filename = function() {
        paste0("tablaContingenciaFisher", input$uiRadioContingenciaChicuadrado)
      },
      
      content = function(file) {
        if(input$uiRadioContingenciaChicuadrado == ".rtf") {
          descargaTablasContingencia$tabla_Tcontingencia_Fisher %>% gt::gtsave("tablaContingenciaFisher.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/tablaContingenciaFisher.rtf"), file )
        } else if (input$uiRadioContingenciaChicuadrado == ".tex") {
          descargaTablasContingencia$tabla_Tcontingencia_Fisher %>% gt::gtsave("tablaContingenciaFisher.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/tablaContingenciaFisher.tex"), file )
          
        }}
    )
    
    
    
    output$descargaTcontingenciaEsperado <- downloadHandler (
      
      filename = function() {
        paste0("tablaContingenciaConteoEsperado", input$uiRadioContingenciaEsperado)
      },
      
      content = function(file) {
        if(input$uiRadioContingenciaEsperado == ".rtf") {
          descargaTablasContingencia$table_calculoEsperadoFinal %>% gt::gtsave("tablaContingenciaConteoEsperado.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/tablaContingenciaConteoEsperado.rtf"), file )
        } else if (input$uiRadioContingenciaEsperado == ".tex") {
          descargaTablasContingencia$table_calculoEsperadoFinal %>% gt::gtsave("tablaContingenciaConteoEsperado.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/tablaContingenciaConteoEsperado.tex"), file )
          
        }}
    )
    
    
############################### SELECTORES CONTINGENCIA   ############
    
    
    
    df_Tcontingencia_Seleccion_Agrupamiento <- reactive({
      
      req(input$rank_list_Tcontingencia_Agrupamiento)
      df_Tcontingencia_Seleccion_Agrupamiento <- r$valuesmydata %>% dplyr::select(input$rank_list_Tcontingencia_Agrupamiento)
      
    })
    
    df_Tcontingencia_Seleccion_Dependiente <- reactive({
      
      req(input$rank_list_Tcontingencia_Dependiente)
      df_Tcontingencia_Seleccion_Dependiente<- r$valuesmydata %>% dplyr::select(input$rank_list_Tcontingencia_Dependiente)
      
    })
    
    output$bucket_1_Tcontingencia <- renderUI({
      
      sortable::bucket_list(
        header = HTML(as.character(  tags$div(tags$span(strong("Entrada"),style = "color: black;")) )), 
        group_name = "bucket_list_Tcontingencia",
        orientation = "horizontal",
        class = c("default-sortable", "custom-sortable"), # add custom style
        
        
        sortable::add_rank_list(
          text = "",
          labels = namesClasified(),
          input_id = ns("rank_list_Tcontingencia_Entrada"),
          css_id = "rank_Tcontingencia_Entrada"
          
        ))
    })
    
    if (banderaContingencia==FALSE) {
      
      
      banderaContingencia==TRUE
      
      insertUI(
        selector=paste0("#",ns("placeholderContingenciaDependiente")),
        where="beforeBegin",
        ui = tags$div(id = ns("contingenciaBoxDependiente"),
                      sortable::bucket_list(
                        header = HTML(as.character(  tags$div(tags$span(strong("Filas"),style = paste0('color: black;')),
                                                              tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )),
                        group_name = "bucket_list_Tcontingencia",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_Tcontingencia_Agrupamiento"),
                          css_id = "rank_Tcontingencia_Agrupamiento"
                        ))
        ))
      
      insertUI(
        selector=paste0("#",ns("placeholderContingenciaAgrupamiento")),
        where="beforeBegin",
        ui = tags$div(id = ns("contingenciaBoxAgrupamiento"),
                      
                      sortable::bucket_list(
                        header = HTML(as.character(   tags$div(tags$span(strong("Columnas"),style = paste0('color: black;')),
                                                               tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )),
                        group_name = "bucket_list_Tcontingencia",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_Tcontingencia_Dependiente"),
                          css_id = "rank_Tcontingencia_Dependiente"
                        ))
                      
                      
        ))
      
      
    }
    
    
    observeEvent( r$inputGuardar,{
      
      golem::print_dev("CONTINGENCIASSSSSSSSSSSSSSSSSSSSSSSSSSSSS")
   
      
      removeUI(
        selector =  paste0("#",ns("contingenciaBoxDependiente"))
      )
      
      removeUI(
        selector =  paste0("#",ns("contingenciaBoxAgrupamiento"))
      )
      
      
      insertUI(selector= paste0("#",ns("placeholderContingenciaDependiente")),
               where='beforeBegin',
               ui =  tags$div(id =ns("contingenciaBoxDependiente"),
                              sortable::bucket_list(
                                header =  HTML(as.character( tags$div(tags$span(strong(' Variables: '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue'),
                                                                      tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')) )),
                                group_name = "bucket_list_Tcontingencia",
                                orientation = 'horizontal',
                                class = c('default-sortable', "custom-sortableDependiente"), 
                                
                                sortable::add_rank_list(
                                  text = '',
                                  labels = NULL,
                                  input_id =ns("rank_list_Tcontingencia_Dependiente"),
                                  css_id ="rank_Tcontingencia_Dependiente" ,
                                  options = sortable::sortable_options( group = list(pull = 'clone',name = 'bucket_list_Descriptiva',put = FALSE)    )))))
      
      
      insertUI(selector= paste0("#",ns("placeholderContingenciaAgrupamiento")),
               where='beforeBegin',
               ui =  tags$div(id = ns("contingenciaBoxAgrupamiento"),
                              sortable::bucket_list(header =  HTML(as.character(  tags$div(tags$span(strong(' Segmentado por : '),style = paste0('color: black;')),
                                                                                           tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),
                                                    
                                                    group_name = "bucket_list_Tcontingencia",
                                                    orientation = 'horizontal',
                                                    
                                                    class = c('default-sortable', "custom-sortableAgrupamiento"),
                                                    sortable::add_rank_list(
                                                      text = '',
                                                      labels = NULL,
                                                      input_id = ns("rank_list_Tcontingencia_Agrupamiento"),
                                                      css_id ="rank_Tcontingencia_Agrupamiento"))))
 
    })
    
    
    
    
    
    })
}
    
## To be copied in the UI
# mod_Contingencia_ui("Contingencia_ui_1")
    
## To be copied in the server
# mod_Contingencia_server("Contingencia_ui_1")

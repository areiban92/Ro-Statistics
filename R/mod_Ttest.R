#' Ttest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Ttest_ui <- function(id){
  ns <- NS(id)
  
  #pagina Ttest
  
  
  tabPanel(div(  id=ns("textoIconos"),
                 
                 div(  id= ns("imgIconos"),           
                       tags$img(src = 'www/iconos2/ttest.png' , style= "width: 50px; margin-top: 0px; margin-bottom: 5px;  height: 30px")),
                 
                 div(" PRUEBAS - T " )
                 
  ),value = "Tab 3",
  sidebarPanel(style =paste0(" height: 650px; overflow-y: auto; "), width = 6,
               
               
               fluidRow(
                 column(
                   
                   width = 6,
                   div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                       uiOutput(ns("bucket_1_Ttest")))
                 ), 
                 column(width = 5,
                        div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                            fluidRow(
                              
                              tags$div(id = ns('placeholderTtestDependiente'))
                            )),
                        div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em", 
                            fluidRow(width = 4,
                                     tags$div(id = ns('placeholderTtestAgrupamiento'))
                                     
                            ))
                 )
               ),#fin fluid row
               
               actionButton(ns("menutestTtest"), "Test"),
               
               fluidRow( div ( id = ns("bloqueTestTtest"),
                               
                               column(width = 4,
                                      
                                      tags$div(tags$span(strong(("Test: ")),style = "color: black;")),
                                      
                                      
                                      #  div( style="display: inline-block;vertical-align:top; width: 20px; " ,   id= "br_Ttest_Tipo0"),
                                      div(  style="display: inline-block;vertical-align:top;  " , id= "br_Ttest_Tipo1",
                                            div(  id = ns("check_Ttest_Student"),checkboxInput( ns("checkInput_Ttest_Student"), "Student", FALSE)),
                                            div(  style ="margin-top: -15px;", id = ns("check_Ttest_Welch"),checkboxInput( ns("checkInput_Ttest_Welch"), "Welch", FALSE)),
                                            div(  style ="margin-top: -15px;",  id = ns("check_Ttest_Mann_Whitney"),checkboxInput( ns("checkInput_Ttest_Mann_Whitney"), "Mann-Whitney", FALSE))
                                      ), #fin del div
                                      
                               ), #fin columna1
                               column(width = 4,
                                      
                                      tags$div(tags$span(strong(("Hip\u00f3tesis: ")),style = "color: black;")),
                                      
                                      
                                      # div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -10px; " ,   id= "br_Ttest_Hipotesis1"),
                                      div( style="display: inline-block;vertical-align:top; margin-top: -10px;", id ="br_Ttest_Hipotesis2", 
                                           div(id= ns("radioTtestHypotesis"),radioButtons(ns("radioInputTtestHypotesis"), "",
                                                                                      c("Grupo1 != Grupo 2"="hipotesis_Igual",
                                                                                        "Grupo 1 > Grupo 2"="hipotesis_Mayor",
                                                                                        "Grupo 1 < Grupo 2"="hipotesis_Menor"
                                                                                      ),
                                                                                      selected = "hipotesis_Igual"))) #Fin radioButons
                                      
                                      
                               ), #fin columna2
                               
                               column(width = 4,
                                      tags$div(tags$span(strong(("Estadística:  ")),style = "color: black;")),
                                      
                                      div(
                                        div( id = ns("check_Ttest_Parametro_Ubicacion"),checkboxInput( ns("checkInput_Ttest_Parametro_Ubicacion"), "Parametro Ubicaci\u00f3n", FALSE)) ,
                                        
                                        
                                        div( style="display: inline-block;vertical-align:top; width: 10px;", id =  ns("br_Ttest_Intervalo_Confidencia")),
                                        div( style="display: inline-block;vertical-align:top; width: 85px; margin-top: -20px; ", id = ns("check_Ttest_Intervalo_Confidencia"),checkboxInput(ns("checkInput_Ttest_Intervalo_Confidencia"), "Int Confid", FALSE)),
                                        div( style="display: inline-block;vertical-align:top; width: 45px; margin-top: -10px; ", id = ns("input_Ttest_Intervalo_Confidencia"),textInput(ns( "txtInput_Ttest_Intervalo_Confidencia"),label= NULL,value = "95")),
                                        
                                        div( style= "margin-top: -25px;",  id = ns( "check_Ttest_Estadistica"),checkboxInput( ns("checkInput_Ttest_Estadistica"), "Descriptiva", FALSE))
                                        
                                      )
                                      
                               )#Fin de column3
                               
               ) #Fin del  DIV
               ),#Fin fluidRow
               
               actionButton(ns("menuSuposicionesTtest"), "Verificaci\u00f3n de Suposiciones"),
               fluidRow( div( id=ns("bloqueSuposicionesTtest"),
                              
                              column(width = 8,
                                     tags$div(tags$span(strong(("Verificaci\u00f3n de Suposiciones")),style = "color: black;")),
                                     
                                     
                                     div(
                                       div(  id = ns("check_Ttest_Suposiciones_Normalidad"),checkboxInput(ns("checkInput_Ttest_Suposiciones_Normalidad"), "Normalidad", FALSE)),
                                       div(style= "margin-top: -15px;", id = ns("check_Ttest_Suposiciones_VarianzasIguales"),checkboxInput(ns( "checkInput_Ttest_Suposiciones_VarianzasIguales"), "Igualdad de Varianza", FALSE))
                                     ), #fin del div
                                     
                              ) #fin columna1
               )# fin del DIV
               )#Fin de FluidRow
               
               
  ),
  
  mainPanel(style =paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6,
            
            h3("An\u00e1lisis T-Test"),
            
            conditionalPanel("input.checkInput_Ttest_Student == true || input.checkInput_Ttest_Mann_Whitney == true || input.checkInput_Ttest_Welch == true", ns = ns, h3("Test de Independencia T-Test"), verbatimTextOutput(ns("txt_Ttest_Independencia_Student")),
                             gt::gt_output(ns("ttesttabla"))
            ),
            
            #check de Validacion de Suposicion
            conditionalPanel("input.checkInput_Ttest_Suposiciones_Normalidad == true || input.checkInput_Ttest_Suposiciones_VarianzasIguales == true ", ns = ns ,h3("Verificaci\u00d3n de Suposiciones"),verbatimTextOutput(ns("txt_Ttest_Independencia_Suposiciones") )),
            conditionalPanel("input.checkInput_Ttest_Suposiciones_Normalidad == true ",ns = ns ,gt::gt_output(ns("table_tTest_VerifSupos_Normalidad") )),
            conditionalPanel("input.checkInput_Ttest_Suposiciones_VarianzasIguales == true ",ns = ns, gt::gt_output(ns( "table_tTest_VerifSupos_VarianzasIguales") )),
            conditionalPanel("input.checkInput_Ttest_Estadistica == true ", ns = ns, gt::gt_output(ns("estadisticasTtest") ))
            

            
  ) # fin main panel
  
  )
  
  
  
}
    
#' Ttest Server Functions
#'
#' @noRd 
mod_Ttest_server <- function(id, namesClasified, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    banderaTtest <- FALSE
    
    ####################################### Check T-Test ######################
    radioTtestHypoValue <- reactive({input$radioInputTtestHypotesis})
    
    checkTtestTipoStudent <- reactive({input$checkInput_Ttest_Student})
    checkTtestTipoWelch <- reactive({input$checkInput_Ttest_Welch})
    checkTtestTipoMann <- reactive({input$checkInput_Ttest_Mann_Whitney})
    
    checkTtest_Supo_Normal <- reactive({ input$checkInput_Ttest_Suposiciones_Normalidad })
    checkTtest_Supo_VarianzasIguales <- reactive({ input$checkInput_Ttest_Suposiciones_VarianzasIguales })
    
    checkTtest_Localizacion <- reactive({ input$checkInput_Ttest_Parametro_Ubicacion})
    txtTtest_IntervaloConfidencia <- reactive({ input$txtInput_Ttest_Intervalo_Confidencia})
    
    checkTtest_IntervaloConfidencia <- reactive({input$checkInput_Ttest_Intervalo_Confidencia})
    checkTtest_Estadisticas <- reactive({input$checkInput_Ttest_Estadistica })
    
    
    descargaTablasTtest <- reactiveValues( tabla_Ttest = NULL,
                                           tabla_Ttest_Normalidad = NULL, 
                                           tabla_Ttest_VarianzaIgual = NULL,
                                           tabla_Ttest_Estadistica = NULL )

    tablaTtestFinal <- reactive({
      calculo_Ttest_Student(session,r$valuesmydata,  df_Ttest_Seleccion_Dependiente(),
                            df_Ttest_Seleccion_Agrupamiento(),radioTtestHypoValue(),
                            checkTtestTipoStudent(),checkTtestTipoWelch(),
                            checkTtestTipoMann(),checkTtest_Localizacion(),
                            checkTtest_IntervaloConfidencia(),txtTtest_IntervaloConfidencia())
      
    })
    
    tabla_calculo_Ttest_Suposiciones_Normal <- reactive({
      
      calculo_Ttest_Suposiciones_Normal(session,r$valuesmydata,
                                        df_Ttest_Seleccion_Dependiente(),df_Ttest_Seleccion_Agrupamiento(),
                                        checkTtest_Supo_Normal())
      
    }) 
    
    table_calculo_Ttest_Suposiciones_VarianzaIgual <- reactive({
      
      
      calculo_Ttest_Suposiciones_VarianzaIgual(session,r$valuesmydata,
                                               df_Ttest_Seleccion_Dependiente(),df_Ttest_Seleccion_Agrupamiento(),
                                               checkTtest_Supo_VarianzasIguales())
      
      
    })
    
    table_calculo_Ttest_Stadisticas <- reactive({
      
      calculo_Ttest_Stadisticas(r$valuesmydata,
                                df_Ttest_Seleccion_Dependiente(),df_Ttest_Seleccion_Agrupamiento(),
                                checkTtest_Estadisticas())
      
      
    })
    
  #####################################Observer-Test################################  
    
    observeEvent(input$checkInput_Ttest_Parametro_Ubicacion, {
      
      # print("SI SE APLASTO")
      shinyjs::toggleState ("checkInput_Ttest_Intervalo_Confidencia")
      shinyjs::toggleState ("txtInput_Ttest_Intervalo_Confidencia") 
      
    })
    
    observe({
      
      if(input$menutestTtest %% 2 == 0){
        
        shinyjs::hide(id = "bloqueTestTtest") 
      }
      else{
        
        shinyjs::show(id = "bloqueTestTtest") 
      }
      
    })
    
    observe({
      
      if(input$menuSuposicionesTtest %% 2 == 0){
        
        shinyjs::hide(id = "bloqueSuposicionesTtest") 
      }
      else{
        
        shinyjs::show(id = "bloqueSuposicionesTtest") 
      }
      
    })
  ###################################### SELECT DE TTTES Y CUADRITOS ##################
    
    df_Ttest_Seleccion_Agrupamiento <- reactive({
      
      req(input$rank_list_Ttest_Agrupamiento)
      df_Ttest_Seleccion_Agrupamiento<- r$valuesmydata %>% dplyr::select(input$rank_list_Ttest_Agrupamiento)
      
    })
    
    df_Ttest_Seleccion_Dependiente <- reactive({
      
      req(input$rank_list_Ttest_Dependiente)
      df_Ttest_Seleccion_Dependiente<- r$valuesmydata %>% dplyr::select(input$rank_list_Ttest_Dependiente)
      
    })
    
    output$bucket_1_Ttest <- renderUI({
      
      sortable::bucket_list(
        header =  HTML(as.character(  tags$div(tags$span(strong("Entrada"),style = "color: black;")) )),    
        group_name = "bucket_list_Ttest",
        orientation = "horizontal",
        class = c("default-sortable", "custom-sortable"), # add custom style
        
        sortable::add_rank_list(
          text = "",
          labels =  namesClasified() ,
          input_id = "rank_list_Ttest_Entrada",
          css_id = "rank_Ttest_Entrada"
          
        ))
    })
    
    if (banderaTtest==FALSE) {
      banderaTtest==TRUE
      
      golem::print_dev("Agregando TTESTTTTTTTT los cuadrios")
      
      insertUI(
        selector= paste0("#",ns("placeholderTtestDependiente")),
        where="beforeBegin",
        ui = tags$div(id = ns("ttestBoxDependiente"),
                      sortable::bucket_list(
                        header =  HTML(as.character(  tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                               tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                        group_name = "bucket_list_Ttest",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_Ttest_Dependiente"),
                          css_id = "rank_Ttest_Dependiente"
                        ))
        ))
      
      insertUI(
        selector= paste0("#",ns("placeholderTtestAgrupamiento")),
        where="beforeBegin",
        ui = tags$div(id = ns("ttestBoxAgrupamiento"),
                      sortable::bucket_list(
                        header = HTML(as.character(   tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                               tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                        
                        group_name = "bucket_list_Ttest",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                        
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_Ttest_Agrupamiento"),
                          css_id = "rank_Ttest_Agrupamiento"
                        ))
        ))
      
    }
    
    
    observeEvent( r$inputGuardar,{
      
      golem::print_dev("T test")
      
      
      removeUI(
        selector =  paste0("#",ns("ttestBoxDependiente"))
      )
      
      removeUI(
        selector =  paste0("#",ns("ttestBoxAgrupamiento"))
      )
      
      
      
      insertUI(selector=paste0("#",ns("placeholderTtestDependiente")),
      where='beforeBegin',
      ui =  tags$div(id =ns("ttestBoxDependiente"),
      sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Variables : '),style = paste0('color: black;')),
      tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),
      group_name = "bucket_list_Ttest",
      orientation = 'horizontal',
      class = c('default-sortable', "custom-sortableDependiente"),
      sortable::add_rank_list(text = '',labels = NULL,
      input_id =ns("rank_list_Ttest_Dependiente"),
      css_id ="rank_Ttest_Dependiente "))))
      
      insertUI(selector=paste0("#",ns("placeholderTtestAgrupamiento")),
      where='beforeBegin',
      ui =  tags$div(id =ns("ttestBoxAgrupamiento"),
      sortable::bucket_list(header = HTML(as.character(  tags$div(tags$span(strong('Variable de Agrupaci\u00f3n : '),style = paste0('color: black;')),
      tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),
      group_name = "bucket_list_Ttest",
      orientation = 'horizontal',
      class = c('default-sortable', "custom-sortableAgrupamiento"),
      sortable::add_rank_list(text = '',labels = NULL,
      input_id = ns("rank_list_Ttest_Agrupamiento"),
      css_id ="rank_Ttest_Agrupamiento"))))
      
      
      
    })
    
    
    
    ###################################################TTESTABLAS##################
    
    
    output$ttesttabla  <- gt::render_gt({
      
      descargaTablasTtest$tabla_Ttest <- apaTtestConfidencia(
        tablaTtestFinal()[[1]],
        "Muestras Agrupamientos T-test",
        tablaTtestFinal()[[2]],
        tablaTtestFinal()[[3]],"ttestConfidencia_tabla")
      
    })
    
    output$table_tTest_VerifSupos_Normalidad <- gt::render_gt({
      
      
      descargaTablasTtest$tabla_Ttest_Normalidad <- apaTtest(
        tabla_calculo_Ttest_Suposiciones_Normal()[[1]],
        "Prueba de Igualdad de Normalidad (Shapiro Wilk)",
        tabla_calculo_Ttest_Suposiciones_Normal()[[2]],"ttestNormalidadShapiro_tabla"
      )
      
      
    })    ####AQUI seguir modificando despues
    
    output$table_tTest_VerifSupos_VarianzasIguales <- gt::render_gt({
      
      descargaTablasTtest$tabla_Ttest_VarianzaIgual <-  apaTtest(table_calculo_Ttest_Suposiciones_VarianzaIgual()[[1]],
                                                                 "Prueba de Igualdad de Varianza (Levene Test)",
                                                                 table_calculo_Ttest_Suposiciones_VarianzaIgual()[[2]],"ttestVarianzaLevene_tabla")
      
    } )
    
    output$estadisticasTtest <- gt::render_gt ({
      
      descargaTablasTtest$tabla_Ttest_Estadistica <- apaTtest(
        table_calculo_Ttest_Stadisticas()[[1]],
        "Estadistica Descriptiva ",
        table_calculo_Ttest_Stadisticas()[[2]],"ttestEstadistica_table"
      )
    })
    

    descargaUInameTtest <- c("descargaTablaTtest","descargaTablaTtestNormalidad",
                             "descargaTablaTtestVarianzaIgual","descargaTablaTtestEstadistica" )
    
    uiTtestRadio <- c("uiRadioTtest","uiRadioTtestNormalidad",
                      "uiRadioTtestVarianza","uiRadioTtestEstadistica")
    
    
    nombregtWidgetTtest <- c("ttesttabla","table_tTest_VerifSupos_Normalidad",
                             "table_tTest_VerifSupos_VarianzasIguales","estadisticasTtest")
    
    
    lapply(1:4, function(i) {
      
      
      shinyjs::onevent("dblclick",nombregtWidgetTtest[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                                                                                 html = TRUE, text = tagList( radioButtons( ns( uiTtestRadio[i]), "", 
                                                                                                                           choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                                                                                                              downloadButton(ns( descargaUInameTtest[i]) ,"Descargar" ))))
    })
    
    
    output$descargaTablaTtest <- downloadHandler (
      
      filename = function() {
        paste0("ttest", input$uiRadioTtest)
      },
      
      content = function(file) {
        if(input$uiRadioTtest == ".rtf") {
          descargaTablasTtest$tabla_Ttest %>% gt::gtsave("ttest.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/ttest.rtf"), file )
        } else if (input$uiRadioTtest == ".tex") {
          descargaTablasTtest$tabla_Ttest %>% gt::gtsave("ttest.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/ttest.tex"), file )
          
        }}
    )
    
    
    
    output$descargaTablaTtestNormalidad <- downloadHandler (
      
      filename = function() {
        paste0("ttestNormalidad", input$uiRadioTtestNormalidad)
      },
      
      content = function(file) {
        if(input$uiRadioTtestNormalidad == ".rtf") {
          descargaTablasTtest$tabla_Ttest_Normalidad %>% gt::gtsave("ttestNormalidad.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/ttestNormalidad.rtf"), file )
        } else if (input$uiRadioTtestNormalidad == ".tex") {
          descargaTablasTtest$tabla_Ttest_Normalidad %>% gt::gtsave("ttestNormalidad.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/ttestNormalidad.tex"), file )
          
        }}
    )
    
    
    
    
    output$descargaTablaTtestVarianzaIgual <- downloadHandler (
      
      filename = function() {
        paste0("ttestVarianza", input$uiRadioTtestVarianza)
      },
      
      content = function(file) {
        if(input$uiRadioTtestVarianza == ".rtf") {
          descargaTablasTtest$tabla_Ttest_VarianzaIgual %>% gt::gtsave("ttestVarianza.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/ttestVarianza.rtf"), file )
        } else if (input$uiRadioTtestVarianza == ".tex") {
          descargaTablasTtest$tabla_Ttest_VarianzaIgual %>% gt::gtsave("ttestVarianza.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/ttestVarianza.tex"), file )
          
        }}
    )
    
    
    
    output$descargaTablaTtestEstadistica <- downloadHandler (
      
      filename = function() {
        paste0("ttestEstadistica", input$uiRadioTtestEstadistica)
      },
      
      content = function(file) {
        if(input$uiRadioTtestEstadistica == ".rtf") {
          descargaTablasTtest$tabla_Ttest_Estadistica %>% gt::gtsave("ttestEstadistica.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/ttestEstadistica.rtf"), file )
        } else if (input$uiRadioTtestEstadistica == ".tex") {
          descargaTablasTtest$tabla_Ttest_Estadistica %>% gt::gtsave("ttestEstadistica.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/ttestEstadistica.tex"), file )
          
        }}
    )
    
    
 
  })
}
    
## To be copied in the UI
# mod_Ttest_ui("Ttest_ui_1")
    
## To be copied in the server
# mod_Ttest_server("Ttest_ui_1")

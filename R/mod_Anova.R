#' Anova UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Anova_ui <- function(id){
  ns <- NS(id)

  
  tabPanel(div(  id=ns("textoIconos"),
                 
                 div(  id= ns("imgIconos"),           
                       tags$img(src = 'www/iconos2/ttest.png' , style= "width: 50px; margin-top: 0px; margin-bottom: 5px;  height: 30px")),
                 
                 div("ANOVA TEST" )
                 
  ),value = "Tab 7",
  sidebarPanel(style =paste0(" height: 650px; overflow-y: auto; "), width = 6,
               
               fluidRow(
                 column(
                   
                   width = 6,
                   div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                       uiOutput(ns("bucket_1_Anova")))
                 ), 
                 column(width = 5,
                        div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                            fluidRow(
                              
                              tags$div(id = ns('placeholderAnovaDependiente'))
                            )),
                        div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em", 
                            fluidRow(width = 4,
                                     tags$div(id = ns('placeholderAnovaAgrupamiento'))
                            ))
                 )
               ),#fin fluid row
               
               actionButton(ns("menuPostHocAnova"), "Pruebas Post Hoc"),
               fluidRow( div( id= ns("bloquePostHocAnova"),
                              
                              column(6,
                                     div(id= ns("tituloAumiendoIguales"), tags$div(tags$span(strong("Asumiendo que son Iguales"),style = "color: black;"))),
                                     div(style="vertical-align:top;  ", id = ns("checkTurkeyEnable_Anova"),checkboxInput(ns("check_Input_Turkey_Anova"), "Turkey", FALSE)),
                                     div(style="vertical-align:top;  ", id = ns("checkBonferroniEnable_Anova"),checkboxInput(ns("check_Input_Bonferroni_Anova"), "Bonferroni", FALSE)),
                                     
                              ),# Fin  de Columna 
                              column(6,
                                     div(id= ns("tituloGraficasPersonalizadas"), tags$div(tags$span(strong("Asumiendo que son diferentes"),style = "color: black;"))),
                                     div(style="display: inline-block;vertical-align:top;  ", id = ns("checkGamesHowellEnable_Anova"),checkboxInput(ns("check_Input_GamesHowell_Anova"), "Games Howell", FALSE)),
                                     
                              )
                              
               ) # fin del div
               ), # fin de fluid Row
               actionButton(ns("menuOpcionesAnova"), "Opciones"),
               # 
               fluidRow(div( id= ns("bloqueOpcionesAnova"),
                             
                             column(6,
                                    div(style=" vertical-align:top;  ", id = ns("estadisticosAnovaEnable_Anova"),checkboxInput( ns("check_Input_estadisticos_Anova"), "Estadisticos", FALSE)),
                                    
                                    div(style=" vertical-align:top;  ", id = ns("homogenidadAnovaEnable_Anova"),checkboxInput( ns("check_Input_homogeniedad_Anova"), "Homogenidad de Varianzas", FALSE)),
                                    
                             ),
                             column(6,
                                    
                                    div(style="vertical-align:top;  ", id = ns("brownAnovaEnable_Anova"),checkboxInput( ns("check_Input_brown_Anova"), "Brown-Forsythe", FALSE)),
                                    div(style="vertical-align:top;  ", id = ns("welchAnovaEnable_Anova"),checkboxInput( ns("check_Input_welch_Anova"), "Welch", FALSE)) 
                             )
               )# fin del div
               )# Fin de fluidRow
               
  ),# fIN DE SIDEBAR
  mainPanel(style = paste0("background-color: transparent; height: 650px; overflow-y: auto; "), width = 6,
            
            h3("An\u00e1lisis ANOVA un Factor"),
            
            gt::gt_output(ns("tablaAnova")),
            conditionalPanel("input.check_Input_estadisticos_Anova == true ",ns=ns, h3("Descriptivas Anova"), gt::gt_output(ns("tablaAnovaDescriptivas")), h5(" Gr\u00e1fica Descriptivas "), plotOutput(ns("boxplotsAnova") )),
            conditionalPanel("input.check_Input_Turkey_Anova == true || input.check_Input_Bonferroni_Anova == true ",ns=ns, h3("Post Hoc"), gt::gt_output(ns("tablaAnovaPostHocIguales"))),
            conditionalPanel("input.check_Input_homogeniedad_Anova == true ",ns=ns, h3("Homogeniedad de Varianza"),gt::gt_output(ns("tablaAnovaHomogeniedad"))),
            conditionalPanel("input.check_Input_GamesHowell_Anova == true  ",ns=ns, h3("Post Hoc"), gt::gt_output(ns("tablaAnovaPostHocDesiguales"))),
            conditionalPanel("input.check_Input_welch_Anova == true  ",ns=ns, h3("Anova Welch"), gt::gt_output(ns("tablaAnovaWelch"))),
            conditionalPanel("input.check_Input_brown_Anova == true  ",ns=ns, h3("Anova Brown-Forsythe"), gt::gt_output(ns("tablaAnovaBrown")))
            
            
            
  )
  )
  
  
  
  
}
    
#' Anova Server Functions
#'
#' @noRd 
mod_Anova_server <- function(id,namesClasified, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    banderaAnova <- FALSE
    
    descargaTablasAnova <- reactiveValues( tabla_TanovaDescriptivas = NULL,tabla_Tanova = NULL,
                                           tabla_Tanova_PostHoc_Desiguales = NULL,tabla_Tanova_PostHoc_Iguales = NULL,
                                           tabla_Tanova_Efectos_Fijos = NULL, tabla_Tanova_Efectos_Aleatorios = NULL,
                                           tabla_Tanova_Homogeniedad = NULL, tabla_TanovaWelch = NULL,
                                           tabla_TanovaBrown = NULL
    )
    
    checkAnovaTurkey <- reactive(input$check_Input_Turkey_Anova)
    checkAnovaBonferroni <- reactive(input$check_Input_Bonferroni_Anova)
    
    checkAnovaGamesHowell <- reactive(input$check_Input_GamesHowell_Anova)
    checkAnovaEfectos <- reactive(input$check_Input_efectos_Anova)
    checkAnovaHomogeniedad <- reactive(input$check_Input_homogeniedad_Anova)
    
    checkAnovaBrown <- reactive(input$check_Input_brown_Anova)
    checkAnovaWelch <- reactive(input$check_Input_welch_Anova)
    
    checkAnovaEstadisticas <- reactive(input$check_Input_estadisticos_Anova)
    
  
  ###############Observer de ANOVA ############## 
    observe({
      #Colapsa Graficad Basica de Descriptivos
      
      #print(input$menuGraficas)
      
      if(input$menuPostHocAnova %% 2 == 0){
        
        shinyjs::hide(id = "bloquePostHocAnova") 
        # shinyjs::hide(id = "bloqueOpcionesAnova")   
        
        
      } 
      else
      {
        shinyjs::show(id = "bloquePostHocAnova") 
        # shinyjs::show(id = "bloqueOpcionesAnova") 
        
        
      }
      
      if(input$menuOpcionesAnova %% 2 == 0){
        
        
        shinyjs::hide(id = "bloqueOpcionesAnova")   
        
        
      } 
      else
      {
        
        shinyjs::show(id = "bloqueOpcionesAnova") 
        
        
      }
      
    })
  ########################  ANova Funtion ########
    
    table_calculo_Anova <- reactive({
      
      calculoTanova(session,r$valuesmydata,df_Anova_Seleccion_Dependiente(),
                    df_Anova_Seleccion_Agrupamiento()
      )
      
    })
    
    table_calculo_Anova_Stadisticas <- reactive({
      
      calculoEstadisticaAnova(session,r$valuesmydata, df_Anova_Seleccion_Dependiente(),
                              df_Anova_Seleccion_Agrupamiento(),checkAnovaEstadisticas())
      
      
    })
    
    table_calculo_Anova_PostHoc_Iguales <- reactive({
      
      calculoPostHocAnovaIguales(session,r$valuesmydata,df_Anova_Seleccion_Dependiente(),
                                 df_Anova_Seleccion_Agrupamiento(),checkAnovaTurkey(),
                                 checkAnovaBonferroni(),table_calculo_Anova()[[4]] )
      
    })
    
    table_calculo_Anova_PostHoc_Desiguales <- reactive({
      
      calculoPostHocAnovaDesiguales(session,r$valuesmydata, df_Anova_Seleccion_Dependiente(),
                                    df_Anova_Seleccion_Agrupamiento(),
                                    checkAnovaGamesHowell(),table_calculo_Anova()[[4]]  )
      
    })
    
    table_calculo_Anova_EfectosFijosyAleatorios <- reactive({ 
      
      calculoEfectosFijosyAleatorios( session,r$valuesmydata,  df_Anova_Seleccion_Dependiente(),  
                                      df_Anova_Seleccion_Agrupamiento(),  checkAnovaEfectos()   )
      
    })
    
    table_calculo_Anova_Suposiciones_VarianzaIgual <- reactive({
      
      #Reuza Suposicion de TTest
    calculo_Ttest_Suposiciones_VarianzaIgual(session, r$valuesmydata, df_Anova_Seleccion_Dependiente(),
                                               df_Anova_Seleccion_Agrupamiento(),
                                               checkAnovaHomogeniedad())
      
      
    })
    
    table_calculo_Anova_Welch <- reactive({
      
      #Reuza Suposicion de TTest
      calculoTanovaWelch(session, r$valuesmydata, df_Anova_Seleccion_Dependiente(),
                         df_Anova_Seleccion_Agrupamiento(),
                         checkAnovaWelch())
      
      
    })
    
    table_calculo_Anova_Brown <- reactive({
      
      #Reuza Suposicion de TTest
      calculoTanovaBrown(session, r$valuesmydata, df_Anova_Seleccion_Dependiente(),
                         df_Anova_Seleccion_Agrupamiento(),
                         checkAnovaBrown())
      
      
    })
    ##################################### TABLAS DE ANOVA ###############
    
    output$tablaAnova <- gt::render_gt({
      
      descargaTablasAnova$tabla_Tanova <- apaAnova(table_calculo_Anova()[[1]],
                                                   "Prueba Anova",
                                                   table_calculo_Anova()[[2]],"anova_Tabla")
    })
    
    output$tablaAnovaDescriptivas <- gt::render_gt({
      
      descargaTablasAnova$tabla_TanovaDescriptivas <- apaAnova(table_calculo_Anova_Stadisticas()[[1]],
                                                               "Estadistica Anova",
                                                               table_calculo_Anova_Stadisticas()[[2]],"anova_Tabla_estadistica")
    })
    
    output$boxplotsAnova <-renderPlot({ 
      
      
      table_calculo_Anova_Stadisticas()[[3]]
      
      
    }, bg="transparent")
    
    output$tablaAnovaPostHocIguales <- gt::render_gt({
      
      descargaTablasAnova$tabla_Tanova_PostHoc_Iguales <- apaAnova(table_calculo_Anova_PostHoc_Iguales()[[1]],
                                                                   "Prueba Post Hoc Comparaci\u00f3n",
                                                                   table_calculo_Anova_PostHoc_Iguales()[[2]],"anova_Tabla_PostHocIguales")
    })
    
    output$tablaAnovaPostHocDesiguales <- gt::render_gt({
      
      descargaTablasAnova$tabla_Tanova_PostHoc_Desiguales <- apaAnova(table_calculo_Anova_PostHoc_Desiguales()[[1]],
                                                                      "Prueba Post Hoc de Games-Howell",
                                                                      table_calculo_Anova_PostHoc_Desiguales()[[2]],"anova_Tabla_PostHocDesigual")
    })
    
    output$tablaAnovaHomogeniedad <- gt::render_gt({
      
      descargaTablasAnova$tabla_Tanova_Homogeniedad <-  apaAnova(table_calculo_Anova_Suposiciones_VarianzaIgual()[[1]],
                                                                 "Prueba de Igualdad de Varianza (Levene Test)",
                                                                 table_calculo_Anova_Suposiciones_VarianzaIgual()[[2]],"anova_Tabla_Homogenidad")
      
    } )
    
    output$tablaAnovaWelch <- gt::render_gt({
      
      descargaTablasAnova$tabla_TanovaWelch <- apaAnova(table_calculo_Anova_Welch()[[1]],
                                                        "Prueba Anova Welch",
                                                        table_calculo_Anova()[[2]],"anova_Tabla_Welch")
    })
    
    output$tablaAnovaBrown <- gt::render_gt({
      
      descargaTablasAnova$tabla_TanovaBrown <- apaAnova(table_calculo_Anova_Brown()[[1]],
                                                        "Prueba Anova Brown",
                                                        table_calculo_Anova_Brown()[[2]],"anova_Tabla_Brown")
    })
    
    
    descargaUInameAnova <- c("descargaAnova","descargaAnovaDescriptivas","descargaAnovaPostHocIguales",
                             "descargaAnovaPostHocDesiguales","descargaAnovaHomogeniedad","descargaAnovaWelch",
                             "descargaAnovaBrown"
    )
    
    uiAnovaRadio  <- c("uiRadioAnova","uiRadioAnovaDescriptivas", "uiRadioAnovaPostHocIguales",
                       "uiRadioAnovaPostHocDesiguales","uiRadioAnovaHomogeniedad","uiAnovaRadioWelch",
                       "uiRadioAnovaBrown")
    
    nombregtWidgetAnova  <- c("tablaAnova","tablaAnovaDescriptivas","tablaAnovaPostHocIguales",
                              "tablaAnovaPostHocDesiguales","tablaAnovaHomogeniedad","tablaAnovaWelch",
                              "tablaAnovaBrown"  )
    
    lapply(1:7, function(i) {
      
      shinyjs::onevent("dblclick",nombregtWidgetAnova[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                                                                                 html = TRUE, text = tagList( radioButtons(ns(uiAnovaRadio[i]), "", 
                                                                                                                           choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                                                                                                              downloadButton(ns(descargaUInameAnova[i]),"Descargar" ))))
    })
    
    
    output$descargaAnovaBrown <- downloadHandler (
      
      filename = function() {
        paste0("anovaBrown", input$uiRadioAnovaBrown)
      },
      
      content = function(file) {
        if(input$uiRadioAnovaBrown == ".rtf") {
          descargaTablasAnova$tabla_TanovaBrown %>% gt::gtsave("anovaBrown.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovaBrown.rtf"), file )
        } else if (input$uiRadioAnovaBrown == ".tex") {
          descargaTablasAnova$tabla_TanovaBrown %>% gt::gtsave("anovaBrown.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovaBrown.tex"), file )
          
        }}
    )
    
    
    
    
    
    
    
    output$descargaAnovaWelch <- downloadHandler (
      
      filename = function() {
        paste0("anovaWelch", input$uiAnovaRadioWelch)
      },
      
      content = function(file) {
        if(input$uiAnovaRadioWelch == ".rtf") {
          descargaTablasAnova$tabla_TanovaWelch %>% gt::gtsave("anovaWelch.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovaWelch.rtf"), file )
        } else if (input$uiAnovaRadioWelch == ".tex") {
          descargaTablasAnova$tabla_TanovaWelch %>% gt::gtsave("anovaWelch.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovaWelch.tex"), file )
          
        }}
    )
    
    
    
    
    
    output$descargaAnovaHomogeniedad <- downloadHandler (
      
      filename = function() {
        paste0("anovahomog", input$uiRadioAnovaHomogeniedad)
      },
      
      content = function(file) {
        if(input$uiRadioAnovaHomogeniedad == ".rtf") {
          descargaTablasAnova$tabla_Tanova_Homogeniedad %>% gt::gtsave("anovahomog.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovahomog.rtf"), file )
        } else if (input$uiRadioAnovaHomogeniedad == ".tex") {
          descargaTablasAnova$tabla_Tanova_Homogeniedad %>% gt::gtsave("anovahomog.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovahomog.tex"), file )
          
        }}
    )
    
    
    output$descargaAnovaPostHocDesiguales <- downloadHandler (
      
      filename = function() {
        paste0("anovaPosthoc2", input$uiRadioAnovaPostHocDesiguales)
      },
      
      content = function(file) {
        if(input$uiRadioAnovaPostHocDesiguales == ".rtf") {
          descargaTablasAnova$tabla_Tanova_PostHoc_Desiguales %>% gt::gtsave("anovaPosthoc2.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovaPosthoc2.rtf"), file )
        } else if (input$uiRadioAnovaPostHocDesiguales == ".tex") {
          descargaTablasAnova$tabla_Tanova_PostHoc_Desiguales %>% gt::gtsave("anovaPosthoc2.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovaPosthoc2.tex"), file )
          
        }}
    )
    
    
    
    output$descargaAnovaPostHocIguales <- downloadHandler (
      
      filename = function() {
        paste0("anovaPosthoc1", input$uiRadioAnovaPostHocIguales)
      },
      
      content = function(file) {
        if(input$uiRadioAnovaPostHocIguales == ".rtf") {
          descargaTablasAnova$tabla_Tanova_PostHoc_Iguales %>% gt::gtsave("anovaPosthoc1.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovaPosthoc1.rtf"), file )
        } else if (input$uiRadioAnovaPostHocIguales == ".tex") {
          descargaTablasAnova$tabla_Tanova_PostHoc_Iguales %>% gt::gtsave("anovaPosthoc1.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovaPosthoc1.tex"), file )
          
        }}
    )
    
    
    
    output$descargaAnova <- downloadHandler (
      
      filename = function() {
        paste0("anova", input$uiRadioAnova)
      },
      
      content = function(file) {
        if(input$uiRadioAnova == ".rtf") {
          descargaTablasAnova$tabla_Tanova %>% gt::gtsave("anova.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/anova.rtf"), file )
        } else if (input$uiRadioAnova == ".tex") {
          descargaTablasAnova$tabla_Tanova %>% gt::gtsave("anova.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/anova.tex"), file )
          
        }}
    )
    
    output$descargaAnovaDescriptivas <- downloadHandler (
      
      filename = function() {
        paste0("anovaDescriptiva", input$uiRadioAnovaDescriptivas)
      },
      
      content = function(file) {
        if(input$uiRadioAnovaDescriptivas == ".rtf") {
          descargaTablasAnova$tabla_TanovaDescriptivas %>% gt::gtsave("anovaDescriptiva.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovaDescriptiva.rtf"), file )
        } else if (input$uiRadioAnovaDescriptivas == ".tex") {
          descargaTablasAnova$tabla_TanovaDescriptivas %>% gt::gtsave("anovaDescriptiva.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/anovaDescriptiva.tex"), file )
          
        }}
    )
    
    
    
    
    
    
    
 ########################### SELECTORES DE ANOVA ##############
    
    
    df_Anova_Seleccion_Agrupamiento <- reactive({
      
      req(input$rank_list_Anova_Agrupamiento)
      df_Anova_Seleccion_Agrupamiento<- r$valuesmydata %>% dplyr::select(input$rank_list_Anova_Agrupamiento)
      
    })
    
    df_Anova_Seleccion_Dependiente <- reactive({
      
      req(input$rank_list_Anova_Dependiente)
      df_Anova_Seleccion_Dependiente<- r$valuesmydata %>% dplyr::select(input$rank_list_Anova_Dependiente)
      
    })
    
    output$bucket_1_Anova <- renderUI({
      
      sortable::bucket_list(
        header =  HTML(as.character(  tags$div(tags$span(strong("Entrada"),style = "color: black;")) )),    
        group_name = "bucket_list_Anova",
        orientation = "horizontal",
        class = c("default-sortable", "custom-sortable"), # add custom style
        
        sortable::add_rank_list(
          text = "",
          labels = namesClasified(),
          input_id = ns("rank_list_Anova_Entrada"),
          css_id = "rank_Anova_Entrada"
          
        ))
    })
    
    
    if (banderaAnova==FALSE) {
      banderaAnova=TRUE
      
      insertUI(
        selector=paste0("#",ns("placeholderAnovaDependiente")),
        where="beforeBegin",
        ui = tags$div(id = ns("anovaBoxDependiente"),
                      sortable::bucket_list(
                        header = HTML(as.character(  tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                              tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                        group_name = "bucket_list_Anova",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_Anova_Dependiente"),
                          css_id = "rank_Anova_Dependiente"
                        ))
        ))
      
      insertUI(
        selector= paste0("#",ns("placeholderAnovaAgrupamiento")),
        where="beforeBegin",
        ui = tags$div(id = ns("anovaBoxAgrupamiento"),
                      sortable::bucket_list(
                        header =  HTML(as.character(  tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                               tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                        
                        group_name = "bucket_list_Anova",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                        
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_Anova_Agrupamiento"),
                          css_id = "rank_Anova_Agrupamiento"
                        ))
        ))
      
    }
    
    
    observeEvent( r$inputGuardar,{
      
      golem::print_dev("ANOVA ")
      
      
      removeUI(
        selector =  paste0("#",ns("anovaBoxDependiente"))
      )
      
      removeUI(
        selector =  paste0("#",ns("anovaBoxAgrupamiento"))
      )
      
      
      
      insertUI(selector=paste0("#",ns("placeholderAnovaDependiente")),
               where='beforeBegin',
               ui =  tags$div(id =ns("anovaBoxDependiente"),
                              sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Variables : '),style = paste0('color: black;')),
                                                                                          tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),
                                    group_name = "bucket_list_Anova",
                                    orientation = 'horizontal',
                                    class = c('default-sortable', "custom-sortableDependiente"),
                                    sortable::add_rank_list(text = '',labels = NULL,
                                                            input_id =ns("rank_list_Anova_Dependiente"),
                                                            css_id ="rank_Anova_Dependiente "))))
      
      insertUI(selector=paste0("#",ns("placeholderAnovaAgrupamiento")),
               where='beforeBegin',
               ui =  tags$div(id =ns("anovaBoxAgrupamiento"),
                              sortable::bucket_list(header = HTML(as.character(  tags$div(tags$span(strong('Variable de Agrupaci\u00f3n : '),style = paste0('color: black;')),
                                                                                          tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),
                                    group_name = "bucket_list_Anova",
                                    orientation = 'horizontal',
                                    class = c('default-sortable', "custom-sortableAgrupamiento"),
                                    sortable::add_rank_list(text = '',labels = NULL,
                                    input_id = ns("rank_list_Anova_Agrupamiento"),
                                    css_id ="rank_Anova_Agrupamiento"))))
      
      
      
    })
    
    
    
    
    
    
    
    
    
    
    
    })
}
    
## To be copied in the UI
# mod_Anova_ui("Anova_ui_1")
    
## To be copied in the server
# mod_Anova_server("Anova_ui_1")

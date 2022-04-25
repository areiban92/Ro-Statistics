#' cargaDatos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cargaDatos_ui <- function(id){
  ns <- NS(id)

    
    navbarMenu( div( id=ns("textoIconos"),
                     tags$img(src ='www/iconos2/guardar.png', style= "width: 50px; margin-top: 15px; margin-bottom: 0px; height: 30px"),
                     "  "
    ),

    ############################# CARGA DE DATOS ######################
    tabPanel("Carga de Datos   ",value = "Tab 1",
             sidebarPanel( width = 3,
                           
                           fluidRow(
                             tags$head(
                               tags$style(HTML("#checkboxDescripcionVariable  {max-height: 200px}"))
                               #                        #tags$style(HTML(".bucket-list-container {min-height: 50px;height: 200px;},rank-list-item"))
                             ),
                             fileInput(ns('file'), 'Eliga la Base de Datos ',
                                       accept= c('application/vnd.ms-excel',
                                                 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                                 '.xls',
                                                 '.xlsx','.csv','.sav'))
                           ), # fin fluid row1
                           
                           fluidRow(
                             
                             uiOutput(ns("checkboxDescripcionVariable"))
                           ), # fin fluid row1
                           fluidRow(
                             actionButton(ns("guardar"), "Guardar")
                           ),#fin del row 2
                           fluidRow(
                             #uiOutput("unique")
                             verbatimTextOutput(ns("unique"))
                             
                           )# fin fluid row3
                           
                           
                           
             ),
             mainPanel(width = 9,
 
                       tabsetPanel(type = "tabs",   
                                   tabPanel("Tabla",  
                                            DT::dataTableOutput(ns('tablaSeleccion')),
                                            
                                            div(style = 'width:100%;margin:auto',DT::dataTableOutput(ns('tablaPrincipal') )),
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
    ),
    
    ################ ACERCA DE RO ###########
    
    tabPanel("Acerca de Ro - Statistics ", value = "Acerca de Ro-Statistics",
             
             
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
                             
                             div( style="display: inline-block;vertical-align:top; font-size: 16px;", "1.0.4  **Beta 3"),
                             br(),  
        
                             div( style="display: inline-block;vertical-align:top; font-size: 16px;","25 de marzo de 2022  " ),
                             br(),
                             
                             div( style="display: inline-block;vertical-align:top; font-size: 16px; ",a("https://rostatictictest.shinyapps.io/RoStatistics/ ", href="https://rostatictictest.shinyapps.io/Ucacue/ ") ),
                             br(),
                             div( style="display: inline-block;vertical-align:top; font-size: 16px; "," RO-StatistiCs 1.0.4 (2022). RO-Statistics (version 1.0.4) [Computer software].  "),
                      ), #Finde columa2
                      
                      
               ) #Finde columna de texto
             ), #Fin de fluidrow
             div(   style="display: inline-block;vertical-align:top; padding-top: 300px;", p("Este programa se proporciona TAL CUAL, 
                                   SIN GARANTÍA DE NINGÚN TIPO, INCLUYENDO LA GARANTÍA DE DISEÑO, 
                                   COMERCIABILIDAD Y APTITUD PARA UN PROP\u00d3SITO PARTICULAR"))
    ),
    
    ######################## AYUDA UI  ##################
    
    tabPanel(div(  id=ns("textoIconos"),
                   
                 #  div( id= ns("imgIconos"),
                 #       tags$img(src='iconos2/info.png', style= "width: 50px; margin-top: 0px; margin-bottom: 5px;  height: 30px")),
                   div( 
                     
                     "InformaciI\u00f3n" ) ,
    ),value = "Tab 6",
    sidebarPanel( width = 6, 
                  
                  radioButtons(inputId = ns("ayudaMenu"), 
                               label = "Tabs", 
                               choices = c("Descriptivos","T-Test","Contingencia","ANOVA","Regresi\u00f3n")
                  )
                  
                  
    ),
    mainPanel(width = 6, 
              
              h1("AYUDA"),
              conditionalPanel("input.ayudaMenu == 'Descriptivos'", ns=ns, uiOutput(ns('descriptivos_ayuda'))),
              conditionalPanel("input.ayudaMenu == 'T-Test'", ns=ns, uiOutput(ns('ttest_ayuda'))),
              conditionalPanel("input.ayudaMenu == 'Correlaci\u00f3n'",ns=ns, uiOutput(ns('correlacion_ayuda'))),
              conditionalPanel("input.ayudaMenu == 'Contingencia'",ns=ns, uiOutput(ns('contingencia_ayuda'))),
    )
    )
    
   
    
    
    ) #Fin de navbar
 
  
}
    
#' cargaDatos Server Functions
#'
#' @noRd 
mod_cargaDatos_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    ################# CARGA DE DATOS#############
    dfEntradaNombres <- reactiveValues(original = 0, conIconos="",updated=NULL)
    tipoVariable <- reactiveValues(select=NULL)
    
    values <- reactiveValues(mydata = NULL,myplot=NULL)
    dfEntradaValues = reactiveValues(original = NULL,conIConos=NULL,transformado=NULL)
    banderas= reactiveValues(dfCargado=0, update=0)
    
    data_file <- reactive({
      
      req(input$file)
      if(is.null(input$file)){return()}
      else{
        
        
        
        dfEntradaNombres$original <-  NULL
        dfEntradaNombres$conIConos <- NULL
        session$sendCustomMessage('unbinding_table_elements', 'tablaSeleccion')
        
        defaultSelect <- NULL
        namesTablaFinal <- NULL
        
        file_spec <- input$file
        golem::print_dev("TIPO DE ARCHIVOOOOO")
        golem::print_dev(tools::file_ext(file_spec$datapath))
        
        if(tools::file_ext(file_spec$datapath)!="csv" 
           & tools::file_ext(file_spec$datapath) !="xlsx" & tools::file_ext(file_spec$datapath)!="xls" 
           & tools::file_ext(file_spec$datapath)!="sav" & tools::file_ext(file_spec$datapath)!="SAV"  ){
          
          golem::print_dev("Formato NO aceptado ")
          aa <- data.frame(empty=c("vacio","vacio"))
          
          shinyalert::shinyalert(
            title = "Error",
            text = "Archivo No compatible, cargue un base de datos compatible\n Archivos soportados: xlsx,xls,sav,csv",
            size = "s",
            closeOnEsc = TRUE,
            closeOnClickOutside = FALSE,
            html = FALSE,
            type = "success",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            
            imageUrl = "",
            animation = TRUE
          )
          
        }
        
        if(tools::file_ext(file_spec$datapath)=="csv"){
          
          aa <-  utils::read.csv(file_spec$datapath)
          
        }
        if(tools::file_ext(file_spec$datapath)=="xlsx" || tools::file_ext(file_spec$datapath)=="xls"){
          
          bb <- readxl::read_excel(file_spec$datapath)
          golem::print_dev("hasta aqui ok")
          aa <- as.data.frame(bb)
          
        }
        if(tools::file_ext(file_spec$datapath)=="sav" || tools::file_ext(file_spec$datapath)=="SAV" ){
          
          aa  <- foreign::read.spss(file_spec$datapath, use.value.label=TRUE, to.data.frame=TRUE)
          aa %>% dplyr::mutate(dplyr::across(where(is.factor), as.character)) -> aa
        }
        golem::print_dev("antessss")
        #aa[aa == ""] <- NA
        #golem::print_dev(aa)
        golem::print_dev("3333333333333333333333333333333333333333333333")
        aa <- make_factors(aa,15,6)
        #golem::print_dev(str(bb))
        #golem::print_dev( (sapply(bb, class))  )
        golem::print_dev("3333333333333333333333333333333333333333333333")
        defaultSelect <- (sapply(aa, class))
        #golem::print_dev(defaultSelect)
        namesTablaFinal=list()
        namesTablaFinalAY = list()
        namesTable <-  colnames(aa)
        dfEntradaNombres$original <-namesTable
        tipoSelect <- vector(mode = "character", length = 0)
        
        for(i in 1:length(names(aa))){
          
          
          if(defaultSelect[[i]]=="POSIXct" || defaultSelect[[i]]=="POSIXt" )
          {
            tipoSelect[i] <- as.character(selectInput(inputId= ns(paste0("tipoDatosEntrada_", i)), label=NULL, choices=c("Escalar","Ordinal","Nominal"),selected="Escalar",width = "100px"))
            namesTablaFinal[[i]]=HTML(  as.character(tags$div(tags$span(class = "fa-stack",icon("ruler","fa-stack-1x",lib = "font-awesome"),style="color:#6d94de"),
                                                              tags$span(namesTable[i],style = paste0('color: gray;'))) ))
          }
          
          if(defaultSelect[[i]][1]=="numeric" || defaultSelect[[i]]=="integer")
          {
            tipoSelect[i] <- as.character(selectInput(inputId = ns(paste0("tipoDatosEntrada_", i)), label=NULL, choices=c("Escalar","Ordinal","Nominal"),selected="Escalar",width = "100px"))
            namesTablaFinal[[i]]= HTML(as.character(tags$div(tags$span(class = "fa-stack",icon("ruler","fa-stack-1x",lib = "font-awesome"),style="color:#6d94de"),
                                                             tags$span(namesTable[i],style = paste0('color: gray;'))) ))
            
            
          }
          
          if(defaultSelect[[i]][1] =="factor")
          {
            
            tipoSelect[i] <- as.character(selectInput(inputId = ns( paste0("tipoDatosEntrada_", i)), label=NULL, choices=c("Escalar","Ordinal","Nominal"),selected="Nominal",width = "100px"))
            namesTablaFinal[[i]]= HTML( as.character(tags$div(tags$span(class = "fa-stack",icon("chart-bar","fa-stack-1x", lib = "font-awesome"),style="color:#a22f2f"),
                                                              tags$span(namesTable[i],style = paste0('color: gray;')))) )
            
          }
          
          
          if(defaultSelect[[i]][1]=="ordered")
          {
            
            tipoSelect[i] <- as.character(selectInput(inputId =  ns(paste0("tipoDatosEntrada_", i)), label=NULL, choices=c("Escalar","Ordinal","Nominal"),selected="Ordinal",width = "100px"))
            namesTablaFinal[[i]]= HTML( as.character(tags$div(tags$span(class = "fa-stack",icon("signal","fa-stack-lg", lib = "font-awesome"),style="color:#a22f2f"),
                                                              tags$span(namesTable[i],style = paste0('color: gray;')))) )
            
          }
          
          
        }
        
      }
      bb <- data.frame(t(tipoSelect))
      colnames(bb) <- colnames(aa)
      
      tipoVariable$select <- bb
      
      #colnames(aa) <- namesTablaFinal 
      #     df = rbind(tipoSelect, aa)
      dfEntradaNombres$conIconos <- namesTablaFinal
      #    golem::print_dev(dfEntradaNombres$conIconos)
      #    golem::print_dev(class(df))
      dfEntradaValues$transformado <- aa
      
      #  golem::print_dev(colnames(df))
      
      
      return (aa)
      
    })  #Fin de Carga Archivo XLSX
    
    #### Coloca el icno de acuerdo a la variable 
    namesClasified <- reactive({
      
      if(length(names(data_file())) == 0 ) {return()} 
      else {
        
        golem::print_dev("Entro en names CLASIFIEDDDDDDDDDDDDDDD")
        
        dfEntradaNombres$conIconos
        
      }
      
    })
    
    ########CLASES PARA MOSTRAR Y EDTAR LA TABLA PRINCIPAL #################
    
    output$tablaPrincipal <- DT::renderDT({ 
      
      DT::datatable( dfEntradaValues$original, editable = TRUE,rownames= FALSE, escape = FALSE,options = list(pageLength = 10, 
                                                                                                              width="100%", scrollX = TRUE, scroller = FALSE
                                                                                                              
      ) ) # set height of rows
      
    })
    
    output$tablaSeleccion <- DT::renderDT({ 
      
      DT::datatable( tipoVariable$select,editable = TRUE, rownames= FALSE, escape = FALSE,options = list(dom = 't', 
                                                                                                         width="100%", scrollX = TRUE,autoWidth = TRUE,
                                                                                                         
                                                                                                         
                                                                                                         drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')                         
      ) )})
    
    ###################
    
    # observeEvent(  isolate(banderas$update),{
    #   
    # 
    # 
    #   if(banderas$update){
    # 
    #     golem::print_dev("se actuliazho")
    #   }
    #   else {
    # 
    #     golem::print_dev("nop se actualizho")
    #   }
    # 
    # 
    # 
    # })
    observe({
      
      
      
      if( !banderas$update){
        golem::print_dev("no es update")
        #Inicializa para carga al inicio los valores
        dfEntradaNombres$conIconos
        dfconIconos <- data_file()
        colnames(dfconIconos) <- dfEntradaNombres$conIconos
        dfEntradaValues$original <- dfconIconos
        
      }
      
      
      if( banderas$update){
        
        
        golem::print_dev("Si es update") 
        dfEntradaValues$original <- values$mydata
        #  banderas$update <- FALSE
        #colnames(dfEntradaValues$original) <- dfEntradaNombres$conIconos
        
      } 
      
      
      
      
    })
    
    
    
    observe({
      
      
      req(banderas$dfCargado)
      
      if(banderas$dfCargado){
        
        golem::print_dev("bandera es 1")
        golem::print_dev("ENTRO EN GUARDAR 22222 ")
        values$mydata <- NULL
        mylist <- list()
        golem::print_dev(length(names(data_file())))
        golem::print_dev(utils::str(dfEntradaValues$transformado))
        
        for(i in 1:length(colnames(data_file()))){
          
          
          golem::print_dev(paste0("La variable es : " ,input[[paste0("tipoDatosEntrada_", i)]]))
          
          golem::print_dev(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])
          
          #Check si la Variables es Escalar Guarda esa Columna como numerica
          if(input[[paste0("tipoDatosEntrada_", i)]]=='Escalar'){
            
            
            # assign(paste0("muted_", dfEntradaNombres$original[i]  ),as.numeric(dfEntradaValues$transformado[,dfEntradaNombres$original[i]]))
            mylist[[i]] <- as.numeric(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])
          }
          
          if(input[[paste0("tipoDatosEntrada_", i)]]=="Ordinal"){
            golem::print_dev("entro en ordinal")
            
            levelss <-  (sort(unique(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])))
            #  assign(paste0("muted_",dfEntradaNombres$original[i]),factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered = TRUE,levels=levelss))
            mylist[[i]] <-  factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered = TRUE,levels=levelss)
            
            
          }
          
          if(input[[paste0("tipoDatosEntrada_", i)]]=="Nominal"){
            
            golem::print_dev("Entro en nomialll")
            # assign(paste0("muted_",dfEntradaNombres$original[i]),factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]]))
            mylist[[i]] <- factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered = FALSE)
            
          }
          
          #  mylist[[i]] <- get(paste0("muted_",dfEntradaNombres$original[i]))
          
          
        }
        golem::print_dev(utils::str(mylist))
        
        values$mydata <- do.call(rbind.data.frame, args = list(mylist, stringsAsFactors=TRUE) )
        shinyalert::shinyalert(
          title = " ",
          text = "Valores Guardados",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "success",
          showConfirmButton = FALSE,
          showCancelButton = FALSE,
          timer = 1000,
          imageUrl = "",
          animation = TRUE
        )
        names(values$mydata) <- names(data_file())
        golem::print_dev("Todo ok hasta aqui5555")
        golem::print_dev(utils::str(values$mydata))
        #   session$sendCustomMessage('unbind-DT', 'tablaPrincipal')
        banderas$dfCargado <- 0
        
        banderas$update=1
        r$valuesmydata <- values$mydata
        
        
      }else {golem::print_dev("Bandera es 0")}
      
      
      
    })
    
    
    
    
    proxy = DT::dataTableProxy('tablaPrincipal')
    
    observeEvent(input$tablaPrincipal_cell_edit, {
      info = input$tablaPrincipal_cell_edit
      #utils::str(info)
      i = info$row
      j = info$col + 1
      v = info$value
      
      # problem starts here
      dfEntradaValues$original[i, j] <- isolate(DT::coerceValue(v, dfEntradaValues$original[i, j]))
      DT::replaceData(proxy, dfEntradaValues$original, resetPaging = FALSE,rownames = FALSE)  # replaces data displayed by the updated table
      dfEntradaValues$transformado <- dfEntradaValues$original
      colnames(dfEntradaValues$transformado) <- dfEntradaNombres$original
      golem::print_dev(utils::str(dfEntradaValues$transformado))
      golem::print_dev("TRANSFORMADOOOOOOOOOOOOOOO")
      banderas$dfCargado=1
      
      
    })
    
    
    #tablaTipo <- reactive({DeteccionTipoVariable(data_file)}) 
    
    
    output$unique <- renderPrint(
      if(is.null(input$select_var))
      { return(
        #utils::str("Vacio")
      ) }
      else{
        req(input$select_var)
        req(dfEntradaValues$transformado)
        
        golem::print_dev( sort(unique(data_file()[-1,input$select_var]) ))
        #sort(unique(dfEntradaValues$transformado[,input$select_var]) )
        
      }
      ,outputArgs = list())
    
    
    
    output$checkboxDescripcionVariable <- renderUI({
      
      radioButtons(inputId = ns("select_var"),
                   label = "Variables En El Dataset",
                   inline = FALSE,
                   
                   choices = if(is.null(dfEntradaNombres$original)){return()}
                   else{(dfEntradaNombres$original)})
    })
    
    
    ######## METODO PARA REACTIVAR LOS CUADROS ########### 
    observeEvent(input$guardar,{
      
      updateActionButton(session, "guardar",
                         label = "Datos Guardados",
                         icon = icon("check"))
      
      golem::print_dev("ENTRO EN GUARDAR 22222 ")
      values$mydata <- NULL
      mylist <- list()
      namesTablaActualizada=list()
      golem::print_dev(length(names(data_file())))
      golem::print_dev(utils::str(dfEntradaValues$transformado))
      values$mydata <- dfEntradaValues$transformado
      
      for(i in 1:length(colnames(data_file()))){
        
        
        golem::print_dev(paste0("La variable es : " ,input[[paste0("tipoDatosEntrada_", i)]]))
        
        #Check si la Variables es Escalar Guarda esa Columna como numerica
        if(input[[paste0("tipoDatosEntrada_", i)]]=='Escalar'){
          
          values$mydata[,i] <- as.numeric(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])
          namesTablaActualizada[[i]]= HTML(as.character(tags$div(tags$span(class = "fa-stack",icon("ruler","fa-stack-1x",lib = "font-awesome"),style="color:#6d94de"),
                                                                 tags$span(dfEntradaNombres$original[i],style = paste0('color: gray;'))) ))
        }
        
        if(input[[paste0("tipoDatosEntrada_", i)]]=="Ordinal"){
          golem::print_dev("entro en ordinal")
          
          levelss <-  ((unique(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])))
          
          values$mydata[,i] <-  factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered = TRUE,levels=levelss)
          
          namesTablaActualizada[[i]]= HTML( as.character(tags$div(tags$span(class = "fa-stack",icon("signal","fa-stack-lg", lib = "font-awesome"),style="color:#a22f2f"),
                                                                  tags$span(dfEntradaNombres$original[i],style = paste0('color: gray;')))) )
        }
        
        if(input[[paste0("tipoDatosEntrada_", i)]]=="Nominal"){
          
          golem::print_dev("Entro en nomimal")
          
          values$mydata[,i] <- factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered=FALSE)
          namesTablaActualizada[[i]]= HTML( as.character(tags$div(tags$span(class = "fa-stack",icon("chart-bar","fa-stack-1x", lib = "font-awesome"),style="color:#780b0b"),
                                                                  tags$span(dfEntradaNombres$original[i],style = paste0('color: gray;')))) )
          
        }
        
        
      }
      golem::print_dev(utils::str(values$mydata))
      
      
      
      shinyalert::shinyalert(
        title = " ",
        text = "Valores Guardados",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "success",
        showConfirmButton = FALSE,
        showCancelButton = FALSE,
        timer = 1000,
        imageUrl = "",
        animation = TRUE
      )
      names(values$mydata) <- names(data_file())
      golem::print_dev("Todo ok hasta aqui2222222")
      golem::print_dev(utils::str(values$mydata))
      
      dfEntradaNombres$conIconos <- namesTablaActualizada
      print("VALORESSSSSSSSSSSSSSSS")
      
      r$inputGuardar = input$guardar
      #print(r$inputGuardar)
      r$valuesmydata <- values$mydata
      #print(r$valuesmydata)
      
      
    })
    
    
    ###################### AYUDA SERVER #############
    
    
    output$descriptivos_ayuda <- renderUI({
      
      tagList(
        
        h2("Descriptivas"),
        p(" Las pruebas estadísticas utilizadas para validar 
                     la efectividad de un estudio se destacan según la 
                     calidad de los datos y el prop\u00f3sito de estudio (Betanzos, 2017). 
                     Es decir, la estadística es coadyuvante con la forma de 
                     obtener, resumir, y analizar informaci\u00f3n (Gonz\u00e1lez, 2017). "),
        h6("Tendencia Central"),
        p(" • Media: Es el resultado de dividir la suma de todas las 
                   observaciones entre el número de ellas (Castro, 2018)"),
        #p(" Formula:"),
        
        # withMathJax(helpText('$$ bar(x) = sum((n_i * X_i)/(N)) $$'))
        withMathJax(
          helpText("$$ \\bar{x}=\\sum \\frac{ x_i}{N}  $$ ")),
        
        p(" • Mediana: Es el punto que divide a la muestra en dos partes iguales. 
    Para calcular la mediana es importante que los datos estén ordenados de mayor
    a menor, o al contrario de menor a mayor."),
        withMathJax(
          helpText(" $$   mediana = \\frac{n +1}{2} $$ ")),
        
        br(),
        
        p(" • Moda: Es el valor que m\u00e1s se repite, ser\u00e1 pues el valor o valores 
    cuya frecuencia absoluta sea la mayor de las observadas, no tiene formula en si
    (Salazar & Castillo, 2018).      "),
        
        br(), 
        p(" • Quartil: Los cuartiles son medidas estadísticas de posici\u00f3n que tienen la 
    propiedad de dividir la serie estadística en cuatro grupos de números 
    iguales de términos (S\u00e1nchez, 2007).      "),
        br(),
        p(" • MAD: Es una medida que se utiliza para calcular cu\u00e1nto varían de su 
    media los valores de un conjunto de datos (Molina, 2014).      "),
        withMathJax(
          helpText(" $$  MAD=\\frac{\\sum_{i-1}^{g}|x_{i}-\\overline{x}|f_{i}}{n}    $$ ")),
        
        p(style="text-align: center;","Donde: "),
        p(style="text-align: center;"," \\(g\\) = número de intervalos o clases de la distribuci\u00f3n de frecuencia."),
        p(style="text-align: center;"," \\(x_{i}\\) = punto medio de la i- ésima clase o marca de clase de cada intervalo."),
        p(style="text-align: center;","  \\(f_{i}\\) =número de	observaciones	clasificadas en	cada clase o intervalo (frecuencia absoluta)."),
        
        br(), 
        p(" • IQR: Consiste en la diferencia entre el tercer y el primer cuartil (Molina, 2014).     "),
        withMathJax(
          helpText(" $$ IQR = Q_{3} - Q_{1} $$ ")),
        
        p(HTML(" <b> Referencia :  </b>")),
        p("Betanzos, F. G., de Le\u00f3n, M. del C. E. P., & L\u00f3pez, J. K. C. (2017). Estadística aplicada en Psicología y Ciencias de la salud. Manual Moderno"),
        br(),
        p(" • Rango: Es un valor numérico que sirve para manifestar la diferencia entre el valor m\u00e1ximo y el valor mínimo de una muestra 
    poblacional (Molina, 2014)."),
        
        withMathJax(
          helpText(" $$ Rango = max_{x} - min_{x} $$ ")),
        
        p( HTML( " <b> Medidas de dispersi\u00f3n:  </b>")),
        p(" • M\u00e1ximo: Devuelve el valor m\u00e1ximo de una lista de valores (\u00c1lvarez, 2007)."), 
        p(" • Mínimo: Devuelve el valor mínimo de una lista de valores (\u00c1lvarez, 2007). "),
        p(" • Desviaci\u00f3n media: es el promedio de los valores absolutos con respecto a la media aritmética (Salazar et al; 2017).  "),
        p(" • Desviaci\u00f3n Est\u00e1ndar: raíz cuadrada de la media aritmética de las desviaciones cuadr\u00e1ticas con relaci\u00f3n 
  a la media (Salazar et al; 2017). "),
        p(" • Varianza: es el cuadrado de la desviaci\u00f3n est\u00e1ndar (Salazar et al; 2017). "),
        p(" • Coeficiente de variaci\u00f3n: es la raz\u00f3n porcentual entre la desviaci\u00f3n est\u00e1ndar y la media aritmética (Salazar et al; 2017). ")
        
        
        
      )
      
    })
    
    output$ttest_ayuda <- renderUI({ 
      
      tagList(
        h2("Prueba T-Test"),
        p(" La prueba 't' de Student es un tipo de estadística deductiva. 
    Se utiliza para determinar si hay una diferencia significativa 
    entre las medias de dos grupos (Janssen, 2005)."),
        h6(" Prueba t para muestras Agrupamientos  "),
        
        p("se refiere a la diferencia entre los promedios de dos poblaciones. 
    Un ejemplo sería comparar cuentas matem\u00e1ticas de un grupo 
    experimental con un grupo de control (Csörgő, 2014)."),
        
        h5("F\u00f3rmula de la distribuci\u00f3n de Student"),
        p("La f\u00f3rmula general para la T de Student es la siguiente: "),
        
        withMathJax(
          helpText("$$ t=\\frac{X-\\mu }{s/\\sqrt{n}} $$ ")),
        
        p(" Si pudiera expresar en un cierto número de pasos 
    para resolver un problema de t de student tendría 
    que declarar los siguientes: "),
        p(HTML(" <b> Paso 1.  </b> Plantear las hip\u00f3tesis nulas  \\(H_0\\)
         y la hip\u00f3tesis alternativa \\(H_1\\). La hip\u00f3tesis 
         alternativa plantea matem\u00e1ticamente lo que 
         queremos demostrar, en tanto que la hip\u00f3tesis 
         nula plantea exactamente lo contrario (Wang & Yang, 2016). ")),
        p(HTML(" <b> Paso 2. </b> Determinar el nivel de significancia 
         (rango de aceptaci\u00f3n de la hip\u00f3tesis alternativa).")),
        br(),
        p(" Se considera un nivel alfa de: 0.05 para proyectos de 
    investigaci\u00f3n; 0.01 para aseguramiento de la calidad; 
    y 0.10 para estudios o encuestas de mercadotecnia (Lachos et al., 2019).  "),
        
        p(HTML(" <b> Paso 3. </b> Evidencia muestral, se calcula 
         la media y la desviaci\u00f3n est\u00e1ndar a partir de la muestra.")),
        br(),
        
        p(HTML(" <b> Paso 4. </b>  Se aplica la distribuci\u00f3n 
         T de Student para calcular la probabilidad de error por 
         medio de la f\u00f3rmula general presentada al principio y se
         contrasta con el valor T obtenido de la tabla 
         correspondiente (Pangemanan, 2020).  ")),
        br(),
        
        p(HTML(" <b> Paso 5. </b>  En base a la evidencia disponible 
         se acepta o se rechaza la hip\u00f3tesis alternativa. 
         Si la probabilidad de error (p) es mayor que el nivel 
         de significancia se rechaza la hip\u00f3tesis alternativa. 
         Si la probabilidad de error (p) es menor que el nivel 
         de significancia se acepta la hip\u00f3tesis alternativa (Flores et al., 2017).    ")),
        
        
      )
      
    })
    
    
    output$contingencia_ayuda <- renderUI({
      
      tagList(
        h2("Tablas de contingencia"),
        p(" Una tabla de contingencia es una distribuci\u00f3n en filas y columnas en la
      que los individuos de una poblaci\u00f3n se clasifican en funci\u00f3n de variables.
      Éstas pueden ser de 2 x 2 o de n x n variable. Las filas y las columnas 
      pueden ser definidas de diferentes formas, según el tipo de diseño 
      estadístico (\u00c1lvarez, Pérez 2008). "),
        h3(" Chi-Cuadrado"),
        
        p("La X2 es una prueba de libre distribuci\u00f3n (no paramétrica)
    que mide la discrepancia entre una distribuci\u00f3n de frecuencias 
    observadas y esperadas. Dentro de sus características generales, 
    la prueba X2 toma valores entre cero e infinito y no tiene valores 
    negativos porque es la suma de valores elevados al cuadrado  (Mendivelso, Rodríguez 2018)."),
        
        p("Formula: "),
        
        withMathJax(
          helpText(" $$\\chi^{2} = \\sum \\frac{(f_o - f_t)^{2}}{f_t}$$ ")),
        
        h3("Prueba de Fisher "),
        
        p("Permite analizar la asociaci\u00f3n entre dos variables dicot\u00f3micas 
      cuando no se cumplen las condiciones necesarias para la aplicaci\u00f3n 
      (Molina,2021)."),
        
        p(" Fisher demostr\u00f3 que podía calcular la probabilidad de cualquiera 
      de las tablas de contingencia utilizando para ello la distribuci\u00f3n 
      de probabilidad hipergeométrica (Molina,2021)."),
        
        p("La prueba de Fisher calcula las probabilidades de todas las posibles 
      tablas y suma las de aquellas que tengan valores de p (Lobo,2020)."),
        
      )
      
      
      
      
    })
    
    output$correlacion_ayuda <- renderUI({ 
      
      tagList(
        h2("Tabla de Contingencia"),
        p("Es un tipo de asociaci\u00f3n entre dos variables numéricas, 
      específicamente evalúa la tendencia (creciente o decreciente) 
      en los datos (Weissbrod et al., 2018)."),
        h3("Pearson’s r"),
        p("Es una prueba que mide la relaci\u00f3n estadística entre dos variables 
      continuas. Si la asociaci\u00f3n entre los elementos no es lineal, 
      entonces el coeficiente no se encuentra representado adecuadamente 
      (Edelmann et al., 2021)."),
        p("Formula :"),
        
        withMathJax(
          helpText("$$ r_{xy} = \\frac{\\sum Z_xZ_y}{N} $$ ")),
        p(" 'x' es igual a la variable número uno, 'y' pertenece a la variable 
      número dos, 'zx' es la desviaci\u00f3n est\u00e1ndar de la variable uno, 'zy' 
      es la desviaci\u00f3n est\u00e1ndar de la variable dos y 'N' es en número de datos.  "),
        h3('Spearman’s '),
        
        p(" Es una medida de dependencia no paramétrica en la cual se calcula la 
      +jerarquía media de las observaciones, se hace el cuadrado a las diferencias 
      y se incorpora en la f\u00f3rmula (Rovetta, 2020). "),
        
        withMathJax(
          helpText("$$ r_s = 1-\\frac{6\\sum d_{i}}{n^{3}-n} $$ ")),
        
        h3("Kendall’s tau"),
        
        p("Mide el grado de asociaci\u00f3n entre varios conjuntos (k) de N entidades. 
    Es útil para determinar el grado de acuerdo entre varios jueces, o la 
    asociaci\u00f3n entre tres o m\u00e1s variables (Akoglu, 2018)."),
        
        
        p("Formula: "),
        
        
        
        
        
        
        
      )
    })
    

    return(

    namesClasified

    )

    



  })
}


## To be copied in the UI
# mod_cargaDatos_ui("cargaDatos_ui_1")
    
## To be copied in the server
# mod_cargaDatos_server("cargaDatos_ui_1")

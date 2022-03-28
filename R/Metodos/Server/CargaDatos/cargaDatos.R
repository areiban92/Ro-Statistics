
# removeIconTxt <- function(txts){
# 
#   golem::print_dev("BBNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB")
#   golem::print_dev(txts)
#     onlynames= c()
#   for(i in 1:length(txts)) {
#   
#   delante <- strsplit(txts[i], split =  'gray;\\">',fixed=FALSE)[[1]][2]
#   
#   onlynames[i] <- gsub("</span>\n</div>$","",delante)
#   golem::print_dev(onlynames[i])
#  
#   }
#    
#     golem::print_dev(onlynames)
#   return(onlynames)
# }

#Carga del Archivo XLSX
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
    
    if(tools::file_ext(file_spec$datapath)=="csv"){
      
      aa <-  utils::read.csv(file_spec$datapath)
      
    }
    if(tools::file_ext(file_spec$datapath)=="xlsx" || tools::file_ext(file_spec$datapath)=="xls"){
     
     bb <- readxl::read_excel(file_spec$datapath)
     aa <- as.data.frame(bb)
   }
    if(tools::file_ext(file_spec$datapath)=="sav"){
      
      aa  <- foreign::read.spss(file_spec$datapath, use.value.label=TRUE, to.data.frame=TRUE)
      aa %>% dplyr::mutate(dplyr::across(where(is.factor), as.character)) -> aa
    }
    
    aa[aa == ""] <- NA
  
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
       tipoSelect[i] <- as.character(selectInput(inputId=paste0("tipoDatosEntrada_", i), label=NULL, choices=c("Escalar","Ordinal","Nominal"),selected="Escalar",width = "100px"))
       namesTablaFinal[[i]]=HTML(  as.character(tags$div(tags$span(class = "fa-stack",icon("ruler","fa-stack-1x",lib = "font-awesome"),style="color:#6d94de"),
                                                  tags$span(namesTable[i],style = paste0('color: gray;'))) ))
     }
     
     if(defaultSelect[[i]][1]=="numeric" || defaultSelect[[i]]=="integer")
     {
       tipoSelect[i] <- as.character(selectInput(inputId=paste0("tipoDatosEntrada_", i), label=NULL, choices=c("Escalar","Ordinal","Nominal"),selected="Escalar",width = "100px"))
       namesTablaFinal[[i]]= HTML(as.character(tags$div(tags$span(class = "fa-stack",icon("ruler","fa-stack-1x",lib = "font-awesome"),style="color:#6d94de"),
                                                  tags$span(namesTable[i],style = paste0('color: gray;'))) ))
       
     
     }
     
     if(defaultSelect[[i]][1] =="factor")
     {
       
       tipoSelect[i] <- as.character(selectInput(inputId=paste0("tipoDatosEntrada_", i), label=NULL, choices=c("Escalar","Ordinal","Nominal"),selected="Nominal",width = "100px"))
       namesTablaFinal[[i]]= HTML( as.character(tags$div(tags$span(class = "fa-stack",icon("chart-bar","fa-stack-1x", lib = "font-awesome"),style="color:#a22f2f"),
                                                  tags$span(namesTable[i],style = paste0('color: gray;')))) )
     
      }
       

     if(defaultSelect[[i]][1]=="ordered")
     {
       
       tipoSelect[i] <- as.character(selectInput(inputId=paste0("tipoDatosEntrada_", i), label=NULL, choices=c("Escalar","Ordinal","Nominal"),selected="Ordinal",width = "100px"))
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
  
 
  proxy = DT::dataTableProxy('tablaPrincipal')
  
  observeEvent(input$tablaPrincipal_cell_edit, {
    info = input$tablaPrincipal_cell_edit
    #utils::str(info)
    i = info$row
    j = info$col + 1
    v = info$value
    
    # problem starts here
    dfEntradaValues$original[i, j] <- isolate(DT::coerceValue(v, dfEntradaValues$original[i, j]))
    replaceData(proxy, dfEntradaValues$original, resetPaging = FALSE,rownames = FALSE)  # replaces data displayed by the updated table
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
    
    radioButtons(inputId = "select_var",
                 label = "Variables En El Dataset",
                 inline = FALSE,
                 
                 choices = if(is.null(dfEntradaNombres$original)){return()}
                 else{(dfEntradaNombres$original)})
  })
  
  
  
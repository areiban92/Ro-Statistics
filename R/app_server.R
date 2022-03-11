#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

 
  data_file <- NULL
  namesClasified <- NULL
  print_dev <- NULL
  
  source("./R/Metodos/Server/Descriptivas/DescriptivasEstadisticas.R",local = TRUE)
  source("./R/Metodos/Server/Descriptivas/DescriptivasEstadisticasTablas.R",local = TRUE)
  source("./R/Metodos/Server/Descriptivas/GraficasDescriptivas.R",local = TRUE)
  source("./R/Metodos/Server/Descriptivas/ObserverEventDescriptivas.R",local = TRUE)
  
  source("./R/Metodos/Server/Ttest/Ttest.R",local = TRUE)
  source("./R/Metodos/Server/Ttest/TtestTablas.R",local = TRUE)
  source("./R/Metodos/Server/Ttest/ObserverTtest.R",local = TRUE)
  source("./R/Metodos/Server/APA/APA.R",local = TRUE)
  source("./R/Metodos/Server/CargaDatos/cargaDatos.R",local = TRUE)
  # 
  source("./R/Metodos/Server/TablasContingencia/Tcontingencia.R",local = TRUE)
  source("./R/Metodos/Server/TablasContingencia/TcontingenciaTablas.R",local = TRUE)
  source("./R/Metodos/Server/TablasContingencia/ObserverContingencia.R",local = TRUE)
  # 
  source("./R/Metodos/Server/Correlacion/Correlacion.R",local = TRUE)
  source("./R/Metodos/Server/Correlacion/CorrelacionTablas.R",local = TRUE)
  source("./R/Metodos/Server/Correlacion/ObserverCorrelacion.R",local = TRUE)
  # 
  source("./R/Metodos/Server/Anova/TanovaTablas.R",local = TRUE)
  source("./R/Metodos/Server/Anova/Tanova.R",local = TRUE)
  source("./R/Metodos/Server/Anova/ObserverAnova.R",local = TRUE)
  # 
  source("./R/Metodos/Server/Ayuda/ayuda.R",local=TRUE,encoding =  "UTF-8")
  # 
  source("./R/Metodos/Server/Regresion/Binomial/RegresionBinomial.R",local=TRUE,encoding =  "UTF-8")
  source("./R/Metodos/Server/Regresion/Binomial/RegresionBinomialTablas.R",local=TRUE,encoding =  "UTF-8")
  # 
  source("./R/Metodos/Server/Regresion/Lineal/RegresionLineal.R",local=TRUE,encoding =  "UTF-8")
  source("./R/Metodos/Server/Regresion/Lineal/RegresionLinealTablas.R",local=TRUE,encoding =  "UTF-8")
  # 
  source("./R/Metodos/Server/Regresion/Multinomial/RegresionMultinomial.R",local=TRUE,encoding =  "UTF-8")
  source("./R/Metodos/Server/Regresion/Multinomial/RegresionMultinomialTablas.R",local=TRUE,encoding =  "UTF-8")
  # 

  banderaDescriptivos <- FALSE
  banderaTtest <- FALSE
  banderaCorrelacion <- FALSE
  banderaContingencia <- FALSE
  banderaAnova <- FALSE
  banderaRegresionBinomial <- FALSE
  banderaRegresionLineal <- FALSE
  banderaRegresionMultinomial <- FALSE
  
  
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
  
  ################################    Checks de Estadisticas Descriptiva    ##########################
  
  checkDescriptivasFrecuencia <- reactive({input$check_tabla_Frecuencia})
  
  #################-------TendenciaCentral------##########
  checkDescriptivasMean <- reactive({input$checkInputDescriptivoEstadisticaTendenciCentralMedias})
  checkDescriptivoEstadisticaTendenciCentralMediana <- reactive({input$checkInputDescriptivoEstadisticaTendenciCentralMedianas})
  checkDescriptivoEstadisticaTendenciCentralModa <- reactive({input$checkInputDescriptivoEstadisticaTendenciCentralModas})
  checkDescriptivoEstadisticaTendenciCentralSuma <- reactive({input$checkInputDescriptivoEstadisticaTendenciCentralSumas})
  
  #################-------Dispersion Teoria Central------############################################
  
  checkDescriptivoDispersionSEmean <-  reactive({input$checkInputDescriptivoDispersionSEmeans})
  checkDescriptivoDispersionMAD <-  reactive({input$checkInputDescriptivoDispersionMADs})
  checkDescriptivoDispersionIQR <-  reactive({ input$checkInputDescriptivoDispersionIQRs})     
  checkDescriptivoDispersionRange <-  reactive({ input$checkInputDescriptivoDispersionRanges})
  checkDescriptivoDispersionMaximo <- reactive({input$checkInputDescriptivoDispersionMaximos})
  
  checkDescriptivoDispersionDesviacionStd <-  reactive({ input$checkInputDescriptivoDispersionDesviacionStds})
  checkDescriptivoDispersionMADRobusto <-  reactive({ input$checkInputDescriptivoDispersionMADRobustos})
  checkDescriptivoDispersionVarianza <-  reactive({ input$checkInputDescriptivoDispersionVarianzas})
  checkDescriptivoDispersionMinimo <-  reactive({ input$checkInputDescriptivoDispersionMinimos})    
  
  #################################################----------------Punto de Corte----------------###############
  
  checkDescriptivoEstadisticaQuartiles <- reactive({input$checkInputDescriptivoEstadisticaQuartiless})
  checkInputDescriptivaEstadisticaPuntoCortes <- reactive ({input$checkInputDescriptivaEstadisticaPuntoCortes})
  txtDescriptivaEstadisticaPuntoCortes <- reactive ({input$txtInputDescriptivaEstadisticaPuntoCortess})
  checkDescriptivaEstadisticaPercentiles <- reactive({input$checkInputDescriptivaEstadisticaPercentiless})
  txtDescriptivaEstadisticaPercentiles <- reactive ({input$txtInputDescriptivaEstadisticaPercentiless})
  
  #################################################----------------CHECK DE BOXPLOTS         ###########
  
  checkDescriptivasBoxplot <- reactive({input$checkBoxplotsEnables})      
  checkDescriptivasBoxplotElement <- reactive({input$checkBoxplotElements})
  checkDescriptivasBoxplotViolin <- reactive({input$checkBoxplotViolins})
  checkDescriptivasBoxplotJitter <- reactive({input$checkBoxplotJitters})
  
  #################################################CHECK DE SccaterPlots ###########
  
  checkDescriptivosDispersion <- reactive({  input$checkDispersionEnables })
  radioDescriptivosButtonplotEncima <- reactive({  input$radioButtonplotEncimas })
  radioDescrptivosButtonplotDerecho <- reactive({  input$radioButtonplotDerechos })
  
  checkDescriptivosDispersionRegresion <- reactive({input$checkDispersionRegresionEnables})
  radioDescriptivosButtonplotRegresion <- reactive({input$radioButtonplotRegresions})
  
  checkDescriptivosDispersionRegresionIntervalo <- reactive({input$checkDispersionRegresionIntervalos})
  txtInputDescriptivosDispersionRegresionIntervalo <- reactive({input$txtInputDispersionRegresionIntervalos})
  
  ##################################  TABLAS DE CORRELACION ################################
  
  radioCorrelacionHypoValue <- reactive({input$radioInputCorrelacionHypotesis})
  
  checkCorrelacionTipoPearson <- reactive({input$checkInput_Correlacion_Pearson})
  checkCorrelacionTipoSpearman <- reactive({input$checkInput_Correlacion_Spearman})
  checkCorrelacionTipoKendall <- reactive({input$checkInput_Correlacion_Kendall})
  
  checkCorrelacion_Significancia<- reactive({ input$checkInput_Correlacion_Significancia})
  
  checkCorrelacion_Estadisticas <- reactive ({input$checkInput_Correlacion_Estadistica })
  checkCorrelacion_MapaCalor <- reactive ({input$checkInput_Correlacion_MapaCalor })
  
  ##################################  TABLAS DE CONTINGENCIA  ################################
  
  radioContingenciaFisher_HypoValue <- reactive({input$radioInputContingenciaHypotesis})
  checkContingenciaTipoFisher <- reactive ({input$fisherTestContingenciaEnables})
  txtContingenciaFisher_IntervaloConfidencia <- reactive({ input$txtInput_Contingencia_Intervalo_Confidencia})
  checkContingenciaPorcentajeFila <- reactive({  input$checkInput_Contingencia_PorcentajeFilas    })
  checkContingenciaPorcentajeColumna <- reactive({  input$checkInput_Contingencia_PorcentajeColumnas    })
  checkContingenciaConteoEsperado <- reactive({  input$checkInput_Contingencia_Conteo    })
  
  ############################################## TABLAS DE ANOVA  #######################################
  
  checkAnovaTurkey <- reactive(input$check_Input_Turkey_Anova)
  checkAnovaBonferroni <- reactive(input$check_Input_Bonferroni_Anova)
  
  checkAnovaGamesHowell <- reactive(input$check_Input_GamesHowell_Anova)
  checkAnovaEfectos <- reactive(input$check_Input_efectos_Anova)
  checkAnovaHomogeniedad <- reactive(input$check_Input_homogeniedad_Anova)
  
  checkAnovaBrown <- reactive(input$check_Input_brown_Anova)
  checkAnovaWelch <- reactive(input$check_Input_welch_Anova)
  
  checkAnovaEstadisticas <- reactive(input$check_Input_estadisticos_Anova)
  
  ###################################################### TABLA DE  REGRESION ##################################################
  
  dfEntradaNombres <- reactiveValues(original = 0, conIconos="",updated=NULL)
  tipoVariable <- reactiveValues(select=NULL)
  
  
  values <- reactiveValues(mydata = NULL,myplot=NULL)
  dfEntradaValues = reactiveValues(original = NULL,conIConos=NULL,transformado=NULL)
  banderas= reactiveValues(dfCargado=0, update=0)
  

  descargaTablasDescriptiva <- reactiveValues( tabla_Descriptivos = NULL, tabla_Descriptivos_Frecuencia = NULL)
  
  
  descargaTablasTtest <- reactiveValues(tabla_Ttest = NULL,
                                        tabla_Ttest_Normalidad = NULL, 
                                        tabla_Ttest_VarianzaIgual = NULL,
                                        tabla_Ttest_Estadistica = NULL )
  
  
  descargaTablasContingencia <-  reactiveValues(tabla_Tcontingencia = NULL,
                                                tabla_Tcontingencia_Fisher = NULL,
                                                tabla_Tcontingencia_Chicuadrado = NULL,
                                                table_calculoEsperadoFinal = NULL)
  
  
  descargaTablasCorrelacion <- reactiveValues(  tabla_Correlacion = NULL )
  
  
  descargaTablasAnova <- reactiveValues( tabla_TanovaDescriptivas = NULL,tabla_Tanova = NULL,
                                         tabla_Tanova_PostHoc_Desiguales = NULL,tabla_Tanova_PostHoc_Iguales = NULL,
                                         tabla_Tanova_Efectos_Fijos = NULL, tabla_Tanova_Efectos_Aleatorios = NULL,
                                         tabla_Tanova_Homogeniedad = NULL, tabla_TanovaWelch = NULL,
                                         tabla_TanovaBrown = NULL
  )
  
  
  descargaTablasRegresion <- reactiveValues(  tabla_TregresionBinomial = NULL,tabla_TregresionBinomialResumenR = NULL,
                                              tabla_TregresionLineal = NULL,tabla_TregresionLinealResumenR = NULL,
                                              tabla_TregresionMultinomial = NULL,tabla_TregresionMultinomialResumenR = NULL )
  
  
  
  
  
  observe({
    
    
    req(banderas$dfCargado)
    
    if(banderas$dfCargado){
      
      print_dev("bandera es 1")
      print_dev("ENTRO EN GUARDAR 22222 ")
      values$mydata <- NULL
      mylist <- list()
      print_dev(length(names(data_file())))
      print_dev(utils::str(dfEntradaValues$transformado))
      
      for(i in 1:length(colnames(data_file()))){
        
        
        print_dev(paste0("La variable es : " ,input[[paste0("tipoDatosEntrada_", i)]]))
        
        print_dev(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])
        
        #Check si la Variables es Escalar Guarda esa Columna como numerica
        if(input[[paste0("tipoDatosEntrada_", i)]]=='Escalar'){
          
          
          # assign(paste0("muted_", dfEntradaNombres$original[i]  ),as.numeric(dfEntradaValues$transformado[,dfEntradaNombres$original[i]]))
          mylist[[i]] <- as.numeric(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])
        }
        
        if(input[[paste0("tipoDatosEntrada_", i)]]=="Ordinal"){
          print_dev("entro en ordinal")
          
          levelss <-  (sort(unique(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])))
          #  assign(paste0("muted_",dfEntradaNombres$original[i]),factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered = TRUE,levels=levelss))
          mylist[[i]] <-  factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered = TRUE,levels=levelss)
        
          
          }
        
        if(input[[paste0("tipoDatosEntrada_", i)]]=="Nominal"){
          
          print_dev("Entro en nomialll")
          # assign(paste0("muted_",dfEntradaNombres$original[i]),factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]]))
          mylist[[i]] <- factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered = FALSE)
          
        }
        
        #  mylist[[i]] <- get(paste0("muted_",dfEntradaNombres$original[i]))
        
        
      }
      print_dev(utils::str(mylist))
      
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
      print_dev("Todo ok hasta aqui5555")
      print_dev(utils::str(values$mydata))
      #   session$sendCustomMessage('unbind-DT', 'tablaPrincipal')
      banderas$dfCargado <- 0
      
      banderas$update=1
      
      
    }else {print_dev("Bandera es 0")}
    
    
    
  })
  
  ######## METODO PARA REACTIVAR LOS CUADROS ########### 
  observeEvent(input$guardar,{
    
    updateActionButton(session, "guardar",
                       label = "Datos Guardados",
                       icon = icon("check"))
    
    removeBOX <-   c("#descriptivaBoxDependiente","#descriptivaBoxAgrupamiento",
                     "#ttestBoxDependiente","#ttestBoxAgrupamiento",
                     "#contingenciaBoxDependiente","#contingenciaBoxAgrupamiento",
                     "#correlacionBoxDependiente", 
                     "#anovaBoxDependiente" ,"#anovaBoxAgrupamiento",
                     "#regresionLinealBoxDependiente","#regresionLinealBoxAgrupamiento",
                     "#regresionBinomialBoxDependiente","#regresionBinomialBoxAgrupamiento",
                     "#regresionMultinomialBoxDependiente","#regresionMultinomialBoxAgrupamiento"
    )
    
    selectorAddBOX <- c("'#placeholderDescriptivaDependiente'","'#placeholderDescriptivaAgrupamiento'",
                        "'#placeholderTtestDependiente'","'#placeholderTtestAgrupamiento'",
                        "'#placeholderContingenciaDependiente'","'#placeholderContingenciaAgrupamiento'",
                        "'#placeholderCorrelacionDependiente'",
                        "'#placeholderAnovaDependiente'","'#placeholderAnovaAgrupamiento'",
                        "'#placeholderRegresionLinealDependiente'","'#placeholderRegresionLinealAgrupamiento'",  
                        "'#placeholderRegresionBinomialDependiente'","'#placeholderRegresionBinomialAgrupamiento'",
                        "'#placeholderRegresionMultinomialDependiente'","'#placeholderRegresionMultinomialAgrupamiento'"
    )
    
    ui_idAddBOX <- c(  "'descriptivaBoxDependiente'","'descriptivaBoxAgrupamiento'",
                       "'ttestBoxDependiente'","'ttestBoxAgrupamiento'",
                       "'contingenciaBoxDependiente'", "'contingenciaBoxAgrupamiento'",
                       "'correlacionBoxDependiente'",
                       "'anovaBoxDependiente'", "'anovaBoxAgrupamiento'",
                       "'regresionLinealBoxDependiente'","'regresionLinealBoxAgrupamiento'",
                       "'regresionBinomialBoxDependiente'","'regresionBinomialBoxAgrupamiento'",
                       "'regresionMultinomialBoxDependiente'","'regresionMultinomialBoxAgrupamiento'"
    )
    
    ###########################
    # 1 dependiente numerica
    # 2 agrupacion  factor
    # 3 dependiente factor
    # 4 agrupacion   numerico 
    # 5 covariate   numerico 
    # 6 covariate   factor 
    # 7 variable   numnerica y factor
    # 8 segmentado por factor
    
    tipo <- c(7,8,
              1,2,
              3,2,
              1,
              1,2,
              1,5,
              3,5,
              3,5)
    
    csstipo <- c("'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
                 "'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
                 "'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
                 "'custom-sortableDependiente'",
                 "'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
                 "'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
                 "'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
                 "'custom-sortableDependiente'","'custom-sortableAgrupamiento'" )
    
    
    
    
    groupName <- c( "'bucket_list_Descriptiva'","'bucket_list_Descriptiva'",
                    "'bucket_list_Ttest'","'bucket_list_Ttest'",
                    "'bucket_list_Tcontingencia'","'bucket_list_Tcontingencia'",
                    "'bucket_list_TCorrelacion'", 
                    "'bucket_list_Anova'","'bucket_list_Anova'",
                    "'bucket_list_RegresionLineal'","'bucket_list_RegresionLineal'",
                    "'bucket_list_RegresionBinomial'","'bucket_list_RegresionBinomial'",
                    "'bucket_list_RegresionMultinomial'","'bucket_list_RegresionMultinomial'"
                    
                    
    )
    
    id_Input <- c( "'rank_list_Descriptiva_Dependiente'","'rank_list_Descriptiva_Agrupamiento'",
                   "'rank_list_Ttest_Dependiente'","'rank_list_Ttest_Agrupamiento'",
                   "'rank_list_Tcontingencia_Dependiente'","'rank_list_Tcontingencia_Agrupamiento'",
                   "'rank_list_TCorrelacion_Dependiente'",
                   "'rank_list_Anova_Dependiente'","'rank_list_Anova_Agrupamiento'",
                   "'rank_list_RegresionLineal_Dependiente'","'rank_list_RegresionLineal_Agrupamiento'",
                   "'rank_list_RegresionBinomial_Dependiente'","'rank_list_RegresionBinomial_Agrupamiento'",
                   "'rank_list_RegresionMultinomial_Dependiente'","'rank_list_RegresionMultinomial_Agrupamiento'" )
    
    
    cssId <- c("'rank_Descriptiva_Dependiente'","'rank_Descriptiva_Agrupamiento'",
               "'rank_Ttest_Dependiente'","'rank_Ttest_Agrupamiento'",
               "'rank_Tcontingencia_Dependiente'","'rank_Tcontingencia_Agrupamiento'",
               "'rank_TCorrelacion_Dependiente'",
               "'rank_Anova_Dependiente'","'rank_Anova_Agrupamiento'",  
               "'rank_RegresionLineal_Dependiente'","'rank_RegresionLineal_Agrupamiento'",  
               "'rank_RegresionBinomial_Dependiente'","'rank_RegresionBinomial_Agrupamiento'",  
               "'rank_RegresionMultinomial_Dependiente'","'rank_RegresionMultinomial_Agrupamiento'"  )
    
    for (box in removeBOX){
      
      # print_dev(box)
      
      removeUI(
        selector =  box
      )
      
      
    }
    
    
    for (i in seq(1:length(selectorAddBOX))) {
      
      
      if(tipo[i]==1)
      {
        
        ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Variables : '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
      }
      
      if (tipo[i]==2)
      {
        
        ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header = HTML(as.character(  tags$div(tags$span(strong('Variable de Agrupaci\u00f3n : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
        
      }
      
      if (tipo[i] == 3)
      {
        
        ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Variable Dependiente  : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
        
      }
      
      if (tipo[i] == 4)
      {
        
        ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Agrupacion Numerico : '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
        
      }
      
      
      if (tipo[i] == 5)
      {
        
        ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Covariante Numerico : '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
        
      }
      
      if (tipo[i] == 6)
      {
        
        ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Covariante  : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
        
      }
      
      if(tipo[i]==7 )
      {
        
        ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong(' Variables: '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue'),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')) )),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),  sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i]," ,options = sortable::sortable_options( group = list(pull = 'clone',name = 'bucket_list_Descriptiva',put = FALSE)    )))))")
      }
      
      if (tipo[i] == 8 )
      {
        
        ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character(  tags$div(tags$span(strong(' Segmentado por : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
        
      }
      
      
      eval(parse(text=ddf))
      
    }
    
    
    
    print_dev("ENTRO EN GUARDAR 22222 ")
    values$mydata <- NULL
    mylist <- list()
    namesTablaActualizada=list()
    print_dev(length(names(data_file())))
    print_dev(utils::str(dfEntradaValues$transformado))
    values$mydata <- dfEntradaValues$transformado
    
    for(i in 1:length(colnames(data_file()))){
      
      
      print_dev(paste0("La variable es : " ,input[[paste0("tipoDatosEntrada_", i)]]))
      
      #  print_dev(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])
      
      #Check si la Variables es Escalar Guarda esa Columna como numerica
      if(input[[paste0("tipoDatosEntrada_", i)]]=='Escalar'){
        
        values$mydata[,i] <- as.numeric(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])
        namesTablaActualizada[[i]]= HTML(as.character(tags$div(tags$span(class = "fa-stack",icon("ruler","fa-stack-1x",lib = "font-awesome"),style="color:#6d94de"),
                                                               tags$span(dfEntradaNombres$original[i],style = paste0('color: gray;'))) ))
      }
      
      if(input[[paste0("tipoDatosEntrada_", i)]]=="Ordinal"){
        print_dev("entro en ordinal")
        
        levelss <-  ((unique(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])))
      
        values$mydata[,i] <-  factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered = TRUE,levels=levelss)
      
        namesTablaActualizada[[i]]= HTML( as.character(tags$div(tags$span(class = "fa-stack",icon("signal","fa-stack-lg", lib = "font-awesome"),style="color:#a22f2f"),
                                                                tags$span(dfEntradaNombres$original[i],style = paste0('color: gray;')))) )
      }
      
      if(input[[paste0("tipoDatosEntrada_", i)]]=="Nominal"){
        
        print_dev("Entro en nomimal")
        
        values$mydata[,i] <- factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered=FALSE)
        namesTablaActualizada[[i]]= HTML( as.character(tags$div(tags$span(class = "fa-stack",icon("chart-bar","fa-stack-1x", lib = "font-awesome"),style="color:#780b0b"),
                                                                tags$span(dfEntradaNombres$original[i],style = paste0('color: gray;')))) )
        
      }
      
      
    }
    print_dev(utils::str(values$mydata))
    
    
   
    
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
    print_dev("Todo ok hasta aqui2222222")
    print_dev(utils::str(values$mydata))
    
    dfEntradaNombres$conIconos <- namesTablaActualizada
    
    
  })
  
  ##################################################################### DESCRIPTIVOS METODOS ##################################
  df_sel <- reactive({
    req(input$rank_list_Descriptiva_Dependiente)
    df_sel <- data_file() %>% dplyr::select(input$rank_list_Descriptiva_Dependiente)
    
  })
  
  # #Seleccion de columnas de pasadas por el filtro
  df_seleccion <- reactive({
    
    req(input$rank_list_Descriptiva_Dependiente)
    df_seleccion<- values$mydata %>% dplyr::select(input$rank_list_Descriptiva_Dependiente)
    
  })
  
  df_Descriptiva_Seleccion_Agrupamiento <- reactive({
    
    #req(input$rank_list_Descriptiva_Agrupamiento)
    
    if(length(input$rank_list_Descriptiva_Agrupamiento) > 0 ){
      df_Descriptiva_Seleccion_Agrupamiento <- values$mydata %>% dplyr::select(input$rank_list_Descriptiva_Agrupamiento)
      
      
    } else {
      
      df_Descriptiva_Seleccion_Agrupamiento <- NULL
      
      
    }
    
  })
  
  df_Descriptiva_Seleccion_Dependiente <- reactive({
    
    req(input$rank_list_Descriptiva_Dependiente)
    df_Descriptiva_Seleccion_Dependiente <- values$mydata %>% dplyr::select(input$rank_list_Descriptiva_Dependiente)
    
  })
  
  output$bucket_1_Descriptiva <- renderUI({
    
    
    sortable::bucket_list(
      header = HTML(as.character(  tags$div(tags$span(strong("Entrada"),style = "color: black;"))) ), 
      group_name = "bucket_list_Descriptiva",
      orientation = "vertical",
      class = c("default-sortable", "custom-sortable"), # add custom style
      #options = sortable::sortable_options(pull = "clone",put = FALSE,multiDrag = TRUE),
      
      sortable::add_rank_list(
        text = " ",
        #labels = names(data_file()),
        
        # labels= validate(
        #   need(namesClasified() != "", "Cargue una base de datos ")
        # ),namesClasified() ,
        labels=   namesClasified(),
        input_id = "rank_list_Descriptiva_Entrada",
        css_id= "rank_Descriptiva_Entrada",
        options = sortable::sortable_options(
          
          group = list(
            pull = "clone",
            name = "bucket_list_Descriptiva",
            put = FALSE
          )
        )
      ) #,
    )
    
  })
  
  if (banderaDescriptivos == FALSE) {
    banderaDescriptivos==TRUE
    
    
    print_dev("Agregando Descriptivos los cuadrios")
    insertUI(
      selector="#placeholderDescriptivaDependiente",
      where="beforeBegin",
      ui = tags$div(id = "descriptivaBoxDependiente",
                    sortable::bucket_list(
                      header = HTML(as.character(  tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                            tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                      group_name = "bucket_list_Descriptiva",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_Descriptiva_Dependiente",
                        css_id = "rank_Descriptiva_Dependiente"
                      ))
      ))
    
    insertUI(
      selector="#placeholderDescriptivaAgrupamiento",
      where="beforeBegin",
      ui = tags$div(id = "descriptivaBoxAgrupamiento",
                    sortable::bucket_list(
                      header = HTML(as.character( tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                           tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                      
                      group_name = "bucket_list_Descriptiva",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                      
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_Descriptiva_Agrupamiento",
                        css_id = "rank_Descriptiva_Agrupamiento"
                      ))
      ))
    
    
  }
  
  
  #######  #######################################################################T-TEST METODOS ###############################
  
  df_Ttest_Seleccion_Agrupamiento <- reactive({
    
    req(input$rank_list_Ttest_Agrupamiento)
    df_Ttest_Seleccion_Agrupamiento<- values$mydata %>% dplyr::select(input$rank_list_Ttest_Agrupamiento)
    
  })
  
  df_Ttest_Seleccion_Dependiente <- reactive({
    
    req(input$rank_list_Ttest_Dependiente)
    df_Ttest_Seleccion_Dependiente<- values$mydata %>% dplyr::select(input$rank_list_Ttest_Dependiente)
    
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
    
    print_dev("Agregando TTESTTTTTTTT los cuadrios")
    
    insertUI(
      selector="#placeholderTtestDependiente",
      where="beforeBegin",
      ui = tags$div(id = "ttestBoxDependiente",
                    sortable::bucket_list(
                      header =  HTML(as.character(  tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                             tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                      group_name = "bucket_list_Ttest",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_Ttest_Dependiente",
                        css_id = "rank_Ttest_Dependiente"
                      ))
      ))
    
    insertUI(
      selector="#placeholderTtestAgrupamiento",
      where="beforeBegin",
      ui = tags$div(id = "ttestBoxAgrupamiento",
                    sortable::bucket_list(
                      header = HTML(as.character(   tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                             tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                      
                      group_name = "bucket_list_Ttest",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                      
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_Ttest_Agrupamiento",
                        css_id = "rank_Ttest_Agrupamiento"
                      ))
      ))
    
  }
  
  #######  ################################################Tablas de Contingencia METODOS ###############################
  
  df_Tcontingencia_Seleccion_Agrupamiento <- reactive({
    
    req(input$rank_list_Tcontingencia_Agrupamiento)
    df_Tcontingencia_Seleccion_Agrupamiento <- values$mydata %>% dplyr::select(input$rank_list_Tcontingencia_Agrupamiento)
    
  })
  
  df_Tcontingencia_Seleccion_Dependiente <- reactive({
    
    req(input$rank_list_Tcontingencia_Dependiente)
    df_Tcontingencia_Seleccion_Dependiente<- values$mydata %>% dplyr::select(input$rank_list_Tcontingencia_Dependiente)
    
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
        input_id = "rank_list_Tcontingencia_Entrada",
        css_id = "rank_Tcontingencia_Entrada"
        
      ))
  })
  
  if (banderaContingencia==FALSE) {
    
    
    banderaContingencia==TRUE
    
    insertUI(
      selector="#placeholderContingenciaDependiente",
      where="beforeBegin",
      ui = tags$div(id = "contingenciaBoxDependiente",
                    sortable::bucket_list(
                      header = HTML(as.character(  tags$div(tags$span(strong("Filas"),style = paste0('color: black;')),
                                                            tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )),
                      group_name = "bucket_list_Tcontingencia",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_Tcontingencia_Agrupamiento",
                        css_id = "rank_Tcontingencia_Agrupamiento"
                      ))
      ))
    
    insertUI(
      selector="#placeholderContingenciaAgrupamiento",
      where="beforeBegin",
      ui = tags$div(id = "contingenciaBoxAgrupamiento",
                    
                    sortable::bucket_list(
                      header = HTML(as.character(   tags$div(tags$span(strong("Columnas"),style = paste0('color: black;')),
                                                             tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )),
                      group_name = "bucket_list_Tcontingencia",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_Tcontingencia_Dependiente",
                        css_id = "rank_Tcontingencia_Dependiente"
                      ))
                    
                    
      ))
    
    
  }
  
  ############################################### TABALAS DE CORRELACION #######################################
  
  df_TCorrelacion_Seleccion_Agrupamiento <- reactive({
    
    req(input$rank_list_TCorrelacion_Agrupamiento)
    df_TCorrelacion_Seleccion_Agrupamiento<- values$mydata %>% dplyr::select(input$rank_list_TCorrelacion_Agrupamiento)
    
  })
  
  df_TCorrelacion_Seleccion_Dependiente <- reactive({
    
    req(input$rank_list_TCorrelacion_Dependiente)
    df_TCorrelacion_Seleccion_Dependiente<- values$mydata %>% dplyr::select(input$rank_list_TCorrelacion_Dependiente)
    
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
        input_id = "rank_list_TCorrelacion_Entrada",
        css_id = "rank_TCorrelacion_Entrada"
        
      ))
  })
  
  if (banderaCorrelacion==FALSE){
    
    banderaCorrelacion=TRUE
    
    # insertUI(
    #   selector="#placeholderCorrelacionAgrupamiento",
    #   where="beforeBegin",
    #   ui = tags$div(id = "correlacionBoxAgrupamiento",
    #                 
    #                 sortable::bucket_list(
    #                   header =HTML(as.character(    tags$div(tags$span(strong("Variables"),style = paste0('color: black;')),
    #                                     tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
    #                   group_name = "bucket_list_TCorrelacion",
    #                   orientation = "horizontal",
    #                   class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
    #                   
    #                   
    #                   sortable::add_rank_list(
    #                     text = "",
    #                     labels = NULL,
    #                     input_id = "rank_list_TCorrelacion_Agrupamiento",
    #                     css_id = "rank_TCorrelacion_Agrupamiento"
    #                   ))
    #                 
    #                
    #   ))
    # 
    
    insertUI(
      selector="#placeholderCorrelacionDependiente",
      where="beforeBegin",
      ui = tags$div(id = "correlacionBoxDependiente",
                    
                    sortable::bucket_list(
                      header = "Condicion",
                      group_name = "bucket_list_TCorrelacion",
                      orientation = "horizontal",
                      
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_TCorrelacion_Dependiente",
                        css_id = "rank_TCorrelacion_Dependiente"
                      ))
                    
                    
                    
                    
      ))
    
  }
  
  
  ############################################# ANOVA TEST ##################################################
  
  df_Anova_Seleccion_Agrupamiento <- reactive({
    
    req(input$rank_list_Anova_Agrupamiento)
    df_Anova_Seleccion_Agrupamiento<- values$mydata %>% dplyr::select(input$rank_list_Anova_Agrupamiento)
    
  })
  
  df_Anova_Seleccion_Dependiente <- reactive({
    
    req(input$rank_list_Anova_Dependiente)
    df_Anova_Seleccion_Dependiente<- values$mydata %>% dplyr::select(input$rank_list_Anova_Dependiente)
    
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
        input_id = "rank_list_Anova_Entrada",
        css_id = "rank_Anova_Entrada"
        
      ))
  })
  
  
  if (banderaAnova==FALSE) {
    banderaAnova=TRUE
    
    insertUI(
      selector="#placeholderAnovaDependiente",
      where="beforeBegin",
      ui = tags$div(id = "anovaBoxDependiente",
                    sortable::bucket_list(
                      header = HTML(as.character(  tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                            tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                      group_name = "bucket_list_Anova",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_Anova_Dependiente",
                        css_id = "rank_Anova_Dependiente"
                      ))
      ))
    
    insertUI(
      selector="#placeholderAnovaAgrupamiento",
      where="beforeBegin",
      ui = tags$div(id = "anovaBoxAgrupamiento",
                    sortable::bucket_list(
                      header =  HTML(as.character(  tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                             tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                      
                      group_name = "bucket_list_Anova",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                      
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_Anova_Agrupamiento",
                        css_id = "rank_Anova_Agrupamiento"
                      ))
      ))
    
  }
  
  ################################################ Regrsion Binomial ########################################
  
  df_RegresionBinomial_Seleccion_Agrupamiento <- reactive({
    
    req(input$rank_list_RegresionBinomial_Agrupamiento)
    df_RegresionBinomial_Seleccion_Agrupamiento <- values$mydata %>% dplyr::select(input$rank_list_RegresionBinomial_Agrupamiento)
    
  })
  
  df_RegresionBinomial_Seleccion_Dependiente <- reactive({
    
    req(input$rank_list_RegresionBinomial_Dependiente)
    df_RegresionBinomial_Seleccion_Dependiente<- values$mydata %>% dplyr::select(input$rank_list_RegresionBinomial_Dependiente)
    
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
        input_id = "rank_list_RegresionBinomial_Entrada",
        css_id = "rank_RegresionBinomial_Entrada"
        
      ))
  })
  
  if (banderaRegresionBinomial == FALSE) {
    banderaRegresionBinomial = TRUE
    
    insertUI(
      selector="#placeholderRegresionBinomialDependiente",
      where="beforeBegin",
      ui = tags$div(id = "regresionBinomialBoxDependiente",
                    sortable::bucket_list(
                      header = HTML(as.character(  tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                            tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                      group_name = "bucket_list_RegresionBinomial",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_RegresionBinomial_Dependiente",
                        css_id = "rank_RegresionBinomial_Dependiente"
                      ))
      ))
    
    insertUI(
      selector="#placeholderRegresionBinomialAgrupamiento",
      where="beforeBegin",
      ui = tags$div(id = "regresionBinomialBoxAgrupamiento",
                    sortable::bucket_list(
                      header = HTML(as.character(  tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                            tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                      
                      group_name = "bucket_list_RegresionBinomial",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_RegresionBinomial_Agrupamiento",
                        css_id = "rank_RegresionBinomial_Agrupamiento"
                      ))
      ))
    
  }
  
  
  ############################################ Regresion Lineal ########################################3##
  
  df_RegresionLineal_Seleccion_Agrupamiento <- reactive({
    
    req(input$rank_list_RegresionLineal_Agrupamiento)
    df_RegresionLineal_Seleccion_Agrupamiento<- values$mydata %>% dplyr::select(input$rank_list_RegresionLineal_Agrupamiento)
    
  })
  
  df_RegresionLineal_Seleccion_Dependiente <- reactive({
    
    req(input$rank_list_RegresionLineal_Dependiente)
    df_RegresionLineal_Seleccion_Dependiente<- values$mydata %>% dplyr::select(input$rank_list_RegresionLineal_Dependiente)
    
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
        input_id = "rank_list_RegresionLineal_Entrada",
        css_id = "rank_RegresionLineal_Entrada"
        
      ))
  })
  
  if (banderaRegresionLineal == FALSE) {
    banderaRegresionLineal = TRUE
    
    insertUI(
      selector="#placeholderRegresionLinealDependiente",
      where="beforeBegin",
      ui = tags$div(id = "regresionLinealBoxDependiente",
                    sortable::bucket_list(
                      header = HTML(as.character(   tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                             tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                      group_name = "bucket_list_RegresionLineal",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_RegresionLineal_Dependiente",
                        css_id = "rank_RegresionLineal_Dependiente"
                      ))
      ))
    
    insertUI(
      selector="#placeholderRegresionLinealAgrupamiento",
      where="beforeBegin",
      ui = tags$div(id = "regresionLinealBoxAgrupamiento",
                    sortable::bucket_list(
                      header =  HTML(as.character(  tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                             tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                      
                      group_name = "bucket_list_RegresionLineal",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_RegresionLineal_Agrupamiento",
                        css_id = "rank_RegresionLineal_Agrupamiento"
                      ))
      ))
    
  }
  
  ########################################## Regresion Multinomial ###################################
  
  
  df_RegresionMultinomial_Seleccion_Agrupamiento <- reactive({
    
    req(input$rank_list_RegresionMultinomial_Agrupamiento)
    df_RegresionMultinomial_Seleccion_Agrupamiento<- values$mydata %>% dplyr::select(input$rank_list_RegresionMultinomial_Agrupamiento)
    
  })
  
  df_RegresionMultinomial_Seleccion_Dependiente <- reactive({
    
    req(input$rank_list_RegresionMultinomial_Dependiente)
    df_RegresionMultinomial_Seleccion_Dependiente<- values$mydata %>% dplyr::select(input$rank_list_RegresionMultinomial_Dependiente)
    
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
        input_id = "rank_list_RegresionMultinomial_Entrada",
        css_id = "rank_RegresionMultinomial_Entrada"
        
      ))
  })
  
  if (banderaRegresionMultinomial == TRUE) {
    banderaRegresionMultinomial = TRUE
    
    insertUI(
      selector="#placeholderRegresionMultinomialDependiente",
      where="beforeBegin",
      ui = tags$div(id = "regresionMultinomialBoxDependiente",
                    sortable::bucket_list(
                      header = HTML(as.character(  tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                            tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                      group_name = "bucket_list_RegresionMultinomial",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_RegresionMultinomial_Dependiente",
                        css_id = "rank_RegresionMultinomial_Dependiente"
                      ))
      ))
    
    insertUI(
      selector="#placeholderRegresionMultinomialAgrupamiento",
      where="beforeBegin",
      ui = tags$div(id = "regresionMultinomialBoxAgrupamiento",
                    sortable::bucket_list(
                      header =  HTML(as.character(  tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                             tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                      
                      group_name = "bucket_list_RegresionMultinomial",
                      orientation = "horizontal",
                      class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                      
                      sortable::add_rank_list(
                        text = "",
                        labels = NULL,
                        input_id = "rank_list_RegresionMultinomial_Agrupamiento",
                        css_id = "rank_RegresionMultinomial_Agrupamiento"
                      ))
      ))
    
  }
  
  
  ######################################## EXIT #####################################################
  
  
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}

  
  
  
  
  
  
  
  
  
  
  
  
  


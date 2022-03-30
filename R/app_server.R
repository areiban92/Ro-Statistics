#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  r <- reactiveValues()
  t <- reactiveValues()
  
  data_file <- NULL
  namesClasified <- NULL
 
  
  namesClasi <- mod_cargaDatos_server("cargaDatos_ui_1", r = r )
  
  mod_UIDescriptivos_server("UIDescriptivos_ui_1",namesClasi, r = r )
  
  mod_Ttest_server("Ttest_ui_1",namesClasi, r = r) 
  
  mod_Contingencia_server("Contingencia_ui_1", namesClasi, r = r )
  
  mod_Anova_server("Anova_ui_1", namesClasi, r = r)
  
  mod_Regresion_server("Regresion_ui_1", namesClasi, r = r)


  ###################################################### TABLA DE  REGRESION ##################################################
  # 
  # dfEntradaNombres <- reactiveValues(original = 0, conIconos="",updated=NULL)
  # tipoVariable <- reactiveValues(select=NULL)
  # 
  # values <- reactiveValues(mydata = NULL,myplot=NULL)
  # dfEntradaValues = reactiveValues(original = NULL,conIConos=NULL,transformado=NULL)
  # banderas= reactiveValues(dfCargado=0, update=0)
  # 

 
  
  
  # descargaTablasTtest <- reactiveValues(tabla_Ttest = NULL,
  #                                       tabla_Ttest_Normalidad = NULL, 
  #                                       tabla_Ttest_VarianzaIgual = NULL,
  #                                       tabla_Ttest_Estadistica = NULL )
  
  

  
  
  

  
  ######## METODO PARA REACTIVAR LOS CUADROS ########### 
  # observeEvent(input$guardar,{
  #   
  #   updateActionButton(session, "guardar",
  #                      label = "Datos Guardados",
  #                      icon = icon("check"))
  #   
  #   removeBOX <-   c("#descriptivaBoxDependiente","#descriptivaBoxAgrupamiento",
  #                    "#ttestBoxDependiente","#ttestBoxAgrupamiento",
  #                    "#contingenciaBoxDependiente","#contingenciaBoxAgrupamiento",
  #                    "#correlacionBoxDependiente", 
  #                    "#anovaBoxDependiente" ,"#anovaBoxAgrupamiento",
  #                    "#regresionLinealBoxDependiente","#regresionLinealBoxAgrupamiento",
  #                    "#regresionBinomialBoxDependiente","#regresionBinomialBoxAgrupamiento",
  #                    "#regresionMultinomialBoxDependiente","#regresionMultinomialBoxAgrupamiento"
  #   )
  #   
  #   selectorAddBOX <- c("'#placeholderDescriptivaDependiente'","'#placeholderDescriptivaAgrupamiento'",
  #                       "'#placeholderTtestDependiente'","'#placeholderTtestAgrupamiento'",
  #                       "'#placeholderContingenciaDependiente'","'#placeholderContingenciaAgrupamiento'",
  #                       "'#placeholderCorrelacionDependiente'",
  #                       "'#placeholderAnovaDependiente'","'#placeholderAnovaAgrupamiento'",
  #                       "'#placeholderRegresionLinealDependiente'","'#placeholderRegresionLinealAgrupamiento'",  
  #                       "'#placeholderRegresionBinomialDependiente'","'#placeholderRegresionBinomialAgrupamiento'",
  #                       "'#placeholderRegresionMultinomialDependiente'","'#placeholderRegresionMultinomialAgrupamiento'"
  #   )
  #   
  #   ui_idAddBOX <- c(  "'descriptivaBoxDependiente'","'descriptivaBoxAgrupamiento'",
  #                      "'ttestBoxDependiente'","'ttestBoxAgrupamiento'",
  #                      "'contingenciaBoxDependiente'", "'contingenciaBoxAgrupamiento'",
  #                      "'correlacionBoxDependiente'",
  #                      "'anovaBoxDependiente'", "'anovaBoxAgrupamiento'",
  #                      "'regresionLinealBoxDependiente'","'regresionLinealBoxAgrupamiento'",
  #                      "'regresionBinomialBoxDependiente'","'regresionBinomialBoxAgrupamiento'",
  #                      "'regresionMultinomialBoxDependiente'","'regresionMultinomialBoxAgrupamiento'"
  #   )
  #   
  #   ###########################
  #   # 1 dependiente numerica
  #   # 2 agrupacion  factor
  #   # 3 dependiente factor
  #   # 4 agrupacion   numerico 
  #   # 5 covariate   numerico 
  #   # 6 covariate   factor 
  #   # 7 variable   numnerica y factor
  #   # 8 segmentado por factor
  #   
  #   tipo <- c(7,8,
  #             1,2,
  #             3,2,
  #             1,
  #             1,2,
  #             1,5,
  #             3,5,
  #             3,5)
  #   
  #   csstipo <- c("'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
  #                "'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
  #                "'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
  #                "'custom-sortableDependiente'",
  #                "'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
  #                "'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
  #                "'custom-sortableDependiente'","'custom-sortableAgrupamiento'",
  #                "'custom-sortableDependiente'","'custom-sortableAgrupamiento'" )
  #   
  #   
  #   
  #   
  #   groupName <- c( "'bucket_list_Descriptiva'","'bucket_list_Descriptiva'",
  #                   "'bucket_list_Ttest'","'bucket_list_Ttest'",
  #                   "'bucket_list_Tcontingencia'","'bucket_list_Tcontingencia'",
  #                   "'bucket_list_TCorrelacion'", 
  #                   "'bucket_list_Anova'","'bucket_list_Anova'",
  #                   "'bucket_list_RegresionLineal'","'bucket_list_RegresionLineal'",
  #                   "'bucket_list_RegresionBinomial'","'bucket_list_RegresionBinomial'",
  #                   "'bucket_list_RegresionMultinomial'","'bucket_list_RegresionMultinomial'"
  #                   
  #                   
  #   )
  #   
  #   id_Input <- c( "'rank_list_Descriptiva_Dependiente'","'rank_list_Descriptiva_Agrupamiento'",
  #                  "'rank_list_Ttest_Dependiente'","'rank_list_Ttest_Agrupamiento'",
  #                  "'rank_list_Tcontingencia_Dependiente'","'rank_list_Tcontingencia_Agrupamiento'",
  #                  "'rank_list_TCorrelacion_Dependiente'",
  #                  "'rank_list_Anova_Dependiente'","'rank_list_Anova_Agrupamiento'",
  #                  "'rank_list_RegresionLineal_Dependiente'","'rank_list_RegresionLineal_Agrupamiento'",
  #                  "'rank_list_RegresionBinomial_Dependiente'","'rank_list_RegresionBinomial_Agrupamiento'",
  #                  "'rank_list_RegresionMultinomial_Dependiente'","'rank_list_RegresionMultinomial_Agrupamiento'" )
  #   
  #   
  #   cssId <- c("'rank_Descriptiva_Dependiente'","'rank_Descriptiva_Agrupamiento'",
  #              "'rank_Ttest_Dependiente'","'rank_Ttest_Agrupamiento'",
  #              "'rank_Tcontingencia_Dependiente'","'rank_Tcontingencia_Agrupamiento'",
  #              "'rank_TCorrelacion_Dependiente'",
  #              "'rank_Anova_Dependiente'","'rank_Anova_Agrupamiento'",  
  #              "'rank_RegresionLineal_Dependiente'","'rank_RegresionLineal_Agrupamiento'",  
  #              "'rank_RegresionBinomial_Dependiente'","'rank_RegresionBinomial_Agrupamiento'",  
  #              "'rank_RegresionMultinomial_Dependiente'","'rank_RegresionMultinomial_Agrupamiento'"  )
  #   
  #   for (box in removeBOX){
  #     
  #     # golem::print_dev(box)
  #     
  #     removeUI(
  #       selector =  box
  #     )
  #     
  #     
  #   }
  #   
  #   
  #   for (i in seq(1:length(selectorAddBOX))) {
  #     
  #     
  #     if(tipo[i]==1)
  #     {
  #       
  #       ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Variables : '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
  #     }
  #     
  #     if (tipo[i]==2)
  #     {
  #       
  #       ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header = HTML(as.character(  tags$div(tags$span(strong('Variable de Agrupaci\u00f3n : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
  #       
  #     }
  #     
  #     if (tipo[i] == 3)
  #     {
  #       
  #       ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Variable Dependiente  : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
  #       
  #     }
  #     
  #     if (tipo[i] == 4)
  #     {
  #       
  #       ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Agrupacion Numerico : '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
  #       
  #     }
  #     
  #     
  #     if (tipo[i] == 5)
  #     {
  #       
  #       ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Covariante Numerico : '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
  #       
  #     }
  #     
  #     if (tipo[i] == 6)
  #     {
  #       
  #       ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Covariante  : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
  #       
  #     }
  #     
  #     if(tipo[i]==7 )
  #     {
  #       
  #       ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong(' Variables: '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue'),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')) )),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),  sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i]," ,options = sortable::sortable_options( group = list(pull = 'clone',name = 'bucket_list_Descriptiva',put = FALSE)    )))))")
  #     }
  #     
  #     if (tipo[i] == 8 )
  #     {
  #       
  #       ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =", ui_idAddBOX[i],",sortable::bucket_list(header =  HTML(as.character(  tags$div(tags$span(strong(' Segmentado por : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
  #       
  #     }
  #     
  #     
  #     eval(parse(text=ddf))
  #     
  #   }
  #   
  #   
  #   
  #   golem::print_dev("ENTRO EN GUARDAR 22222 ")
  #   values$mydata <- NULL
  #   mylist <- list()
  #   namesTablaActualizada=list()
  #   golem::print_dev(length(names(data_file())))
  #   golem::print_dev(utils::str(dfEntradaValues$transformado))
  #   values$mydata <- dfEntradaValues$transformado
  #   
  #   for(i in 1:length(colnames(data_file()))){
  #     
  #     
  #     golem::print_dev(paste0("La variable es : " ,input[[paste0("tipoDatosEntrada_", i)]]))
  #     
  #     #  golem::print_dev(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])
  #     
  #     #Check si la Variables es Escalar Guarda esa Columna como numerica
  #     if(input[[paste0("tipoDatosEntrada_", i)]]=='Escalar'){
  #       
  #       values$mydata[,i] <- as.numeric(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])
  #       namesTablaActualizada[[i]]= HTML(as.character(tags$div(tags$span(class = "fa-stack",icon("ruler","fa-stack-1x",lib = "font-awesome"),style="color:#6d94de"),
  #                                                              tags$span(dfEntradaNombres$original[i],style = paste0('color: gray;'))) ))
  #     }
  #     
  #     if(input[[paste0("tipoDatosEntrada_", i)]]=="Ordinal"){
  #       golem::print_dev("entro en ordinal")
  #       
  #       levelss <-  ((unique(dfEntradaValues$transformado[,dfEntradaNombres$original[i]])))
  #     
  #       values$mydata[,i] <-  factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered = TRUE,levels=levelss)
  #     
  #       namesTablaActualizada[[i]]= HTML( as.character(tags$div(tags$span(class = "fa-stack",icon("signal","fa-stack-lg", lib = "font-awesome"),style="color:#a22f2f"),
  #                                                               tags$span(dfEntradaNombres$original[i],style = paste0('color: gray;')))) )
  #     }
  #     
  #     if(input[[paste0("tipoDatosEntrada_", i)]]=="Nominal"){
  #       
  #       golem::print_dev("Entro en nomimal")
  #       
  #       values$mydata[,i] <- factor(dfEntradaValues$transformado[,dfEntradaNombres$original[i]],ordered=FALSE)
  #       namesTablaActualizada[[i]]= HTML( as.character(tags$div(tags$span(class = "fa-stack",icon("chart-bar","fa-stack-1x", lib = "font-awesome"),style="color:#780b0b"),
  #                                                               tags$span(dfEntradaNombres$original[i],style = paste0('color: gray;')))) )
  #       
  #     }
  #     
  #     
  #   }
  #   golem::print_dev(utils::str(values$mydata))
  # 
  #   
  #   shinyalert::shinyalert(
  #     title = " ",
  #     text = "Valores Guardados",
  #     size = "s",
  #     closeOnEsc = TRUE,
  #     closeOnClickOutside = FALSE,
  #     html = FALSE,
  #     type = "success",
  #     showConfirmButton = FALSE,
  #     showCancelButton = FALSE,
  #     timer = 1000,
  #     imageUrl = "",
  #     animation = TRUE
  #   )
  #   names(values$mydata) <- names(data_file())
  #   golem::print_dev("Todo ok hasta aqui2222222")
  #   golem::print_dev(utils::str(values$mydata))
  #   
  #   dfEntradaNombres$conIconos <- namesTablaActualizada
  #   
  #   
  # })
  # 
  



  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}

  
  
  
  
  
  
  
  
  
  
  
  
  


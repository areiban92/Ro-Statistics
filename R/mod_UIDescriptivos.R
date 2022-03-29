#' UIDescriptivos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_UIDescriptivos_ui <- function(idxxx){
  ns <- NS(idxxx)

    
    
    tabPanel( div( id=ns("textoIconos"),
                   
                   div( id=ns("imgIconos"),
                        tags$img(src ='www/iconos2/descriptivos.png', style= "width: 50px; margin-top: 0px; margin-bottom: 5px; height: 30px;")),
                   div( 
                     "DESCRIPTIVOS" )
    ), value = "Tab 2",
    sidebarPanel( style =paste0(" height: 620px; overflow-y: auto; "),  width = 6,
                
                  fluidRow(
                    column(
                      #tags$b("VARIABLES"),
                      width = 6,
                      div( style = "font-size: 12px; padding: 14px 0px; margin:0%",
                           uiOutput( ns("bucket_1_Descriptiva") ))
                    ), 
                    column(width = 5,
                           div(style = "font-size: 12px; padding: 14px 0px; margin:0%",
                               fluidRow(
                                 
                                 tags$div(id = ns('placeholderDescriptivaDependiente'))
                               )),
                           div(style = "font-size: 12px; padding: 0px 0px; margin-top:-3em", 
                               fluidRow(width = 4,
                                        tags$div(id = ns('placeholderDescriptivaAgrupamiento'))
                                        # uiOutput("bucket_3_Ttest")
                               ))
                    )
                  ),#fin fluid row
                  
                  uiOutput(ns("checkbox")),
                  
                  checkboxInput( ns("check_tabla_Frecuencia"), tags$div(tags$span(strong("Tablas de Frecuencia"),style = paste0('color: black;')),
                                                                   tags$span(class = "fa-stack",icon("chart-bar","fa-stack-1x",lib = "font-awesome"), style="color:grey")),FALSE),
                  actionButton(ns("menuGraficas"), "Gr\u00e1ficas"),
                  
                  div(
                    fluidRow(
                      column(6,div(id=ns("bloqueDescriptivasPersonalizadas"),
                                   div(id= ns("tituloGraficasPersonalizadas"), tags$div(tags$span(strong("Gr\u00e1ficas Personalizadas"),style = "color: black;"))),
                                   
                                   #div(style="display: inline-block;vertical-align:top; width: 10px; " , id= "br_Descriptivos_Boxplot"),
                                   div(style="display: inline-block;vertical-align:top;  ", id =ns("checkBoxplotsEnable"),checkboxInput(ns("checkBoxplotsEnables"), "Boxplots", FALSE)),
                                   
                                   div( style = "width: 10px;",id= ns("br_Descriptivos_Boxplot2")),
                                   
                                   div( style = "display: inline-block; vertical-align:top; width: 20px; margin-top: -20px ",id= ns("br_Descriptivos_Boxplot3")),
                                   div( style="display: inline-block;vertical-align:top;  ",  id = ns("Br_bloqueGraficaBoxplot") ,
                                        
                                        div(style="margin-top: -20px", id = ns("checkBoxplotElement"),checkboxInput(ns("checkBoxplotElements"), "Element Boxplot", FALSE)),
                                        div(style="margin-top: -15px", id = ns("checkBoxplotViolin"),checkboxInput(ns("checkBoxplotViolins"), "Violin Boxplot", FALSE)),
                                        div( style="margin-top: -15px", id = ns("checkBoxplotJitter"),checkboxInput(ns("checkBoxplotJitters"), "Jitter Boxplot", FALSE))
                                   ),#Fin Div engloba opciones Boxplot 
                                   
                                   ############################    ############ CHECKBOX GRAFICO DISOERSION  #################################
                                   div( id = ns("br_Descriptivos_Dispersion0")),
                                   # div(style="display: inline-block;vertical-align:top; width: 10px; " , id= "br_Descriptivos_Dispersion"),
                                   div(style="display: inline-block;vertical-align:top; ", id =ns("checkDispersionEnable"),checkboxInput(ns("checkDispersionEnables") ,"Graficos Dispersi\u00f3n",FALSE)),
                                   
                                   div( style = "width: 10px;",id= ns("br_Descriptivos_Dispersion2")),
                                   
                                   div( style = "display: inline-block;vertical-align:top; width: 20px; ",id= ns("br_Descriptivos_Dispersion3 ")),
                                   div( style = "display: inline-block;vertical-align:top;  ",id=ns("radioButtonplotEncima"), radioButtons(ns("radioButtonplotEncimas"),"Sobre el grafico de Disapersi\u00f3n", c("Densidad"="den","Histograma"="Hist","Ninguno"="Ning."),selected = "Ning.")),
                                   
                                   div( style = "width: 10px;",id= ns("br_Descriptivos_Dispersion4")),
                                   
                                   div(style="display: inline-block;vertical-align:top; width: 20px; " , id= ns("br_Descriptivos_Dispersion5")),
                                   div(style="display: inline-block;vertical-align:top;  ", id = ns("radioButtonplotDerecho"),radioButtons(ns("radioButtonplotDerechos"),"Derecha del grafico de dispersi\u00f3n", c("Densidad"="den","Histograma"="Hist","Ninguno"="Ning."),selected = "Ning.")),
                                   
                                   #######LInea de Regresion
                                   div( id= ns("br_Descriptivos_Dispersion6")),
                                   
                                   div(style="display: inline-block;vertical-align:top; width: 20px; " , id= ns("br_Descriptivos_Dispersion7")),
                                   div(style="display: inline-block;vertical-align:top;  ", id = ns("checkDispersionRegresionEnable"),checkboxInput(ns("checkDispersionRegresionEnables") ," Regresi\u00f3n Lineal", FALSE)),
                                   
                                   div( style = "width: 10px;",id= ns("br_Descriptivos_Dispersion8") ),
                                   
                                   div(style="display: inline-block;vertical-align:top; width: 30px; " , id= ns("br_Descriptivos_Dispersion9")),
                                   div(style="display: inline-block;vertical-align:top; margin-top: -25px ", id = ns("radioButtonplotRegresion"),radioButtons(ns("radioButtonplotRegresions"),"", c("Smooth"="Smooth","Lineal"="Lineal"),selected = "Smooth")),
                                   
                                   div( style = "width: 10px;",id= ns("br_Descriptivos_Dispersion10")),
                                   
                                   div(style="display: inline-block;vertical-align:top; width: 30px; " , id= ns("br_Descriptivos_Dispersion11")),
                                   div(style="display: inline-block;vertical-align:top;  margin-top: -20px  " , id = ns("checkDispersionRegresionIntervalo"),checkboxInput(ns("checkDispersionRegresionIntervalos") ," Intervalo Confidencia", FALSE)),
                                   div(style="display: inline-block;vertical-align:top; width: 50px; margin-top: -20px " , id = ns("txtInputDispersionRegresionIntervalo"), textInput(ns("txtInputDispersionRegresionIntervalos"),label= NULL,value = "95")),
                                   div( style="display: inline-block;vertical-align:top; width: 2px;margin-top: -20px   ", id =ns( "txtDispersionRegresionIntervalos"),h4("%"))
                                   
                      ) # fin bloque personalizada
                      ),
                      column(6,div( id= ns("bloqueDescriptivasBasicas"),
                                    div(id= ns("tituloGraficasBasicas"),tags$div(tags$span(strong("Gr\u00e1ficas Personalizadas"),style = "color: black;"))),
                                    # div(style="display: inline-block;vertical-align:top; width: 10px; " , id= "br_Descriptivos_Grafica"),
                                    div(style="display: inline-block;vertical-align:top;  ", id = ns("BloqueGraficaBasica") ,
                                        
                                        div( id = ns("checkDistribucion"),checkboxInput(ns("checkDistribucions"), "Gr\u00e1fica Distribuci\u00f3n", FALSE)),
                                        
                                        div( style="display: inline-block;vertical-align:top; width: 10px; margin-top: -20px " , id = ns("br_Descriptivos_Grafica_Densidad")),
                                        div( style="display: inline-block;vertical-align:top; margin-top: -20px ", id = ns("checkDensidad"),checkboxInput(ns("checkDensidads"), "Grafica Densidad", FALSE)),
                                        div( style=" margin-top: -20px ", id = ns("checkCorrelacion"),checkboxInput( ns("checkCorrelacions"), "Grafica Correlaci\u00f3n", FALSE)),
                                        div( style=" margin-top: -15px ", id = ns("checkPastel"),checkboxInput(ns("checkPastels"), "Grafico Pastel", FALSE)),
                                        div( style="margin-top: -15px ", id = ns("checkQQ"),checkboxInput(ns("checkQQs"), "Grafica Q_Q", FALSE)))
                                    
                      ) #Fin bloque div de Basicas
                      
                      )
                      
                    )), #fin del div
                  
                  actionButton(ns("menuEstadistica"), "Estadística"),
                  
                  fluidRow(
                    
                    column(6,div(id=ns("bloqueValoresPorcentilesColumna1"),
                                 
                                 tags$div(tags$span(strong("Valores Porcentiles"),style = "color: black;")),
                                 
                                 
                                 # div( style="display: inline-block;vertical-align:top; width: 20px; " ,   id= "br_Descriptiva_Estadistica1"),
                                 div( style="display: inline-block;vertical-align:top;  ", id =ns("checkInputDescriptivoEstadisticaQuartiles"),checkboxInput( ns("checkInputDescriptivoEstadisticaQuartiless"), "Quartil", FALSE)),
                                 
                                 div( style = "width: 10px;",id= ns("br_Descriptiva_Estadistica2")),
                                 
                                 #  div(style="display: inline-block;vertical-align:top; width: 20px; " , id= "br_Descriptiva_Estadistica3"),
                                 div(style="display: inline-block;vertical-align:top; margin-top: -20px ", id = ns("checkInputDescriptivaEstadisticaPuntoCorte"),checkboxInput(ns("checkInputDescriptivaEstadisticaPuntoCortes")," Puntos de Corte : ")),
                                 div(style="display: inline-block;vertical-align:top; width: 50px; margin-top: -12px " , id = ns("txtInputDescriptivaEstadisticaPuntoCortes"), textInput(ns("txtInputDescriptivaEstadisticaPuntoCortess"),label= NULL,value = "4")),
                                 div( style="display: inline-block;vertical-align:top; width: 2px;margin-top: -12px   ", id = ns( "txtDescriptivaEstadisticaPuntoCortes"),h5("grupos")),
                                 
                                 div( style = "width: 10px;",id= ns("br_Descriptiva_Estadistica4")),
                                 
                                 #   div(style="display: inline-block;vertical-align:top; width: 20px; " , id= "br_Descriptiva_Estadistica5"),
                                 div(style="display: inline-block;vertical-align:top; margin-top: -18px ", id = ns("checkDescriptivaEstadisticaPercentiles"),checkboxInput(ns("checkInputDescriptivaEstadisticaPercentiless") ," Percentiles : ")),
                                 div(style="display: inline-block;vertical-align:top; width: 50px; margin-top: -8px; " , id = ns("txtInputDescriptivaEstadisticaPercentiles"), textInput(ns("txtInputDescriptivaEstadisticaPercentiless"),label= NULL,value = "5")),
                                 
                                 
                                 tags$div(tags$span(strong("Dispersi\u00f3n"),style = "color: black;")),
                                 
                                 fluidRow(
                                   column(6,
                                          
                                          #div( style="display: inline-block;vertical-align:top; width: 20px; " ,   id= "br_Descriptiva_DispersionValores"),
                                          div( style="display: inline-block;vertical-align:top;  ", id =ns("checkInputDescriptivoDispersionSEmean"),checkboxInput(ns("checkInputDescriptivoDispersionSEmeans"), "S.E. mean", FALSE)),
                                          
                                          div( style = "width: 10px;",id= "br_Descriptiva_DispersionValores1"),
                                          # div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -20px " ,   id= "br_Descriptiva_DispersionValores2"),
                                          div( style="display: inline-block;vertical-align:top; margin-top: -20px ", id =ns("checkInputDescriptivoDispersionMAD"),checkboxInput(ns("checkInputDescriptivoDispersionMADs"), "MAD", FALSE)),
                                          
                                          div( style = "width: 10px;",id= "br_Descriptiva_DispersionValores3"),
                                          # div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -20px " ,   id= "br_Descriptiva_DispersionValores4"),
                                          div( style="display: inline-block;vertical-align:top; margin-top: -20px ", id =ns("checkInputDescriptivoDispersionIQR"),checkboxInput(ns("checkInputDescriptivoDispersionIQRs"), "IQR", FALSE)),
                                          
                                          div( style = "width: 10px;",id= "br_Descriptiva_DispersionValores5"),
                                          #  div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -20px " ,   id= "br_Descriptiva_DispersionValores6"),
                                          div( style="display: inline-block;vertical-align:top; margin-top: -20px ", id =ns("checkInputDescriptivoDispersionRange"),checkboxInput(ns("checkInputDescriptivoDispersionRanges"), "Range", FALSE)),
                                          
                                          
                                          div( style = "width: 10px;",id= ns("br_Descriptiva_DispersionValores51")),
                                        
                                          div( style="display: inline-block;vertical-align:top; margin-top: -20px ", id = ns("checkInputDescriptivoDispersionMaximo"),checkboxInput(ns("checkInputDescriptivoDispersionMaximos"), "Maximo", TRUE)),
                                          
                                   ), #Fin columna 1 Valores de Disperson
                                   column(6,
                                          
                                          #div( style="display: inline-block;vertical-align:top; width: 20px; " ,   id= "br_Descriptiva_DispersionValores7"),
                                          div( id =ns("checkInputDescriptivoDispersionDesviacionStd"),checkboxInput(ns("checkInputDescriptivoDispersionDesviacionStds"), "Desviacion Std.", TRUE)),
                                          
                                          #div( style = "width: 10px;",id= "br_Descriptiva_DispersionValores8"),
                                          
                                          # div( style="display: inline-block;vertical-align:top; width: 20px; margin-top: -25px " ,   id= "br_Descriptiva_DispersionValores9"),
                                          div( style=" margin-top: -10px ", id =ns("checkInputDescriptivoDispersionMADRobusto"),checkboxInput(ns("checkInputDescriptivoDispersionMADRobustos"), "MAD Robusto", FALSE)),
                                          div( style=" margin-top: -10px ", id =ns("checkInputDescriptivoDispersionVarianza"),checkboxInput(ns("checkInputDescriptivoDispersionVarianzas"), "Varianza", FALSE)),
                                          div( style="margin-top: -10px ", id =ns("checkInputDescriptivoDispersionMinimos"),checkboxInput(ns("checkInputDescriptivoDispersionMinimos"), "Minimo", TRUE)),
                                          
                                   )#Fin columna 2 Valores de Disperson
                                 )
                                 
                    ) # Fin div bloque
                    ), # fin columna 1
                    column(6, div(id= ns("bloqueValoresPorcentilesColumna2"),
                                  
                                  tags$div(tags$span(strong("Tendencia Central"),style = "color: black;")),        
                                  #h5("Tendencia Central"),
                                  
                                  div(  id = ns("checkInputDescriptivoEstadisticaTendenciCentralMedia"),checkboxInput(ns("checkInputDescriptivoEstadisticaTendenciCentralMedias"), "Media", TRUE)),
                                  div( style ="margin-top: -15px;", id =ns("checkInputDescriptivoEstadisticaTendenciCentralMediana"),checkboxInput(ns("checkInputDescriptivoEstadisticaTendenciCentralMedianas"), "Mediana", FALSE)),
                                  div( style ="margin-top: -15px;", id =ns("checkInputDescriptivoEstadisticaTendenciCentralModa"),checkboxInput(ns("checkInputDescriptivoEstadisticaTendenciCentralModas"), "Moda", FALSE)),
                                  div(style ="margin-top: -15px;", id =ns("checkInputDescriptivoEstadisticaTendenciCentralSuma"),checkboxInput(ns("checkInputDescriptivoEstadisticaTendenciCentralSumas"), "Suma", FALSE)),
                    )#Fin columna 2
                    
                    )  #fin columna 2
                    
                  ),  # fin FluidROW de menu sidebr estadisticas
                  
    ), #fin del sidebar panel Descriptivas
    
    mainPanel(style =paste0("background-color: transparent; height: 585px; overflow-y: auto; "),width = 6,
              
              tags$div(tags$span(strong(h3("Estadísticas Descriptivas")),style = "color: black;")),
            
              gt::gt_output(ns("tablaDescriptivos")),
              
              #conditionalPanel( eval(parse(text=paste0("'input.",ns("check_tabla_Frecuencia")," == true'"))), h2("Tabla de Frecuencia"),ns = ns )#, gt::gt_output(ns("tablaDescriptivos_Frecuencias"))),
              conditionalPanel( "input.check_tabla_Frecuencia == true",ns = ns, h2("Tabla de Frecuencia"), gt::gt_output(ns("tablaDescriptivos_Frecuencias"))),
              conditionalPanel("input.checkDistribucions == true", ns = ns, h2("Grafica de Distribucion"),plotOutput(ns("graficaDistribucion"))),
              conditionalPanel("input.checkCorrelacions == true", ns = ns, h2("Grafica de Correlacion"), plotOutput(ns("graficaCorrelacion"))),
              conditionalPanel("input.checkPastels == true", ns = ns, h2("Grafica de Pastel"),plotOutput(ns("graficaPie"))),
              conditionalPanel("input.checkQQs == true", ns = ns, h2("Grafica Q-Q"), plotOutput(ns("graficaQQ"))),
              # COndiciones para Grafica Boxplot
              conditionalPanel("input.checkDispersionEnables == true", ns = ns, h2("Grafica Dispersion"), plotOutput(ns("DescriptivosDispersion"))),

              conditionalPanel("input.checkBoxplotsEnables == true", ns = ns, h2("Boxplots"), plotOutput(ns("DescriptivosBoxplots")))

              
              
    )#fin del main panel
    )
    
    
    
 
  
}
    
#' UIDescriptivos Server Functions
#'
#' @noRd 
mod_UIDescriptivos_server <- function(id, namesClasified, r){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    namesClasified <- NULL
    banderaDescriptivos <- FALSE
    descargaTablasDescriptiva <- reactiveValues( tabla_Descriptivos = NULL, tabla_Descriptivos_Frecuencia = NULL)
    
    
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
    
 
    ############ Metodos de GRAFICAS DESCRIPTIVAS ############
    
    
    # Se grafica las variables selecionadas
    My_Theme = ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 16),
      axis.text.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 16),
      plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
      panel.background = ggplot2::element_rect(fill='transparent'),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
      #legend.background = ggplot2::element_rect(fill = "transparent"), # get rid of legend bg
      #legend.box.background = ggplot2::element_rect(fill = "transparent") # get rid of legend panel bg
    )
    
    My_Theme_No_Bordes = ggplot2::theme(
      axis.title.x=ggplot2::element_blank(),
      axis.text.x=ggplot2::element_blank(),
      axis.ticks.x=ggplot2::element_blank(),
      axis.title.y=ggplot2::element_blank(),
      axis.text.y=ggplot2::element_blank(),
      axis.ticks.y=ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
      panel.background = ggplot2::element_rect(fill='transparent'),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
      
    )
    
    
    output$graficaDistribucion <-renderPlot({
      #class(df_sel2()) 
      
      plot_list = c()
      # print(length(df_Descriptiva_Seleccion_Dependiente()))
      if(input$checkDistribucions){
        for (i in 1:length(df_Descriptiva_Seleccion_Dependiente())){
          
          defaultSelect <- class(df_sel()[[i]])
          print(defaultSelect)
          
          
          
          if(defaultSelect=="numeric" || defaultSelect=="integer"){
            
            nbreaks <- pretty(range.default(as.numeric(df_sel()[[i]])),n = nclass.Sturges(as.numeric(df_sel()[[i]])),min.n = 1)
            print(nbreaks)
            #my_data_num <- as.numeric(df_sel2()[[i]])  
            utils::str("Entro en for de distrubucion")
            
            
            g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(input$rank_list_Descriptiva_Dependiente[[i]])) + ggplot2::geom_histogram(col="red", fill="green", alpha = .2, breaks=nbreaks) + ggplot2::theme_bw() + My_Theme
            
            if(input$checkDensidads){
              
              print("Entro en densidad")
              
              g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(input$rank_list_Descriptiva_Dependiente[[i]])) +  ggplot2::geom_histogram(ggplot2::aes(y=..count../sum(..count..)),col="red", 
                                                                                                                                                       fill="green", 
                                                                                                                                                       alpha = .2,breaks=nbreaks) +  ggplot2::geom_density() +
                ggplot2::theme_bw() + My_Theme
            } 
          }
          else{
            
            g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(input$rank_list_Descriptiva_Dependiente[[i]])) + ggplot2::geom_bar(col="red", fill="green",alpha = .2) + ggplot2::theme_bw() + My_Theme
          }
          
          plot_list[[i]] <- g
        }
        
        cowplot::plot_grid(plotlist = plot_list)
        #utils::str("Entro en distribucion") 
      } 
      else {golem::print_dev("no entro distribucion")}        },bg="transparent")
    
    
    output$graficaCorrelacion <-renderPlot({
      
      
      
      if(input$checkCorrelacions){
        plot_list_corr = c()
        print("Entro en Correlacion")
        
        if(length(df_Descriptiva_Seleccion_Dependiente()) >= 2){
          h <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(input$rank_list_Descriptiva_Dependiente[[2]],input$rank_list_Descriptiva_Dependiente[[1]])) + ggplot2::geom_point() + ggplot2::theme_bw() + My_Theme
          
        }
        else{ 
          
          
          shinyalert::shinyalert("Oops!", "Se necesita dos variables para gr\u00e1fica de correlaci\u00f3n", type = "error",size = "xs")
          h <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
          
        }
        plot_list_corr[[1]] <- h
        cowplot::plot_grid(plotlist = plot_list_corr) } else{utils::str("correlacion no")}    
      
    },bg="transparent")
    
    
    output$graficaQQ <-renderPlot({
      
      
      plot_list_qq = c()
      
      #if(is.null(data_file()[[input$rank_list_Descriptiva_Dependiente[[1]]]])){}
      if(input$checkQQs){
        for (i in 1:length(df_Descriptiva_Seleccion_Dependiente())){
          
          g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(sample=input$rank_list_Descriptiva_Dependiente[[i]])) + ggplot2::stat_qq()
          plot_list_qq[[i]] <- g
        }
        
        utils::str("Entro qq")
        cowplot::plot_grid(plotlist = plot_list_qq)
        
      }
      else{utils::str("no qq")}      },bg="transparent")
    #   
    output$graficaPie <-renderPlot({
      plot_list_pie = c()
      
      #if(is.null(data_file()[[input$rank_list_Descriptiva_Dependiente[[1]]]])){}
      if(input$checkPastels){
        
        for (i in 1:length(df_Descriptiva_Seleccion_Dependiente())){
          
          frq <- plyr::count(r$valuesmydata , input$rank_list_Descriptiva_Dependiente[[i]])
          
          print(frq)
          
          #print(frq[[input$rank_list_Descriptiva_Dependiente[[i]]]])
          # print("cuys")
          # print(frq$freq)
          #g <- ggplot2::ggplot(data = frq, ggplot2::aes_string( y=frq$rank_list_Descriptiva_Dependiente[[i]], fill=frq$freq ) ) + ggplot2::geom_bar( width=1) + ggplot2::coord_polar("y",start=0)
          g= ggplot2::ggplot(data= frq, ggplot2::aes_string(x="1", y= "freq"  ,fill= factor(frq[[input$rank_list_Descriptiva_Dependiente[[i]]]])))+ ggplot2::geom_bar(width = 1,stat="identity") + ggplot2::coord_polar("y",start=0) 
          g= g + ggplot2::geom_text(ggplot2::aes(label = paste0(round(freq), "")), position = ggplot2::position_stack(vjust = 0.5))
          g= g +ggplot2::labs(fill=input$rank_list_Descriptiva_Dependiente[[i]]) + ggplot2::theme_void() # remove background, grid, numeric labels
          
          # g <- pie(frq$freq ,frq[[input$rank_list_Descriptiva_Dependiente[[i]]]],main="city_pie_chart")
          #g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string( y=input$rank_list_Descriptiva_Dependiente[[i]], fill=input$rank_list_Descriptiva_Dependiente[[i]])  ) + ggplot2::geom_bar( width=1) + ggplot2::coord_polar("y",start=0)
          plot_list_pie[[i]] <- g
          
          
        }
        
        utils::str("Entro pies")
        cowplot::plot_grid(plotlist = plot_list_pie)
        
        
      }
      else{utils::str("no pies")}      },bg="transparent")
    
    
    output$DescriptivosBoxplots <- renderPlot({
      
      
      plot_list = c()
      
      
      if(input$checkBoxplotsEnables){
        for (i in 1:length(df_Descriptiva_Seleccion_Dependiente())){
          
          #my_data_num <- as.numeric(df_sel2()[[i]])  
          utils::str("Entro en for de Boxplot")
          
          g <- ggplot2::ggplot() + ggplot2::theme_void()
          
          if(input$checkBoxplotElements && !input$checkBoxplotJitters && !input$checkBoxplotViolins)
          {
            g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + ggplot2::stat_boxplot(color=4, width = 0.2) + ggplot2::theme_bw()  + My_Theme #+ scale_x_discrete(limits = c("-3","0")) +
            
          }
          if(input$checkBoxplotJitters && !input$checkBoxplotElements  && !input$checkBoxplotViolins) 
            
          {
            g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) +  ggplot2::geom_jitter(color=4,width=0.2)  +  ggplot2::theme_bw() + My_Theme
            
          }
          if(input$checkBoxplotViolins  && !input$checkBoxplotElements &&  !input$checkBoxplotJitters)
          {
            g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + ggplot2::geom_violin(color=4,width=0.2,trim = FALSE) +   ggplot2::theme_bw() + My_Theme
          }
          
          if(input$checkBoxplotElements && input$checkBoxplotViolins && !input$checkBoxplotJitters  )
          {
            
            g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + 
              ggplot2::stat_boxplot(color=4, width = 0.2, alpha=0.4) +  ggplot2::geom_violin(color=5,width=0.2,trim = FALSE,alpha=0.4) +
              ggplot2::theme_bw()  + My_Theme 
          }
          if(input$checkBoxplotElements && !input$checkBoxplotViolins && input$checkBoxplotJitters  )
          {
            
            g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + 
              ggplot2::stat_boxplot(color=4, width = 0.2,alpha=0.4) +   ggplot2::geom_jitter(color=5,width=0.2,alpha=0.4) +
              ggplot2::theme_bw()  + My_Theme 
          }
          
          if(!input$checkBoxplotElements && input$checkBoxplotViolins && input$checkBoxplotJitters  )
          {
            
            g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + 
              ggplot2::geom_violin(color=4,width=0.2,trim = FALSE,alpha=0.4)  +   ggplot2::geom_jitter(color=5,width=0.2,alpha=0.4) +
              ggplot2::theme_bw()  + My_Theme 
          }
          
          if(input$checkBoxplotElements && !input$checkBoxplotViolins && input$checkBoxplotJitters  )
          {
            
            g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + 
              ggplot2::stat_boxplot(color=4, width = 0.2,alpha=0.4)  +   ggplot2::geom_jitter(color=5,width=0.2,alpha=0.4) +
              ggplot2::theme_bw()  + My_Theme 
          }
          if(input$checkBoxplotElements && input$checkBoxplotViolins && input$checkBoxplotJitters  )
          {
            
            g <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + 
              ggplot2::stat_boxplot(color=3, width = 0.2,alpha=0.4)  + ggplot2::geom_violin(color=4,width=0.2,trim = FALSE,alpha=0.4) + ggplot2::geom_jitter(color=5,width=0.2,alpha=0.4) +
              ggplot2::theme_bw()  + My_Theme 
          }
          
          plot_list[[i]] <- g
        }
        
        cowplot::plot_grid(plotlist = plot_list)
        #utils::str("Entro en distribucion") 
      } 
      else {golem::print_dev("no entro distribucion")}
      
      
      
      
      
    },bg="transparent")
    
    
    output$DescriptivosDispersion <- renderPlot({
      
      plot_list = c()
      
      # print(length(df_Descriptiva_Seleccion_Dependiente()))
      if(input$checkDispersionEnables){
        print("Entro dispersionsssssss")
        print(length(df_Descriptiva_Seleccion_Dependiente()))
        
        if(length(df_Descriptiva_Seleccion_Dependiente()) == 2){
          utils::str("Entro en for de Dispersion")
          
          h <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[1]], y=input$rank_list_Descriptiva_Dependiente[[2]])) +
            ggplot2::geom_point(size=2, shape=1,colour = 4) + ggplot2::theme_bw() + My_Theme
          
          
          if(input$radioButtonplotEncimas == "den")
          {
            
            
            
            x <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[1]])) +
              ggplot2::geom_density(fill="lightblue") + ggplot2::theme_bw() + My_Theme_No_Bordes
            
            
          }
          if(input$radioButtonplotEncimas == "Hist")
          {
            defaultSelect <- class( r$valuesmydata [[input$rank_list_Descriptiva_Dependiente[[1]] ]])
            golem::print_dev(defaultSelect)
            
            
            if(defaultSelect=="numeric" || defaultSelect=="integer"){
              nbreaks <- pretty(range.default(as.numeric(df_sel()[[1]])),n = nclass.Sturges(as.numeric(df_sel()[[1]])),min.n = 1)
              
              x <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[1]])) + 
                ggplot2::geom_histogram(color = 1, fill = "white",breaks=nbreaks) + ggplot2::theme_bw() + My_Theme_No_Bordes
            }else{
              
              x <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[1]])) + 
                ggplot2::geom_bar(color = 1, fill = "white") + ggplot2::theme_bw() + My_Theme_No_Bordes
              
            }
            
          }
          if(input$radioButtonplotEncimas == "Ning.")
          {
            
            x <- ggplot2::ggplot() + ggplot2::theme_void()
            
          }
          
          
          if(input$radioButtonplotDerechos == "den")
          {
            
            y <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[2]])) + 
              ggplot2::geom_density(fill="lightblue") + ggplot2::theme_bw() + My_Theme_No_Bordes  + ggpubr::rotate()
            #  plot_list[[1]] <- y
            
          }
          if(input$radioButtonplotDerechos == "Hist")
          {
            defaultSelect <- class( r$valuesmydata [[input$rank_list_Descriptiva_Dependiente[[1]] ]])
            golem::print_dev(defaultSelect)
            
            
            if(defaultSelect=="numeric" || defaultSelect=="integer"){
              
              
              nbreaks <- pretty(range.default(as.numeric(df_sel()[[2]])),n = nclass.Sturges(as.numeric(df_sel()[[2]])),min.n = 1)
              y <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[2]])) + 
                ggplot2::geom_histogram(color = 1, fill = "white",breaks=nbreaks) + ggplot2::theme_bw() + My_Theme_No_Bordes + ggpubr::rotate()
              
            }else {
              y <- ggplot2::ggplot(data = r$valuesmydata , ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[2]])) + 
                ggplot2::geom_bar(color = 1, fill = "white") + ggplot2::theme_bw() + My_Theme_No_Bordes + ggpubr::rotate()
              
            }
            
            
          }
          if(input$radioButtonplotDerechos == "Ning.")
          {
            
            y <- ggplot2::ggplot() + ggplot2::theme_void() + ggpubr::rotate()
            
          }
          
          if(input$checkDispersionRegresionEnables){
            
            
            if(input$checkDispersionRegresionIntervalos){
              
              # h <- h + ggplot2::geom_smooth(method = "lm", se = FALSE,level=as.numeric(input$txtInputDispersionRegresionIntervalos))
              if(input$radioButtonplotRegresions == "Smooth"){
                golem::print_dev("dispersionnnnnnnnnnnnnn")
                
                h <- h + ggplot2::stat_smooth(method = "loess",formula = y ~ x, se = TRUE,level=as.numeric(input$txtInputDispersionRegresionIntervalos)/100) + 
                  ggplot2::theme_bw() + My_Theme
              }
              if (input$radioButtonplotRegresions == "Lineal"){
                
                h <- h + ggplot2::stat_smooth(method = "lm", formula = y ~ x,se = TRUE,level=as.numeric(input$txtInputDispersionRegresionIntervalos)/100) + 
                  ggplot2::theme_bw() + My_Theme
                
              } 
              
              
            }
            else
            {
              
              if(input$radioButtonplotRegresions == "Smooth"){
                
                h <- h + ggplot2::stat_smooth(method = "loess", formula = y ~ x,se = FALSE,level=0.95) + ggplot2::theme_bw() + My_Theme
              }
              if (input$radioButtonplotRegresions == "Lineal"){
                
                h <- h + ggplot2::stat_smooth(method = "lm",formula = y ~ x ,se = FALSE,level=0.95) + ggplot2::theme_bw() + My_Theme
                
              } 
            }
          }
          
        }
        else{
          print("ENtro en no dos")
          shinyalert::shinyalert("Oops!", "Se necesita dos variables para gr\u00e1fica de dispersi\u00f3n", type = "error",size = "xs")
          h <- ggplot2::ggplot() + ggplot2::theme_void()
          x <- ggplot2::ggplot() + ggplot2::theme_void()
          y <- ggplot2::ggplot() + ggplot2::theme_void()
          # plot_list[[1]]    <- ggplot2::ggplot() + ggplot2::theme_void()
        }
        
        
        
        ggpubr::ggarrange(x, NULL, h, y, 
                          ncol = 2, nrow = 2,  align = "hv", 
                          widths = c(2, 1), heights = c(1, 2))
        
        # grid.arrange(h,x,y, ncol=2)
        
        
      } 
      else {golem::print_dev("no entro dispersion")}
      
      
      
      
      
    },bg="transparent")
    
    
    ###################     OBSERVER METODOSSSSSSS   ###################
    
    
    observe({
      #Colapsa Graficad Basica de Descriptivos
      
      #print(input$menuGraficas)
      
      if( input$menuGraficas %% 2 == 0){
        
        shinyjs::hide(id = "bloqueDescriptivasBasicas") 
        shinyjs::hide(id = "bloqueDescriptivasPersonalizadas")   
        shinyjs::hide(id = "bloqueValoresPorcentiles") 
        
      }
      else{
        
        shinyjs::show(id = "bloqueDescriptivasBasicas") 
        shinyjs::show(id = "bloqueDescriptivasPersonalizadas")   
        shinyjs::show(id = "bloqueValoresPorcentiles") 
      }
      
      # shinyjs::toggle(id = "bloqueDescriptivasBasicas") 
      #  shinyjs::toggle(id = "bloqueDescriptivasPersonalizadas")   
      
      #shinyjs::toggle(id = "bloqueValoresPorcentiles") 
      
    })
    
    #####ESTA FUNCION COLAPSA LOS CHECKBOX DE Estadistica
    observe({
      
      
      if( input$menuEstadistica %% 2 == 0){
        
        shinyjs::hide(id = "bloqueValoresPorcentilesColumna1") 
        shinyjs::hide(id = "bloqueValoresPorcentilesColumna2")   
        
      }
      else{
        
        shinyjs::show(id = "bloqueValoresPorcentilesColumna1") 
        shinyjs::show(id = "bloqueValoresPorcentilesColumna2")   
        
        
      }
      
      
      #shinyjs::toggle(id = "bloqueValoresPorcentilesColumna1") 
      # shinyjs::toggle(id = "bloqueValoresPorcentilesColumna2") 
      
    })
    
    #####################Activa o desactiva a los Elemento
    observeEvent(input$checkDistribucions, {
      
      shinyjs::toggleState ("checkDensidads")
      
      
    })
    
    observeEvent(input$checkBoxplotsEnables, {
      
      
      shinyjs::toggleState ("checkBoxplotElements")
      shinyjs::toggleState ("checkBoxplotViolins")
      shinyjs::toggleState ("checkBoxplotJitters")
      
      
    })
    
    observeEvent(input$checkDispersionEnables, {
      
      shinyjs::toggleState ("radioButtonplotEncimas")
      shinyjs::toggleState ("radioButtonplotDerechos")
      shinyjs::toggleState ("checkDispersionRegresionEnables")
      
      
      
    })
    
    observeEvent(input$checkDispersionRegresionEnables, {
      
      shinyjs::toggleState ("radioButtonplotRegresions")
      shinyjs::toggleState ("checkDispersionRegresionIntervalos")
      
      
    })
    
    observeEvent(input$checkDispersionRegresionIntervalos, {
      
      shinyjs::toggleState ("txtInputDispersionRegresionIntervalos")
      
    })
    
    observeEvent(input$checkInputDescriptivaEstadisticaPuntoCortes, {
      
      shinyjs::toggleState ("txtInputDescriptivaEstadisticaPuntoCortess")
      
    })
    
    observeEvent(input$checkInputDescriptivaEstadisticaPercentiless, {
      
      shinyjs::toggleState ("txtInputDescriptivaEstadisticaPercentiless")
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
    
    
    observeEvent( r$inputGuardar,{
      
      golem::print_dev("DESCREIPTIVOSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS")
      
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
        
        # golem::print_dev(box)
        
        removeUI(
          selector =  box
        )
        
        
      }
      
      
      for (i in seq(1:length(selectorAddBOX))) {
        
        
        if(tipo[i]==1)
        {
          
          ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =ns(", ui_idAddBOX[i],"),sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Variables : '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
        }
        
        if (tipo[i]==2)
        {
          
          ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =ns(", ui_idAddBOX[i],"),sortable::bucket_list(header = HTML(as.character(  tags$div(tags$span(strong('Variable de Agrupaci\u00f3n : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
          
        }
        
        if (tipo[i] == 3)
        {
          
          ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =ns(", ui_idAddBOX[i],"),sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Variable Dependiente  : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
          
        }
        
        if (tipo[i] == 4)
        {
          
          ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =ns(", ui_idAddBOX[i],"),sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Agrupacion Numerico : '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
          
        }
        
        
        if (tipo[i] == 5)
        {
          
          ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =ns(", ui_idAddBOX[i],"),sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Covariante Numerico : '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
          
        }
        
        if (tipo[i] == 6)
        {
          
          ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =ns(", ui_idAddBOX[i],"),sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong('Covariante  : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
          
        }
        
        if(tipo[i]==7 )
        {
          
          ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id =ns(", ui_idAddBOX[i],"),sortable::bucket_list(header =  HTML(as.character( tags$div(tags$span(strong(' Variables: '),style = paste0('color: black;')),tags$span(class = 'fa-stack', icon('ruler','fa-lg',lib = 'font-awesome'),style='color:blue'),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')) )),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),  sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i]," ,options = sortable::sortable_options( group = list(pull = 'clone',name = 'bucket_list_Descriptiva',put = FALSE)    )))))")
        }
        
        if (tipo[i] == 8 )
        {
          
          ddf  <-  paste0("insertUI(selector=",selectorAddBOX[i],",where='beforeBegin',ui =  tags$div(id = ns(", ui_idAddBOX[i],"),sortable::bucket_list(header =  HTML(as.character(  tags$div(tags$span(strong(' Segmentado por : '),style = paste0('color: black;')),tags$span(class = 'fa-stack',icon('chart-bar','fa-lg',lib = 'font-awesome'), style='color:#a22f2f'),tags$span(class = 'fa-stack',icon('signal','fa-stack-lg', lib = 'font-awesome'),style='color:#a22f2f')))),group_name = ", groupName[i],",orientation = 'horizontal',class = c('default-sortable', ",csstipo[i],"),sortable::add_rank_list(text = '',labels = NULL,input_id =",id_Input[i],",css_id =",cssId[i],"))))")
          
        }
        
        
        eval(parse(text=ddf))
        
      }
      
    })
    
    
    
    if (banderaDescriptivos == FALSE) {
      banderaDescriptivos==TRUE
      
      
      golem::print_dev("Agregando Descriptivos los cuadrios")
      insertUI(
        selector= paste0("#",ns("placeholderDescriptivaDependiente")),
        where="beforeBegin",
        ui = tags$div(id = ns("descriptivaBoxDependiente"),
                      sortable::bucket_list(
                        header = HTML(as.character(  tags$div(tags$span(strong("Variable"),style = paste0('color: black;')),
                                                              tags$span(class = "fa-stack", icon("ruler","fa-lg",lib = "font-awesome"),style="color:grey")) )),
                        group_name = "bucket_list_Descriptiva",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableDependiente"), # add custom style
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_Descriptiva_Dependiente"),
                          css_id = "rank_Descriptiva_Dependiente"
                        ))
        ))
      
      insertUI(
        selector= paste0("#",ns("placeholderDescriptivaAgrupamiento")),
        where="beforeBegin",
        ui = tags$div(id = ns("descriptivaBoxAgrupamiento"),
                      sortable::bucket_list(
                        header = HTML(as.character( tags$div(tags$span(strong("Variable de Agrupaci\u00f3n  "),style = paste0('color: black;')),
                                                             tags$span(class = "fa-stack",icon("chart-bar","fa-lg",lib = "font-awesome"), style="color:grey")) )), 
                        
                        group_name = "bucket_list_Descriptiva",
                        orientation = "horizontal",
                        class = c("default-sortable", "custom-sortableAgrupamiento"), # add custom style
                        
                        
                        sortable::add_rank_list(
                          text = "",
                          labels = NULL,
                          input_id = ns("rank_list_Descriptiva_Agrupamiento"),
                          css_id = "rank_Descriptiva_Agrupamiento"
                        ))
        ))
      
      
      
    }
    
    
    df_sel <- reactive({
      req(input$rank_list_Descriptiva_Dependiente)
      df_sel <- r$valuesmydata %>% dplyr::select(input$rank_list_Descriptiva_Dependiente)
      
    })
    
    # #Seleccion de columnas de pasadas por el filtro
    df_seleccion <- reactive({
      
      req(input$rank_list_Descriptiva_Dependiente)
      df_seleccion <- r$valuesmydata  %>% dplyr::select(input$rank_list_Descriptiva_Dependiente)
      
    })
    
    df_Descriptiva_Seleccion_Agrupamiento <- reactive({
      
      #req(input$rank_list_Descriptiva_Agrupamiento)
      
      if(length(input$rank_list_Descriptiva_Agrupamiento) > 0 ){
        df_Descriptiva_Seleccion_Agrupamiento <- r$valuesmydata  %>% dplyr::select(input$rank_list_Descriptiva_Agrupamiento)
        
        
      } else {
        
        df_Descriptiva_Seleccion_Agrupamiento <- NULL
        
        
      }
      
    })
    
    df_Descriptiva_Seleccion_Dependiente <- reactive({
      
      req(input$rank_list_Descriptiva_Dependiente)
      df_Descriptiva_Seleccion_Dependiente <- r$valuesmydata  %>% dplyr::select(input$rank_list_Descriptiva_Dependiente)
      
    })
    
    
    
    
    # mod_DescriptivosTablas_server("DescriptivosTablas_ui_1")
    table_calculoVariables <- reactive({
      
      calculoVariables(session,r$valuesmydata ,df_Descriptiva_Seleccion_Dependiente(),df_Descriptiva_Seleccion_Agrupamiento(),checkDescriptivasMean(),checkDescriptivoEstadisticaTendenciCentralMediana(),
                       checkDescriptivoEstadisticaTendenciCentralModa(),checkDescriptivoEstadisticaTendenciCentralSuma(),
                       checkDescriptivoDispersionSEmean(),checkDescriptivoDispersionMAD(),checkDescriptivoDispersionIQR(),
                       checkDescriptivoDispersionRange(),checkDescriptivoDispersionMaximo(),checkDescriptivoDispersionDesviacionStd(),
                       checkDescriptivoDispersionMADRobusto(),checkDescriptivoDispersionVarianza(),checkDescriptivoDispersionMinimo(),
                       checkDescriptivoEstadisticaQuartiles(),checkInputDescriptivaEstadisticaPuntoCortes(),
                       txtDescriptivaEstadisticaPuntoCortes(),checkDescriptivaEstadisticaPercentiles(),txtDescriptivaEstadisticaPercentiles()
      )
    })
    
    tabla_calculoFrecuencias <- reactive({
      
      calculoFrecuencias(session,df_Descriptiva_Seleccion_Dependiente(),checkDescriptivasFrecuencia())
      
    })
    
    
    output$tablaDescriptivos_Frecuencias <- gt::render_gt({
      
      descargaTablasDescriptiva$tabla_Descriptivos_Frecuencia <- apaDescriptivos(tabla_calculoFrecuencias(),"Tabla Frecuencias","","DescriptivosfrecuenciaTable")
      
    })
    
    
    output$tablaDescriptivos <-  gt::render_gt({
      
      descargaTablasDescriptiva$tabla_Descriptivos <- apaDescriptivos(table_calculoVariables(), "Estad\u00edsticas Descriptivas","Notas. Estad\u00edsticas Descriptivas","DescriptivosTable")
      
    })
    
    
    descargaUInameDescriptivos <- c("descargaTablaDescriptivos","descargaTablaDescriptivosFrecuencias")
    
    
    uiDescriptivosRadio <- c("uiRadioDescriptivos","uiRadioDescriptivosFrecuencia")
    
    
    
    nombregtWidgetDescriptivos <- c("tablaDescriptivos","tablaDescriptivos_Frecuencias")
    
    
    lapply(1:2, function(i) {
      
      
      shinyjs::onevent("dblclick",nombregtWidgetDescriptivos[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                                                                                        html = TRUE, text = tagList( radioButtons( ns(  uiDescriptivosRadio[i] ), "", 
                                                                                                                                   choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                                                                                                                     downloadButton( ns( descargaUInameDescriptivos[i] ),"Descargar" ))))
    })
    
    
    output$descargaTablaDescriptivos <- downloadHandler (
      
      filename = function() {
        paste0("descriptivos", input$uiRadioDescriptivos)
      },
      
      content = function(file) {
        if(input$uiRadioDescriptivos == ".rtf") {
          descargaTablasDescriptiva$tabla_Descriptivos %>% gt::gtsave("descriptivos.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/descriptivos.rtf"), file)
        } else if (input$uiRadioDescriptivos == ".tex") {
          descargaTablasDescriptiva$tabla_Descriptivos %>% gt::gtsave("descriptivos.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/descriptivos.tex"), file)
          
        }}
    )
    
    
    output$descargaTablaDescriptivosFrecuencias <- downloadHandler (
      
      filename = function() {
        paste0("descriptivos_frecuencia", input$uiRadioDescriptivosFrecuencia)
      },
      
      content = function(file) {
        if(input$uiRadioDescriptivosFrecuencia == ".rtf") {
          descargaTablasDescriptiva$tabla_Descriptivos_Frecuencia %>% gt::gtsave("descriptivos_frecuencia.rtf",  path = tempdir())
          file.copy(paste0(tempdir(),"/descriptivos_frecuencia.rtf"), file)
        } else if (input$uiRadioDescriptivosFrecuencia == ".tex") {
          descargaTablasDescriptiva$tabla_Descriptivos_Frecuencia %>% gt::gtsave("descriptivos_frecuencia.tex",  path = tempdir())
          file.copy(paste0(tempdir(),"/descriptivos_frecuencia.tex"), file)
          
        }
        
      }
    )
    
    
    
    
    
    
   
  })
}
    
## To be copied in the UI
# mod_UIDescriptivos_ui("UIDescriptivos_ui_1")
    
## To be copied in the server
# mod_UIDescriptivos_server("UIDescriptivos_ui_1")

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    
    
    fluidPage(  
      tags$style(type = 'text/css', 
                 HTML(".navbar-nav {width: 80%;}
                                    .navbar-nav :last-child{float:right}")),
      
      #theme = shinythemes::shinytheme("simplex"),
      tags$link(rel="stylesheet", type="text/css", href="www/dark_mode.css"),
        
      navbarPage(
                
                title = div(tags$img(src = "www/g12.png", style= "width: 200px; margin-top: 5px;  height: 50px")),
                windowTitle="Ro-Statistic",
                ###################################################DESCRIPTIVOS########################
         
                eval(parse("./R/Metodos/UI/Descriptivos/UI_Descriptivos.R",encoding =  "UTF-8")),   
             
                ###########################################################INICIO T TEST  #################################################
               
                eval(parse("./R/Metodos/UI/TTest/UI_Ttest.R", encoding="UTF-8")),
                ##########################################################Tablas COntingencia###################
                
                eval(parse("./R/Metodos/UI/TablasContingencia/UI_Contingencia.R", encoding="UTF-8")),
                #########################################################   ANOVA  TEST ####################
                
                eval(parse("./R/Metodos/UI/Anova/UI_Anova.R", encoding="UTF-8")),
                #########################################################   Regresions Test  ####################
                
                eval(parse("./R/Metodos/UI/Regresion/UI_Regresion.R", encoding="UTF-8")),
                #############################################################################################ARCHIVOS#######################
             
                eval(parse("./R/Metodos/UI/CargaDatos/UI_CargaDatos.R", encoding="UTF-8"))
                
              ) #fin NAVBARPAGE
    
    )
    
    
    
    
    
    
    
    )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'RoStatistics'
    ),
    shinyjs::useShinyjs(),
    withMathJax(),
    
    

  )
}


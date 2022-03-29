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
      
     # theme = shinythemes::shinytheme("simplex"),
    # theme = bslib::bs_theme(bootswatch = "flatly" ),
      tags$link(rel="stylesheet", type="text/css", href="www/dark_mode.css"),
        
      navbarPage(
                
                title = div(tags$img(src = "www/g12.png", style= "width: 200px; margin-top: 5px;  height: 50px")),
                windowTitle="Ro-Statistic",
                ###################################################        DESCRIPTIVOS########################
                mod_UIDescriptivos_ui("UIDescriptivos_ui_1"),
                ########################################################### INICIO T TEST  #################################################
                mod_Ttest_ui("Ttest_ui_1"),
                ##########################################################  CONTINGENCIA ####################
                mod_Contingencia_ui("Contingencia_ui_1"),
                ##########################################################   ANOVA   #########################
                mod_Anova_ui("Anova_ui_1"),
                ##########################################################   REGRESION  ####################
                mod_Regresion_ui("Regresion_ui_1"),
                #############################################################################################ARCHIVOS#######################
                mod_cargaDatos_ui("cargaDatos_ui_1")
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
      app_title = 'RO-Statistics'
    ),
    shinyjs::useShinyjs(),
    withMathJax(),
    
    
    

  )
}


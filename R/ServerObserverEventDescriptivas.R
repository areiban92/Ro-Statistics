

observe({
  #Colapsa Graficad Basica de Descriptivos
  
  #print(input$menuGraficas)
  
  if(input$menuGraficas %% 2 == 0){
    
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
  
  
  if(input$menuEstadistica %% 2 == 0){
    
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

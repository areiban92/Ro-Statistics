
observeEvent(input$fisherTestContingenciaEnables, {
  
  shinyjs::toggleState ("radioContingenciaHypotesis")   
  shinyjs::toggleState ("checkInput_Contingencia_Parametro_Ubicacion")
  shinyjs::toggleState ("checkInput_Contingencia_Intervalo_Confidencia")
  shinyjs::toggleState ("txtInput_Contingencia_Intervalo_Confidencia")
  
})

observe({

  if(input$menuTestContingencia %% 2 == 0){

    shinyjs::hide(id = "bloqueTestContingencia")
  }
  else{

    shinyjs::show(id = "bloqueTestContingencia")
  }

})

observe({

  if(input$menuPorcentajesContingencia %% 2 == 0){

    shinyjs::hide(id = "bloquePorcentajesContingencia")
  }
  else{

    shinyjs::show(id = "bloquePorcentajesContingencia")
  }

})
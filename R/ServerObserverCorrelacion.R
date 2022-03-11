observe({

  if(input$menuTestCorrelacion %% 2 == 0){

    shinyjs::hide(id = "bloqueTestCorrelacion")
  }
  else{

    shinyjs::show(id = "bloqueTestCorrelacion")
  }

})

observe({

  if(input$menuOpcionesCorrelacion %% 2 == 0){

    shinyjs::hide(id = "bloqueOpcionesCorrelacion")
  }
  else{

    shinyjs::show(id = "bloqueOpcionesCorrelacion")
  }

})
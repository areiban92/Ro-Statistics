

observeEvent(input$checkInput_Ttest_Parametro_Ubicacion, {
  
 # print("SI SE APLASTO")
  shinyjs::toggleState ("checkInput_Ttest_Intervalo_Confidencia")
  shinyjs::toggleState ("txtInput_Ttest_Intervalo_Confidencia") 

})


observe({

  if(input$menutestTtest %% 2 == 0){
    
    shinyjs::hide(id = "bloqueTestTtest") 
   }
  else{
    
    shinyjs::show(id = "bloqueTestTtest") 
    }
  
})

observe({
  
  if(input$menuSuposicionesTtest %% 2 == 0){
    
    shinyjs::hide(id = "bloqueSuposicionesTtest") 
  }
  else{
    
    shinyjs::show(id = "bloqueSuposicionesTtest") 
  }
  
})
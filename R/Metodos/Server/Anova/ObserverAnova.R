#Event de GUI de ANOVA
observe({
  #Colapsa Graficad Basica de Descriptivos
  
  #print(input$menuGraficas)
  
  if(input$menuPostHocAnova %% 2 == 0){
    
    shinyjs::hide(id = "bloquePostHocAnova") 
   # shinyjs::hide(id = "bloqueOpcionesAnova")   
   
    
  } 
  else
  {
    shinyjs::show(id = "bloquePostHocAnova") 
   # shinyjs::show(id = "bloqueOpcionesAnova") 
    
    
  }

  if(input$menuOpcionesAnova %% 2 == 0){
    

     shinyjs::hide(id = "bloqueOpcionesAnova")   
    
    
  } 
  else
  {
   
     shinyjs::show(id = "bloqueOpcionesAnova") 
    
    
  }
  
  })
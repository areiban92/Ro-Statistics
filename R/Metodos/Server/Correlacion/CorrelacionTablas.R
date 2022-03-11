


output$tablaCorrelacion <- gt::render_gt({
  
  descargaTablasCorrelacion$tabla_Correlacion <<- apaCorrelacion(table_calculoTCorrelacion()[[1]],
                 "Correlacion",
                 table_calculoTCorrelacion()[[2]],"correlacion_tabla")
})


descargaUInameCorrelacion <- c("descargaCorrelacion")

uiCorrelacionRadio  <- c("uiRadioCorrelacion")

nombregtWidgetCorrelacion  <- c("tablaCorrelacion")


lapply(1:2, function(i) {
  
  shinyjs::onevent("dblclick",nombregtWidgetCorrelacion[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                  html = TRUE, text = tagList( radioButtons(uiCorrelacionRadio[i], "", 
                  choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                  downloadButton(descargaUInameCorrelacion[i],"Descargar" ))))
})



output$descargaCorrelacion <- downloadHandler (
  
  filename = function() {
    paste0("correlacion", input$uiRadioCorrelacion)
  },
  
  content = function(file) {
    if(input$uiRadioCorrelacion == ".rtf") {
      descargaTablasCorrelacion$tabla_Correlacion %>% gt::gtsave("correlacion.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/correlacion.rtf"), file )
    } else if (input$uiRadioCorrelacion == ".tex") {
      descargaTablasCorrelacion$tabla_Correlacion %>% gt::gtsave("correlacion.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/correlacion.tex"), file )
      
    }}
)






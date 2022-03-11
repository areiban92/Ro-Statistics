

output$tablaRegresionLineal <- gt::render_gt({
  
  descargaTablasRegresion$tabla_TregresionLineal <<- apaRegresion(table_calculo_RegresionLineal()[[1]],
                                            "Prueba regresionLineal",
                                            table_calculo_RegresionLineal()[[2]],"regresionLineal_Tabla")
})


output$tablaRegresionLinealResumenR <- gt::render_gt({
  
  descargaTablasRegresion$tabla_TregresionLinealResumenR <<- apaRegresion(table_calculo_RegresionLineal()[[4]],
                                                    "Regresion Lineal R summary",
                                                    table_calculo_RegresionLineal()[[2]],"regresionLinealResumenR_Tabla")
})


descargaUInameRegresionLineal <- c("descargaRegresionLineal","descargaRegresionLinealResumen")

uiRegresionLinealRadio  <- c("uiRadioRegresionLineal","uiRadioRegresionLinealResumen")

nombregtWidgetRegresionLineal  <- c("tablaRegresionLineal","tablaRegresionLinealResumenR")


lapply(1:2, function(i) {
  
  shinyjs::onevent("dblclick",nombregtWidgetRegresionLineal[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                html = TRUE, text = tagList( radioButtons(uiRegresionLinealRadio[i], "", 
                choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                downloadButton(descargaUInameRegresionLineal[i],"Descargar" ))))
})



output$descargaRegresionLineal <- downloadHandler (
  
  filename = function() {
    paste0("regresionLineal", input$uiRadioRegresionLineal)
  },
  
  content = function(file) {
    if(input$uiRadioRegresionLineal == ".rtf") {
      descargaTablasRegresion$tabla_TregresionLineal %>% gt::gtsave("regresionLineal.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionLineal.rtf"), file )
    } else if (input$uiRadioRegresionLineal == ".tex") {
      descargaTablasRegresion$tabla_TregresionLineal %>% gt::gtsave("regresionLineal.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionLineal.tex"), file )
      
    }}
)


output$descargaRegresionLinealResumen <- downloadHandler (
  
  filename = function() {
    paste0("regresionLinealResumen", input$uiRadioRegresionLinealResumen)
  },
  
  content = function(file) {
    if(input$uiRadioRegresionLinealResumen == ".rtf") {
      descargaTablasRegresion$tabla_TregresionLinealResumenR %>% gt::gtsave("regresionLinealResumen.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionLinealResumen.rtf"), file )
    } else if (input$uiRadioRegresionLinealResumen == ".tex") {
      descargaTablasRegresion$tabla_TregresionLinealResumenR %>% gt::gtsave("regresionLinealResumen.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionLinealResumen.tex"), file )
      
    }}
)



output$tablaRegresionBinomial <- gt::render_gt({
  
  descargaTablasRegresion$tabla_TregresionBinomial <<- apaRegresion(table_calculo_RegresionBinomial()[[1]],
                            "Prueba regresionBinomial",
                            table_calculo_RegresionBinomial()[[2]],"regresionBinomial_Tabla")
})


output$tablaRegresionBinomialResumenR <- gt::render_gt({
  
  descargaTablasRegresion$tabla_TregresionBinomialResumenR <<- apaRegresion(table_calculo_RegresionBinomial()[[4]],
                                            "Regresion Binomial R summary",
                                            table_calculo_RegresionBinomial()[[2]],"regresionBinomialResumenR_Tabla")
})


descargaUInameRegresionBinomial <- c("descargaRegresionBinomial","descargaRegresionBinomialResumen")

uiRegresionBinomialRadio  <- c("uiRadioRegresionBinomial","uiRadioRegresionBinomialResumen")

nombregtWidgetRegresionBinomial  <- c("tablaRegresionBinomial","tablaRegresionBinomialResumenR")


lapply(1:2, function(i) {
  
  shinyjs::onevent("dblclick",nombregtWidgetRegresionBinomial[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                   html = TRUE, text = tagList( radioButtons(uiRegresionBinomialRadio[i], "", 
                   choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                    downloadButton(descargaUInameRegresionBinomial[i],"Descargar" ))))
})



output$descargaRegresionBinomial <- downloadHandler (
  
  filename = function() {
    paste0("regresionBinomial", input$uiRadioRegresionBinomial)
  },
  
  content = function(file) {
    if(input$uiRadioRegresionBinomial == ".rtf") {
      descargaTablasRegresion$tabla_TregresionBinomial %>% gt::gtsave("regresionBinomial.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionBinomial.rtf"), file )
    } else if (input$uiRadioRegresionBinomial == ".tex") {
      descargaTablasRegresion$tabla_TregresionBinomial %>% gt::gtsave("regresionBinomial.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionBinomial.tex"), file )
      
    }}
)


output$descargaRegresionBinomialResumen <- downloadHandler (
  
  filename = function() {
    paste0("regresionBinomialResumen", input$uiRadioRegresionBinomialResumen)
  },
  
  content = function(file) {
    if(input$uiRadioRegresionBinomialResumen == ".rtf") {
      descargaTablasRegresion$tabla_TregresionBinomialResumenR %>% gt::gtsave("regresionBinomialResumen.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionBinomialResumen.rtf"), file )
    } else if (input$uiRadioRegresionBinomialResumen == ".tex") {
      descargaTablasRegresion$tabla_TregresionBinomialResumenR %>% gt::gtsave("regresionBinomialResumen.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionBinomialResumen.tex"), file )
      
    }}
)


output$tablaRegresionMultinomial <- gt::render_gt({
  
  descargaTablasRegresion$tabla_TregresionMultinomial <<- apaRegresion(table_calculo_RegresionMultinomial()[[1]],
                            "Prueba regresionMultinomial",
                            table_calculo_RegresionMultinomial()[[2]],"regresionMultinomial_Tabla")
})


output$tablaRegresionMultinomialResumenR <- gt::render_gt({
  
  descargaTablasRegresion$tabla_TregresionMultinomialResumenR <<- apaRegresion(table_calculo_RegresionMultinomial()[[4]],
                                            "Regresion Multinomial R summary",
                                            table_calculo_RegresionMultinomial()[[2]],"regresionMultinomialResumenR_Tabla")
})


descargaUInameRegresionMultinomial <- c("descargaRegresionMultinomial","descargaRegresionMultinomialResumen")

uiRegresionMultinomialRadio  <- c("uiRadioRegresionMultinomial","uiRadioRegresionMultinomialResumen")

nombregtWidgetRegresionMultinomial  <- c("tablaRegresionMultinomial","tablaRegresionMultinomialResumenR")


lapply(1:2, function(i) {
  
  shinyjs::onevent("dblclick",nombregtWidgetRegresionMultinomial[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                                                                                         html = TRUE, text = tagList( radioButtons(uiRegresionMultinomialRadio[i], "", 
                                                                                                                                   choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                                                                                                                      downloadButton(descargaUInameRegresionMultinomial[i],"Descargar" ))))
})



output$descargaRegresionMultinomial <- downloadHandler (
  
  filename = function() {
    paste0("regresionMultinomial", input$uiRadioRegresionMultinomial)
  },
  
  content = function(file) {
    if(input$uiRadioRegresionMultinomial == ".rtf") {
      descargaTablasRegresion$tabla_TregresionMultinomial %>% gt::gtsave("regresionMultinomial.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionMultinomial.rtf"), file )
    } else if (input$uiRadioRegresionMultinomial == ".tex") {
      descargaTablasRegresion$tabla_TregresionMultinomial %>% gt::gtsave("regresionMultinomial.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionMultinomial.tex"), file )
      
    }}
)


output$descargaRegresionMultinomialResumen <- downloadHandler (
  
  filename = function() {
    paste0("regresionMultinomialResumen", input$uiRadioRegresionMultinomialResumen)
  },
  
  content = function(file) {
    if(input$uiRadioRegresionMultinomialResumen == ".rtf") {
      descargaTablasRegresion$tabla_TregresionMultinomialResumenR %>% gt::gtsave("regresionMultinomialResumen.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionMultinomialResumen.rtf"), file )
    } else if (input$uiRadioRegresionMultinomialResumen == ".tex") {
      descargaTablasRegresion$tabla_TregresionMultinomialResumenR %>% gt::gtsave("regresionMultinomialResumen.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/regresionMultinomialResumen.tex"), file )
      
    }}
)


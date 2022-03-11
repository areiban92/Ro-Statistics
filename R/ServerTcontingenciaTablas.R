


output$tablaContingencia <- gt::render_gt({
  
  descargaTablasContingencia$tabla_Tcontingencia <- apaContingencia(table_calculoTcontingencia()[[1]],
                  "Tabla de Contingencia",
                  table_calculoTcontingencia()[[2]],"contingencia_Tabla")
})

output$tablaFisherContingencia <- gt::render_gt({
  
  
  descargaTablasContingencia$tabla_Tcontingencia_Fisher <- apaContingenciaFisher(table_calculoFisherContingencia()[[1]],
                        " Prueba de Fisher",
                        table_calculoFisherContingencia()[[2]],"contingenciaFisher_tabla")
})

output$tablaChiCUadradoContingencia <- gt::render_gt({
  
  
  descargaTablasContingencia$tabla_Tcontingencia_Chicuadrado <- apaContingenciaCuadrado(table_estadisticaCalculoContingencia()[[1]],
                          "Prueba Chi-Cuadrado",
                          table_estadisticaCalculoContingencia()[[2]],"contingenciaChi_tabla")
})

output$tablaConteoEsperado <- gt::render_gt({
  
  
  descargaTablasContingencia$table_calculoEsperadoFinal <- apaContingencia(table_calculoEsperado()[[1]],
                                                              "Tabla Conteo Esperado",
                                            table_calculoEsperado()[[2]],"contingencia_ConteoTabla")
})


descargaUInameContingencia <- c("descargaTcontingencia","descargaTcontingenciaChicuadrado",
                    "descargaTcontingenciaFisher","descargaTcontingenciaEsperado" )

uiContingenciaRadio <- c("uiRadioContingencia","uiRadioContingenciaChicuadrado",
             "uiRadioContingenciaFisher","uiRadioContingenciaEsperado")

nombregtWidgetContingencia <- c("tablaContingencia","tablaChiCUadradoContingencia",
                    "tablaFisherContingencia","tablaConteoEsperado")


lapply(1:4, function(i) {
  
  
  shinyjs::onevent("dblclick",nombregtWidgetContingencia[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                   html = TRUE, text = tagList( radioButtons(uiContingenciaRadio[i], "", 
                  choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                  downloadButton(descargaUInameContingencia[i],"Descargar" ))))
})



output$descargaTcontingencia <- downloadHandler (
  
  filename = function() {
    paste0("contingencia", input$uiRadioContingencia)
  },
  
  content = function(file) {
    if(input$uiRadioContingencia == ".rtf") {
      descargaTablasContingencia$tabla_Tcontingencia %>% gt::gtsave("contingencia.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/contingencia.rtf"), file )
    } else if (input$uiRadioContingencia == ".tex") {
      descargaTablasContingencia$tabla_Tcontingencia %>% gt::gtsave("contingencia.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/contingencia.tex"), file )
      
    }}
)



output$descargaTcontingenciaChicuadrado <- downloadHandler (
  
  filename = function() {
    paste0("tablaContingenciaChicuadrado", input$uiRadioContingenciaChicuadrado)
  },
  
  content = function(file) {
    if(input$uiRadioContingenciaChicuadrado == ".rtf") {
      descargaTablasContingencia$tabla_Tcontingencia_Chicuadrado %>% gt::gtsave("tablaContingenciaChicuadrado.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/tablaContingenciaChicuadrado.rtf"), file )
    } else if (input$uiRadioContingenciaChicuadrado == ".tex") {
      descargaTablasContingencia$tabla_Tcontingencia_Chicuadrado %>% gt::gtsave("tablaContingenciaChicuadrado.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/tablaContingenciaChicuadrado.tex"), file )
      
    }}
)




output$descargaTcontingenciaFisher <- downloadHandler (
  
  filename = function() {
    paste0("tablaContingenciaFisher", input$uiRadioContingenciaChicuadrado)
  },
  
  content = function(file) {
    if(input$uiRadioContingenciaChicuadrado == ".rtf") {
      descargaTablasContingencia$tabla_Tcontingencia_Fisher %>% gt::gtsave("tablaContingenciaFisher.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/tablaContingenciaFisher.rtf"), file )
    } else if (input$uiRadioContingenciaChicuadrado == ".tex") {
      descargaTablasContingencia$tabla_Tcontingencia_Fisher %>% gt::gtsave("tablaContingenciaFisher.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/tablaContingenciaFisher.tex"), file )
      
    }}
)



output$descargaTcontingenciaEsperado <- downloadHandler (
  
  filename = function() {
    paste0("tablaContingenciaConteoEsperado", input$uiRadioContingenciaEsperado)
  },
  
  content = function(file) {
    if(input$uiRadioContingenciaEsperado == ".rtf") {
      descargaTablasContingencia$table_calculoEsperadoFinal %>% gt::gtsave("tablaContingenciaConteoEsperado.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/tablaContingenciaConteoEsperado.rtf"), file )
    } else if (input$uiRadioContingenciaEsperado == ".tex") {
      descargaTablasContingencia$table_calculoEsperadoFinal %>% gt::gtsave("tablaContingenciaConteoEsperado.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/tablaContingenciaConteoEsperado.tex"), file )
      
    }}
)



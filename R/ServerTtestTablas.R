#################### Metodos para Tablas

output$ttesttabla  <- gt::render_gt({
  
  descargaTablasTtest$tabla_Ttest <- apaTtestConfidencia(
    tablaTtestFinal()[[1]],
    "Muestras Agrupamientos T-test",
    tablaTtestFinal()[[2]],
    tablaTtestFinal()[[3]],"ttestConfidencia_tabla")
  
})

output$table_tTest_VerifSupos_Normalidad <- gt::render_gt({
  
  
  descargaTablasTtest$tabla_Ttest_Normalidad <- apaTtest(
    tabla_calculo_Ttest_Suposiciones_Normal()[[1]],
    "Prueba de Igualdad de Normalidad (Shapiro Wilk)",
    tabla_calculo_Ttest_Suposiciones_Normal()[[2]],"ttestNormalidadShapiro_tabla"
  )
  
  
})    ####AQuIII seguir modificando despues

output$table_tTest_VerifSupos_VarianzasIguales <- gt::render_gt({
  
  descargaTablasTtest$tabla_Ttest_VarianzaIgual <-  apaTtest(table_calculo_Ttest_Suposiciones_VarianzaIgual()[[1]],
           "Prueba de Igualdad de Varianza (Levene Test)",
           table_calculo_Ttest_Suposiciones_VarianzaIgual()[[2]],"ttestVarianzaLevene_tabla")
  
} )

output$estadisticasTtest <- gt::render_gt ({
  
  descargaTablasTtest$tabla_Ttest_Estadistica <- apaTtest(
    table_calculo_Ttest_Stadisticas()[[1]],
    "Estadistica Descriptiva ",
    table_calculo_Ttest_Stadisticas()[[2]],"ttestEstadistica_table"
  )
  
})



descargaUInameTtest <- c("descargaTablaTtest","descargaTablaTtestNormalidad",
                    "descargaTablaTtestVarianzaIgual","descargaTablaTtestEstadistica" )

uiTtestRadio <- c("uiRadioTtest","uiRadioTtestNormalidad",
             "uiRadioTtestVarianza","uiRadioTtestEstadistica")

                        
nombregtWidgetTtest <- c("ttesttabla","table_tTest_VerifSupos_Normalidad",
                    "table_tTest_VerifSupos_VarianzasIguales","estadisticasTtest")


lapply(1:4, function(i) {
  
  
  shinyjs::onevent("dblclick",nombregtWidgetTtest[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                             html = TRUE, text = tagList( radioButtons(uiTtestRadio[i], "", 
                             choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                              downloadButton(descargaUInameTtest[i],"Descargar" ))))
})


output$descargaTablaTtest <- downloadHandler (
  
  filename = function() {
    paste0("ttest", input$uiRadioTtest)
  },
  
  content = function(file) {
    if(input$uiRadioTtest == ".rtf") {
      descargaTablasTtest$tabla_Ttest %>% gt::gtsave("ttest.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/ttest.rtf"), file )
    } else if (input$uiRadioTtest == ".tex") {
      descargaTablasTtest$tabla_Ttest %>% gt::gtsave("ttest.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/ttest.tex"), file )
      
    }}
)



output$descargaTablaTtestNormalidad <- downloadHandler (
  
  filename = function() {
    paste0("ttestNormalidad", input$uiRadioTtestNormalidad)
  },
  
  content = function(file) {
    if(input$uiRadioTtestNormalidad == ".rtf") {
      descargaTablasTtest$tabla_Ttest_Normalidad %>% gt::gtsave("ttestNormalidad.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/ttestNormalidad.rtf"), file )
    } else if (input$uiRadioTtestNormalidad == ".tex") {
      descargaTablasTtest$tabla_Ttest_Normalidad %>% gt::gtsave("ttestNormalidad.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/ttestNormalidad.tex"), file )
      
    }}
)




output$descargaTablaTtestVarianzaIgual <- downloadHandler (
  
  filename = function() {
    paste0("ttestVarianza", input$uiRadioTtestVarianza)
  },
  
  content = function(file) {
    if(input$uiRadioTtestVarianza == ".rtf") {
      descargaTablasTtest$tabla_Ttest_VarianzaIgual %>% gt::gtsave("ttestVarianza.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/ttestVarianza.rtf"), file )
    } else if (input$uiRadioTtestVarianza == ".tex") {
      descargaTablasTtest$tabla_Ttest_VarianzaIgual %>% gt::gtsave("ttestVarianza.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/ttestVarianza.tex"), file )
      
    }}
)



output$descargaTablaTtestEstadistica <- downloadHandler (
  
  filename = function() {
    paste0("ttestEstadistica", input$uiRadioTtestEstadistica)
  },
  
  content = function(file) {
    if(input$uiRadioTtestEstadistica == ".rtf") {
      descargaTablasTtest$tabla_Ttest_Estadistica %>% gt::gtsave("ttestEstadistica.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/ttestEstadistica.rtf"), file )
    } else if (input$uiRadioTtestEstadistica == ".tex") {
      descargaTablasTtest$tabla_Ttest_Estadistica %>% gt::gtsave("ttestEstadistica.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/ttestEstadistica.tex"), file )
      
    }}
)



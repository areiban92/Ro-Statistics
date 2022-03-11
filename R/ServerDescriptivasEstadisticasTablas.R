table_calculoVariables <- reactive({
  
  calculoVariables(session,values$mydata,df_Descriptiva_Seleccion_Dependiente(),df_Descriptiva_Seleccion_Agrupamiento(),checkDescriptivasMean(),checkDescriptivoEstadisticaTendenciCentralMediana(),
                   checkDescriptivoEstadisticaTendenciCentralModa(),checkDescriptivoEstadisticaTendenciCentralSuma(),
                   checkDescriptivoDispersionSEmean(),checkDescriptivoDispersionMAD(),checkDescriptivoDispersionIQR(),
                   checkDescriptivoDispersionRange(),checkDescriptivoDispersionMaximo(),checkDescriptivoDispersionDesviacionStd(),
                   checkDescriptivoDispersionMADRobusto(),checkDescriptivoDispersionVarianza(),checkDescriptivoDispersionMinimo(),
                   checkDescriptivoEstadisticaQuartiles(),checkInputDescriptivaEstadisticaPuntoCortes(),
                   txtDescriptivaEstadisticaPuntoCortes(),checkDescriptivaEstadisticaPercentiles(),txtDescriptivaEstadisticaPercentiles()
  )
})

tabla_calculoFrecuencias <- reactive({
  
 calculoFrecuencias(session,df_Descriptiva_Seleccion_Dependiente(),checkDescriptivasFrecuencia())
  
})


output$tablaDescriptivos_Frecuencias <- gt::render_gt({
  
  descargaTablasDescriptiva$tabla_Descriptivos_Frecuencia <- apaDescriptivos(tabla_calculoFrecuencias(),"Tabla Frecuencias","","DescriptivosfrecuenciaTable")
    
}) 


output$tablaDescriptivos <-  gt::render_gt({
  
  descargaTablasDescriptiva$tabla_Descriptivos <- apaDescriptivos(table_calculoVariables(), "Estad\u00edsticas Descriptivas","Notas. Estad\u00edsticas Descriptivas","DescriptivosTable")
  
}) 


descargaUInameDescriptivos <- c("descargaTablaDescriptivos","descargaTablaDescriptivosFrecuencias")
                    

uiDescriptivosRadio <- c("uiRadioDescriptivos","uiRadioDescriptivosFrecuencia")
                  


nombregtWidgetDescriptivos <- c("tablaDescriptivos","tablaDescriptivos_Frecuencias")
                    

lapply(1:2, function(i) {
  
  
shinyjs::onevent("dblclick",nombregtWidgetDescriptivos[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                html = TRUE, text = tagList( radioButtons(uiDescriptivosRadio[i], "", 
             choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
            downloadButton(descargaUInameDescriptivos[i],"Descargar" ))))
})


output$descargaTablaDescriptivos <- downloadHandler (
  
  filename = function() {
    paste0("descriptivos", input$uiRadioDescriptivos)
  },
  
  content = function(file) {
    if(input$uiRadioDescriptivos == ".rtf") {
      descargaTablasDescriptiva$tabla_Descriptivos %>% gt::gtsave("descriptivos.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/descriptivos.rtf"), file)
    } else if (input$uiRadioDescriptivos == ".tex") {
      descargaTablasDescriptiva$tabla_Descriptivos %>% gt::gtsave("descriptivos.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/descriptivos.tex"), file)
      
    }}
)


output$descargaTablaDescriptivosFrecuencias <- downloadHandler (

  filename = function() {
    paste0("descriptivos_frecuencia", input$uiRadioDescriptivosFrecuencia)
  },

  content = function(file) {
    if(input$uiRadioDescriptivosFrecuencia == ".rtf") {
      descargaTablasDescriptiva$tabla_Descriptivos_Frecuencia %>% gt::gtsave("descriptivos_frecuencia.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/descriptivos_frecuencia.rtf"), file)
    } else if (input$uiRadioDescriptivosFrecuencia == ".tex") {
      descargaTablasDescriptiva$tabla_Descriptivos_Frecuencia %>% gt::gtsave("descriptivos_frecuencia.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/descriptivos_frecuencia.tex"), file)

    }

  }
 )









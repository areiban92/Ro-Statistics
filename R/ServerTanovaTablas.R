

output$tablaAnova <- gt::render_gt({
  
  descargaTablasAnova$tabla_Tanova <- apaAnova(table_calculo_Anova()[[1]],
                                          "Prueba Anova",
                            table_calculo_Anova()[[2]],"anova_Tabla")
})

output$tablaAnovaDescriptivas <- gt::render_gt({
  
  descargaTablasAnova$tabla_TanovaDescriptivas <- apaAnova(table_calculo_Anova_Stadisticas()[[1]],
                            "Estadistica Anova",
                            table_calculo_Anova_Stadisticas()[[2]],"anova_Tabla_estadistica")
})

output$boxplotsAnova <-renderPlot({ 
  
  
  table_calculo_Anova_Stadisticas()[[3]]
  
  
}, bg="transparent")

output$tablaAnovaPostHocIguales <- gt::render_gt({
  
  descargaTablasAnova$tabla_Tanova_PostHoc_Iguales <- apaAnova(table_calculo_Anova_PostHoc_Iguales()[[1]],
                            "Prueba Post Hoc Comparaci\u00f3n",
                            table_calculo_Anova_PostHoc_Iguales()[[2]],"anova_Tabla_PostHocIguales")
})


output$tablaAnovaPostHocDesiguales <- gt::render_gt({
  
  descargaTablasAnova$tabla_Tanova_PostHoc_Desiguales <- apaAnova(table_calculo_Anova_PostHoc_Desiguales()[[1]],
                                    "Prueba Post Hoc de Games-Howell",
                                    table_calculo_Anova_PostHoc_Desiguales()[[2]],"anova_Tabla_PostHocDesigual")
})


output$tablaAnovaHomogeniedad <- gt::render_gt({
  
  descargaTablasAnova$tabla_Tanova_Homogeniedad <-  apaAnova(table_calculo_Anova_Suposiciones_VarianzaIgual()[[1]],
                                          "Prueba de Igualdad de Varianza (Levene Test)",
                                          table_calculo_Anova_Suposiciones_VarianzaIgual()[[2]],"anova_Tabla_Homogenidad")
  
} )

output$tablaAnovaWelch <- gt::render_gt({
  
  descargaTablasAnova$tabla_TanovaWelch <- apaAnova(table_calculo_Anova_Welch()[[1]],
                            "Prueba Anova Welch",
                            table_calculo_Anova()[[2]],"anova_Tabla_Welch")
})

output$tablaAnovaBrown <- gt::render_gt({
  
  descargaTablasAnova$tabla_TanovaBrown <- apaAnova(table_calculo_Anova_Brown()[[1]],
                                 "Prueba Anova Brown",
                                 table_calculo_Anova_Brown()[[2]],"anova_Tabla_Brown")
})


descargaUInameAnova <- c("descargaAnova","descargaAnovaDescriptivas","descargaAnovaPostHocIguales",
                         "descargaAnovaPostHocDesiguales","descargaAnovaHomogeniedad","descargaAnovaWelch",
                         "descargaAnovaBrown"
                         )

uiAnovaRadio  <- c("uiRadioAnova","uiRadioAnovaDescriptivas", "uiRadioAnovaPostHocIguales",
                   "uiRadioAnovaPostHocDesiguales","uiRadioAnovaHomogeniedad","uiAnovaRadioWelch",
                   "uiRadioAnovaBrown")

nombregtWidgetAnova  <- c("tablaAnova","tablaAnovaDescriptivas","tablaAnovaPostHocIguales",
                                "tablaAnovaPostHocDesiguales","tablaAnovaHomogeniedad","tablaAnovaWelch",
                          "tablaAnovaBrown"  )

lapply(1:7, function(i) {
  
  shinyjs::onevent("dblclick",nombregtWidgetAnova[i], shinyalert::shinyalert(title = "Descarga:",size = "xs",
                  html = TRUE, text = tagList( radioButtons(uiAnovaRadio[i], "", 
                  choices = c("rtf" = ".rtf","latex" = ".tex" ),inline = TRUE),
                  downloadButton(descargaUInameAnova[i],"Descargar" ))))
})


output$descargaAnovaBrown <- downloadHandler (
  
  filename = function() {
    paste0("anovaBrown", input$uiRadioAnovaBrown)
  },
  
  content = function(file) {
    if(input$uiRadioAnovaBrown == ".rtf") {
      descargaTablasAnova$tabla_TanovaBrown %>% gt::gtsave("anovaBrown.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovaBrown.rtf"), file )
    } else if (input$uiRadioAnovaBrown == ".tex") {
      descargaTablasAnova$tabla_TanovaBrown %>% gt::gtsave("anovaBrown.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovaBrown.tex"), file )
      
    }}
)







output$descargaAnovaWelch <- downloadHandler (
  
  filename = function() {
    paste0("anovaWelch", input$uiAnovaRadioWelch)
  },
  
  content = function(file) {
    if(input$uiAnovaRadioWelch == ".rtf") {
      descargaTablasAnova$tabla_TanovaWelch %>% gt::gtsave("anovaWelch.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovaWelch.rtf"), file )
    } else if (input$uiAnovaRadioWelch == ".tex") {
      descargaTablasAnova$tabla_TanovaWelch %>% gt::gtsave("anovaWelch.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovaWelch.tex"), file )
      
    }}
)





output$descargaAnovaHomogeniedad <- downloadHandler (
  
  filename = function() {
    paste0("anovahomog", input$uiRadioAnovaHomogeniedad)
  },
  
  content = function(file) {
    if(input$uiRadioAnovaHomogeniedad == ".rtf") {
      descargaTablasAnova$tabla_Tanova_Homogeniedad %>% gt::gtsave("anovahomog.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovahomog.rtf"), file )
    } else if (input$uiRadioAnovaHomogeniedad == ".tex") {
      descargaTablasAnova$tabla_Tanova_Homogeniedad %>% gt::gtsave("anovahomog.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovahomog.tex"), file )
      
    }}
)


output$descargaAnovaPostHocDesiguales <- downloadHandler (
  
  filename = function() {
    paste0("anovaPosthoc2", input$uiRadioAnovaPostHocDesiguales)
  },
  
  content = function(file) {
    if(input$uiRadioAnovaPostHocDesiguales == ".rtf") {
      descargaTablasAnova$tabla_Tanova_PostHoc_Desiguales %>% gt::gtsave("anovaPosthoc2.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovaPosthoc2.rtf"), file )
    } else if (input$uiRadioAnovaPostHocDesiguales == ".tex") {
      descargaTablasAnova$tabla_Tanova_PostHoc_Desiguales %>% gt::gtsave("anovaPosthoc2.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovaPosthoc2.tex"), file )
      
    }}
)



output$descargaAnovaPostHocIguales <- downloadHandler (
  
  filename = function() {
    paste0("anovaPosthoc1", input$uiRadioAnovaPostHocIguales)
  },
  
  content = function(file) {
    if(input$uiRadioAnovaPostHocIguales == ".rtf") {
      descargaTablasAnova$tabla_Tanova_PostHoc_Iguales %>% gt::gtsave("anovaPosthoc1.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovaPosthoc1.rtf"), file )
    } else if (input$uiRadioAnovaPostHocIguales == ".tex") {
      descargaTablasAnova$tabla_Tanova_PostHoc_Iguales %>% gt::gtsave("anovaPosthoc1.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovaPosthoc1.tex"), file )
      
    }}
)



output$descargaAnova <- downloadHandler (
  
  filename = function() {
    paste0("anova", input$uiRadioAnova)
  },
  
  content = function(file) {
    if(input$uiRadioAnova == ".rtf") {
      descargaTablasAnova$tabla_Tanova %>% gt::gtsave("anova.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/anova.rtf"), file )
    } else if (input$uiRadioAnova == ".tex") {
      descargaTablasAnova$tabla_Tanova %>% gt::gtsave("anova.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/anova.tex"), file )
      
    }}
)

output$descargaAnovaDescriptivas <- downloadHandler (
  
  filename = function() {
    paste0("anovaDescriptiva", input$uiRadioAnovaDescriptivas)
  },
  
  content = function(file) {
    if(input$uiRadioAnovaDescriptivas == ".rtf") {
      descargaTablasAnova$tabla_TanovaDescriptivas %>% gt::gtsave("anovaDescriptiva.rtf",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovaDescriptiva.rtf"), file )
    } else if (input$uiRadioAnovaDescriptivas == ".tex") {
      descargaTablasAnova$tabla_TanovaDescriptivas %>% gt::gtsave("anovaDescriptiva.tex",  path = tempdir())
      file.copy(paste0(tempdir(),"/anovaDescriptiva.tex"), file )
      
    }}
)



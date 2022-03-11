calculoTRegresionMultinomial <- function(session, df, Agrupamiento, Dependiente) {
  
  sumaDeCuadrados = c()
  mediaCuadrada = c()
  valorF = c()
  valorP = c()
  
  nombreAgrupamiento <- c(names(Agrupamiento))
  nombreDependiente <- c(names(Dependiente))[[1]]
  
  levelsDependiente <- length(sapply(Dependiente, levels))
  levelsDependienteNombre <- sapply(Dependiente, levels)
  levelsAgrupamiento <- length(sapply(Agrupamiento, levels))
  levelsAgrupamientoNombre <- sapply(Agrupamiento, levels)
  
  casos <- c(nombreAgrupamiento,"residuales")
  print_dev("dsde aqui Regresion Multinomial")
  
  print_dev(class(nombreAgrupamiento))
  print_dev("dsde aqui Regresion Multinomial")
  print_dev(levelsDependienteNombre)
  #print_dev(nombreDependiente)
  
  if (levelsDependiente > 2) {
    
  
    df$nivel <- relevel(df[[nombreDependiente]] , ref=input$inputNivelReferencia)
      if(length(nombreAgrupamiento)  > 1 ){
      
      txtMultinomialLogit <- paste0("nivel"," ~ ",paste(nombreAgrupamiento, collapse = '+'))
      print_dev(txtMultinomialLogit)
    }
    else
    {
      #txtMultinomial <- paste0(nombreDependiente,"~ ",nombreAgrupamiento[[1]])
      txtMultinomialLogit <- paste0("nivel"," ~ ",nombreAgrupamiento[[1]])
      print_dev(txtMultinomialLogit)
    }
  
    multi_model <- nnet::multinom(eval(parse(text = txtMultinomialLogit)), data= df )
    txtMultinomial <- nnet::paste0(nombreDependiente,"~ 1")
    
    multi_model_empty <- multinom( eval(parse(text = txtMultinomial)), data= df  )
    
    #print_dev(utils::str(multi_model))
    modelSummaryMultinomial <- summary(multi_model)
   # print_dev(modelSummaryMultinomial)
    print_dev(modelSummaryMultinomial)
    
    lmMultinomialEstimate <- as.data.frame(modelSummaryMultinomial[["coefficients"]][,])
    nombresRow <- rownames(lmMultinomialEstimate)
    
    estimate <-c(t(lmMultinomialEstimate))   
    
    lmMultinomialSE <- as.data.frame(modelSummaryMultinomial[["standard.errors"]][,])
    
    se <-c(t(lmMultinomialSE))   
    z <- estimate/se
    p <- (1 - pnorm(abs(z), 0, 1)) * 2
    
    nombres <- c("Intercep",nombreAgrupamiento)
    print_dev(paste0("nombres es : ", nombres))
    
    nombreRowss <- c()
    suma <- length(nombres)
    print_dev(paste0("suma es : ", suma) )
    
    ayudavector <- c(1)
    for (variable in 2:length(nombresRow)) {
      
      ayudavector[variable] <-  ayudavector[variable-1] + suma
      
    }
    
    final <- c()
    for (variable in seq(1:(length(nombresRow)*suma) )) {
      
      if(variable %in% ayudavector){
        
        final[variable] <-nombresRow[match(variable,ayudavector)]
        #print_dev(match(variable,ayudavector))
        print_dev("Si es igual")  
        
      }
      else{
        
        final[variable] <- ""
        
      }
      
    }
    
    print_dev(final)
    # nombreRowss[variable] = nombresRow[variable]
    #  nombreRowss[i+1] = ""
    lmMultinomial <- data.frame(final,nombres,estimate,se,z,p)
    lmMultinomial <- dplyr::mutate(lmMultinomial, dplyr::across(where(is.numeric), round, 3))
    lmMultinomial["p"][lmMultinomial["p"] <= 0.01] <- "<0.001"
    
    aiclmMultinomial <- modelSummaryMultinomial$AIC
    deviancelmMultinomial <- modelSummaryMultinomial$deviance
    
    rCuadradoMultinomial <- 1 - logLik(multi_model)/logLik(multi_model_empty)
    
    resumenR <- data.frame(aiclmMultinomial,deviancelmMultinomial,rCuadradoMultinomial)
    colnames(resumenR) <- c("AIC", "Deviance", "R² McF" )  #$$ R^{2}_{McF} $$"  )  
    resumenR <- dplyr::mutate(resumenR, dplyr::across(where(is.numeric), round, 3))
    
    
  }
  else {
    shinyalert::shinyalert(
      title = "Error ",
      text = "La variable Dependiente tiene mas de dos factores",
      size = "xs", 
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    
    nombreVariablesEmpty <- " "
    lmMultinomial <- data.frame(nombreVariablesEmpty )
    colnames(lmMultinomial) <- c("   ")
    
    rCuadradolmMultinomialEmpty <- "  "
    resumenR <- data.frame(rCuadradolmMultinomialEmpty)
    
  }
  
  return(list(lmMultinomial," ",FALSE,resumenR))
  
}

table_calculo_RegresionMultinomial <- reactive({
  
  calculoTRegresionMultinomial(session,values$mydata,
                            df_RegresionMultinomial_Seleccion_Agrupamiento(),
                            
                            df_RegresionMultinomial_Seleccion_Dependiente()   )
  
})



output$nivelRefenciaRegresionMultinomial <- renderUI({
  
  
  selectInput(
    inputId="inputNivelReferencia",
    label= "Variable de Referencia ",
    choices= sapply(df_RegresionMultinomial_Seleccion_Dependiente(), levels)
   
  )
  
  
})
calculoTRegresionBinomial <- function(session, df, Agrupamiento, Dependiente) {
  
  #print_dev("dependenciaaaaaaaa de binomial")
  #print_dev(Dependiente)
  
  casos = c()
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
  print_dev("dsde aqui Regresion Binomial")
  
 print_dev(class(nombreAgrupamiento))
 print_dev("dsde aqui Regresion Binomial")
 print_dev(levelsDependienteNombre)
 
 if (levelsDependiente <= 2) {
 
 
  if(length(nombreAgrupamiento)  > 1 ){
    
      
      print_dev(paste(nombreAgrupamiento, collapse = '+'))
 
    
    txtBinomial <- paste0(nombreDependiente,"~ ",paste(nombreAgrupamiento, collapse = '+'))
  }
  else
  {
    txtBinomial <- paste0(nombreDependiente,"~ ",nombreAgrupamiento[[1]])
    
    }
  #txtBinomial <- paste0(nombreDependiente,"~ ",nombreAgrupamiento)
   txtBinomial2 <- paste0(nombreDependiente,"~ ","1")
   
  model <- glm(eval(parse(text = txtBinomial)), data= df, family = binomial )
  model2 <- glm(eval(parse(text = txtBinomial2)), data= df, family = binomial )
  
  modelSummaryBinomial <- summary(model)
  print_dev(modelSummaryBinomial)
  
  lmBinomial <- as.data.frame(modelSummaryBinomial[["coefficients"]][,])
  nombreVariablesBinomial <- rownames(lmBinomial)
  lmBinomial <- data.frame(nombreVariablesBinomial, lmBinomial )
  colnames(lmBinomial) <- c(" ","Coeficientes","Error.Est","Z","p")
  lmBinomial <- dplyr::mutate(lmBinomial, dplyr::across(where(is.numeric), round, 3))
  lmBinomial["p"][lmBinomial["p"] <= 0.01] <- "<0.001"
  
  
  aiclmBinomial <- modelSummaryBinomial$aic
  deviancelmBinomial <-modelSummaryBinomial$deviance
  rCuadradoBinomial <- 1- logLik(model)/logLik(model2)
  
  resumenR <- data.frame(aiclmBinomial,deviancelmBinomial,rCuadradoBinomial)
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
   lmBinomial <- data.frame(nombreVariablesEmpty )
   colnames(lmBinomial) <- c("   ")
   
   rCuadradolmBinomialEmpty <- "  "
   resumenR <- data.frame(rCuadradolmBinomialEmpty)
   
 }

  return(list(lmBinomial," ",FALSE,resumenR))
  
}

table_calculo_RegresionBinomial <- reactive({
  
  calculoTRegresionBinomial(session,values$mydata,
                            df_RegresionBinomial_Seleccion_Agrupamiento(),
                            
                            df_RegresionBinomial_Seleccion_Dependiente()   )
  
})
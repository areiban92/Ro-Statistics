#' regresionFuntions 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd



calculoTRegresionLineal <- function(session, df, Agrupamiento, Dependiente) {
  
  

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
  golem::print_dev("dsde aqui Regresion Lineal")
  
  if(length(nombreAgrupamiento)  > 1 ){
    
    golem::print_dev(paste(nombreAgrupamiento, collapse = '+'))
    txtLineal <- paste0(nombreDependiente,"~ ",paste(nombreAgrupamiento, collapse = '+'))
  }
  else
  {
    txtLineal <- paste0(nombreDependiente,"~ ",nombreAgrupamiento[[1]])
  }
  #txtLineal <- paste0(nombreDependiente,"~ ",nombreAgrupamiento)
  
  model <- lm(eval(parse(text = txtLineal)), data= df )
  
  modelSummaryLineal <- summary(model)
  lmLineal <- as.data.frame(modelSummaryLineal[["coefficients"]][,])
  nombreVariables <- rownames(lmLineal)
  lmLineal <- data.frame(nombreVariables, lmLineal )
  colnames(lmLineal) <- c(" ","Coeficientes","Error.Est","t","p")
  
  lmLineal <- dplyr::mutate(lmLineal, dplyr::across(where(is.numeric), round, 3))
  lmLineal["p"][lmLineal["p"] <= 0.01] <- "<0.001"
  
  
  rCuadradolmLineal <- modelSummaryLineal[["r.squared"]]
  rCuadradoAdjlmLineal <- modelSummaryLineal[["adj.r.squared"]] 
  
  
  resumenR <- data.frame(rCuadradolmLineal,rCuadradoAdjlmLineal)
  colnames(resumenR) <- c("R", "R²")
  resumenR <- dplyr::mutate(resumenR, dplyr::across(where(is.numeric), round, 3))
  
  return(list(lmLineal," ",FALSE,resumenR))
  
}


calculoTRegresionBinomial <- function(session, df, Agrupamiento, Dependiente) {
  

  
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
  golem::print_dev("dsde aqui Regresion Binomial")
  
  golem::print_dev(class(nombreAgrupamiento))
  golem::print_dev("dsde aqui Regresion Binomial")
  golem::print_dev(levelsDependienteNombre)
  
  if (levelsDependiente <= 2) {
    
    
    if(length(nombreAgrupamiento)  > 1 ){
      
      
      golem::print_dev(paste(nombreAgrupamiento, collapse = '+'))
      
      
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
    golem::print_dev(modelSummaryBinomial)
    
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


calculoTRegresionMultinomial <- function(session, df, Agrupamiento, Dependiente, selector ) {
  

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
  golem::print_dev("dsde aqui Regresion Multinomial")
  
  golem::print_dev(class(nombreAgrupamiento))
  golem::print_dev("dsde aqui Regresion Multinomial")
  golem::print_dev(levelsDependienteNombre)
  #golem::print_dev(nombreDependiente)
  
  if (levelsDependiente > 2) {
    
    
    df$nivel <- relevel(df[[nombreDependiente]] , ref= selector)
    if(length(nombreAgrupamiento)  > 1 ){
      
      txtMultinomialLogit <- paste0("nivel"," ~ ",paste(nombreAgrupamiento, collapse = '+'))
      golem::print_dev(txtMultinomialLogit)
    }
    else
    {
      #txtMultinomial <- paste0(nombreDependiente,"~ ",nombreAgrupamiento[[1]])
      txtMultinomialLogit <- paste0("nivel"," ~ ",nombreAgrupamiento[[1]])
      golem::print_dev(txtMultinomialLogit)
    }
    
    multi_model <- nnet::multinom(eval(parse(text = txtMultinomialLogit)), data= df )
    txtMultinomial <- paste0(nombreDependiente,"~ 1")
    
    multi_model_empty <- nnet::multinom( eval(parse(text = txtMultinomial)), data= df  )
    
    #golem::print_dev(utils::str(multi_model))
    modelSummaryMultinomial <- summary(multi_model)
    # golem::print_dev(modelSummaryMultinomial)
    print_dev(modelSummaryMultinomial)
    
    lmMultinomialEstimate <- as.data.frame(modelSummaryMultinomial[["coefficients"]][,])
    nombresRow <- rownames(lmMultinomialEstimate)
    
    estimate <-c(t(lmMultinomialEstimate))   
    
    lmMultinomialSE <- as.data.frame(modelSummaryMultinomial[["standard.errors"]][,])
    
    se <-c(t(lmMultinomialSE))   
    z <- estimate/se
    p <- (1 - pnorm(abs(z), 0, 1)) * 2
    
    nombres <- c("Intercep",nombreAgrupamiento)
    golem::print_dev(paste0("nombres es : ", nombres))
    
    nombreRowss <- c()
    suma <- length(nombres)
    golem::print_dev(paste0("suma es : ", suma) )
    
    ayudavector <- c(1)
    for (variable in 2:length(nombresRow)) {
      
      ayudavector[variable] <-  ayudavector[variable-1] + suma
      
    }
    
    final <- c()
    for (variable in seq(1:(length(nombresRow)*suma) )) {
      
      if(variable %in% ayudavector){
        
        final[variable] <-nombresRow[match(variable,ayudavector)]
        #golem::print_dev(match(variable,ayudavector))
        golem::print_dev("Si es igual")  
        
      }
      else{
        
        final[variable] <- ""
        
      }
      
    }
    
    golem::print_dev(final)
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


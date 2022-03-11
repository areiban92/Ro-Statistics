calculoTRegresionLineal <- function(session, df, Agrupamiento, Dependiente) {
  
  #print_dev("dependenciaaaaaaaa de Lineal")
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
  print_dev("dsde aqui Regresion Lineal")
  
  if(length(nombreAgrupamiento)  > 1 ){
    
    print_dev(paste(nombreAgrupamiento, collapse = '+'))
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

table_calculo_RegresionLineal <- reactive({
  
  calculoTRegresionLineal(session,values$mydata,
                            df_RegresionLineal_Seleccion_Agrupamiento(),
                            
                            df_RegresionLineal_Seleccion_Dependiente()   )
  
})
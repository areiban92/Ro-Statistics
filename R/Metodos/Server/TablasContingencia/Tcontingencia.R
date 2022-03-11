

calculoTcontingencia <- function(session, df, Dependiente, Agrupamiento, filaEnable, columnaEnable) {
  
  agrupamiento = c()
  dependiente =c()
  total = c()
  
  lista = list()
  
  porcentajecolum <- list()
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  nombreDependiente <- variable2[[1]]
  nombreAgrupamiento <- variable[[1]]
  
  levelsDependiente <- nrow(unique(Dependiente))
  #levelsDependienteNombre <- sapply(Dependiente, levels)
  
  levelsAgrupamiento <- length(sapply(Agrupamiento, levels))
  levelsAgrupamientoNombre <- sapply(Agrupamiento, levels)
  
  print_dev("dependiente")
  print_dev(levelsDependiente)
  print_dev("dependientefinal")
  print_dev("agrupamiento")
  print_dev(levelsAgrupamientoNombre)
  print_dev(levelsAgrupamiento)
  print_dev("agrupamientofinal")
  tablacontingencia <- table(Agrupamiento[[1]],Dependiente[[1]])
  nombresFilas <- c( unlist((attributes(tablacontingencia))[[2]][1]), "Total" )
 # print_dev(tablacontingencia)
  
  tablacontingencia <- as.data.frame.matrix(tablacontingencia) 
 tablacontingencia <- tablacontingencia[gtools::mixedorder(colnames(tablacontingencia))]
 nombresColumnas <- c(nombreAgrupamiento, names(tablacontingencia), "Total")
 
 tabla <- transform(tablacontingencia, sum=rowSums(tablacontingencia))
 #Suma de Columnas
 SumColumnas <- colSums(tabla)
 #anade a la ultima fila
 tabla[nrow(tabla) + 1,] <- SumColumnas
 contingencia <- data.frame(nombresFilas,tabla)
 colnames(contingencia) <- nombresColumnas
 
 numeroFilasGeneral <- nrow(contingencia)
 numeroColumnasGeneral <- ncol(contingencia)
 
 filas <- contingencia[,-c(1,numeroColumnasGeneral)]
 
 #######################################FILA DESDE AQUI#################
 print_dev("DESDe aqui fila Enable")
 temporal <- vector()
 nombreFilas <- data.frame(contingencia[,1])
 colnames(nombreFilas) <- nombreAgrupamiento
 #print_dev(nombreFilas)
 nombreFilasRelleno <- data.frame(rep(" ",times=nrow(contingencia)))
 colnames(nombreFilasRelleno) <- nombreAgrupamiento
 mat_combined1_Nombres <- rbind(nombreFilas, nombreFilasRelleno) 
 # print_dev(mat_combined1_Nombres)
 filaTotal <- data.frame(contingencia[,numeroColumnasGeneral])  # Se obtiene los valores de la ultima columna
 colnames(filaTotal) <- "Total"
 #  print_dev(filaTotal) 
 valoresDeTotal <- data.frame(rep(100, times=nrow(filas)))
 colnames(valoresDeTotal) <- "Total"
 #print_dev(valoresDeTotal)
 porcentajeFila <- round(filas/rowSums(filas)*100, 2)
 totalesCombinado <- rbind(filaTotal,valoresDeTotal)
 mat_combined1 <- rbind(filas, porcentajeFila) 
 mat_combined2 <- cbind(mat_combined1, totalesCombinado)
 secuencia1 <- rep(seq(nrow(filas),(nrow(filas)*-1)+1,length.out = 2),times=nrow(filas) - 1)
 secuencia <- c(0,secuencia1,nrow(filas))
 for (j in 1:nrow(mat_combined2)) {
   
   if(j==1) {
     temporal[j]=1
   }
   else
   {
     
     temporal[j]=temporal[[j-1]] + secuencia[j]
     
   }
 }
 #print_dev(temporal)
 tablaPorcentajesFilas_Values <- mat_combined2[temporal,]
 tablaPorcentajesFilas_Names <- mat_combined1_Nombres[temporal,]
 nombreConteo <- c("Conteo", "% de la Fila")
 #print_dev(tablaPorcentajesFilas_Names)
 tablaProcentajesFilas <- data.frame(tablaPorcentajesFilas_Names,nombreConteo,tablaPorcentajesFilas_Values)
 colnames(tablaProcentajesFilas) <- c(nombreAgrupamiento," ",names(tablacontingencia),"Total")
 ###########################################################
 
 ####################################COLUMNA##################################
 print_dev("DESDe aqui Columna Enable")
 temporal <- vector()
 columnas <- contingencia[-numeroFilasGeneral,-1]
 nombreFilas <- data.frame(contingencia[,1])
 colnames(nombreFilas) <- nombreAgrupamiento
 numColumnas <- ncol(columnas)
 nombreFilasRelleno <- data.frame(rep(" ",times=nrow(contingencia)))
 colnames(nombreFilasRelleno) <- nombreAgrupamiento
 mat_combined1_Nombres <- rbind(nombreFilas, nombreFilasRelleno) 
 print_dev(contingencia)
 valoresDeTotal <- rep(100, times=nrow(contingencia))
 #rownames(filaTotal) <- "Total"
 for(i in 1:numColumnas) {
   porcentajecolum[i] = list(columnas[,i]/sum(columnas[,i]))
 }
 # print_dev(porcentajecolum)
 columnasF <- do.call(rbind.data.frame, porcentajecolum)
 columns <- data.table::transpose(columnasF)
 columns <- t(columnasF)
 columns <- round(columns*100, 2)
 totalesCombinado <- rbind(columns,valoresDeTotal)
 colnames(totalesCombinado) <- c(names(tablacontingencia), "Total")
 mat_combinedcolumn <- rbind(contingencia[,-1],totalesCombinado)
 secuencia1c <- rep(seq(nrow(totalesCombinado),(nrow(totalesCombinado)*-1)+1,length.out = 2),times=nrow(totalesCombinado) - 1)
 secuenciac <- c(0,secuencia1c,nrow(totalesCombinado))
 #print_dev(secuenciac)
 for (j in 1:nrow(mat_combinedcolumn)) {
   
   if(j==1) {
     temporal[j]=1
   }
   else
   {
     
     temporal[j]=temporal[[j-1]] + secuenciac[j]
     
   }
 }
 tablaPorcentajesFilas_Values <- mat_combinedcolumn[temporal,]
 tablaPorcentajesFilas_Names <- mat_combined1_Nombres[temporal,]
 nombreConteo <- c("Conteo", "% de Columna")
 tablaProcentajesColumnas <- data.frame(tablaPorcentajesFilas_Names,nombreConteo,tablaPorcentajesFilas_Values)
 colnames(tablaProcentajesColumnas) <- c(nombreAgrupamiento," ",names(tablacontingencia),"Total")
 #############################################################################################

 if (filaEnable == TRUE && columnaEnable == FALSE) {
  

 return(list(tablaProcentajesFilas,paste0(" " )))
 }
 
 if(columnaEnable == TRUE && filaEnable == FALSE){
 
 return(list(tablaProcentajesColumnas,paste0(" ")))
 
 }
 
 if(filaEnable==TRUE && columnaEnable==TRUE){
   
   temporal <- vector()
   combinados <- rbind(tablaProcentajesFilas, tablaProcentajesColumnas)   
   numeroFilasCombinado <- nrow(combinados)
   numeroColumnasCombinado <- ncol(combinados)

   secuencia1c <- rep(c(1,seq(nrow(tablaProcentajesFilas),-(nrow(tablaProcentajesFilas)) +1,length.out = 2)),times=nrow(tablaProcentajesFilas)-(nrow(tablaProcentajesFilas)-2))
   
   secuenciac <- c(secuencia1c,1,nrow(tablaProcentajesFilas))

   for (j in 1:(numeroFilasCombinado -3)) {
     
     if(j==1) {
       temporal[j]=1
     }
     else
     {
       temporal[j]=temporal[[j-1]] + secuenciac[j-1]
     }
   }
   
   tablaPorcentajeTotales <- combinados[temporal,]
   return(list(tablaPorcentajeTotales,paste0(" ")))
   
 }
 
  return(list(contingencia,paste0(" " )))
  

}



calculoConteoEsperado  <- function(session, df, Agrupamiento, Dependiente, conteoEsperadoEnable) {
  
  agrupamiento = c()
  dependiente =c()
  total = c()
  
  lista = list()
  
  porcentajecolum <- list()
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  nombreDependiente <- variable2[[1]]
  nombreAgrupamiento <- variable[[1]]
  
  levelsDependiente <- length(sapply(Dependiente, levels))
  levelsDependienteNombre <- sapply(Dependiente, levels)
  
  levelsAgrupamiento <- length(sapply(Agrupamiento, levels))
  levelsAgrupamientoNombre <- sapply(Agrupamiento, levels)
  
  tablacontingencia <- table(Agrupamiento[[1]],Dependiente[[1]])
  nombresFilas <- c( unlist((attributes(tablacontingencia))[[2]][1]), "Total" )
  # print_dev(tablacontingencia)
  
  tablacontingencia <- as.data.frame.matrix(tablacontingencia) 
  tablacontingencia <- tablacontingencia[gtools::mixedorder(colnames(tablacontingencia))]
  nombresColumnas <- c(nombreAgrupamiento, names(tablacontingencia), "Total")
  
  tabla <- transform(tablacontingencia, sum=rowSums(tablacontingencia))
  #Suma de Columnas
  SumColumnas <- colSums(tabla)
  #anade a la ultima fila
  tabla[nrow(tabla) + 1,] <- SumColumnas
  contingencia <- data.frame(nombresFilas,tabla)
  colnames(contingencia) <- nombresColumnas
  
  numeroFilasGeneral <- nrow(contingencia)
  numeroColumnasGeneral <- ncol(contingencia)
  
  filas <- contingencia[,-c(1,numeroColumnasGeneral)]
  
  if(conteoEsperadoEnable == TRUE){
    
    temporal <- vector()
    
    # Sacar nueva tabla solo de datos
    totalEsperados <- contingencia[numeroFilasGeneral,numeroColumnasGeneral]
    
    matrixEsperados <-  matrix(numeric((numeroColumnasGeneral-2)*(numeroFilasGeneral-1)), nrow = (numeroColumnasGeneral-2), ncol = (numeroFilasGeneral-1))
    
    for (k in 1:(numeroFilasGeneral-1)){
      for (j in 2:(numeroColumnasGeneral-1)){
        
        columnaValue <- contingencia[k,numeroColumnasGeneral]
        
        filaValue <- contingencia[numeroFilasGeneral,j]
        
        multiplicacion = (columnaValue*filaValue)/totalEsperados
        
        
        print_dev(paste0(k,",",(j-1)))
        matrixEsperados[k,(j-1)] = multiplicacion
        
        
        #print_dev(Multiplicacion)
      }
    }
    margins <- addmargins(matrixEsperados)
    dataFrameExpected <- as.data.frame(margins)
    
    print_dev(dataFrameExpected)
    
    nombreFilas <- data.frame(contingencia[,1])
    colnames(nombreFilas) <- nombreAgrupamiento
    
    nombreFilasRelleno <- data.frame(rep(" ",times=nrow(contingencia)))
    tablaEsperada <- cbind(nombreFilasRelleno,dataFrameExpected)
    colnames(tablaEsperada) <- c(nombreAgrupamiento,names(tablacontingencia),"Total")
    print_dev(tablaEsperada)
    print_dev(contingencia)
    
    #union de tablas
    esperadoTablaUnida <- rbind(contingencia,tablaEsperada)
    print_dev(esperadoTablaUnida)
    
    secuencia1 <- rep(seq(nrow(filas),(nrow(filas)*-1)+1,length.out = 2),times=nrow(filas) - 1)
    secuencia <- c(0,secuencia1,nrow(filas))
    
    print_dev(secuencia)
    
    for (j in 1:nrow(esperadoTablaUnida)) {
      
      if(j==1) {
        temporal[j]=1
      }
      else
      {
        temporal[j]=temporal[[j-1]] + secuencia[j]
      }
    }
    
    tablaPorcentajesFilas_Values_Esperado <- esperadoTablaUnida[temporal,]
    
    #print_dev(tablaPorcentajesFilas_Values_Esperado)
    #tablaPorcentajesFilas_Names <- mat_combined1_Nombres[temporal,]
    
    nombreConteo <- c("Conteo", "Conteo Esperado ")
    
    tablaProcentajesFilas <- as.data.frame(append(tablaPorcentajesFilas_Values_Esperado, list(nombreConteo), after = 1))
    colnames(tablaProcentajesFilas) <- c(nombreAgrupamiento," ",names(tablacontingencia),"Total")
    
    print_dev(tablaProcentajesFilas)
    
    return(list(tablaProcentajesFilas,paste0(" ")))
    
  }
  
  
  
  
}


estadisticaCalculoContingencia <- function(session, df, Agrupamiento, Dependiente) {
  
  
  agrupamiento = c()
  dependiente =c()
  total = c()
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  nombreDependiente <- variable2[[1]]
  nombreAgrupamiento <- variable[[1]]
  
  levelsDependiente <- length(sapply(Dependiente, levels))
  levelsDependienteNombre <- sapply(Dependiente, levels)
  
  levelsAgrupamiento <- length(sapply(Agrupamiento, levels))
  levelsAgrupamientoNombre <- sapply(Agrupamiento, levels)
  
  chi <-chisq.test(Agrupamiento[[1]],Dependiente[[1]], correct=FALSE)
  
  nombre <- "X@2~"
  
  chiStadistic <- round(chi$statistic, digits = 3)  
  chiParameter <- round(chi$parameter, digits = 3)
  
  if(chi$p.value < 0.01)
  {
    chiValue     <- "< 0.01"
    
  }
  else
  {
    chiValue     <- round(chi$p.value, digits = 3)   
    }
   
  
  
  
  nuevo <- data.frame(nombre,chiStadistic,chiParameter,chiValue)
  colnames(nuevo) <- c(" ","Valor","df","p")
  
  
  
 # print_dev(utils::str(chi))
  
  return(list(nuevo,paste0("Nota. Prueba Chi-Cuadrado" )))
  
  
  
  
  
}


calculoFisherContingencia <- function(session, df, Agrupamiento, Dependiente, Hypot,intervalo) {
  
  agrupamiento = c()
  dependiente =c()
  total = c()
  intervaloValue <- (as.numeric(intervalo))
  
  levelsDependiente <- nrow(unique(Dependiente))
  
  levelsAgrupamiento <- length(sapply(Agrupamiento, levels))
  
     print_dev(paste0("Dependiente ",levelsDependiente))
     print_dev(paste0("Agrupamiento  ",levelsAgrupamiento))
  
  
  if(levelsAgrupamiento == 2 && levelsDependiente ==2 )
  {
    print_dev("DEsde aqui testtt")

        if(intervaloValue  < 100 ){
          
      
          intervaloValue <- intervaloValue/100
          
          if (Hypot == "hipotesis_Mayor"){
            print_dev("mayor")
          
          fisher <-fisher.test(table(Agrupamiento[[1]], Dependiente[[1]]),alternative = "greater", conf.level = intervaloValue)
          print_dev(fisher)
          nombre <- "Fisher Test"
          fisherEstimate <- round(fisher$estimate, digits = 3)
          fisherValorconfInt <- round(fisher$conf.int, digits = 3)
          fisherPvalue <- fisher$p.value
      
          
          fisherTable <- data.frame(nombre,fisherEstimate,fisherValorconfInt[[1]],fisherValorconfInt[[2]],fisherPvalue)
          colnames(fisherTable) <- c(" ","Odds Ratio","Lower","Upper","p")
          }
          
          if (Hypot == "hipotesis_Igual"){
            
            print_dev("igual")
           # print_dev("HIpotesis fisher iguallll-----------------")
           # print_dev(table(Agrupamiento[[1]], Dependiente[[1]]))
           # print_dev("HIpotesis fisher iguallll-----------------")
            fisher <- stats::fisher.test(table(Agrupamiento[[1]], Dependiente[[1]]),alternative = "two.sided", conf.level = intervaloValue)
          #  print_dev(fisher)
          #  print_dev(utils::str(fisher))
            nombre <- "Fisher Test"
            fisherEstimate <- round(fisher$estimate, digits = 3)
            print_dev(fisherEstimate)
            fisherValorconfInt <- round(fisher$conf.int, digits = 3)
            fisherPvalue <- fisher$p.value
            
          #  print_dev(fisherPvalue)
            
            
            if(fisherPvalue  < 0.01)
            {
            #  print_dev("ghsahgsghsashga")
              fisherPvalue      <- "< 0.01"
            }
            else
            {
             # print_dev("ghsahgsghsashga")
              #print_dev(fisher$fisherPvalue)
              fisherPvalue      <- round(fisherPvalue, digits = 3)   
            }
            
            fisherTable <- data.frame(nombre,fisherEstimate,fisherValorconfInt[[1]],fisherValorconfInt[[2]],fisherPvalue)
            colnames(fisherTable) <- c(" ","Odds Ratio","Lower","Upper","p")
          }
          
          if (Hypot == "hipotesis_Menor"){
            print_dev("menor")
            
            fisher <-fisher.test(table(Agrupamiento[[1]], Dependiente[[1]]),alternative = "less", conf.level = intervaloValue,conf.int = TRUE)
            print(fisher)
            
            nombre <- "Fisher Test"
            fisherEstimate <- round(fisher$estimate, digits = 3)
            fisherValorconfInt <- round(fisher$conf.int, digits = 3)
            fisherPvalue <- fisher$p.value
            
            print_dev(fisherPvalue)
            
            
        
            
            fisherTable <- data.frame(nombre,fisherEstimate,fisherValorconfInt[[1]],fisherValorconfInt[[2]],fisherPvalue)
            colnames(fisherTable) <- c(" ","Odds Ratio","Lower","Upper","p")
          }
          
          
          fisherTable <- dplyr::mutate(fisherTable, dplyr::across(where(is.numeric), round, 3))
        }else{
          emptyFisher  <- ""
          fisherTable <- data.frame(emptyFisher)
          
        }
  }
 else {
    
   shinyalert::shinyalert(
     title = "Las variables deben contener dos Factores unicamente",
     text = "This is a modal",
     size = "s", 
     closeOnEsc = TRUE,
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
    
    emptyFisher  <- ""
    fisherTable <- data.frame(emptyFisher)
    
    
    
  }
    
    return(list(fisherTable,paste0(" " )))

  
}


table_calculoTcontingencia <- reactive({
  
  calculoTcontingencia(session,values$mydata,df_Tcontingencia_Seleccion_Dependiente(), df_Tcontingencia_Seleccion_Agrupamiento(),
                       checkContingenciaPorcentajeFila(),
                       checkContingenciaPorcentajeColumna() )
  
})



table_calculoEsperado <- reactive({
  
  calculoConteoEsperado( session,values$mydata,df_Tcontingencia_Seleccion_Dependiente(), df_Tcontingencia_Seleccion_Agrupamiento(),
                         checkContingenciaConteoEsperado() )
    

})



table_calculoFisherContingencia <- reactive({
  
  calculoFisherContingencia(session,values$mydata,df_Tcontingencia_Seleccion_Agrupamiento(), df_Tcontingencia_Seleccion_Dependiente(),
                          radioContingenciaFisher_HypoValue(),
                            txtContingenciaFisher_IntervaloConfidencia())
  
})

table_estadisticaCalculoContingencia <- reactive({
  
  estadisticaCalculoContingencia(session,values$mydata,
                                 df_Tcontingencia_Seleccion_Dependiente(), df_Tcontingencia_Seleccion_Agrupamiento())
  
  
})



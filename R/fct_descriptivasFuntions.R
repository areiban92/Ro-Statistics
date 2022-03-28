#' descriptivasFuntions 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#' 


modafuntion <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

make_factors <- function(data, max_levels,min_levels) {
  # convert all columns in <data> that are not already factors
  # and that have fewer than <max_levels> distinct values into factors. 
  # If the column is numeric, it becomes an ordered factor.
  
  stopifnot(is.data.frame(data))
  for(n in names(data)){
    
    
    if(!is.factor(data[[n]]) && (length(unique(data[[n]])) <= min_levels) ){
      data[[n]] <- as.factor(data[[n]]) }
    
    if(!is.factor(data[[n]]) && (length(unique(data[[n]])) >= min_levels && length(unique(data[[n]])) <= max_levels) ){
      #golem::golem::print_dev("Ordered")
      #golem::golem::print_dev(unique(data[[n]]))
      data[[n]] <- ordered(data[[n]], levels = unique(data[[n]])) }
    
    
  }
  data
}




calculoVariables <- function(session,dforiginal,Dependiente,Agrupamiento,Mean,Mediana,Moda,Suma,
                             SE,MAD,Iqr,Rango,Maximo,DsvEstandar,MADRobust,Varianza,Minimo,
                             Quartiles,PuntoCorte,PuntoCorteValue,Percentil,PercentilValue){
 
  
  df <- dforiginal
  if (  length(names(Agrupamiento)) > 0  )
  {
    
    NoValidos = c()
    Validos =c()
    Mean_vector = c()
    Mediana_vector = c()
    Moda_vector =c()
    Suma_vector = c()
    SE.MEAN_vector= c()
    MAD_vector= c()
    IQR_vector= c()
    Rango_vector = c()
    rangoCalc =c()
    Max_vector = c()
    
    Desv.Est_vector = c()
    MadRobust_vector =c()
    Varianza_vector = c()
    Min_vector = c()
    Quartiles_vector  = c()
    PuntoCorte_vector  = c()
    Percentil_vector  = c()
    PercentilValue_names  = c()
    summaryVariableTable2 = c()
    
    
    golem::print_dev(" Con variable de agrupamiento")
    
    nombreAgrupamiento <- c(names(Agrupamiento))
    nombreDependiente <- c(names(Dependiente))
    
    numeroVariablesDependientes <-length(nombreDependiente)
    numeroVariablesAgrupamiento <- length(nombreAgrupamiento )
    
    for (i in 1:length(Dependiente)){
      
      if(class(Dependiente[[i]])=="numeric"){
        
        df$temp <-  as.numeric(Dependiente[[i]])
        golem::print_dev("AHORA ES NUMERICO EL NUMERICCO")
      }
      else{
        
        df$temp  <- ( as.numeric(Dependiente[[i]]) -1)
        
        golem::print_dev("AHORA ES NUMERICO EL CATEGORICO")
      }
      
      # varActual <- paste0( nombreDependiente[i]  ," ~ ",paste(nombreAgrupamiento, collapse = '+'))
      varActual <- paste0( "temp"  ," ~ ",paste(nombreAgrupamiento, collapse = '+'))
      golem::print_dev(varActual)
      golem::print_dev(aggregate( eval(parse(text = varActual ))   , df, sum))
      rowSVariables <- aggregate( eval(parse(text = varActual ))   , df, sum)[c(seq(1:(numeroVariablesAgrupamiento)))]
      
      if(Mean==TRUE){
        golem::print_dev("Si es True MEAN")
        golem::print_dev(  aggregate( eval(parse(text = varActual ))   , df, mean)  )
        Mean_vector[i] <- list( round(aggregate( eval(parse(text = varActual ))   , df, mean)[[(numeroVariablesAgrupamiento+1)]] ,3) )
        
      }
      if(Mediana==TRUE){
        golem::print_dev("Si es True Mediana")
        Mediana_vector[i] <- list(round( aggregate( eval(parse(text = varActual ))   , df, median)[[(numeroVariablesAgrupamiento+1)]],3)) 
        
      }
      if(Moda==TRUE){
        
        golem::print_dev("si es moda vector")
        Moda_vector[i] <- list(round( aggregate( eval(parse(text = varActual ))  , df, modafuntion )[[(numeroVariablesAgrupamiento+1)]],3)) 
        
      }
      if(Suma==TRUE){
        golem::print_dev("Si es True Suma")
        Suma_vector[i] <- list(round( aggregate( eval(parse(text = varActual ))   , df, sum)[[(numeroVariablesAgrupamiento+1)]],3)) 
        
      }
      if(SE==TRUE){
        golem::print_dev("Si es True SE")
        SE.MEAN_vector[i]<-list(round(aggregate( eval(parse(text = varActual ))   , df, rapportools::se.mean)[[(numeroVariablesAgrupamiento+1)]],3)) 
        
      }
      if(MAD==TRUE){
        golem::print_dev("Si es True MAD con factor")
        MAD_vector[i]<- list( aggregate( eval(parse(text = varActual ))   , df, mad ,constant = 1)[[(numeroVariablesAgrupamiento+1)]] )
        print(MAD_vector[i])
        #  MAD_vector[i]<- list(aggregate( eval(parse(text = varActual ))   , df, FUN = "mad",constant = 1))
        
      }
      if(Iqr==TRUE){
        golem::print_dev("Si es True IQR")
        IQR_vector[i] <- list(round(aggregate( eval(parse(text = varActual ))   , df, IQR)[[(numeroVariablesAgrupamiento+1)]],3)) 
        
        
      }
      if(Rango==TRUE){
        golem::print_dev("Si es True Rango con Factor")
        golem::print_dev(aggregate( eval(parse(text = varActual ))   , df, range)[[(numeroVariablesAgrupamiento+1)]])
        
        rangoCalc [i]<- list(as.list(data.frame(t( aggregate( eval(parse(text = varActual ))   , df, range)[[(numeroVariablesAgrupamiento+1)]] ))))
        for (j in 1:length(rangoCalc[[1]])) {
          
          Rango_vector[j] =toString( unlist(rangoCalc[[1]][j]) )
          
        }
        
        
        golem::print_dev(Rango_vector)
      }
      if(Maximo==TRUE){
        golem::print_dev("Si es True Maximo")
        Max_vector[i]<- list(round(aggregate( eval(parse(text = varActual ))   , df, max)[[(numeroVariablesAgrupamiento+1)]],3)) 
        
      }
      if(DsvEstandar==TRUE){
        golem::print_dev("Si es True DsvEstandar")
        Desv.Est_vector[i]<- list(round(aggregate( eval(parse(text = varActual ))   , df, sd)[[(numeroVariablesAgrupamiento+1)]],3)) 
        
      }
      if(MADRobust==TRUE){
        golem::print_dev("Si es True MADRobust")
        MadRobust_vector[i] <- list(round( aggregate( eval(parse(text = varActual ))   , df, mad ,constant = 1.4826)[[(numeroVariablesAgrupamiento+1)]],3))
        # MadRobust_vector[i]
        
      }
      if(Varianza==TRUE){
        golem::print_dev("Si es True Varianza")
        Varianza_vector[i]<-   list(round(aggregate( eval(parse(text = varActual ))   , df, var)[[(numeroVariablesAgrupamiento+1)]],3)) 
      }
      if(Minimo==TRUE){
        golem::print_dev("Si es True Minimo")
        Min_vector[i]<- list(round(aggregate( eval(parse(text = varActual ))   , df, min)[[(numeroVariablesAgrupamiento+1)]],3)) 
        
      }
      if(Quartiles==TRUE){
        golem::print_dev("Si es True Quartiles con factor")
        # golem::print_dev(aggregate( eval(parse(text = varActual ))   , df, FUN = "quantile",na.rm = TRUE))
        Quartiles_vector[i] <-  list(round(aggregate( eval(parse(text = varActual ))   , df, quantile,na.rm = TRUE)[[(numeroVariablesAgrupamiento+1)]][,c(-1,-5)],3))
        
        
        
        #  golem::print_dev(c(t(dfff[[(numeroVariablesAgrupamiento+1)]])))
        # Quartiles_vector[i]
      }
      if(PuntoCorte==TRUE){
        
        if(as.numeric(PuntoCorteValue) >= 2){
          golem::print_dev("Si es True PuntodeCorte con factor")
          stepValue <-(1/(as.numeric(PuntoCorteValue)))
          #golem::print_dev(stepValue)
          #PuntoCorte_vector[i] <- list(quantile(varActual, prob=seq(0,1,(1/(as.numeric(PuntoCorteValue))))   ,na.rm = TRUE))
          # golem::print_dev(aggregate( eval(parse(text = varActual ))   , df, FUN = "quantile", prob=seq(0,1,stepValue ) ,na.rm = TRUE)[[(numeroVariablesAgrupamiento+1)]])
          PuntoCorte_vector[i] <- list(round(aggregate( eval(parse(text = varActual ))   , df, quantile, prob=seq(0,1,stepValue ) ,na.rm = TRUE)[[(numeroVariablesAgrupamiento+1)]] [,c(-1,-(as.numeric(PuntoCorteValue) + 1))],3))
          
          
        }
        
        #golem::print_dev("Si es True PuntoCorte")
      }
      if(Percentil==TRUE){
        golem::print_dev("Si es True Percentil")
        if(as.numeric(PercentilValue) >= 2){
          
          #golem::print_dev(  aggregate( eval(parse(text = varActual ))  , df, FUN= "quantile",prob=seq(0,1,length = 101),na.rm = TRUE) )
          valores <- aggregate( eval(parse(text = varActual ))  , df, quantile,prob=seq(0,1,length = 101),na.rm = TRUE)[[(numeroVariablesAgrupamiento+1)]]
          
          Percentil_vector[i] <- list(round(valores[,(as.numeric(PercentilValue) + (1))],3))
          PercentilValue_names[i] <- colnames(valores)[(as.numeric(PercentilValue) + (1))]
          
          
          
          #   Percentil_vector[i] <- valores[as.numeric(PercentilValue)]
        }
      }
      
      
      Validos[i] <-   list(round(aggregate( (eval(parse(text = varActual )))   , df, length )[[(numeroVariablesAgrupamiento+1)]],3))
      #  golem::print_dev(Validos)
      NoValidos [i] <- list(round(aggregate( (eval(parse(text = varActual )))   , df, function(x) {sum(is.na(x))}, na.action = NULL )[[(numeroVariablesAgrupamiento+1)]],3))
      
      
      
      summaryVariableTable <- data.frame(NoValidos[i],Validos[i])
      colnames(summaryVariableTable) <- c("Missing","Validos")
      
      if(Mean==TRUE){summaryVariableTable$Media=unlist(Mean_vector[i])} else{summaryVariableTable$Media <- NULL}
      if(DsvEstandar==TRUE){summaryVariableTable$Dsv.Est=unlist(Desv.Est_vector[1])} else{summaryVariableTable$Dsv.Est <- NULL}
      if(Minimo==TRUE){summaryVariableTable$Minimo=unlist(Min_vector[i])} else{summaryVariableTable$Minimo <- NULL}
      if(Maximo==TRUE){summaryVariableTable$Maximo=unlist(Max_vector[i])} else{summaryVariableTable$Maximo <- NULL}
      if(Mediana==TRUE){summaryVariableTable$Mediana=unlist(Mediana_vector[i])} else{summaryVariableTable$Mediana <- NULL}
      if(Moda==TRUE){summaryVariableTable$Moda=unlist(Moda_vector[i])} else{summaryVariableTable$Moda <- NULL}
      if(Suma==TRUE){summaryVariableTable$Suma=unlist(Suma_vector[i])} else{summaryVariableTable$Suma <- NULL}
      if(SE==TRUE){summaryVariableTable$SE.Media=unlist(SE.MEAN_vector[i])} else{summaryVariableTable$SE.Media <- NULL}
      if(MAD==TRUE){summaryVariableTable$MAD=unlist(MAD_vector[i])} else{summaryVariableTable$MAD <- NULL}
      if(Iqr==TRUE){summaryVariableTable$IQR=unlist(IQR_vector[i])} else{summaryVariableTable$IQR <- NULL}
      if(Rango==TRUE){summaryVariableTable$Rango=unlist(Rango_vector[i])} else{summaryVariableTable$Rango <- NULL}
      if(MADRobust==TRUE){summaryVariableTable$MAD.Escalado=unlist(MadRobust_vector[i])} else{summaryVariableTable$MAD.Escalado <- NULL}
      if(Varianza==TRUE){summaryVariableTable$Varianza=unlist(Varianza_vector[i])} else{summaryVariableTable$Varianza <- NULL}
      
      if(Quartiles==TRUE){
        
        summaryVariableTable$"25%" = unlist(Quartiles_vector[[i]][,1])
        summaryVariableTable$"50%" = unlist(Quartiles_vector[[i]][,2])
        summaryVariableTable$"75%" = unlist(Quartiles_vector[[i]][,3])
        
      } else{
        summaryVariableTable$"25%" = NULL
        summaryVariableTable$"50%" = NULL
        summaryVariableTable$"75%" = NULL
      }
      
      if(PuntoCorte==TRUE)
      {
        
        golem::print_dev( colnames(unlist(PuntoCorte_vector[[1]]))  )
        
        lengthNames <-length( colnames(unlist(PuntoCorte_vector[[1]]))  )
        
        
        for(j in 1:lengthNames){
          # golem::print_dev(paste0(colnames(unlist(PuntoCorte_vector[[1]]))[j]," "))
          # golem::print_dev(PuntoCorte_vector[[1]][,j])
          summaryVariableTable[, paste0(colnames(unlist(PuntoCorte_vector[[1]]))[j]," ")] <- unlist( PuntoCorte_vector[[1]][,j])
          
        }
      } else{
        
        lengthNames <-length( colnames(unlist(PuntoCorte_vector[[1]]))  )
        
        if(lengthNames == 0){
          
        }else{
          for(i in 1:lengthNames){
            summaryVariableTable[, paste0(colnames(unlist(PuntoCorte_vector[[1]]))[i]," ")] = NULL
          }
        }
      }
      
      
      if(Percentil==TRUE)
      {
        # golem::print_dev(i)
        # golem::print_dev(  paste0(PercentilValue_names[i]," Percentil") )
        # golem::print_dev(Percentil_vector[[i]])
        # golem::print_dev(paste0(PercentilValue_names[1]," Percentil"))
        summaryVariableTable[, paste0(PercentilValue_names[i]," Percentil")] <- Percentil_vector[[i]]
        
      }else
      {
        summaryVariableTable[, paste0(PercentilValue_names," Percentil")] <-  NULL
      }
      
      colnamesSummary <- colnames(summaryVariableTable)
      length(rownames(summaryVariableTable))
      nombreMetodosDescriptivos <- rep(colnamesSummary, each= length(rownames(summaryVariableTable)))
      
      golem::print_dev(summaryVariableTable)
      repVacios <- rep("", (length(rownames(summaryVariableTable)) -1 ))
      summaryVariableTable2[i] <- data.frame(unlist(summaryVariableTable))
      
    }
    
    nombreMetodos<- c()
    
    for (i in 1: length(colnamesSummary))
    {
      golem::print_dev(i)
      nombreMetodos <- append(nombreMetodos,colnamesSummary[i])
      nombreMetodos <- append(nombreMetodos,repVacios)
      
    }
    
    
    #summaryVariableTable2[i] <- data.frame(nombreMetodosDescriptivos,rowSVariables,unlist(summaryVariableTable))
    finalNames <- c(" ",nombreAgrupamiento,nombreDependiente )
    golem::print_dev(finalNames)
    
    finalDescriptivas <- data.frame(nombreMetodos,rowSVariables,summaryVariableTable2)
    colnames(finalDescriptivas) <- finalNames
    summaryVariableTable3 <- dplyr::mutate(finalDescriptivas, dplyr::across(where(is.numeric), round, 3))
    
    
  }else {
    golem::print_dev("Sin variable de Grupo Descriptivas")
    
    Variable=c( names(Dependiente))
    
    NoValidos = c()
    Validos =c()
    Mean_vector = c()
    Mediana_vector = c()
    Moda_vector =c()
    Suma_vector = c()
    SE.MEAN_vector= c()
    MAD_vector= c()
    IQR_vector= c()
    Rango_vector = c()
    Max_vector = c()
    
    Desv.Est_vector = c()
    MadRobust_vector =c()
    Varianza_vector = c()
    Min_vector = c()
    Quartiles_vector  = c()
    PuntoCorte_vector  = c()
    Percentil_vector  = c()
    PercentilValue_names  = c()
    
    
    for (i in 1:length(Dependiente)){
      
      
      defaultSelect <- class(Dependiente[[i]])
      golem::print_dev("ASSSSSSSSSSSSSDDDDDDDDD")
      golem::print_dev(defaultSelect)
      golem::print_dev("ASSSSSSSSSSSSSDDDDDDDDD")
      if(defaultSelect=="numeric"){
        
        varActual <- (as.numeric(Dependiente[[i]]))
        
      }
      else{
        
        varActual <- (as.numeric(Dependiente[[i]]) )
      }
      
      golem::print_dev(utils::str(varActual))
      
      if(Mean==TRUE){
        golem::print_dev("Si es True MEAN")
        
        Mean_vector[i] <- round( mean(varActual, na.rm = TRUE),digits = 3 )
        golem::print_dev(Mean_vector[i])
        
      }
      if(Mediana==TRUE){
        golem::print_dev("Si es True Mediana")
        Mediana_vector[i] <-round( median(varActual,na.rm = TRUE),digits = 3)
        
        
      }
      if(Moda==TRUE){
        
        Moda_vector <- round( modafuntion(varActual),digits = 3)
        
      }
      if(Suma==TRUE){
        golem::print_dev("Si es True Suma")
        Suma_vector[i] <-round( sum(varActual,na.rm = TRUE), digits = 3)
        
      }
      if(SE==TRUE){
        golem::print_dev("Si es True SE")
        SE.MEAN_vector[i]<- round(  rapportools::se.mean(varActual), digits = 3)  
        
      }
      if(MAD==TRUE){
        golem::print_dev("Si es True MAD")
        MAD_vector[i]<- round(  mad(varActual,constant = 1,na.rm = TRUE), digits = 3)
        
      }
      if(Iqr==TRUE){
        golem::print_dev("Si es True IQR")
        IQR_vector[i]<- round(  IQR(varActual,na.rm = TRUE), digits = 3)
        
      }
      if(Rango==TRUE){
        golem::print_dev("Si es True Rango")
        #golem::print_dev(range(varActual))
        Rango_vector[i]<-paste(range(varActual,na.rm = TRUE), collapse = " ") 
        
      }
      if(Maximo==TRUE){
        golem::print_dev("Si es True Maximo")
        Max_vector[i]<-  round(   max(varActual,na.rm = TRUE), digits = 3)  
        
      }
      if(DsvEstandar==TRUE){
        golem::print_dev("Si es True DsvEstandar")
        Desv.Est_vector[i]<-round(  sd(varActual,na.rm = TRUE), digits = 3)   
        
      }
      if(MADRobust==TRUE){
        golem::print_dev("Si es True MADRobust")
        MadRobust_vector[i] <-round(  mad(varActual,constant = 1.4826,na.rm = TRUE), digits = 3) 
        
      }
      if(Varianza==TRUE){
        golem::print_dev("Si es True Varianza")
        Varianza_vector[i]<-round( var(varActual,na.rm = TRUE), digits = 3)
        
      }
      if(Minimo==TRUE){
        golem::print_dev("Si es True Minimo")
        Min_vector[i]<- round( min(varActual,na.rm = TRUE), digits = 3)
        
      }
      if(Quartiles==TRUE){
        golem::print_dev("Si es True Quartiles")
        Quartiles_vector[i] <- list(round(quantile(varActual,na.rm = TRUE),3))
        print(quantile(varActual,na.rm = TRUE))
        
      }
      if(PuntoCorte==TRUE){
        
        if(as.numeric(PuntoCorteValue) >= 2){
          golem::print_dev("Si es True PuntodeCorte")
          stepValue <-(1/(as.numeric(PuntoCorteValue)))
          #golem::print_dev(stepValue)
          #PuntoCorte_vector[i] <- list(quantile(varActual, prob=seq(0,1,(1/(as.numeric(PuntoCorteValue))))   ,na.rm = TRUE))
          PuntoCorte_vector[i] <- list(round(quantile(varActual, prob=seq(0,1,stepValue )   ,na.rm = TRUE),3) )
        }
        
        #golem::print_dev("Si es True PuntoCorte")
      }
      if(Percentil==TRUE){
        golem::print_dev("Si es True Percentil redondeado")
        if(as.numeric(PercentilValue) >= 2){
          
          valores <- quantile(varActual, prob=seq(0,1,length = 101)   ,na.rm = TRUE)
          golem::print_dev(valores)
          PercentilValue_names <- names(valores)
          
          
          Percentil_vector[i] <- round(valores[as.numeric(PercentilValue)],3)
        }
      }
      NoValidos[i] <-  sum(is.na(varActual))
      Validos[i] <- length(varActual) - NoValidos[i]
    }
    
    golem::print_dev(PercentilValue_names)
    summaryVariableTable <- data.frame(NoValidos,Validos)
    
    if(Mean==TRUE){summaryVariableTable$Media= Mean_vector} else{summaryVariableTable$Media <- NULL}
    if(DsvEstandar==TRUE){summaryVariableTable$Dsv.Est=Desv.Est_vector} else{summaryVariableTable$Dsv.Est <- NULL}
    if(Minimo==TRUE){summaryVariableTable$Minimo=Min_vector} else{summaryVariableTable$Minimo <- NULL}
    if(Maximo==TRUE){summaryVariableTable$Maximo=Max_vector} else{summaryVariableTable$Maximo <- NULL}
    if(Mediana==TRUE){summaryVariableTable$Mediana=Mediana_vector} else{summaryVariableTable$Mediana <- NULL}
    if(Moda==TRUE){summaryVariableTable$Moda=Moda_vector} else{summaryVariableTable$Moda <- NULL}
    if(Suma==TRUE){summaryVariableTable$Suma=Suma_vector} else{summaryVariableTable$Suma <- NULL}
    if(SE==TRUE){summaryVariableTable$SE.Media=SE.MEAN_vector} else{summaryVariableTable$SE.Media <- NULL}
    if(MAD==TRUE){summaryVariableTable$MAD=MAD_vector} else{summaryVariableTable$MAD <- NULL}
    if(Iqr==TRUE){summaryVariableTable$IQR=IQR_vector} else{summaryVariableTable$IQR <- NULL}
    if(Rango==TRUE){summaryVariableTable$Rango=Rango_vector} else{summaryVariableTable$Rango <- NULL}
    if(MADRobust==TRUE){summaryVariableTable$MAD.Escalado=MadRobust_vector} else{summaryVariableTable$MAD.Escalado <- NULL}
    if(Varianza==TRUE){summaryVariableTable$Varianza=Varianza_vector} else{summaryVariableTable$Varianza <- NULL}
    
    if(Quartiles==TRUE){
      
      summaryVariableTable$"25%" = unlist(Quartiles_vector)[2]
      summaryVariableTable$"50%" = unlist(Quartiles_vector)[3]
      summaryVariableTable$"75%" = unlist(Quartiles_vector)[4]
      
    } 
    else{
      summaryVariableTable$"25%" = NULL
      summaryVariableTable$"50%" = NULL
      summaryVariableTable$"75%" = NULL
    }
    
    if(PuntoCorte==TRUE)
    {
      
      golem::print_dev(names(unlist(PuntoCorte_vector)))
      lenghtNamesPC <-length(names(unlist(PuntoCorte_vector)))
      
      for(i in seq(2, as.numeric(lenghtNamesPC) - 1, by=1)){
        
        summaryVariableTable[, paste0(names(unlist(PuntoCorte_vector))[i]," ")] <- unlist(PuntoCorte_vector)[i]
        
      }
      
    } 
    else{
      
      lenghtNamesPC <-length(names(unlist(PuntoCorte_vector)))
      if(lenghtNamesPC == 0){
        
      }else{
        for(i in 1:lenghtNamesPC-1){
          summaryVariableTable[paste0(names(unlist(PuntoCorte_vector))[i]," ")] = NULL
          #names(unlist(PuntoCorte_vector))
        }
      }
    }
    if(Percentil==TRUE)
    {
      
      summaryVariableTable[, paste0(unlist(stringr::str_split(PercentilValue_names[as.numeric(PercentilValue) + 1],""))[1]," Percentil")] <- Percentil_vector
      
    } 
    else
    {
      summaryVariableTable[, paste0(unlist(stringr::str_split(PercentilValue_names[as.numeric(PercentilValue) + 1],""))[1]," Percentil")] <- NULL
    }
    
    summaryVariableTable2 <- data.frame(t(summaryVariableTable))
    
    summaryVariableTable2 <- dplyr::mutate(summaryVariableTable2, dplyr::across(where(is.numeric), as.character))
    golem::print_dev(str(summaryVariableTable2))
    
    rownames(summaryVariableTable2) <- NULL
    golem::print_dev(colnames(summaryVariableTable))
    
    summaryVariableTable3 <- data.frame( colnames(summaryVariableTable), summaryVariableTable2  )
    colnames(summaryVariableTable3) <- c(" ",Variable)
    
    #golem::print_dev(str(summaryVariableTable3))
    
    #finalDescriptivas <- dplyr::mutate(summaryVariableTable3, dplyr::across(where(is.numeric), as.character))
    
    
    
  }
  
  return(summaryVariableTable3)
  
  
  
  
}


calculoFrecuencias <- function(session,df,frecuenciaEnable) {
  
 
  if (frecuenciaEnable == TRUE) {
    
    
    #nbreaks <- pretty(range.default(as.numeric(df_sel()[[i]])),n = nclass.Sturges(as.numeric(df_sel()[[i]])),min.n = 1)
    tablas  = c()
    nombreVariable <- names(df)[[1]]
    golem::print_dev(nombreVariable)
    
    tablaFrecuencia <- summarytools::freq(df[[1]], order = "freq")
    
    # tablaFrecuencia <- summarytools::freq(df, order = "freq")
    elemntos <- attributes(tablaFrecuencia)[[2]][[1]]
    golem::print_dev(elemntos)
    freq <- tablaFrecuencia[,1]
    validos <-  tablaFrecuencia[,2]
    validosCum <- tablaFrecuencia[,3]
    total <- tablaFrecuencia[,4]
    totalCum <- tablaFrecuencia[,5]
    
    descriptivasFrecuencia <- data.frame(elemntos,freq,validos,totalCum)
    row.names(descriptivasFrecuencia) <- NULL
    golem::print_dev(descriptivasFrecuencia)
    descriptivasFrecuencia <- dplyr::mutate(descriptivasFrecuencia, dplyr::across(where(is.numeric), round, 3))
    colnames(descriptivasFrecuencia) <- c(nombreVariable,"Frecuencia","Porcentaje Validos","Porcentaje Acumulado")
    
    #newdata <- mtcars[order(mpg),]
    
    # tablas[[i]] <- df
    
    return(descriptivasFrecuencia)
    
  }
  if(frecuenciaEnable == FALSE)
    
  {
    elemntos <- " "
    freq <- " "
    validos <-  " "
    validosCum <- " "
    total <- " "
    totalCum <- " "
    
    descriptivasFrecuencia <- data.frame(elemntos,freq,validos,totalCum)
    row.names(descriptivasFrecuencia) <- NULL
    golem::print_dev(descriptivasFrecuencia)
    descriptivasFrecuencia <- dplyr::mutate(descriptivasFrecuencia, dplyr::across(where(is.numeric), round, 3))
    colnames(descriptivasFrecuencia) <- c("nombreVariable","Frecuencia","Procentaje Validos","Procentaje Acumulado")
    
    return(descriptivasFrecuencia)
  }
  
  
  
}



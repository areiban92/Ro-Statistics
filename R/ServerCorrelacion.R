
calculoTCorrelacion <- function(session, df, Agrupamiento, Dependiente,
                                pearson,spearman,kendall,
                                Hypot,significancia,estadistica
                                ) {
  
  agrupamiento = c()
  total = c()
  valorConfidencia="95"
  intervaloValue <- (as.numeric(valorConfidencia))
  print_dev("significancia")
  print_dev(significancia)

  variable <- c(names(Agrupamiento))
  metodo <- c()
  var <- c()
  nColumnas <- length(variable)
  nFilas <- length(variable)
 
  if (intervaloValue < 100 ){
    
    correlacionmatrixCor = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas) #  
    correlacionmatrixP.value = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas) #  
    
    print_dev("Si es menor y cumple requisitos")
    
    intervaloValue <- intervaloValue/100
   # print_dev(intervaloValue)
    
    if(pearson == FALSE &&  spearman == FALSE && kendall == FALSE ) # ok revissar
    {
      
      nombreAgrupamiento <- " "
      valorEstadistico <- " "
      valorParametro <- " "
      valorPvalue <- " "
      #print_dev(resss)
      tablaCorrelacion <- data.frame(nombreAgrupamiento,valorEstadistico)
      colnames(tablaCorrelacion) <- c("--"," --")
      #return(list(ttestTable," "))
      return(list(tablaCorrelacion,paste0("Nota. Matriz de Correlacion" )))
      
    }
    
    if(pearson == TRUE &&  spearman == FALSE && kendall == FALSE )  #1  ok 
    {
      columnasTemp <- list()
      temporal <- vector()
      
      print_dev("SI CUMPLIO  # 1")
      tipoTtest = c("pearson")

      if (Hypot == "correlacion"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionPearsonP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionmatrixCor[i,j] = correlacionPearsonCor
            correlacionmatrixP.value[i,j] = correlacionPearsonP.value
          }
        }
     
   
        
      }
     
      if (Hypot == "correlacion_Positiva"){
        # print_dev("HYPOTESIS POSITOVA ")
        
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("greater"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionPearsonP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("greater"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("greater"),conf.level = intervaloValue))$estimate[[1]],3)
            
            
            
            correlacionmatrixCor[i,j] = correlacionPearsonCor
            correlacionmatrixP.value[i,j] = correlacionPearsonP.value
          }
        }
       
        
        
      }
       
      if (Hypot == "correlacion_Negativa"){
        print_dev("HYPOMENOR")


        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionPearsonP.value     <- "< 0.01"
            }
            else
            {
              correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionmatrixCor[i,j] = correlacionPearsonCor
            correlacionmatrixP.value[i,j] = correlacionPearsonP.value
          }
        }
       # print_dev(correlacionmatrixCor)
  

      }

      ############Elima parte superior de las matrices
      print_dev("obtener parte arriba igual")
      
      correlacionmatrixCor[upper.tri(correlacionmatrixCor)] <- '----'
      diag(correlacionmatrixCor) <- '----'  
      corrMatrix <- correlacionmatrixCor
  
      correlacionmatrixP.value[upper.tri(correlacionmatrixP.value)] <- '----'
      diag(correlacionmatrixP.value) <- '----'  
      
      var <- variable

      if( significancia == TRUE ){  # Si activa se Calcula diferencia de medias
        
        print_dev("significancia trueeeeee")
        tipoTtest = c("Pearson's r","valor P")
        
        
        mat_combined1 <- rbind(correlacionmatrixCor, correlacionmatrixP.value)   
        print_dev(mat_combined1)
        print_dev("datossssssssss")
        
        secuencia1 <- rep(seq(nrow(correlacionmatrixCor),(nrow(correlacionmatrixCor)*-1)+1,length.out = 2),times=nrow(correlacionmatrixCor) - 1)
        secuencia <- c(0,secuencia1,nrow(correlacionmatrixCor))
        print_dev(secuencia)
        
        for (j in 1:nrow(correlacionmatrixCor)) {
          
          var[j+(j-1)] <- variable[j]
          var[j*2] <- " "
          
        }
        
        for (j in 1:nrow(mat_combined1)) {
          
          if(j==1) {
          temporal[j]=1
          }
          else
          {
          
              temporal[j]=temporal[[j-1]] + secuencia[j]
              
          }
          
          #columnasTemp[j] <- list.append(j)
        }

        corrMatrix <- mat_combined1[temporal,]
        
        
      }

      tablaCorrelacion <- as.data.frame(corrMatrix)
      tablaCorrelacion <- data.frame(var,tipoTtest,tablaCorrelacion)
      colnames(tablaCorrelacion) <- c(" ","modelo",variable)
      

      return(list(tablaCorrelacion,paste0(" ")))
    
    }
    
    
    if(pearson == FALSE &&  spearman == TRUE && kendall == FALSE )  #1  ok 
    {
      columnasTemp <- list()
      temporal <- vector()
      
      print_dev("SI CUMPLIO  # 1")
      tipoTtest = c("spearman")
      
      if (Hypot == "correlacion"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionmatrixCor[i,j] = correlacionSpearmanCor
            correlacionmatrixP.value[i,j] = correlacionSpearmanP.value
          }
        }
        
        
        
      }
      
      if (Hypot == "correlacion_Positiva"){
        # print_dev("HYPOTESIS IGUAL ")
        
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("greater"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("greater"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("greater"),conf.level = intervaloValue))$estimate[[1]],3)
            
            
            
            correlacionmatrixCor[i,j] = correlacionSpearmanCor
            correlacionmatrixP.value[i,j] = correlacionSpearmanP.value
          }
        }
        
        
        
      }
      
      if (Hypot == "correlacion_Negativa"){
        print_dev("HYPOMENOR")
        
        
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionmatrixCor[i,j] = correlacionSpearmanCor
            correlacionmatrixP.value[i,j] = correlacionSpearmanP.value
          }
        }
        # print_dev(correlacionmatrixCor)
        
        
      }
      
      ############Elima parte superior de las matrices
      print_dev("obtener parte arriba igual")
      
      correlacionmatrixCor[upper.tri(correlacionmatrixCor)] <- '----'
      diag(correlacionmatrixCor) <- '----'  
      corrMatrix <- correlacionmatrixCor
      
      correlacionmatrixP.value[upper.tri(correlacionmatrixP.value)] <- '----'
      diag(correlacionmatrixP.value) <- '----'  
      
      var <- variable
      
      if( significancia == TRUE ){  # Si activa se Calcula diferencia de medias
        
        print_dev("significancia trueeeeee")
        tipoTtest = c("Spearman","valor P")
        
        
        mat_combined1 <- rbind(correlacionmatrixCor, correlacionmatrixP.value)   
        print_dev(mat_combined1)
        print_dev("datossssssssss")
        
        secuencia1 <- rep(seq(nrow(correlacionmatrixCor),(nrow(correlacionmatrixCor)*-1)+1,length.out = 2),times=nrow(correlacionmatrixCor) - 1)
        secuencia <- c(0,secuencia1,nrow(correlacionmatrixCor))
        print_dev(secuencia)
        
        for (j in 1:nrow(correlacionmatrixCor)) {
          
          var[j+(j-1)] <- variable[j]
          var[j*2] <- " "
          
        }
        
        for (j in 1:nrow(mat_combined1)) {
          
          if(j==1) {
            temporal[j]=1
          }
          else
          {
            
            temporal[j]=temporal[[j-1]] + secuencia[j]
            
          }
          
          #columnasTemp[j] <- list.append(j)
        }
        
        corrMatrix <- mat_combined1[temporal,]
        
        
      }
      
      
      tablaCorrelacion <- as.data.frame(corrMatrix)
      tablaCorrelacion <- data.frame(var,tipoTtest,tablaCorrelacion)
      colnames(tablaCorrelacion) <- c(" ","modelo",variable)
      
      
      return(list(tablaCorrelacion,paste0(" ")))
      
    }
    
    if(pearson == FALSE &&  spearman == FALSE && kendall == TRUE )  #1  ok 
    {
      columnasTemp <- list()
      temporal <- vector()
      
      print_dev("SI CUMPLIO  # 1")
      tipoTtest = c("kendall")
      
      if (Hypot == "correlacion"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionmatrixCor[i,j] = correlacionKendallCor
            correlacionmatrixP.value[i,j] = correlacionKendallP.value
          }
        }
        
        
        
      }
      
      if (Hypot == "correlacion_Positiva"){
        # print_dev("HYPOTESIS IGUAL ")
        
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("greater"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("greater"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("greater"),conf.level = intervaloValue))$estimate[[1]],3)
            
            
            
            correlacionmatrixCor[i,j] = correlacionKendallCor
            correlacionmatrixP.value[i,j] = correlacionKendallP.value
          }
        }
        
        
        
      }
      
      if (Hypot == "correlacion_Negativa"){
        print_dev("HYPOMENOR")
        
        
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionmatrixCor[i,j] = correlacionKendallCor
            correlacionmatrixP.value[i,j] = correlacionKendallP.value
          }
        }
        # print_dev(correlacionmatrixCor)
        
        
      }
      
      ############Elima parte superior de las matrices
      print_dev("obtener parte arriba igual")
      
      correlacionmatrixCor[upper.tri(correlacionmatrixCor)] <- '----'
      diag(correlacionmatrixCor) <- '----'  
      corrMatrix <- correlacionmatrixCor
      
      correlacionmatrixP.value[upper.tri(correlacionmatrixP.value)] <- '----'
      diag(correlacionmatrixP.value) <- '----'  
      
      var <- variable
      
      if( significancia == TRUE ){  # Si activa se Calcula diferencia de medias
        
        print_dev("significancia trueeeeee")
        tipoTtest = c("kendall","valor P")
        
        
        mat_combined1 <- rbind(correlacionmatrixCor, correlacionmatrixP.value)   
        print_dev(mat_combined1)
        print_dev("datossssssssss")
        
        secuencia1 <- rep(seq(nrow(correlacionmatrixCor),(nrow(correlacionmatrixCor)*-1)+1,length.out = 2),times=nrow(correlacionmatrixCor) - 1)
        secuencia <- c(0,secuencia1,nrow(correlacionmatrixCor))
        print_dev(secuencia)
        
        for (j in 1:nrow(correlacionmatrixCor)) {
          
          var[j+(j-1)] <- variable[j]
          var[j*2] <- " "
          
        }
        
        for (j in 1:nrow(mat_combined1)) {
          
          if(j==1) {
            temporal[j]=1
          }
          else
          {
            
            temporal[j]=temporal[[j-1]] + secuencia[j]
            
          }
          
          #columnasTemp[j] <- list.append(j)
        }
        
        corrMatrix <- mat_combined1[temporal,]
        
        
      }
      
      
      tablaCorrelacion <- as.data.frame(corrMatrix)
      tablaCorrelacion <- data.frame(var,tipoTtest,tablaCorrelacion)
      colnames(tablaCorrelacion) <- c(" ","modelo",variable)
      
      
      return(list(tablaCorrelacion,paste0(" ")))
      
    }
    
    if(pearson == TRUE &&  spearman == FALSE && kendall == TRUE )  #1  ok 
      
    {
      columnasTemp <- list()
      temporal <- vector()
      
      correlacionmatrixCorP = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixP.valueP = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixCorS = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixP.valueS = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      
      print_dev("SI CUMPLIO  # 3")
      tipoTtest = c("Pearson","Kendall")
      
      if (Hypot == "correlacion"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionPearsonP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            
            correlacionmatrixCorP[i,j] = correlacionPearsonCor
            correlacionmatrixP.valueP[i,j] = correlacionPearsonP.value
            
            correlacionmatrixCorS[i,j] = correlacionKendallCor
            correlacionmatrixP.valueS[i,j] = correlacionKendallP.value
          }
        }
        
        #print_dev(variable)
        #print_dev(length((variable)))
        
      }
      
      if (Hypot == "correlacion_Positiva"){
        # print_dev("HYPOTESIS IGUAL ")
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("greater"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionPearsonP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("greater"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("greater"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("greater"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("greater"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("greater"),conf.level = intervaloValue))$estimate[[1]],3)
            
            correlacionmatrixCorP[i,j] = correlacionPearsonCor
            correlacionmatrixP.valueP[i,j] = correlacionPearsonP.value
            
            correlacionmatrixCorS[i,j] = correlacionKendallCor
            correlacionmatrixP.valueS[i,j] = correlacionKendallP.value
          }
        }
        
        
      }
      
      if (Hypot == "correlacion_Negativa"){
        print_dev("HYPOMENOR")
        
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionPearsonP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            
            correlacionmatrixCorP[i,j] = correlacionPearsonCor
            correlacionmatrixP.valueP[i,j] = correlacionPearsonP.value
            
            correlacionmatrixCorS[i,j] = correlacionKendallCor
            correlacionmatrixP.valueS[i,j] = correlacionKendallP.value
          }
        }
      }
      
      ############ Se obtiene parte de arriba de matrix Correlation
      
      print_dev("obtener parte abajo de Ambas matrices Corr")
      
      correlacionmatrixCorP[upper.tri(correlacionmatrixCorP)] <- '----'
      diag(correlacionmatrixCorP) <- '----'  
      correlacionmatrixCorS[upper.tri(correlacionmatrixCorS)] <- '----'
      diag(correlacionmatrixCorS) <- '----'  
      mat_combinedPS <- rbind(correlacionmatrixCorP, correlacionmatrixCorS)     
      
      ############ Se obtiene parte de arriba de matrix P Value
      
      correlacionmatrixP.valueP[upper.tri(correlacionmatrixP.valueP)] <- '----' 
      diag(correlacionmatrixP.valueP) <- '----'  
      correlacionmatrixP.valueS[upper.tri(correlacionmatrixP.valueS)] <- '----' 
      diag(correlacionmatrixP.valueS) <- '----'  
      mat_combinedPS_Pvalue <- rbind(correlacionmatrixP.valueP, correlacionmatrixP.valueS)     
      
      #Secuencia para  Obtener 2 metodos en la tabla
      secuencia1 <- rep(seq(nrow(correlacionmatrixCorP),(nrow(correlacionmatrixCorP)*-1)+1,length.out = 2),times=nrow(correlacionmatrixCorP) - 1)
      secuencia <- c(0,secuencia1,nrow(correlacionmatrixCorP))
      
      print_dev(secuencia)
      
      for (j in 1:nrow(correlacionmatrixCorP)) {
        
        var[j+(j-1)] <- variable[j]
        var[j*2] <- " "
        
      }
      
      for (j in 1:nrow(mat_combinedPS)) {
        
        if(j==1) {
          temporal[j]=1
        }
        else
        {
          
          
          temporal[j]=temporal[[j-1]] + secuencia[j]
          
        }}
      
      corrMatrix <- mat_combinedPS[temporal,]
      corrMatrixPvalue <- mat_combinedPS_Pvalue[temporal,]
      
      if( significancia == TRUE ){  # Si activa significancia de 2 de tres
        
        print_dev("significancia trueeeeee")
        tipoTtest = c("Pearson's r","valor P","Kendall","valor P")
        
        mat_combined1 <- rbind(corrMatrix, corrMatrixPvalue)
        print_dev("Union de significancia de pearson y speramn")
        
        
        secuencia1 <- rep(seq(nrow(mat_combinedPS),(nrow(mat_combinedPS)*-1)+1,length.out = 2),times=nrow(mat_combinedPS) - 1)
        secuencia <- c(0,secuencia1,nrow(mat_combinedPS))
        print_dev(secuencia)
        
        maxVar = nrow(mat_combined1) / 4
        print_dev(maxVar)
        
        for (j in 1:maxVar) {
          
          var[((j*4)- 3)] = variable[[j]]
          var[(j*4)- (2)] = " "
          var[(j*4)- (1)] = " "
          var[(j*4)] = " "
          
        }
        
        
        
        for (j in 1:nrow(mat_combined1)) {
          
          if(j==1) {
            temporal[j]=1
          }
          else
          {
            
            temporal[j]=temporal[[j-1]] + secuencia[j]
            
          }
          
        }
        print_dev(temporal)
        corrMatrix <- mat_combined1[temporal,]
        
        
      }
      
      tablaCorrelacion <- as.data.frame(corrMatrix)
      tablaCorrelacion <- data.frame(var,tipoTtest,tablaCorrelacion)
      colnames(tablaCorrelacion) <- c(" ","modelo",variable)
      
      return(list(tablaCorrelacion,paste0(" ")))
      
    }
    
    
    if(pearson == TRUE &&  spearman == TRUE && kendall == FALSE )  #1  ok 
      
    {
      columnasTemp <- list()
      temporal <- vector()
      
      correlacionmatrixCorP = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixP.valueP = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixCorS = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixP.valueS = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      
      print_dev("SI CUMPLIO  # 3")
      tipoTtest = c("Pearson","Spearman")
      
      if (Hypot == "correlacion"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionPearsonP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            
            correlacionmatrixCorP[i,j] = correlacionPearsonCor
            correlacionmatrixP.valueP[i,j] = correlacionPearsonP.value
            
            correlacionmatrixCorS[i,j] = correlacionSpearmanCor
            correlacionmatrixP.valueS[i,j] = correlacionSpearmanP.value
          }
        }
        
        #print_dev(variable)
        #print_dev(length((variable)))
        
      }
      
      if (Hypot == "correlacion_Positiva"){
        # print_dev("HYPOTESIS IGUAL ")
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("greater"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionPearsonP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("greater"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("greater"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("greater"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("greater"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("greater"),conf.level = intervaloValue))$estimate[[1]],3)
            
            correlacionmatrixCorP[i,j] = correlacionPearsonCor
            correlacionmatrixP.valueP[i,j] = correlacionPearsonP.value
            
            correlacionmatrixCorS[i,j] = correlacionSpearmanCor
            correlacionmatrixP.valueS[i,j] = correlacionSpearmanP.value
          }
        }
        
        
      }
      
      if (Hypot == "correlacion_Negativa"){
        print_dev("HYPOMENOR")
        
        #
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionPearsonP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            
            correlacionmatrixCorP[i,j] = correlacionPearsonCor
            correlacionmatrixP.valueP[i,j] = correlacionPearsonP.value
            
            correlacionmatrixCorS[i,j] = correlacionSpearmanCor
            correlacionmatrixP.valueS[i,j] = correlacionSpearmanP.value
          }
        }
        
      }
      
      ############ Se obtiene parte de arriba de matrix Correlation
      
      print_dev("obtener parte abajo de Ambas matrices Corr")
      
      correlacionmatrixCorP[upper.tri(correlacionmatrixCorP)] <- '----'
      diag(correlacionmatrixCorP) <- '----'  
      correlacionmatrixCorS[upper.tri(correlacionmatrixCorS)] <- '----'
      diag(correlacionmatrixCorS) <- '----'  
      mat_combinedPS <- rbind(correlacionmatrixCorP, correlacionmatrixCorS)     
      
      ############ Se obtiene parte de arriba de matrix P Value
      
      correlacionmatrixP.valueP[upper.tri(correlacionmatrixP.valueP)] <- '----' 
      diag(correlacionmatrixP.valueP) <- '----'  
      correlacionmatrixP.valueS[upper.tri(correlacionmatrixP.valueS)] <- '----' 
      diag(correlacionmatrixP.valueS) <- '----'  
      mat_combinedPS_Pvalue <- rbind(correlacionmatrixP.valueP, correlacionmatrixP.valueS)     
      
      #Secuencia para  Obtener 2 metodos en la tabla
      secuencia1 <- rep(seq(nrow(correlacionmatrixCorP),(nrow(correlacionmatrixCorP)*-1)+1,length.out = 2),times=nrow(correlacionmatrixCorP) - 1)
      secuencia <- c(0,secuencia1,nrow(correlacionmatrixCorP))
      
      print_dev(secuencia)
      
      for (j in 1:nrow(correlacionmatrixCorP)) {
        
        var[j+(j-1)] <- variable[j]
        var[j*2] <- " "
        
      }
     
      for (j in 1:nrow(mat_combinedPS)) {
        
        if(j==1) {
          temporal[j]=1
          }
        else
        {
      
   
          temporal[j]=temporal[[j-1]] + secuencia[j]
          
        }}
      
      corrMatrix <- mat_combinedPS[temporal,]
      corrMatrixPvalue <- mat_combinedPS_Pvalue[temporal,]
      
      if( significancia == TRUE ){  # Si activa significancia de 2 de tres
        
        print_dev("significancia trueeeeee")
        tipoTtest = c("Pearson's r","valor P","Spearman","valor P")

        mat_combined1 <- rbind(corrMatrix, corrMatrixPvalue)
        print_dev("Union de significancia de pearson y speramn")
  
        
        secuencia1 <- rep(seq(nrow(mat_combinedPS),(nrow(mat_combinedPS)*-1)+1,length.out = 2),times=nrow(mat_combinedPS) - 1)
        secuencia <- c(0,secuencia1,nrow(mat_combinedPS))
        print_dev(secuencia)
        
        maxVar = nrow(mat_combined1) / 4
        print_dev(maxVar)
        
        for (j in 1:maxVar) {
         
            var[((j*4)- 3)] = variable[[j]]
            var[(j*4)- (2)] = " "
            var[(j*4)- (1)] = " "
            var[(j*4)] = " "
       
        }


        
        for (j in 1:nrow(mat_combined1)) {
        
          if(j==1) {
            temporal[j]=1
           }
          else
          {
         
            temporal[j]=temporal[[j-1]] + secuencia[j]
        
          }
        
        }
        print_dev(temporal)
        corrMatrix <- mat_combined1[temporal,]
      
      
}

      tablaCorrelacion <- as.data.frame(corrMatrix)
      tablaCorrelacion <- data.frame(var,tipoTtest,tablaCorrelacion)
      colnames(tablaCorrelacion) <- c(" ","modelo",variable)
      
      return(list(tablaCorrelacion,paste0(" ")))
      
    }
    
    if(pearson == FALSE &&  spearman == TRUE && kendall == TRUE )  #1  ok 
      
    {
      columnasTemp <- list()
      temporal <- vector()
      
      correlacionmatrixCorP = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixP.valueP = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixCorS = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixP.valueS = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      
      print_dev("SI CUMPLIO  # 3")
      tipoTtest = c("Spearman","Kendall")
      
      if (Hypot == "correlacion"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            
            correlacionmatrixCorP[i,j] = correlacionSpearmanCor
            correlacionmatrixP.valueP[i,j] = correlacionSpearmanP.value
            
            correlacionmatrixCorS[i,j] = correlacionKendallCor
            correlacionmatrixP.valueS[i,j] = correlacionKendallP.value
          }
        }
        
        #print_dev(variable)
        #print_dev(length((variable)))
        
      }
      
      if (Hypot == "correlacion_Positiva"){
        # print_dev("HYPOTESIS IGUAL ")
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("greater"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("greater"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("greater"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("greater"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("greater"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("greater"),conf.level = intervaloValue))$estimate[[1]],3)
            
            correlacionmatrixCorP[i,j] = correlacionSpearmanCor
            correlacionmatrixP.valueP[i,j] = correlacionSpearmanP.value
            
            correlacionmatrixCorS[i,j] = correlacionKendallCor
            correlacionmatrixP.valueS[i,j] = correlacionKendallP.value
          }
        }
        
        
      }
      
      if (Hypot == "correlacion_Negativa"){
        print_dev("HYPOMENOR")
        
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            
            correlacionmatrixCorP[i,j] = correlacionSpearmanCor
            correlacionmatrixP.valueP[i,j] = correlacionSpearmanP.value
            
            correlacionmatrixCorS[i,j] = correlacionKendallCor
            correlacionmatrixP.valueS[i,j] = correlacionKendallP.value
          }
        }
      }
      
      ############ Se obtiene parte de arriba de matrix Correlation
      
      print_dev("obtener parte abajo de Ambas matrices Corr")
      
      correlacionmatrixCorP[upper.tri(correlacionmatrixCorP)] <- '----'
      diag(correlacionmatrixCorP) <- '----'  
      correlacionmatrixCorS[upper.tri(correlacionmatrixCorS)] <- '----'
      diag(correlacionmatrixCorS) <- '----'  
      mat_combinedPS <- rbind(correlacionmatrixCorP, correlacionmatrixCorS)     
      
      ############ Se obtiene parte de arriba de matrix P Value
      
      correlacionmatrixP.valueP[upper.tri(correlacionmatrixP.valueP)] <- '----' 
      diag(correlacionmatrixP.valueP) <- '----'  
      correlacionmatrixP.valueS[upper.tri(correlacionmatrixP.valueS)] <- '----' 
      diag(correlacionmatrixP.valueS) <- '----'  
      mat_combinedPS_Pvalue <- rbind(correlacionmatrixP.valueP, correlacionmatrixP.valueS)     
      
      #Secuencia para  Obtener 2 metodos en la tabla
      secuencia1 <- rep(seq(nrow(correlacionmatrixCorP),(nrow(correlacionmatrixCorP)*-1)+1,length.out = 2),times=nrow(correlacionmatrixCorP) - 1)
      secuencia <- c(0,secuencia1,nrow(correlacionmatrixCorP))
      
      print_dev(secuencia)
      
      for (j in 1:nrow(correlacionmatrixCorP)) {
        
        var[j+(j-1)] <- variable[j]
        var[j*2] <- " "
        
      }
      
      for (j in 1:nrow(mat_combinedPS)) {
        
        if(j==1) {
          temporal[j]=1
        }
        else
        {
          
          
          temporal[j]=temporal[[j-1]] + secuencia[j]
          
        }}
      
      corrMatrix <- mat_combinedPS[temporal,]
      corrMatrixPvalue <- mat_combinedPS_Pvalue[temporal,]
      
      if( significancia == TRUE ){  # Si activa significancia de 2 de tres
        
        print_dev("significancia trueeeeee")
        tipoTtest = c("Spearman's r","valor P","Kendall","valor P")
        
        mat_combined1 <- rbind(corrMatrix, corrMatrixPvalue)
        print_dev("Union de significancia de spearman y speramn")
        
        
        secuencia1 <- rep(seq(nrow(mat_combinedPS),(nrow(mat_combinedPS)*-1)+1,length.out = 2),times=nrow(mat_combinedPS) - 1)
        secuencia <- c(0,secuencia1,nrow(mat_combinedPS))
        print_dev(secuencia)
        
        maxVar = nrow(mat_combined1) / 4
        print_dev(maxVar)
        
        for (j in 1:maxVar) {
          
          var[((j*4)- 3)] = variable[[j]]
          var[(j*4)- (2)] = " "
          var[(j*4)- (1)] = " "
          var[(j*4)] = " "
          
        }
        
        
        
        for (j in 1:nrow(mat_combined1)) {
          
          if(j==1) {
            temporal[j]=1
          }
          else
          {
            
            temporal[j]=temporal[[j-1]] + secuencia[j]
            
          }
          
        }
        print_dev(temporal)
        corrMatrix <- mat_combined1[temporal,]
        
        
      }
      
      tablaCorrelacion <- as.data.frame(corrMatrix)
      tablaCorrelacion <- data.frame(var,tipoTtest,tablaCorrelacion)
      colnames(tablaCorrelacion) <- c(" ","modelo",variable)
      
      return(list(tablaCorrelacion,paste0(" ")))
      
    }
    
    
    if(pearson == TRUE &&  spearman == TRUE && kendall == TRUE )  #1  ok 
      
    {
      columnasTemp <- list()
      temporal <- vector()
      
      correlacionmatrixCorP = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixP.valueP = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixCorS = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixP.valueS = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixCorK = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      correlacionmatrixP.valueK = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas)
      
      print_dev("SI CUMPLIO  # 3")
      tipoTtest = c("Pearson","Spearman","Kendall")
      
      if (Hypot == "correlacion"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionPearsonP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
            
            
            correlacionmatrixCorP[i,j] = correlacionPearsonCor
            correlacionmatrixP.valueP[i,j] = correlacionPearsonP.value
            
            correlacionmatrixCorS[i,j] = correlacionSpearmanCor
            correlacionmatrixP.valueS[i,j] = correlacionSpearmanP.value
            
            correlacionmatrixCorK[i,j] = correlacionKendallCor
            correlacionmatrixP.valueK[i,j] = correlacionKendallP.value
          }
        }
        
  
        
      }
      
      if (Hypot == "correlacion_Positiva"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionPearsonP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            
            
            correlacionmatrixCorP[i,j] = correlacionPearsonCor
            correlacionmatrixP.valueP[i,j] = correlacionPearsonP.value
            
            correlacionmatrixCorS[i,j] = correlacionSpearmanCor
            correlacionmatrixP.valueS[i,j] = correlacionSpearmanP.value
            
            correlacionmatrixCorK[i,j] = correlacionKendallCor
            correlacionmatrixP.valueK[i,j] = correlacionKendallP.value
          }
        }
        
        
        
      }
      
      if (Hypot == "correlacion_Negativa"){
        print_dev("HYPOMENOR")
        
        for (i in 1:length(variable)) {
          for (j in 1:length(variable)) {
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionSpearmanP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionSpearmanP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$p.value < 0.01)
            {
              correlacionKendallP.value     <- "< 0.01"
              
            }
            else
            {
              correlacionKendallP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$p.value,3)
            }
            
            correlacionSpearmanCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("spearman"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            correlacionKendallCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("kendall"),alternative = c("less"),conf.level = intervaloValue))$estimate[[1]],3)
            
            correlacionmatrixCorP[i,j] = correlacionSpearmanCor
            correlacionmatrixP.valueP[i,j] = correlacionSpearmanP.value
            
            correlacionmatrixCorS[i,j] = correlacionKendallCor
            correlacionmatrixP.valueS[i,j] = correlacionKendallP.value
          }
        }
      }
      
      ############ Se obtiene parte de arriba de matrix Correlation
      
      print_dev("obtener parte abajo de tres matrices Corr")
      
      correlacionmatrixCorP[upper.tri(correlacionmatrixCorP)] <- '----'
      diag(correlacionmatrixCorP) <- '----'  
      correlacionmatrixCorS[upper.tri(correlacionmatrixCorS)] <- '----'
      diag(correlacionmatrixCorS) <- '----'  
      correlacionmatrixCorK[upper.tri(correlacionmatrixCorK)] <- '----'
      diag(correlacionmatrixCorK) <- '----'  
      
      mat_combinedPS <- rbind(correlacionmatrixCorP, correlacionmatrixCorS) 
      mat_combinedPSK <- rbind(mat_combinedPS, correlacionmatrixCorK) 
      
      ############ Se obtiene parte de arriba de matrix P Value
      
      correlacionmatrixP.valueP[upper.tri(correlacionmatrixP.valueP)] <- '----' 
      diag(correlacionmatrixP.valueP) <- '----'  
      correlacionmatrixP.valueS[upper.tri(correlacionmatrixP.valueS)] <- '----' 
      diag(correlacionmatrixP.valueS) <- '----'  
      correlacionmatrixP.valueK[upper.tri(correlacionmatrixP.valueK)] <- '----' 
      diag(correlacionmatrixP.valueK) <- '----'  
      
      mat_combinedPS_Pvalue <- rbind(correlacionmatrixP.valueP, correlacionmatrixP.valueS)     
      mat_combinedPSK_Pvalue <- rbind(mat_combinedPS_Pvalue, correlacionmatrixP.valueK)  
      
     # print_dev(mat_combinedPSK)
    #  print_dev(nrow(mat_combinedPSK))
      #Secuencia para  Obtener 3 metodos en la tabla
      
      nVariables =nFilas
      nUnido= nrow(mat_combinedPSK)
      print_dev(nVariables)
      print_dev(nUnido)
      
      secuencia1 = seq(nVariables,nVariables,length = 2)
      secuencia = c(0,rep(c(secuencia1,(nVariables-(nUnido-1))),times=(nVariables-1)),secuencia1)
      print_dev(secuencia)
      
     
      maxVar = nrow(mat_combinedPSK) / 3
      print_dev(maxVar)
      
      for (j in 1:maxVar) {
        
        var[((j*3)- 2)] = variable[[j]]
        var[(j*3)- (1)] = " "
        var[(j*3)] = " "
        
        
      }
      
      
      for (j in 1:nrow(mat_combinedPSK)) {
        
        if(j==1) {
          temporal[j]=1
        }
        else
        {
          
          
          temporal[j]=temporal[[j-1]] + secuencia[j]
          
        }}
      
     # print_dev(temporal)
     #print_dev(mat_combinedPSK)
      corrMatrix <- mat_combinedPSK[temporal,]
      corrMatrixPvalue <- mat_combinedPSK_Pvalue[temporal,]
      print_dev(corrMatrix)
      
      
      if( significancia == TRUE ){  # Si activa significancia de 2 de tres
        
        print_dev("significancia trueeeeee")
        tipoTtest = c("Pearson","valor P","Spearman's r","valor P","Kendall","valor P")
        
        print_dev("j,dfsnjkbfdjdjkd")
        
        print_dev(mat_combinedPSK)
        print_dev("j,dfsnjkbfdjdjkd")
        print_dev(mat_combinedPSK_Pvalue)
        print_dev("nbbxcxcbxcxzcxzcxzcxzczxc")
        mat_combined1 <- rbind(corrMatrix, corrMatrixPvalue)
        print_dev("Union de significancia de Pearson, Spearman y kendall")
        
        
        secuencia1 <- rep(seq(nrow(mat_combinedPSK),(nrow(mat_combinedPSK)*-1)+1,length.out = 2),times=nrow(mat_combinedPSK) - 1)
        secuencia <- c(0,secuencia1,nrow(mat_combinedPSK))
        print_dev(secuencia)
        
        maxVar = nrow(mat_combined1) / 6
        print_dev(maxVar)
        
        for (j in 1:maxVar) {
          
          var[((j*6)- 5)] = variable[[j]]
          var[(j*6)- 4] = " "
          var[(j*6)- 3] = " "
          var[(j*6)- 2] = " "
          var[(j*6)- 1] = " "
          var[(j*6)] = " "
          
        }
        
        
        
        for (j in 1:nrow(mat_combined1)) {
          
          if(j==1) {
            temporal[j]=1
          }
          else
          {
            
            temporal[j]=temporal[[j-1]] + secuencia[j]
            
          }
          
        }
        print_dev(temporal)
        corrMatrix <- mat_combined1[temporal,]
        
        
      }
      
      tablaCorrelacion <- as.data.frame(corrMatrix)
      tablaCorrelacion <- data.frame(var,tipoTtest,tablaCorrelacion)
      colnames(tablaCorrelacion) <- c(" ","modelo",variable)
      
      return(list(tablaCorrelacion,paste0(" ")))
      
    }
    
    
    
  }
  
 
}

calculoMapadeCalor_Correlacion <- function(session, df, Agrupamiento, Dependiente, mapaCalor) {
    
    agrupamiento = c()
    total = c()
    valorConfidencia="95"
    intervaloValue <- (as.numeric(valorConfidencia))
   
    
    variable <- c(names(Agrupamiento))
    metodo <- c()
    var <- c()
    nColumnas <- length(variable)
    nFilas <- length(variable)
    
      correlacionmatrixCor = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas) #  
      correlacionmatrixP.value = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas) #  
      
      print_dev("Si es menor y cumple requisitos de calor de correlacion")
      
      intervaloValue <- intervaloValue/100
      # print_dev(intervaloValue)
      
        columnasTemp <- list()
        temporal <- vector()
        
        print_dev("SI CUMPLIO  # 1")
        tipoTtest = c("pearson")

          for (i in 1:length(variable)) {
            for (j in 1:length(variable)) {
              
              if((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value < 0.01)
              {
                correlacionPearsonP.value     <- "< 0.01"
                
              }
              else
              {
                correlacionPearsonP.value <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$p.value,3)
              }
              
              correlacionPearsonCor <- round((cor.test(Agrupamiento[[i]],Agrupamiento[[j]] , method=c("pearson"),alternative = c("two.sided"),conf.level = intervaloValue))$estimate[[1]],3)
              correlacionmatrixCor[i,j] = correlacionPearsonCor
              correlacionmatrixP.value[i,j] = correlacionPearsonP.value
            }
          }
          
        
          
          colnames(correlacionmatrixCor) <- variable
          rownames(correlacionmatrixCor) <- variable
          print_dev(correlacionmatrixCor)
        return(correlacionmatrixCor)
    
}

calculoEstadistica_Correlacion <- function(session, df, Agrupamiento, Dependiente, estadistica) {
  
  agrupamiento = c()
  total = c()
  valorConfidencia="95"
  intervaloValue <- (as.numeric(valorConfidencia))
  
  
  variable <- c(names(Agrupamiento))
  metodo <- c()
  var <- c()
  nColumnas <- length(variable)
  nFilas <- length(variable)
  

    correlacionmatrixCor = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas) #  
    correlacionmatrixP.value = matrix(numeric(nColumnas*nFilas), nrow = nColumnas, ncol = nFilas) #  
    
    print_dev("Si es menor y cumple requisitos de descriptivass")
    
    intervaloValue <- intervaloValue/100
    # print_dev(intervaloValue)
    
      columnasTemp <- list()
      temporal <- vector()
      
    #  print_dev(Agrupamiento)
      
      data <- data.frame(Agrupamiento)
      
      print_dev(data)
      return(data)
 
  }

table_calculoTCorrelacion <- reactive({
  
  calculoTCorrelacion(
    session,values$mydata,df_TCorrelacion_Seleccion_Dependiente(),df_TCorrelacion_Seleccion_Agrupamiento(),
    checkCorrelacionTipoPearson(),checkCorrelacionTipoSpearman(),checkCorrelacionTipoKendall(),
    radioCorrelacionHypoValue(),checkCorrelacion_Significancia(),checkCorrelacion_Estadisticas()
     )
  
  
  
})

output$mapaCalor <- renderPlot({
  
  
  
  if(checkCorrelacion_MapaCalor()==TRUE) {
         if (length(df_TCorrelacion_Seleccion_Dependiente()) >= 2){
      
        print_dev("si ok plot de calor")
        
      col<- colorRampPalette(c("blue", "white", "red"))(20)
      heatmap(x = calculoMapadeCalor_Correlacion(session,values$mydata,df_TCorrelacion_Seleccion_Dependiente(),
                                                 df_TCorrelacion_Seleccion_Agrupamiento(),checkCorrelacion_MapaCalor()
                                                  ), col = col, symm = TRUE)
      
      } else {
        
        shinyalert::shinyalert("Oops!", "Se necesita al menos dos variables para gr\u00e1fica de calor", type = "error",size = "xs")
        
      }
  }
  
}, bg="transparent")


output$estadisticaCorrelacion <- renderPlot({
  
  if(checkCorrelacion_Estadisticas()==TRUE){

    PerformanceAnalytics::chart.Correlation(calculoEstadistica_Correlacion(session,values$mydata,df_TCorrelacion_Seleccion_Dependiente(),df_TCorrelacion_Seleccion_Agrupamiento(),
                                                   checkCorrelacion_Estadisticas()),  method = c("pearson", "kendall", "spearman"),histogram=TRUE, pch=19)
  }
  }, bg="transparent")

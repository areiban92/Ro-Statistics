#' tTestFuntions 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


calculo_Ttest_Stadisticas <- function(df,Agrupamiento,Dependiente,Estadistica) {
  
  print_dev <- NULL
  #print_dev("Stadisticas ttes")
  #variabless <- c(names(Dependiente))
  #nombreAgrupamiento <- c(variable[[1]])
  #nombreAgrupamiento <- 
  levelsDependienteNombre <- sapply(Dependiente, levels)
  print_dev(levelsDependienteNombre)
  nombreAgrupamiento <- c((c(names(Agrupamiento)))[[1]]," ")
  #print_dev(utils::utils::str(Dependiente))
  #print_dev(variabless)
  
  if(Estadistica==TRUE){
    
    mean_Ttest_Descriptivas <- with(df, tapply(Agrupamiento[[1]], Dependiente[[1]], mean))
    sd_Ttest_Descriptivas <- with(df, tapply(Agrupamiento[[1]], Dependiente[[1]], sd))  
    se_Ttest_Descriptivas <- with(df, tapply(Agrupamiento[[1]], Dependiente[[1]], rapportools::se.mean))
    lenght_Ttest_Descriptivas <-  with(df, tapply(Agrupamiento[[1]], Dependiente[[1]], length))
    se_error <- sqrt((sd_Ttest_Descriptivas[[1]]**2/(lenght_Ttest_Descriptivas[[1]])) + (sd_Ttest_Descriptivas[[2]]**2/(lenght_Ttest_Descriptivas[[2]]))) 
    
    stadisticas_TtestTable <- data.frame(nombreAgrupamiento,
                                         levelsDependienteNombre,lenght_Ttest_Descriptivas,
                                         mean_Ttest_Descriptivas,sd_Ttest_Descriptivas,se_Ttest_Descriptivas)
    colnames(stadisticas_TtestTable) <- c(" ","Grupo","N","mean","SD","SE")
    row.names(stadisticas_TtestTable) <- NULL
    
    stadisticas_TtestTable <- dplyr::mutate(stadisticas_TtestTable, dplyr::across(where(is.numeric), round, 3))
    return(list((stadisticas_TtestTable)," "))
  }
  else {
    
    print_dev("ENTRO FALSO T TESTTTTTTTTTTTTTTTTTTTT")
    mean_Ttest_Descriptivas <- " "
    sd_Ttest_Descriptivas <- " " 
    se_Ttest_Descriptivas <- " "
    lenght_Ttest_Descriptivas <-  " "
    se_error <- " "
    
    
    stadisticas_TtestTable <- data.frame(nombreAgrupamiento,
                                         levelsDependienteNombre,lenght_Ttest_Descriptivas,
                                         mean_Ttest_Descriptivas,sd_Ttest_Descriptivas,se_Ttest_Descriptivas)
    colnames(stadisticas_TtestTable) <- c(" ","Grupo","N","mean","SD","SE")
    row.names(stadisticas_TtestTable) <- NULL
    
    stadisticas_TtestTable <- dplyr::mutate(stadisticas_TtestTable, dplyr::across(where(is.numeric), round, 3))
    return(list((stadisticas_TtestTable)," "))
    
    
  }
  
  
  #  error <- sqrt((sd_Ttest_Descriptivas[[1]]**2/(lenght_Ttest_Descriptivas[[1]])) + (sd_Ttest_Descriptivas[[2]]**2/(lenght_Ttest_Descriptivas[[2]])))
  #print_dev(se_error)
  # group_by(df, "group") %>%
  #    summarise(
  #     # count = n(),
  #      mean = mean(Agrupamiento[[1]], na.rm = TRUE),
  #      sd = sd(Agrupamiento[[1]], na.rm = TRUE)
  #    )
  
}

calculo_Ttest_Student <- function(session,df,Agrupamiento,Dependiente,Hypot,student,Welch,Mann,localizacion,intervaloActiva,intervalo) {
  
  print_dev <- NULL
  localizacionGT = FALSE
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  intervaloValue <- (as.numeric(intervalo))
  
  # print_dev(variable)
  nombreDependiente <- variable2[[1]]
  # print_dev(Hypot)
  
  levelsDependiente <- length(sapply(Dependiente, levels))
  levelsDependienteNombre <- sapply(Dependiente, levels)
  
  
  if(levelsDependiente==2 && intervaloValue  < 100 ){
    
    print_dev("Si es menor y cumple requisitos")
    
    intervaloValue <- intervaloValue/100
    print_dev(intervaloValue)
    
    if(student == FALSE &&  Welch == FALSE && Mann == FALSE ) # ok revissar
    {
      
      nombreAgrupamiento <- " "
      valorEstadistico <- " "
      valorParametro <- " "
      valorPvalue <- " "
      #print_dev(resss)
      correlacionTabla <- data.frame(nombreAgrupamiento,valorPvalue)
      colnames(correlacionTabla) <- c(" --","--")
      return(list(correlacionTabla," ",localizacionGT))
      
      
    }
    
    
    if(student == TRUE &&  Welch == FALSE && Mann == FALSE )  #1  ok 
      
    {
      
      print_dev("SI CUMPLIO  # 1")
      
      tipoTtest = c("student")
      statictis = c()
      df = c() 
      p = c()
      meanValues = c()
      stderr = c()
      confInt = c()
      
      
      nombreAgrupamiento <- c(variable[[1]])
      
      
      print_dev("SI CUMPLIO True False False ")
      
      if (Hypot == "hipotesis_Igual"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided", var.equal = TRUE, conf.level = intervaloValue)
        print_dev(utils::str(student))
        #Valores del Mann
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMedias <- student$estimate
        valorstderr  <- round(student$stderr,3)
        valorconfInt <- student$conf.int
        
        
        statictis <- c(valorEstadisticoStudent)
        df <-  c(valorParametroStudent)
        p <- c(valorPvalueStudent)
        meanValues = c(valorMedias[[1]]- valorMedias[[2]])
        stderr = c(valorstderr)
        
        
        
      }
      
      if (Hypot == "hipotesis_Mayor"){
        # print_dev("HYPOTESIS mayor ")
        #
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater", var.equal = TRUE,conf.level = intervaloValue)
        
        #Valores del Mann
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMedias <- student$estimate
        valorstderr  <- round(student$stderr,3)
        valorconfInt <- student$conf.int
        
        
        statictis <- c(valorEstadisticoStudent)
        df <-  c(valorParametroStudent)
        p <- c(valorPvalueStudent)
        meanValues = c(valorMedias[[1]]- valorMedias[[2]])
        stderr = c(valorstderr)
        
        #print_dev(resss)
        ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p)
        colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p")
        print_dev(utils::str(ttestTable))
        ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        
        
      }
      
      if (Hypot == "hipotesis_Menor"){
        print_dev("HYPOMENOR")
        
        
        #
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less", var.equal = TRUE,conf.level = intervaloValue)
        
        #Valores del Mann
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMedias <- student$estimate
        valorstderr  <-student$stderr
        valorconfInt <- student$conf.int
        
        
        
        statictis <- c(valorEstadisticoStudent)
        df <-  c(valorParametroStudent)
        p <- c(valorPvalueStudent)
        meanValues = c(valorMedias[[1]]- valorMedias[[2]])
        stderr = c(valorstderr)
        
        #print_dev(resss)
        ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p)
        colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p")
        
        
        
      }
      
      
      if( localizacion == TRUE ){  # Si activa se Calcula diferencia de medias
        
        
        
        if(intervaloActiva == TRUE){ # Si activa se calcula limites de intevalos
          localizacionGT=TRUE
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr, valorconfInt[[1]], valorconfInt[[2]])
          colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p","Dif de medias","Dif SE","inferior","superior")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        else
        {
          localizacionGT=FALSE
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr)
          colnames(ttestTable) <- c(" ","Test","Statistic","df","p","Diff de Media","Diff SE")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable," Nota. Prueba Student ",localizacionGT))
          
        }
        else
        {
          
          print_dev("Vaolr de P")
          print_dev(valorPvalueStudent)
          
          if(valorPvalueStudent > 0.05 ){
            #print_dev(levelsDependienteNombre[[1]])
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
          
        }
        
      }
      else{
        localizacionGT=FALSE
        ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p)
        colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p")  
        
        ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        #  ttestTable %>% dplyr::mutate_if(is.numeric, ~round(., 3))
        
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable," Nota. Prueba Student ",localizacionGT))
          
        }
        else {
          
          if(valorPvalueStudent > 0.05){
            #print_dev(levelsDependienteNombre[[1]])
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
        }
        
        
      }
      
    }
    
    
    if(student == FALSE &&  Welch == TRUE && Mann == FALSE )  #2 ok
      
    {
      localizacionGT=FALSE
      print_dev("SI CUMPLIO WELCH")
      
      tipoTtest = c("Welch")
      statictis = c()
      df = c() 
      p = c()
      meanValues = c()
      stderr = c()
      confInt = c()
      
      
      nombreAgrupamiento <- c(variable[[1]])
      
      
      print_dev("SI CUMPLIO False True False ")
      
      if (Hypot == "hipotesis_Igual"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided",conf.level = intervaloValue )
        
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- welch$parameter
        valorPvalueWelch <- welch$p.value
        
        valorMedias <- welch$estimate
        valorstderr  <-round(welch$stderr,3)
        valorconfInt <- welch$conf.int
        
        statictis <- c(valorEstadisticoWelch)
        df <-  c(valorParametroWelch)
        p <- c(valorPvalueWelch)
        meanValues = c(valorMedias[[1]]- valorMedias[[2]])
        stderr = c(valorstderr)
        
        
        # ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p)
        # colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p")
        # return(list(ttestTable," "))
        
        
      }
      
      if (Hypot == "hipotesis_Mayor"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        
        
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater",conf.level = intervaloValue )
        
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- welch$parameter
        valorPvalueWelch <- welch$p.value
        
        valorMedias <- welch$estimate
        valorstderr  <- round(welch$stderr,3)
        valorconfInt <- welch$conf.int
        
        statictis <- c(valorEstadisticoWelch)
        df <-  c(valorParametroWelch)
        p <- c(valorPvalueWelch)
        meanValues = c(valorMedias[[1]]- valorMedias[[2]])
        stderr = c(valorstderr)
        
        
        
        
        
      }
      
      if (Hypot == "hipotesis_Menor"){
        print_dev("HYPOMENOR")
        # print_dev("HYPOTESIS IGUAL ")
        #
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less",conf.level = intervaloValue )
        
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- welch$parameter
        valorPvalueWelch <- welch$p.value
        
        valorMedias <- welch$estimate
        valorstderr  <-round(welch$stderr,3)
        valorconfInt <- welch$conf.int
        
        statictis <- c(valorEstadisticoWelch)
        df <-  c(valorParametroWelch)
        p <- c(valorPvalueWelch)
        meanValues = c(valorMedias[[1]]- valorMedias[[2]])
        stderr = c(valorstderr)
        
      }
      
      
      if( localizacion == TRUE ){  # Si activa se Calcula diferencia de medias
        
        if(intervaloActiva == TRUE){ # Si activa se calcula limites de intevalos
          localizacionGT=TRUE
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr, valorconfInt[[1]], valorconfInt[[2]])
          colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p","Dif de medias","Dif SE","inferior","superior")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        else
        {
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr)
          colnames(ttestTable) <- c(" ","Test","Statistic","df","p","Diff de Media","Diff SE")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable," Nota. Prueba Welch ",localizacionGT))
          
        }
        else {
          
          if(valorPvalueWelch > 0.05){
            #print_dev(levelsDependienteNombre[[1]])
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
        }
        
      }
      else{
        
        localizacionGT=FALSE
        ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p)
        colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p")  
        ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable," Nota. Prueba Welch ",localizacionGT))
          
        }
        else {
          
          if(valorPvalueWelch > 0.05){
            
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
          
        }
        
      }
      
      
      
      
      
    }
    
    
    if(student == FALSE &&  Welch == FALSE && Mann == TRUE ) #3 ok revisado
      
    {
      
      print_dev("SI CUMPLIO WILCOM")
      localizacionGT=FALSE
      
      tipoTtest = c("Mann-Whitney")
      statictis = c()
      df = c() 
      p = c()
      
      meanValues = c()
      stderr = c()
      confInt = c()
      
      
      
      nombreAgrupamiento <- c(variable[[1]])
      # nombreDependiente <- c(variable2[[1]],"vvvvv ")
      #nombreAgrupamiento 
      
      print_dev("SI CUMPLIO False False True ")
      
      if (Hypot == "hipotesis_Igual"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided",conf.level = intervaloValue,conf.int=TRUE)
        
        print_dev(utils::str(mann))
        
        #Valores del Mann
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMedias <- mann$estimate
        valorstderr  <- " "
        valorconfInt <- mann$conf.int
        
        
        statictis <- c(valorEstadisticoMann)
        df <-  c(valorParametroMann)
        p <- c(valorPvalueMann)
        meanValues = c(valorMedias[[1]])
        stderr = c(valorstderr)
        
        #print_dev(resss)
        # ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p)
        # colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p")
        # return(list(ttestTable," "))
        
        #print_dev(ttestTable,row.names = FALSE)
      }
      
      if (Hypot == "hipotesis_Mayor"){
        # print_dev("HYPOTESIS IGUAL ")
        
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater",conf.level = intervaloValue,conf.int=TRUE)
        
        print_dev(utils::str(mann))
        
        #Valores del Mann
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMedias <- mann$estimate
        valorstderr  <- " "
        valorconfInt <- mann$conf.int
        
        
        statictis <- c(valorEstadisticoMann)
        df <-  c(valorParametroMann)
        p <- c(valorPvalueMann)
        meanValues = c(valorMedias[[1]])
        stderr = c(valorstderr)
        
        
        
      }
      
      if (Hypot == "hipotesis_Menor"){
        print_dev("HYPOMENOR")
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less",conf.level = intervaloValue,conf.int=TRUE)
        
        print_dev(utils::str(mann))
        
        #Valores del Mann
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMedias <- mann$estimate
        valorstderr  <- " "
        valorconfInt <- mann$conf.int
        
        
        statictis <- c(valorEstadisticoMann)
        df <-  c(valorParametroMann)
        p <- c(valorPvalueMann)
        meanValues = c(valorMedias[[1]])
        stderr = c(valorstderr)
        
        
        
        
      }
      
      
      
      
      if( localizacion == TRUE ){  # Si activa se Calcula diferencia de medias
        
        if(intervaloActiva == TRUE){ # Si activa se calcula limites de intevalos
          localizacionGT=TRUE
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr, valorconfInt[[1]], valorconfInt[[2]])
          print_dev(utils::str(ttestTable))
          colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p","Dif de medias","Dif SE","inferior","superior")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        else
          
        {
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr)
          colnames(ttestTable) <- c(" ","Test","Statistic","df","p","Diff de Media","Diff SE")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable," Nota. Prueba Mann-Whitney ",localizacionGT))
          
        }
        else {
          
          if(valorPvalueMann > 0.05){
            #print_dev(levelsDependienteNombre[[1]])
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
          
        }
        
      }
      else{
        localizacionGT=FALSE
        ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p)
        colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p") 
        ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable," Nota. Prueba Mann-Whitney ",localizacionGT))
          
        }
        else {
          
          if(valorPvalueMann > 0.05){
            
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
          
        }
        
      }
      
      
      
      
      
      
      
    }
    
    
    if(student == FALSE &&  Welch == TRUE && Mann == TRUE )  #4 ok
      
    {
      
      localizacionGT=FALSE
      tipoTtest = c("Welch","Mann-Whitney")
      statictis = c()
      df = c() 
      p = c()
      meanValues = c()
      stderr = c()
      confIntLower = c()
      confIntUpper = c()
      
      nombreAgrupamiento <- c(variable[[1]]," ")
      # nombreDependiente <- c(variable2[[1]],"vvvvv ")
      #nombreAgrupamiento 
      
      print_dev("SI CUMPLIO False True True ")
      
      if (Hypot == "hipotesis_Igual"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided",conf.level = intervaloValue,conf.int=TRUE)
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided",conf.level = intervaloValue )
        
        print_dev(mann)
        print_dev(welch)
        #Valores del Mann
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMediasMann <- mann$estimate
        valorstderrMann  <- " "
        valorconfIntMann <- mann$conf.int
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- round(welch$parameter, digits = 3)
        valorPvalueWelch <- welch$p.value
        valorMediasWelch <- welch$estimate
        valorstderrWelch  <-round(welch$stderr,3)
        valorconfIntWelch <- welch$conf.int
        
        statictis <- c(valorEstadisticoWelch,valorEstadisticoMann)
        df <-  c(valorParametroWelch,valorParametroMann)
        p <- c(valorPvalueWelch,valorPvalueMann)
        
        meanValues = c(valorMediasWelch[[1]]- valorMediasWelch[[2]],valorMediasMann[[1]])
        
        stderr = c(valorstderrWelch,valorstderrMann)
        confIntLower = c(valorconfIntWelch[[1]],valorconfIntMann[[1]])
        confIntUpper= c(valorconfIntWelch[[2]], valorconfIntMann[[2]])
        
      }
      
      if (Hypot == "hipotesis_Mayor"){
        # print_dev("HYPOTESIS IGUAL ")
        
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater",conf.level = intervaloValue,conf.int=TRUE)
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater",conf.level = intervaloValue )
        
        print_dev(mann)
        print_dev(welch)
        #Valores del Mann
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMediasMann <- mann$estimate
        valorstderrMann  <- " "
        valorconfIntMann <- mann$conf.int
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- round(welch$parameter, digits = 3)
        valorPvalueWelch <- welch$p.value
        valorMediasWelch <- welch$estimate
        valorstderrWelch  <-round(welch$stderr,3)
        valorconfIntWelch <- welch$conf.int
        
        statictis <- c(valorEstadisticoWelch,valorEstadisticoMann)
        df <-  c(valorParametroWelch,valorParametroMann)
        p <- c(valorPvalueWelch,valorPvalueMann)
        
        meanValues = c(valorMediasWelch[[1]]- valorMediasWelch[[2]],valorMediasMann[[1]])
        
        stderr = c(valorstderrWelch,valorstderrMann)
        confIntLower = c(valorconfIntWelch[[1]],valorconfIntMann[[1]])
        confIntUpper= c(valorconfIntWelch[[2]], valorconfIntMann[[2]])
        
        
      }
      
      if (Hypot == "hipotesis_Menor"){
        print_dev("HYPOMENOR")
        
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less",conf.level = intervaloValue,conf.int=TRUE)
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less",conf.level = intervaloValue )
        
        print_dev(mann)
        print_dev(welch)
        #Valores del Mann
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMediasMann <- mann$estimate
        valorstderrMann  <- " "
        valorconfIntMann <- mann$conf.int
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- round(welch$parameter, digits = 3)
        valorPvalueWelch <- welch$p.value
        valorMediasWelch <- welch$estimate
        valorstderrWelch  <- round(welch$stderr,3)
        valorconfIntWelch <- welch$conf.int
        
        statictis <- c(valorEstadisticoWelch,valorEstadisticoMann)
        df <-  c(valorParametroWelch,valorParametroMann)
        p <- c(valorPvalueWelch,valorPvalueMann)
        
        meanValues = c(valorMediasWelch[[1]]- valorMediasWelch[[2]],valorMediasMann[[1]])
        
        stderr = c(valorstderrWelch,valorstderrMann)
        confIntLower = c(valorconfIntWelch[[1]],valorconfIntMann[[1]])
        confIntUpper= c(valorconfIntWelch[[2]], valorconfIntMann[[2]])
        
      }
      
      
      
      if( localizacion == TRUE ){  # Si activa se Calcula diferencia de medias
        
        if(intervaloActiva == TRUE){ # Si activa se calcula limites de intevalos
          
          localizacionGT=TRUE
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr, confIntLower, confIntUpper)
          print_dev(utils::str(ttestTable))
          colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p","Dif de medias","Dif SE","inferior","superior")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        else
          
        {
          localizacionGT=FALSE
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr)
          colnames(ttestTable) <- c(" ","Test","Statistic","df","p","Diff de Media","Diff SE")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable,"  ",localizacionGT))
          
        }
        else
        {
          
          if(valorPvalueMann > 0.05 && valorPvalueWelch > 0.05){
            #print_dev(levelsDependienteNombre[[1]])
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
        }
        
      }
      else{
        localizacionGT=FALSE
        ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p)
        colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p")  
        ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable,"  ",localizacionGT))
          
        }
        else
        {
          if(valorPvalueMann > 0.05 && valorPvalueWelch > 0.05){
            
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
        }
        
        
      }
      
      
      
      
    }
    
    if(student == TRUE &&  Welch == FALSE && Mann == TRUE )  #5 ok
      
    {
      
      print_dev("SI CUMPLIO #5")
      localizacionGT=FALSE
      tipoTtest = c("Student","Mann-Whitney")
      statictis = c()
      df = c() 
      p = c()
      meanValues = c()
      stderr = c()
      confInt = c()
      
      nombreAgrupamiento <- c(variable[[1]]," ")
      # nombreDependiente <- c(variable2[[1]],"vvvvv ")
      #nombreAgrupamiento 
      
      print_dev("SI CUMPLIO True False True ")
      
      if (Hypot == "hipotesis_Igual"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided", var.equal = TRUE, conf.level = intervaloValue )
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided",conf.level = intervaloValue,conf.int=TRUE)
        
        
        
        #Valores del Mann
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMediasStudent <- student$estimate
        valorstderrStudent  <-round(student$stderr,3)
        valorconfIntStudent <- student$conf.int
        
        
        #Valores del Mann
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMediasMann <- mann$estimate
        valorstderrMann  <- " "
        valorconfIntMann <- mann$conf.int
        
        
        statictis <- c(valorEstadisticoStudent,valorEstadisticoMann)
        df <-  c(valorParametroStudent,valorParametroMann)
        p <- c(valorPvalueStudent,valorPvalueMann)
        
        meanValues = c(valorMediasStudent[[1]]- valorMediasStudent[[2]],valorMediasMann[[1]])
        stderr = c(valorstderrStudent,valorstderrMann)
        confIntLower = c(valorconfIntStudent[[1]],valorconfIntMann[[1]])
        confIntUpper= c(valorconfIntStudent[[2]], valorconfIntMann[[2]])
        
        
      }
      
      if (Hypot == "hipotesis_Mayor"){
        # print_dev("HYPOTESIS IGUAL ")
        
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater", var.equal = TRUE, conf.level = intervaloValue )
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater",conf.level = intervaloValue,conf.int=TRUE)
        
        
        
        #Valores del Mann
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMediasStudent <- student$estimate
        valorstderrStudent  <-round(student$stderr,3)
        valorconfIntStudent <- student$conf.int
        
        
        #Valores del Mann
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMediasMann <- mann$estimate
        valorstderrMann  <- " "
        valorconfIntMann <- mann$conf.int
        
        
        statictis <- c(valorEstadisticoStudent,valorEstadisticoMann)
        df <-  c(valorParametroStudent,valorParametroMann)
        p <- c(valorPvalueStudent,valorPvalueMann)
        
        meanValues = c(valorMediasStudent[[1]]- valorMediasStudent[[2]],valorMediasMann[[1]])
        stderr = c(valorstderrStudent,valorstderrMann)
        confIntLower = c(valorconfIntStudent[[1]],valorconfIntMann[[1]])
        confIntUpper= c(valorconfIntStudent[[2]], valorconfIntMann[[2]])
        
        
        
      }
      
      if (Hypot == "hipotesis_Menor"){
        print_dev("HYPOMENOR")
        # print_dev("HYPOTESIS IGUAL ")
        #
        
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less", var.equal = TRUE, conf.level = intervaloValue )
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less",conf.level = intervaloValue,conf.int=TRUE)
        
        
        
        #Valores del Mann
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMediasStudent <- student$estimate
        valorstderrStudent  <-round(student$stderr,3)
        valorconfIntStudent <- student$conf.int
        
        
        #Valores del Mann
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMediasMann <- mann$estimate
        valorstderrMann  <- " "
        valorconfIntMann <- mann$conf.int
        
        
        statictis <- c(valorEstadisticoStudent,valorEstadisticoMann)
        df <-  c(valorParametroStudent,valorParametroMann)
        p <- c(valorPvalueStudent,valorPvalueMann)
        
        meanValues = c(valorMediasStudent[[1]]- valorMediasStudent[[2]],valorMediasMann[[1]])
        stderr = c(valorstderrStudent,valorstderrMann)
        confIntLower = c(valorconfIntStudent[[1]],valorconfIntMann[[1]])
        confIntUpper= c(valorconfIntStudent[[2]], valorconfIntMann[[2]])
        
        
        
        
        
      }
      
      if( localizacion == TRUE ){  # Si activa se Calcula diferencia de medias
        
        if(intervaloActiva == TRUE){ # Si activa se calcula limites de intevalos
          localizacionGT=TRUE
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr, confIntLower, confIntUpper)
          print_dev(utils::str(ttestTable))
          colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p","Dif de medias","Dif SE","inferior","superior")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        else
          
        {
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr)
          colnames(ttestTable) <- c(" ","Test","Statistic","df","p","Diff de Media","Diff SE") 
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable,"  ",localizacionGT))
          
        }
        else
        {
          
          if(valorPvalueStudent > 0.05 && valorPvalueMann > 0.05){
            #print_dev(levelsDependienteNombre[[1]])
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
        }
      }
      else{
        
        localizacionGT=FALSE
        ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p)
        colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p")  
        ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable,"  ",localizacionGT))
          
        }
        else
        {
          
          if(valorPvalueStudent > 0.05 && valorPvalueMann > 0.05){
            
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
          
        }
      }
      
      
      
    }
    
    
    if(student == TRUE &&  Welch == TRUE && Mann == FALSE )  #6
      
    {
      
      print_dev("SI CUMPLIO #6")
      localizacionGT=FALSE
      
      tipoTtest = c("Student","Welch")
      statictis = c()
      df = c() 
      p = c()
      
      meanValues = c()
      stderr = c()
      confInt = c()
      
      nombreAgrupamiento <- c(variable[[1]]," ")
      # nombreDependiente <- c(variable2[[1]],"vvvvv ")
      #nombreAgrupamiento 
      
      print_dev("SI CUMPLIO False True True ")
      
      if (Hypot == "hipotesis_Igual"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided", var.equal = TRUE, conf.level = intervaloValue )
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided", conf.level = intervaloValue )
        
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMediasStudent <- student$estimate
        valorstderrStudent  <-round(student$stderr,3)
        valorconfIntStudent <- student$conf.int
        
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- round(welch$parameter, digits = 3)
        valorPvalueWelch <- welch$p.value
        valorMediasWelch <- welch$estimate
        valorstderrWelch  <- round(welch$stderr,3)
        valorconfIntWelch <- welch$conf.int
        
        statictis <- c(valorEstadisticoStudent,valorEstadisticoWelch)
        df <-  c(valorParametroStudent,valorParametroWelch)
        p <- c(valorPvalueStudent,valorPvalueWelch)
        
        meanValues = c(valorMediasStudent[[1]]- valorMediasStudent[[2]],valorMediasWelch[[1]]- valorMediasWelch[[2]])
        
        stderr = c(valorstderrStudent,valorstderrWelch)
        confIntLower = c(valorconfIntStudent[[1]], valorconfIntWelch[[1]])
        confIntUpper= c( valorconfIntStudent[[2]], valorconfIntWelch[[2]])
        
        
      }
      
      if (Hypot == "hipotesis_Mayor"){
        # print_dev("HYPOTESIS IGUAL ")
        #
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater", var.equal = TRUE, conf.level = intervaloValue )
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater", conf.level = intervaloValue )
        
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMediasStudent <- student$estimate
        valorstderrStudent  <-round(student$stderr,3)
        valorconfIntStudent <- student$conf.int
        
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- round(welch$parameter, digits = 3)
        valorPvalueWelch <- welch$p.value
        valorMediasWelch <- welch$estimate
        valorstderrWelch  <-round(welch$stderr,3)
        valorconfIntWelch <- welch$conf.int
        
        statictis <- c(valorEstadisticoStudent,valorEstadisticoWelch)
        df <-  c(valorParametroStudent,valorParametroWelch)
        p <- c(valorPvalueStudent,valorPvalueWelch)
        
        meanValues = c(valorMediasStudent[[1]]- valorMediasStudent[[2]],valorMediasWelch[[1]]- valorMediasWelch[[2]])
        
        stderr = c(valorstderrStudent,valorstderrWelch)
        confIntLower = c(valorconfIntStudent[[1]], valorconfIntWelch[[1]])
        confIntUpper= c( valorconfIntStudent[[2]], valorconfIntWelch[[2]])
        
        
      }
      
      if (Hypot == "hipotesis_Menor"){
        print_dev("HYPOMENOR")
        # print_dev("HYPOTESIS IGUAL ")
        #
        
        
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less", var.equal = TRUE, conf.level = intervaloValue )
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less", conf.level = intervaloValue )
        
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMediasStudent <- student$estimate
        valorstderrStudent  <-round(student$stderr,3)
        valorconfIntStudent <- student$conf.int
        
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- welch$parameter
        valorPvalueWelch <- welch$p.value
        valorMediasWelch <- welch$estimate
        valorstderrWelch  <-round(welch$stderr,3)
        valorconfIntWelch <- welch$conf.int
        
        statictis <- c(valorEstadisticoStudent,valorEstadisticoWelch)
        df <-  c(valorParametroStudent,valorParametroWelch)
        p <- c(valorPvalueStudent,valorPvalueWelch)
        
        meanValues = c(valorMediasStudent[[1]]- valorMediasStudent[[2]],valorMediasWelch[[1]]- valorMediasWelch[[2]])
        
        stderr = c(valorstderrStudent,valorstderrWelch)
        confIntLower = c(valorconfIntStudent[[1]], valorconfIntWelch[[1]])
        confIntUpper= c( valorconfIntStudent[[2]], valorconfIntWelch[[2]])
        
        
        
      }
      
      
      if( localizacion == TRUE ){  # Si activa se Calcula diferencia de medias
        
        if(intervaloActiva == TRUE){ # Si activa se calcula limites de intevalos
          
          localizacionGT=TRUE
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr, confIntLower, confIntUpper)
          print_dev(utils::str(ttestTable))
          colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p","Dif de medias","Dif SE","inferior","superior")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        else
          
        {
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr)
          colnames(ttestTable) <- c(" ","Test","Statistic","df","p","Diff de Media","Diff SE")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable,"  ",localizacionGT))
          
        }
        else
        {
          
          if(valorPvalueStudent > 0.05  && valorPvalueWelch > 0.05  ){
            #print_dev(levelsDependienteNombre[[1]])
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
        }
        
      }
      else{
        localizacionGT=FALSE
        ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p)
        colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p")  
        ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable,"  ",localizacionGT))
          
        }
        else
        {
          if(valorPvalueStudent > 0.05 && valorPvalueWelch > 0.05){
            
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
        }
        
      }
      
      
      
    }
    
    
    if(student == TRUE &&  Welch == TRUE && Mann == TRUE ) #7
      
    {
      
      print_dev("SI CUMPLIO #8")
      localizacionGT=FALSE
      
      tipoTtest = c("Student","Welch","Mann-Whitney")
      statictis = c()
      df = c() 
      p = c()
      
      meanValues = c()
      stderr = c()
      confInt = c()
      
      nombreAgrupamiento <- c(variable[[1]]," "," ")
      
      
      print_dev("SI CUMPLIO True True True ")
      
      if (Hypot == "hipotesis_Igual"){
        
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided", var.equal = TRUE, conf.level = intervaloValue )
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided", conf.level = intervaloValue )
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "two.sided",conf.level = intervaloValue,conf.int=TRUE)
        
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMediasStudent <- student$estimate
        valorstderrStudent  <-round(student$stderr,3)
        valorconfIntStudent <- student$conf.int
        
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- round(welch$parameter, digits = 3)
        valorPvalueWelch <- welch$p.value
        valorMediasWelch <- welch$estimate
        valorstderrWelch  <-round(welch$stderr,3)
        valorconfIntWelch <- welch$conf.int
        
        
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMediasMann <- mann$estimate
        valorstderrMann  <- " "
        valorconfIntMann <- mann$conf.int
        
        
        
        statictis <- c(valorEstadisticoStudent,valorEstadisticoWelch,valorEstadisticoMann)
        df <-  c(valorParametroStudent,valorParametroWelch,valorParametroMann)
        p <- c(valorPvalueStudent,valorPvalueWelch,valorPvalueMann)
        
        meanValues = c(valorMediasStudent[[1]]- valorMediasStudent[[2]],valorMediasWelch[[1]]- valorMediasWelch[[2]],valorMediasMann[[1]])
        
        stderr = c(valorstderrStudent,valorstderrWelch,valorstderrMann)
        confIntLower = c(valorconfIntStudent[[1]], valorconfIntWelch[[1]], valorconfIntMann[[1]])
        confIntUpper= c( valorconfIntStudent[[2]], valorconfIntWelch[[2]],valorconfIntMann[[2]])
        
        
        
      }
      
      if (Hypot == "hipotesis_Mayor"){
        # print_dev("HYPOTESIS IGUAL ")
        
        
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater", var.equal = TRUE, conf.level = intervaloValue )
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater", conf.level = intervaloValue )
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "greater",conf.level = intervaloValue,conf.int=TRUE)
        
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMediasStudent <- student$estimate
        valorstderrStudent  <-round(student$stderr,3)
        valorconfIntStudent <- student$conf.int
        
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- round(welch$parameter, digits = 3)
        valorPvalueWelch <- welch$p.value
        valorMediasWelch <- welch$estimate
        valorstderrWelch  <- round(welch$stderr,3)
        valorconfIntWelch <- welch$conf.int
        
        
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMediasMann <- mann$estimate
        valorstderrMann  <- " "
        valorconfIntMann <- mann$conf.int
        
        
        
        statictis <- c(valorEstadisticoStudent,valorEstadisticoWelch,valorEstadisticoMann)
        df <-  c(valorParametroStudent,valorParametroWelch,valorParametroMann)
        p <- c(valorPvalueStudent,valorPvalueWelch,valorPvalueMann)
        
        meanValues = c(valorMediasStudent[[1]]- valorMediasStudent[[2]],valorMediasWelch[[1]]- valorMediasWelch[[2]],valorMediasMann[[1]])
        
        stderr = c(valorstderrStudent,valorstderrWelch,valorstderrMann)
        confIntLower = c(valorconfIntStudent[[1]], valorconfIntWelch[[1]], valorconfIntMann[[1]])
        confIntUpper= c( valorconfIntStudent[[2]], valorconfIntWelch[[2]],valorconfIntMann[[2]])
        
        
        
      }
      
      if (Hypot == "hipotesis_Menor"){
        
        
        student <-  t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less", var.equal = TRUE, conf.level = intervaloValue )
        welch <- t.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less", conf.level = intervaloValue )
        mann <- wilcox.test(Agrupamiento[[1]] ~ Dependiente[[1]],data = df,alternative = "less",conf.level = intervaloValue,conf.int=TRUE)
        
        valorEstadisticoStudent <- student$statistic
        valorParametroStudent <- student$parameter
        valorPvalueStudent <- student$p.value
        valorMediasStudent <- student$estimate
        valorstderrStudent  <-round(student$stderr,3)
        valorconfIntStudent <- student$conf.int
        
        
        valorEstadisticoWelch <- welch$statistic
        valorParametroWelch <- round(welch$parameter, digits = 3)
        valorPvalueWelch <- welch$p.value
        valorMediasWelch <- welch$estimate
        valorstderrWelch  <- round(welch$stderr,3)
        valorconfIntWelch <- welch$conf.int
        
        
        valorEstadisticoMann <- mann$statistic
        valorParametroMann <- " "
        valorPvalueMann <- mann$p.value
        valorMediasMann <- mann$estimate
        valorstderrMann  <- " "
        valorconfIntMann <- mann$conf.int
        
        
        
        statictis <- c(valorEstadisticoStudent,valorEstadisticoWelch,valorEstadisticoMann)
        df <-  c(valorParametroStudent,valorParametroWelch,valorParametroMann)
        p <- c(valorPvalueStudent,valorPvalueWelch,valorPvalueMann)
        
        meanValues = c(valorMediasStudent[[1]]- valorMediasStudent[[2]],valorMediasWelch[[1]]- valorMediasWelch[[2]],valorMediasMann[[1]])
        
        stderr = c(valorstderrStudent,valorstderrWelch,valorstderrMann)
        confIntLower = c(valorconfIntStudent[[1]], valorconfIntWelch[[1]], valorconfIntMann[[1]])
        confIntUpper= c( valorconfIntStudent[[2]], valorconfIntWelch[[2]],valorconfIntMann[[2]])
        
        
        
      }
      
      
      if( localizacion == TRUE ){  # Si activa se Calcula diferencia de medias
        
        if(intervaloActiva == TRUE){ # Si activa se calcula limites de intevalos
          
          localizacionGT=TRUE
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr, confIntLower, confIntUpper)
          print_dev(utils::str(ttestTable))
          colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p","Dif de medias","Dif SE","inferior","superior")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        else
          
        {
          ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p,meanValues,stderr)
          colnames(ttestTable) <- c(" ","Test","Statistic","df","p","Diff de Media","Diff SE")  
          ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        }
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable,"  ",localizacionGT))
          
        }
        else
        {
          
          if(valorPvalueStudent > 0.05  && valorPvalueWelch > 0.05 && valorPvalueMann > 0.05 ){
            #print_dev(levelsDependienteNombre[[1]])
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
        }
      }
      else{
        localizacionGT=FALSE
        ttestTable <- data.frame(nombreAgrupamiento,tipoTtest, statictis,df,p)
        colnames(ttestTable) <- c(" ","Prueba","valor t","df","valor p")  
        ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
        
        if(Hypot == "hipotesis_Igual"){
          
          return(list(ttestTable," ",localizacionGT))
          
        }
        else
        {
          
          if(valorPvalueStudent > 0.05 && valorPvalueWelch > 0.05 && valorPvalueMann > 0.05) {
            
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
            
          } else 
            
          {
            return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
                                          levelsDependienteNombre[[1]]," es mayor al Grupo ",levelsDependienteNombre[[2]] ),localizacionGT))
          }
        }
        
      }
      
      
    }
    
    
  }
  else {
    
    shinyalert::shinyalert("Oops!", "El numero de niveles del factor ≠ 2 ", type = "error",size = "xs")
    
    nombreAgrupamiento <- " "
    valorEstadistico <- " "
    valorParametro <- " "
    valorPvalue <- " "
    #print_dev(resss)
    ttestTable <- data.frame(nombreAgrupamiento[[1]],valorEstadistico,valorParametro,valorPvalue)
    colnames(ttestTable) <- c(" ","t","df","p")
    #ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
    return(list(ttestTable," ",localizacionGT))
    
    # return(list(ttestTable,paste0("Nota. La hipotesis especifica que el Grupo ",
    #levelsDependienteNombre[[1]]," es menor al Grupo ",levelsDependienteNombre[[2]] )))
    
  }
  
}

calculo_Ttest_Suposiciones_Normal <- function(session,df,Agrupamiento,Dependiente,Normalidad) {
  
  print_dev <- NULL
  print_dev("Desde aqui Suposiciones Shapiro") 
  #print_dev(Normalidad)
  #print_dev("debug step")
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  # print_dev(variable)
  #print_dev(variable2)
  #nombreAgrupamiento <- variable[[1]]
  nombreDependiente <- variable2[[1]]
  
  levelsDependiente <- length(sapply(Dependiente, levels))
  levelsDependienteNombre <- sapply(Dependiente, levels)
  
  print_dev("Si es menor")
  
  if(Normalidad == FALSE ) # 
  {
    
    nombreAgrupamiento <- " "
    valorEstadistico <- " "
    valorParametro <- " "
    valorPvalue <- " "
    #print_dev(resss)
    ttestTable <- data.frame(nombreAgrupamiento,valorEstadistico,valorParametro,valorPvalue)
    colnames(ttestTable) <- c(" ","F","df","p")
    ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
    return(list(ttestTable," "))
    
  }
  
  
  if(Normalidad == TRUE )  #1  ok 
    
  {
    valorF = c()
    df = c() 
    p = c()
    
    nombreAgrupamiento <- vector(mode="character", length=levelsDependiente)
    
    nombreAgrupamiento[1] <- c(variable[[1]])   
    
    print_dev(nombreAgrupamiento)
    
    print_dev("SI CUMPLIO True ")
    
    
    shapiro <-  tapply(Agrupamiento[[1]], Dependiente[[1]], shapiro.test)
    shapiroPvalue <- unlist(lapply(shapiro, function(x) x$p.value))  
    
    # print_dev(variablesNOmbres)
    shapiroStaticstic <- unlist(lapply(shapiro, function(x) x$statistic))
    
    
    shapiroTtestTable <- data.frame(nombreAgrupamiento,levelsDependienteNombre,shapiroPvalue,shapiroStaticstic )
    print_dev(shapiroTtestTable,row.names = FALSE)
    
    colnames(shapiroTtestTable) <- c(" ","  ","p","W")
    shapiroTtestTable <- dplyr::mutate(shapiroTtestTable, dplyr::across(where(is.numeric), round, 3))
    
    return(list(shapiroTtestTable,paste0("Nota. Resultado" )))
    
    #   
    # 
    #   if(valorPvalue > 0.05){
    #     #print_dev(levelsDependienteNombre[[1]])
    #     return(list(leveneTtestTable,paste0("Nota. Levene Test" )))
    # 
    #   } else
    # 
    #   {
    #     return(list(leveneTtestTable,paste0("Nota. Levene Test" )))
    # 
    #   }
    
    
  }
}

calculo_Ttest_Suposiciones_VarianzaIgual <- function(session,df,Agrupamiento,Dependiente,VarianzaIgual) {
  
  print_dev <- NULL
  print_dev("Desde aqui Suposiciones Varianza Igual") 
  #print_dev(VarianzaIgual)
  #print_dev("debug step")
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  
  nombreDependiente <- variable2[[1]]
  
  
  levelsDependiente <- length(sapply(Dependiente, levels))
  levelsDependienteNombre <- sapply(Dependiente, levels)
  
  
  if(VarianzaIgual == FALSE ) # 
  {
    
    nombreAgrupamiento <- " "
    valorEstadistico <- " "
    valorParametro <- " "
    valorPvalue <- " "
    #print_dev(resss)
    ttestTable <- data.frame(nombreAgrupamiento,valorEstadistico,valorParametro,valorPvalue)
    colnames(ttestTable) <- c(" ","F","df","p")
    ttestTable <- dplyr::mutate(ttestTable, dplyr::across(where(is.numeric), round, 3))
    return(list(ttestTable," "))
    
    
  }
  
  
  if(VarianzaIgual == TRUE )  #1  ok 
    
  {
    valorF = c()
    df = c() 
    p = c()
    
    nombreAgrupamiento <- c(variable[[1]])
    print_dev("SI CUMPLIO True ")
    
    Levene <- car::leveneTest(Agrupamiento[[1]] ~ Dependiente[[1]],data = df, center = mean )
    print_dev(utils::str(Levene))
    
    # #Valores de Normalidad
    
    valorDFLevene <- Levene$Df[[1]]
    valorFvalue <- Levene$"F value"[[1]]
    valorPvalue <- Levene$"Pr(>F)"[[1]]
    
    leveneTtestTable <- data.frame(nombreAgrupamiento, valorFvalue,valorDFLevene,valorPvalue)
    colnames(leveneTtestTable) <- c(" ","F","df","p")
    leveneTtestTable <- dplyr::mutate(leveneTtestTable, dplyr::across(where(is.numeric), round, 3))
    
    
    if(valorPvalue > 0.05){
      #print_dev(levelsDependienteNombre[[1]])
      return(list(leveneTtestTable,paste0("Nota. Levene Test" )))
      
    } else
      
    {
      return(list(leveneTtestTable,paste0("Nota. Levene Test" )))
      
    }
    
    
  }
}


#' ANOVAFuntions 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd



calculoTanova <- function(session, df, Dependiente, Agrupamiento) {
  

  casos = c()
  sumaDeCuadrados = c()
  #df = c()
  mediaCuadrada = c()
  valorF = c()
  valorP = c()
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  nombreDependiente <- variable2[[1]]
  nombreAgrupamiento <- variable[[1]]
  
  levelsDependiente <- length(sapply(Dependiente, levels))
  levelsDependienteNombre <- sapply(Dependiente, levels)
  
  levelsAgrupamiento <- length(sapply(Agrupamiento, levels))
  levelsAgrupamientoNombre <- sapply(Agrupamiento, levels)
  
  
  casos <- c(nombreAgrupamiento,"residuales")
  
  #anovaTabla <- data.frame(nombreAgrupamiento,valorPvalue)
  #colnames(anovaTabla) <- c(" --","--")
  
  golem::print_dev("dsde aqui aov")
  resAOV <- aov(Dependiente[[1]] ~  Agrupamiento[[1]], data = df)
  resAOVsummary <- summary(resAOV)
  
  sumaDeCuadrados <- resAOVsummary[[1]]$'Sum Sq'
  dftabla <- resAOVsummary[[1]]$'Df'
  mediaCuadrada <- resAOVsummary[[1]]$'Mean Sq'
  valorF <- resAOVsummary[[1]]$'F value'
  valorP <- resAOVsummary[[1]]$'Pr(>F)'
  
  
  anovaTabla <- data.frame(casos,sumaDeCuadrados,dftabla,mediaCuadrada,valorF,valorP)
  
  anovaTabla <- dplyr::mutate(anovaTabla, dplyr::across(where(is.numeric), round, 3))
  
  
  return(list(anovaTabla," ",FALSE,resAOV))
  
}


calculoTanovaWelch <- function(session, df, Dependiente, Agrupamiento,  WelchAnova) {
  
  
  sumaDeCuadrados = c()
  
  mediaCuadrada = c()
  valorF = c()
  valorP = c()
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  nombreDependiente <- variable2[[1]]
  nombreAgrupamiento <- variable[[1]]
  
  if(WelchAnova == TRUE){
    
    levelsDependiente <- length(sapply(Dependiente, levels))
    levelsDependienteNombre <- sapply(Dependiente, levels)
    
    levelsAgrupamiento <- length(sapply(Agrupamiento, levels))
    levelsAgrupamientoNombre <- sapply(Agrupamiento, levels)
    
    golem::print_dev("dsde aqui aov")
    resAOVWelch <- oneway.test(Dependiente[[1]] ~ Agrupamiento[[1]], data = df)
    
    
    #sumaDeCuadrados <- resAOVsummary[[1]]$'Sum Sq'
    #dftabla <- resAOVsummary[[1]]$'Df'
    #mediaCuadrada <- resAOVsummary[[1]]$'Mean Sq'
    #valorF <- resAOVWelch[[1]]$'F value'
    #valorP <- resAOVWelch[[1]]$'Pr(>F)'
    valorP <- resAOVWelch[["p.value"]]
    valorF <- resAOVWelch[["statistic"]]
    
    anovaTablaWelch <- data.frame("Welch",valorF,valorP)
    colnames(anovaTablaWelch) <- c(" ","valor F ","Pr(>F)")
    
    anovaTablaWelch <- dplyr::mutate(anovaTablaWelch, dplyr::across(where(is.numeric), round, 3))
  }else{
    
    valorP <- ""
    valorF <- ""
    
    anovaTablaWelch <- data.frame("Welch",valorF,valorP)
    
    anovaTablaWelch <- dplyr::mutate(anovaTablaWelch, dplyr::across(where(is.numeric), round, 3))
    
  }
  
  return(list(anovaTablaWelch," ",FALSE))
  
}

calculoTanovaBrown <- function(session, df, Dependiente, Agrupamiento, BrownAnova) {
  
  
  sumaDeCuadrados = c()
  
  mediaCuadrada = c()
  valorF = c()
  valorP = c()
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  nombreDependiente <- variable2[[1]]
  nombreAgrupamiento <- variable[[1]]
  
  if(BrownAnova == TRUE){
    
    levelsDependiente <- length(sapply(Dependiente, levels))
    levelsDependienteNombre <- sapply(Dependiente, levels)
    
    levelsAgrupamiento <- length(sapply(Agrupamiento, levels))
    levelsAgrupamientoNombre <- sapply(Agrupamiento, levels)
    
    golem::print_dev("dsde aqui aovbrown")
    resAOVBrown <- onewaytests::bf.test(Dependiente[[1]] ~ Agrupamiento[[1]], data = df)
    
    
    #sumaDeCuadrados <- resAOVsummary[[1]]$'Sum Sq'
    #dftabla <- resAOVsummary[[1]]$'Df'
    #mediaCuadrada <- resAOVsummary[[1]]$'Mean Sq'
    #valorF <- resAOVWelch[[1]]$'F value'
    #valorP <- resAOVWelch[[1]]$'Pr(>F)'
    valorP <- resAOVBrown[["p.value"]]
    valorF <- resAOVBrown[["statistic"]]
    
    anovaTablaBrown <- data.frame("Brown-Forsythe",valorF,valorP)
    colnames(anovaTablaBrown) <- c(" ","valor F ","Pr(>F)")
    
    anovaTablaBrown <- dplyr::mutate(anovaTablaBrown, dplyr::across(where(is.numeric), round, 3))
  }else{
    
    valorP <- ""
    valorF <- ""
    
    anovaTablaBrown <- data.frame("Welch",valorF,valorP)
    
    anovaTablaBrown <- dplyr::mutate(anovaTablaBrown, dplyr::across(where(is.numeric), round, 3))
    
  }
  
  return(list(anovaTablaBrown," ",FALSE))
  
}


calculoPostHocAnovaIguales <- function(session, df,  Dependiente, Agrupamiento, checkAnovaTurkey, 
                                       checkAnovaBonferroni,anovaTablaresult) {
  
  var1AnovaIguales = c()
  var2AnovaIguales= c()
  
  sumaDeCuadrados = c()
  
  mediaCuadrada = c()
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  nombreDependiente <- variable2[[1]]
  nombreAgrupamiento <- variable[[1]]
  
  levelsDependiente <- length(sapply(Dependiente, levels))
  levelsDependienteNombre <- sapply(Dependiente, levels)
  
  levelsAgrupamiento <- length(sapply(Agrupamiento, levels))
  levelsAgrupamientoNombre <- sapply(Agrupamiento, levels)
  
  tablaAux <- TukeyHSD(anovaTablaresult )  
  #tablaAux <- TukeyHSD(table_calculo_Anova()[[4]])
  
  tablaGeneral<-as.data.frame(tablaAux[1:1])
  namesrow <- row.names(tablaGeneral)
  
  for (i in 1:length(namesrow)) {
    
    namesVars = strsplit(namesrow[i], split = "-")
    
    var1AnovaIguales[i] <- unlist(namesVars)[1]
    var2AnovaIguales[i] <- unlist(namesVars)[2]
    
  }
  
  
  tablaGeneral <- data.frame(var2AnovaIguales,var1AnovaIguales,tablaGeneral)
  turkey <- tablaGeneral[,6]
  bonferrony <- pairwise.t.test(Dependiente[[1]],Agrupamiento[[1]], p.adjust="bonferroni", pool.sd = T)
  bonferronyPvalue <- bonferrony$p.value 
  tablaGeneral <- tablaGeneral[,c(-4,-5,-6)]
  
  if(checkAnovaTurkey &&  !checkAnovaBonferroni){
    
    tablaturkeyFinal <- data.frame(tablaGeneral,turkey)
    
    colnames(tablaturkeyFinal) <- c(" ","  ","Diff Medias","Turkey")
    
    tablaturkeyFinal <- dplyr::mutate(tablaturkeyFinal, dplyr::across(where(is.numeric), round, 3))
    tablaturkeyFinal["Turkey"][tablaturkeyFinal["Turkey"] <= 0.01] <- "<0.001"
    
    #colnames(tuekeyTabla) <- c()
    # golem::print_dev(tablaturkeyFinal)
    #golem::print_dev(dimnames(turkey[[1]]))
    #golem::print_dev((turkey))
    
    
    #golem::print_dev(table_calculoTanova()[[4]])
    return(list(tablaturkeyFinal," ",FALSE," "))
    
  } 
  
  if(checkAnovaBonferroni && !checkAnovaTurkey) {
    
    golem::print_dev("Bonferroni")
    
    bonPvalue=c(bonferronyPvalue)
    bonPvalue<-bonPvalue[!is.na(bonPvalue)]
    
    
    tablabonferroniFinal <- data.frame(tablaGeneral,bonPvalue)
    
    #golem::print_dev(tablabonferroniFinal)
    colnames(tablabonferroniFinal) <- c(" ","  ","Diff Medias","Bonferroni")
    tablabonferroniFinal <- dplyr::mutate(tablabonferroniFinal, dplyr::across(where(is.numeric), round, 3))
    tablabonferroniFinal <- tablabonferroniFinal[order(tablabonferroniFinal$" "),]
    
    tablabonferroniFinal["Bonferroni"][tablabonferroniFinal["Bonferroni"] <= 0.01] <- "<0.001"
    
    
    return(list(tablabonferroniFinal," ",FALSE," "))
    
  }
  
  if(checkAnovaBonferroni && checkAnovaTurkey) {
    
    golem::print_dev("final")
    bonPvalue=c(bonferronyPvalue)
    bonPvalue<-bonPvalue[!is.na(bonPvalue)]
    
    tablaPostHOcFinal <- data.frame(tablaGeneral,turkey,bonPvalue)
    colnames(tablaPostHOcFinal) <- c(" ","  ","Diff Medias","Turkey","Bonferroni")
    tablaPostHOcFinal <- dplyr::mutate(tablaPostHOcFinal, dplyr::across(where(is.numeric), round, 3))
    tablaPostHOcFinal <- tablaPostHOcFinal[order(tablaPostHOcFinal$" "),]
    
    
    tablaPostHOcFinal["Bonferroni"][tablaPostHOcFinal["Bonferroni"] <= 0.01] <- "<0.001"
    tablaPostHOcFinal["Turkey"][tablaPostHOcFinal["Turkey"] <= 0.01] <- "<0.001"
    
    return(list(tablaPostHOcFinal," ",FALSE," "))
    
  }
  
  if(!checkAnovaTurkey && !checkAnovaBonferroni)
    
  {
    golem::print_dev("Entro en vacio")
    
    mean_Anova_Iguales <- " "
    sd_Anova_Iguales <- " "
    
    posthoc_Anova_Des_empty <- data.frame(mean_Anova_Iguales,  sd_Anova_Iguales)
    
    colnames(posthoc_Anova_Des_empty) <- c(" ","  ")
    row.names(posthoc_Anova_Des_empty) <- NULL
    
    posthoc_Anova_Des_empty <- dplyr::mutate(posthoc_Anova_Des_empty, dplyr::across(where(is.numeric), round, 3))
    posthoc_Anova_Des_empty = posthoc_Anova_Des_empty[FALSE,]
    
    return(list((posthoc_Anova_Des_empty)," "))
    
    
  }
  
  
  
}

calculoPostHocAnovaDesiguales <- function(session, df,  Dependiente, Agrupamiento, checkAnovaGamesHowell )
{
  
  
  casos = c()
  sumaDeCuadrados = c()
  var1AnovaDesiguales = c()
  var2AnovaDesiguales = c()
  
  mediaCuadrada = c()
  valorF = c()
  valorP = c()
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  nombreDependiente <- variable2[[1]]
  nombreAgrupamiento <- variable[[1]]
  
  levelsDependiente <- length(sapply(Dependiente, levels))
  levelsDependienteNombre <- sapply(Dependiente, levels)
  
  levelsAgrupamiento <- length(sapply(Agrupamiento, levels))
  levelsAgrupamientoNombre <- sapply(Agrupamiento, levels)
  
  tablaAuxDes <- TukeyHSD(table_calculo_Anova()[[4]])
  #golem::print_dev(utils::str(tablaAux))
  
  tablaGeneralDes<-as.data.frame(tablaAuxDes[1:1])
  namesrowDes <- row.names(tablaGeneralDes)
  
  
  for (i in 1:length(namesrowDes)) {
    
    namesVarsDes = strsplit(namesrowDes[i], split = "-")
    
    var1AnovaDesiguales[i] <- unlist(namesVarsDes)[1]
    var2AnovaDesiguales[i] <- unlist(namesVarsDes)[2]
    
  }
  
  tablaGeneralDes <- data.frame(var2AnovaDesiguales,var1AnovaDesiguales,tablaGeneralDes)
  tablaGeneralDes <- tablaGeneralDes[,c(-4,-5,-6)]
  
  golem::print_dev("Desde aqui Howellllllllllll")
  #golem::print_dev(tablaGeneralDes)
  
  if (checkAnovaGamesHowell) 
  {
    
    
    
    gamesTxt <- paste0("rstatix::games_howell_test(df,",eval(nombreDependiente),"~",eval(nombreAgrupamiento),")")
    golem::print_dev(gamesTxt)
    games <- eval(parse(text=gamesTxt))
    
    #golem::print_dev(games)
    gamesPvalue <- games$p.adj
    
    #golem::print_dev(gamesPvalue)
    
    
    tablaGames <- data.frame(tablaGeneralDes,gamesPvalue)
    
    # golem::print_dev(colnames(tablaGames))
    
    golem::print_dev("cuys2")
    colnames(tablaGames) <- c(" ","  ","Diff Medias","PHowell")
    
    #golem::print_dev(tablaGames)
    
    tablaGames <- dplyr::mutate(tablaGames, dplyr::across(where(is.numeric), round, 3))
    tablaGames["PHowell"][tablaGames["PHowell"] <= 0.01] <- "<0.001"
    
    
    return(list(tablaGames," ",FALSE," "))
    
  }
  else
  {
    
  }
  
}

calculoEstadisticaAnova <- function(session, df,Dependiente, Agrupamiento,  Estadistica){
  
 
  
  levelsDependienteNombre <- sapply(Dependiente, levels)
  levelsAgrupamientoNombre <- sapply(Agrupamiento, levels)
  #golem::print_dev(levelsDependienteNombre)
  nombreAgrupamiento <- c((c(names(Agrupamiento)))[[1]]," ")
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  nombreDependiente <- variable2[[1]]
  nombreAgrupamiento <- variable[[1]]
  
  if(Estadistica==TRUE){
    golem::print_dev("Estadsitica true anova")
    
    mean_Anova_Descriptivas <- with(df, tapply(Dependiente[[1]], Agrupamiento[[1]], mean))
    golem::print_dev(mean_Anova_Descriptivas)
    
    sd_Anova_Descriptivas <- with(df, tapply(Dependiente[[1]],Agrupamiento[[1]], sd))  
    
    se_Anova_Descriptivas <- with(df, tapply(Dependiente[[1]],Agrupamiento[[1]], rapportools::se.mean))
    
    lenght_Anova_Descriptivas <-  with(df, tapply(Dependiente[[1]],Agrupamiento[[1]], length))
    golem::print_dev(lenght_Anova_Descriptivas)
    
    
    stadisticas_AnovaTable <- data.frame(nombreAgrupamiento,
                                         levelsAgrupamientoNombre,lenght_Anova_Descriptivas,
                                         mean_Anova_Descriptivas,sd_Anova_Descriptivas,se_Anova_Descriptivas)
    
    golem::print_dev(stadisticas_AnovaTable)
    colnames(stadisticas_AnovaTable) <- c(" ","Grupo","N","mean","SD","SE")
    row.names(stadisticas_AnovaTable) <- NULL
    
    stadisticas_AnovaTable <- dplyr::mutate(stadisticas_AnovaTable, dplyr::across(where(is.numeric), round, 3))
    
    gs <- ggplot2::ggplot(data = df, ggplot2::aes_string(x=Agrupamiento[[1]], y= Dependiente[[1]], color= Dependiente[[1]]  )) + ggplot2::geom_boxplot() + ggplot2::theme_bw() +  
      ggplot2::labs(x = nombreDependiente, y= nombreAgrupamiento) +
      ggplot2::theme(legend.position = "none",
                     plot.background = ggplot2::element_rect(fill = "transparent",colour = NA)
                     
                     
      )  #+ scale_x_discrete(limits = c("-3","0")) +
    
    return(list((stadisticas_AnovaTable)," ",  gs ) )
    
    
  }
  else {
    
    nombreAgrupamiento <- " "
    levelsDependienteNombre <- " "
    mean_Anova_Descriptivas <- " "
    sd_Anova_Descriptivas <- " " 
    se_Anova_Descriptivas <- " "
    lenght_Anova_Descriptivas <-  " "
    se_error <- " "
    stadisticas_AnovaTable <- data.frame(nombreAgrupamiento,
                                         levelsDependienteNombre,lenght_Anova_Descriptivas,
                                         mean_Anova_Descriptivas,sd_Anova_Descriptivas,se_Anova_Descriptivas)
    colnames(stadisticas_AnovaTable) <- c(" ","Grupo","N","mean","SD","SE")
    row.names(stadisticas_AnovaTable) <- NULL
    
    stadisticas_AnovaTable <- dplyr::mutate(stadisticas_AnovaTable, dplyr::across(where(is.numeric), round, 3))
    stadisticas_AnovaTable = stadisticas_AnovaTable[FALSE,]
    gs <- ggplot2::ggplot() + ggplot2::theme_void()
    return(list((stadisticas_AnovaTable)," ",gs))
    
    
  }
  
  
  
}

calculoEfectosFijosyAleatorios <- function(session, df, Agrupamiento, Dependiente, checkAleatorios) 
  
{
  
  variable <- c(names(Agrupamiento))
  variable2 <- c(names(Dependiente))
  
  nombreDependiente <- variable2[[1]]
  nombreAgrupamiento <- variable[[1]]
  
  levelsDependiente <- length(sapply(Dependiente, levels))
  levelsDependienteNombre <- sapply(Dependiente, levels)
  
  levelsAgrupamiento <- length(sapply(Agrupamiento, levels))
  levelsAgrupamientoNombre <- sapply(Agrupamiento, levels)
  
  
  golem::print_dev("EFECTOS ALEATORIOS")
  golem::print_dev(levelsDependiente)
  golem::print_dev(levelsDependienteNombre)
  
  
  
  
}


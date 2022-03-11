# Se grafica las variables selecionadas
My_Theme = ggplot2::theme(
  axis.title.x = ggplot2::element_text(size = 16),
  axis.text.x = ggplot2::element_text(size = 14),
  axis.title.y = ggplot2::element_text(size = 16),
  plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
  panel.background = ggplot2::element_rect(fill='transparent'),
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.border = ggplot2::element_blank()
  #legend.background = ggplot2::element_rect(fill = "transparent"), # get rid of legend bg
  #legend.box.background = ggplot2::element_rect(fill = "transparent") # get rid of legend panel bg
)

My_Theme_No_Bordes = ggplot2::theme(
  axis.title.x=ggplot2::element_blank(),
  axis.text.x=ggplot2::element_blank(),
  axis.ticks.x=ggplot2::element_blank(),
  axis.title.y=ggplot2::element_blank(),
  axis.text.y=ggplot2::element_blank(),
  axis.ticks.y=ggplot2::element_blank(),
  plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
  panel.background = ggplot2::element_rect(fill='transparent'),
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.border = ggplot2::element_blank()
  
)


output$graficaDistribucion <-renderPlot({
  #class(df_sel2()) 
  
  plot_list = c()
  # print(length(df_Descriptiva_Seleccion_Dependiente()))
  if(input$checkDistribucions){
    for (i in 1:length(df_Descriptiva_Seleccion_Dependiente())){
      
      defaultSelect <- class(df_sel()[[i]])
      print(defaultSelect)
      
      
      
      if(defaultSelect=="numeric" || defaultSelect=="integer"){
        
        nbreaks <- pretty(range.default(as.numeric(df_sel()[[i]])),n = nclass.Sturges(as.numeric(df_sel()[[i]])),min.n = 1)
        print(nbreaks)
        #my_data_num <- as.numeric(df_sel2()[[i]])  
        utils::str("Entro en for de distrubucion")
        
        
        g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(input$rank_list_Descriptiva_Dependiente[[i]])) + ggplot2::geom_histogram(col="red", fill="green", alpha = .2, breaks=nbreaks) + ggplot2::theme_bw() + My_Theme
        
        if(input$checkDensidads){
          
          print("Entro en densidad")
          
          g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(input$rank_list_Descriptiva_Dependiente[[i]])) +  ggplot2::geom_histogram(ggplot2::aes(y=..count../sum(..count..)),col="red", 
                                                                                                                          fill="green", 
                                                                                                                          alpha = .2,breaks=nbreaks) +  ggplot2::geom_density() +
            ggplot2::theme_bw() + My_Theme
        } 
      }
      else{
        
        g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(input$rank_list_Descriptiva_Dependiente[[i]])) + ggplot2::geom_bar(col="red", fill="green",alpha = .2) + ggplot2::theme_bw() + My_Theme
      }
      
      plot_list[[i]] <- g
    }
    
    cowplot::plot_grid(plotlist = plot_list)
    #utils::str("Entro en distribucion") 
  } 
  else {print_dev("no entro distribucion")}        },bg="transparent")


output$graficaCorrelacion <-renderPlot({
  
  
  
  if(input$checkCorrelacions){
    plot_list_corr = c()
    print("Entro en Correlacion")
    
    if(length(df_Descriptiva_Seleccion_Dependiente()) >= 2){
      h <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(input$rank_list_Descriptiva_Dependiente[[2]],input$rank_list_Descriptiva_Dependiente[[1]])) + ggplot2::geom_point() + ggplot2::theme_bw() + My_Theme
      
    }
    else{ 
      
      
      shinyalert::shinyalert("Oops!", "Se necesita dos variables para gr\u00e1fica de correlaci\u00f3n", type = "error",size = "xs")
      h <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
      
    }
    plot_list_corr[[1]] <- h
    cowplot::plot_grid(plotlist = plot_list_corr) } else{utils::str("correlacion no")}    
  
},bg="transparent")


output$graficaQQ <-renderPlot({
  
  
  plot_list_qq = c()
  
  #if(is.null(data_file()[[input$rank_list_Descriptiva_Dependiente[[1]]]])){}
  if(input$checkQQs){
    for (i in 1:length(df_Descriptiva_Seleccion_Dependiente())){
      
      g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(sample=input$rank_list_Descriptiva_Dependiente[[i]])) + ggplot2::stat_qq()
      plot_list_qq[[i]] <- g
    }
    
    utils::str("Entro qq")
    cowplot::plot_grid(plotlist = plot_list_qq)
    
  }
  else{utils::str("no qq")}      },bg="transparent")
#   
output$graficaPie <-renderPlot({
  plot_list_pie = c()
  
  #if(is.null(data_file()[[input$rank_list_Descriptiva_Dependiente[[1]]]])){}
  if(input$checkPastels){
    
    for (i in 1:length(df_Descriptiva_Seleccion_Dependiente())){
      
      frq <- plyr::count(values$mydata, input$rank_list_Descriptiva_Dependiente[[i]])
      
      print(frq)
      
      #print(frq[[input$rank_list_Descriptiva_Dependiente[[i]]]])
      # print("cuys")
      # print(frq$freq)
      #g <- ggplot2::ggplot(data = frq, ggplot2::aes_string( y=frq$rank_list_Descriptiva_Dependiente[[i]], fill=frq$freq ) ) + ggplot2::geom_bar( width=1) + ggplot2::coord_polar("y",start=0)
      g= ggplot2::ggplot(data= frq, ggplot2::aes_string(x="1", y= "freq"  ,fill= factor(frq[[input$rank_list_Descriptiva_Dependiente[[i]]]])))+ ggplot2::geom_bar(width = 1,stat="identity") + ggplot2::coord_polar("y",start=0) 
      g= g + ggplot2::geom_text(ggplot2::aes(label = paste0(round(freq), "")), position = ggplot2::position_stack(vjust = 0.5))
      g= g +ggplot2::labs(fill=input$rank_list_Descriptiva_Dependiente[[i]]) + ggplot2::theme_void() # remove background, grid, numeric labels
      
      # g <- pie(frq$freq ,frq[[input$rank_list_Descriptiva_Dependiente[[i]]]],main="city_pie_chart")
      #g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string( y=input$rank_list_Descriptiva_Dependiente[[i]], fill=input$rank_list_Descriptiva_Dependiente[[i]])  ) + ggplot2::geom_bar( width=1) + ggplot2::coord_polar("y",start=0)
      plot_list_pie[[i]] <- g
      
      
    }
    
    utils::str("Entro pies")
    cowplot::plot_grid(plotlist = plot_list_pie)
    
    
  }
  else{utils::str("no pies")}      },bg="transparent")


output$DescriptivosBoxplots <- renderPlot({
  
  
  plot_list = c()
  
  
  if(input$checkBoxplotsEnables){
    for (i in 1:length(df_Descriptiva_Seleccion_Dependiente())){
      
      #my_data_num <- as.numeric(df_sel2()[[i]])  
      utils::str("Entro en for de Boxplot")
      
      g <- ggplot2::ggplot() + ggplot2::theme_void()
      
      if(input$checkBoxplotElements && !input$checkBoxplotJitters && !input$checkBoxplotViolins)
      {
        g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + ggplot2::stat_boxplot(color=4, width = 0.2) + ggplot2::theme_bw()  + My_Theme #+ scale_x_discrete(limits = c("-3","0")) +
        
      }
      if(input$checkBoxplotJitters && !input$checkBoxplotElements  && !input$checkBoxplotViolins) 
        
      {
        g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) +  ggplot2::geom_jitter(color=4,width=0.2)  +  ggplot2::theme_bw() + My_Theme
        
      }
      if(input$checkBoxplotViolins  && !input$checkBoxplotElements &&  !input$checkBoxplotJitters)
      {
        g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + ggplot2::geom_violin(color=4,width=0.2,trim = FALSE) +   ggplot2::theme_bw() + My_Theme
      }
      
      if(input$checkBoxplotElements && input$checkBoxplotViolins && !input$checkBoxplotJitters  )
      {
        
        g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + 
          ggplot2::stat_boxplot(color=4, width = 0.2, alpha=0.4) +  ggplot2::geom_violin(color=5,width=0.2,trim = FALSE,alpha=0.4) +
          ggplot2::theme_bw()  + My_Theme 
      }
      if(input$checkBoxplotElements && !input$checkBoxplotViolins && input$checkBoxplotJitters  )
      {
        
        g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + 
          ggplot2::stat_boxplot(color=4, width = 0.2,alpha=0.4) +   ggplot2::geom_jitter(color=5,width=0.2,alpha=0.4) +
          ggplot2::theme_bw()  + My_Theme 
      }
      
      if(!input$checkBoxplotElements && input$checkBoxplotViolins && input$checkBoxplotJitters  )
      {
        
        g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + 
          ggplot2::geom_violin(color=4,width=0.2,trim = FALSE,alpha=0.4)  +   ggplot2::geom_jitter(color=5,width=0.2,alpha=0.4) +
          ggplot2::theme_bw()  + My_Theme 
      }
      
      if(input$checkBoxplotElements && !input$checkBoxplotViolins && input$checkBoxplotJitters  )
      {
        
        g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + 
          ggplot2::stat_boxplot(color=4, width = 0.2,alpha=0.4)  +   ggplot2::geom_jitter(color=5,width=0.2,alpha=0.4) +
          ggplot2::theme_bw()  + My_Theme 
      }
      if(input$checkBoxplotElements && input$checkBoxplotViolins && input$checkBoxplotJitters  )
      {
        
        g <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=factor(0), y=input$rank_list_Descriptiva_Dependiente[[i]])) + 
          ggplot2::stat_boxplot(color=3, width = 0.2,alpha=0.4)  + ggplot2::geom_violin(color=4,width=0.2,trim = FALSE,alpha=0.4) + ggplot2::geom_jitter(color=5,width=0.2,alpha=0.4) +
          ggplot2::theme_bw()  + My_Theme 
      }
      
      plot_list[[i]] <- g
    }
    
    cowplot::plot_grid(plotlist = plot_list)
    #utils::str("Entro en distribucion") 
  } 
  else {print_dev("no entro distribucion")}
  
  
  
  
  
},bg="transparent")


output$DescriptivosDispersion <- renderPlot({
  
  plot_list = c()
  
  # print(length(df_Descriptiva_Seleccion_Dependiente()))
  if(input$checkDispersionEnables){
    print("Entro dispersionsssssss")
    print(length(df_Descriptiva_Seleccion_Dependiente()))
    
    if(length(df_Descriptiva_Seleccion_Dependiente()) == 2){
      utils::str("Entro en for de Dispersion")
      
      h <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[1]], y=input$rank_list_Descriptiva_Dependiente[[2]])) +
        ggplot2::geom_point(size=2, shape=1,colour = 4) + ggplot2::theme_bw() + My_Theme
      
      
      if(input$radioButtonplotEncimas == "den")
      {
        
        
        
        x <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[1]])) +
          ggplot2::geom_density(fill="lightblue") + ggplot2::theme_bw() + My_Theme_No_Bordes
        
        
      }
      if(input$radioButtonplotEncimas == "Hist")
      {
        defaultSelect <- class( values$mydata[[input$rank_list_Descriptiva_Dependiente[[1]] ]])
        print_dev(defaultSelect)
        
        
        if(defaultSelect=="numeric" || defaultSelect=="integer"){
          nbreaks <- pretty(range.default(as.numeric(df_sel()[[1]])),n = nclass.Sturges(as.numeric(df_sel()[[1]])),min.n = 1)
        
        x <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[1]])) + 
          ggplot2::geom_histogram(color = 1, fill = "white",breaks=nbreaks) + ggplot2::theme_bw() + My_Theme_No_Bordes
        }else{
          
        x <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[1]])) + 
            ggplot2::geom_bar(color = 1, fill = "white") + ggplot2::theme_bw() + My_Theme_No_Bordes
          
        }
        
      }
      if(input$radioButtonplotEncimas == "Ning.")
      {
        
        x <- ggplot2::ggplot() + ggplot2::theme_void()
        
      }
      
      
      if(input$radioButtonplotDerechos == "den")
      {
        
        y <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[2]])) + 
          ggplot2::geom_density(fill="lightblue") + ggplot2::theme_bw() + My_Theme_No_Bordes  + ggpubr::rotate()
        #  plot_list[[1]] <- y
        
      }
      if(input$radioButtonplotDerechos == "Hist")
      {
        defaultSelect <- class( values$mydata[[input$rank_list_Descriptiva_Dependiente[[1]] ]])
        print_dev(defaultSelect)
        
        
        if(defaultSelect=="numeric" || defaultSelect=="integer"){
        
        
        nbreaks <- pretty(range.default(as.numeric(df_sel()[[2]])),n = nclass.Sturges(as.numeric(df_sel()[[2]])),min.n = 1)
        y <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[2]])) + 
          ggplot2::geom_histogram(color = 1, fill = "white",breaks=nbreaks) + ggplot2::theme_bw() + My_Theme_No_Bordes + ggpubr::rotate()
        
        }else {
          y <- ggplot2::ggplot(data = values$mydata, ggplot2::aes_string(x=input$rank_list_Descriptiva_Dependiente[[2]])) + 
            ggplot2::geom_bar(color = 1, fill = "white") + ggplot2::theme_bw() + My_Theme_No_Bordes + ggpubr::rotate()
          
        }
        
        
      }
      if(input$radioButtonplotDerechos == "Ning.")
      {
        
        y <- ggplot2::ggplot() + ggplot2::theme_void() + ggpubr::rotate()
        
      }
      
      if(input$checkDispersionRegresionEnables){
        
        
        if(input$checkDispersionRegresionIntervalos){
          
          # h <- h + ggplot2::geom_smooth(method = "lm", se = FALSE,level=as.numeric(input$txtInputDispersionRegresionIntervalos))
          if(input$radioButtonplotRegresions == "Smooth"){
            print_dev("dispersionnnnnnnnnnnnnn")
            
            h <- h + ggplot2::stat_smooth(method = "loess",formula = y ~ x, se = TRUE,level=as.numeric(input$txtInputDispersionRegresionIntervalos)/100) + 
              ggplot2::theme_bw() + My_Theme
          }
          if (input$radioButtonplotRegresions == "Lineal"){
            
            h <- h + ggplot2::stat_smooth(method = "lm", formula = y ~ x,se = TRUE,level=as.numeric(input$txtInputDispersionRegresionIntervalos)/100) + 
              ggplot2::theme_bw() + My_Theme
            
          } 
          
          
        }
        else
        {
          
          if(input$radioButtonplotRegresions == "Smooth"){
            
            h <- h + ggplot2::stat_smooth(method = "loess", formula = y ~ x,se = FALSE,level=0.95) + ggplot2::theme_bw() + My_Theme
          }
          if (input$radioButtonplotRegresions == "Lineal"){
            
            h <- h + ggplot2::stat_smooth(method = "lm",formula = y ~ x ,se = FALSE,level=0.95) + ggplot2::theme_bw() + My_Theme
            
          } 
        }
      }
      
    }
    else{
      print("ENtro en no dos")
      shinyalert::shinyalert("Oops!", "Se necesita dos variables para gr\u00e1fica de dispersi\u00f3n", type = "error",size = "xs")
      h <- ggplot2::ggplot() + ggplot2::theme_void()
      x <- ggplot2::ggplot() + ggplot2::theme_void()
      y <- ggplot2::ggplot() + ggplot2::theme_void()
      # plot_list[[1]]    <- ggplot2::ggplot() + ggplot2::theme_void()
    }
    
    
    
    ggpubr::ggarrange(x, NULL, h, y, 
              ncol = 2, nrow = 2,  align = "hv", 
              widths = c(2, 1), heights = c(1, 2))
    
    # grid.arrange(h,x,y, ncol=2)
    
    
  } 
  else {print_dev("no entro dispersion")}
  
  
  
  
  
},bg="transparent")

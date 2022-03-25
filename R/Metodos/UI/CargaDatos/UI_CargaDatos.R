navbarMenu( div( id="textoIconos",
  
      tags$img(src ='www/iconos2/guardar.png', style= "width: 50px; margin-top: 15px; margin-bottom: 0px; height: 30px"),
  "  "
  
  ),
           
           
 # source("./R/Metodos/UI/CargaDatos/UI_CargaDatos_Carga.R",local = TRUE)$value, 
  eval(parse("./R/Metodos/UI/CargaDatos/UI_CargaDatos_Carga.R", encoding="UTF-8")),
  
#  source("./R/Metodos/UI/CargaDatos/UI_CargaDatos_InfoEstadistica.R",local = TRUE)$value,
  eval(parse("./R/Metodos/UI/CargaDatos/UI_CargaDatos_InfoEstadistica.R", encoding="UTF-8")),
  
 # source("./R/Metodos/UI/CargaDatos/UI_CargaDatos_Acerca.R",local = TRUE)$value      
  eval(parse("./R/Metodos/UI/CargaDatos/UI_CargaDatos_Acerca.R", encoding="UTF-8"))
  
  
) # FIN NAVBARMENU
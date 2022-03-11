##UI_Anova

navbarMenu( div(  id="textoIconos",
                  
                  div(  id="imgIconos",           
                        tags$img(src ='www/iconos2/ttest.png', style= "width: 50px; margin-top: 0px; margin-bottom: 5px;  height: 30px")),
                  
                  div("REGRESI\u00d3N" )
                  
),
                 

eval(parse("./R/Metodos/UI/Regresion/UI_Correlacion.R", encoding="UTF-8")),


eval(parse("./R/Metodos/UI/Regresion/UI_RegresionLineal.R", encoding="UTF-8")),


eval(parse("./R/Metodos/UI/Regresion/UI_RegresionBinomial.R", encoding="UTF-8")),


eval(parse("./R/Metodos/UI/Regresion/UI_RegresionMultinomial.R", encoding="UTF-8"))


)
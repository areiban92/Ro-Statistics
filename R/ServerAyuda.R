

output$descriptivos_ayuda <- renderUI({
  
  tagList(
    
    h2("Descriptivas"),
  p(" Las pruebas estadísticas utilizadas para validar 
                     la efectividad de un estudio se destacan según la 
                     calidad de los datos y el prop\u00f3sito de estudio (Betanzos, 2017). 
                     Es decir, la estadística es coadyuvante con la forma de 
                     obtener, resumir, y analizar informaci\u00f3n (Gonz\u00e1lez, 2017). "),
  h6("Tendencia Central"),
  p(" • Media: Es el resultado de dividir la suma de todas las 
                   observaciones entre el número de ellas (Castro, 2018)"),
  #p(" Formula:"),
  
  # withMathJax(helpText('$$ bar(x) = sum((n_i * X_i)/(N)) $$'))
  withMathJax(
    helpText("$$ \\bar{x}=\\sum \\frac{ x_i}{N}  $$ ")),
  
  p(" • Mediana: Es el punto que divide a la muestra en dos partes iguales. 
    Para calcular la mediana es importante que los datos estén ordenados de mayor
    a menor, o al contrario de menor a mayor."),
  withMathJax(
    helpText(" $$   mediana = \\frac{n +1}{2} $$ ")),
  
  br(),
  
  p(" • Moda: Es el valor que m\u00e1s se repite, ser\u00e1 pues el valor o valores 
    cuya frecuencia absoluta sea la mayor de las observadas, no tiene formula en si
    (Salazar & Castillo, 2018).      "),

  br(), 
  p(" • Quartil: Los cuartiles son medidas estadísticas de posici\u00f3n que tienen la 
    propiedad de dividir la serie estadística en cuatro grupos de números 
    iguales de términos (S\u00e1nchez, 2007).      "),
  br(),
  p(" • MAD: Es una medida que se utiliza para calcular cu\u00e1nto varían de su 
    media los valores de un conjunto de datos (Molina, 2014).      "),
  withMathJax(
    helpText(" $$  MAD=\\frac{\\sum_{i-1}^{g}|x_{i}-\\overline{x}|f_{i}}{n}    $$ ")),
  
  p(style="text-align: center;","Donde: "),
  p(style="text-align: center;"," \\(g\\) = número de intervalos o clases de la distribuci\u00f3n de frecuencia."),
  p(style="text-align: center;"," \\(x_{i}\\) = punto medio de la i- ésima clase o marca de clase de cada intervalo."),
  p(style="text-align: center;","  \\(f_{i}\\) =número de	observaciones	clasificadas en	cada clase o intervalo (frecuencia absoluta)."),
  
  br(), 
  p(" • IQR: Consiste en la diferencia entre el tercer y el primer cuartil (Molina, 2014).     "),
  withMathJax(
    helpText(" $$ IQR = Q_{3} - Q_{1} $$ ")),
  
  p(HTML(" <b> Referencia :  </b>")),
  p("Betanzos, F. G., de Le\u00f3n, M. del C. E. P., & L\u00f3pez, J. K. C. (2017). Estadística aplicada en Psicología y Ciencias de la salud. Manual Moderno"),
  br(),
  p(" • Rango: Es un valor numérico que sirve para manifestar la diferencia entre el valor m\u00e1ximo y el valor mínimo de una muestra 
    poblacional (Molina, 2014)."),

  withMathJax(
    helpText(" $$ Rango = max_{x} - min_{x} $$ ")),
   
  p( HTML( " <b> Medidas de dispersi\u00f3n:  </b>")),
  p(" • M\u00e1ximo: Devuelve el valor m\u00e1ximo de una lista de valores (\u00c1lvarez, 2007)."), 
  p(" • Mínimo: Devuelve el valor mínimo de una lista de valores (\u00c1lvarez, 2007). "),
  p(" • Desviaci\u00f3n media: es el promedio de los valores absolutos con respecto a la media aritmética (Salazar et al; 2017).  "),
    p(" • Desviaci\u00f3n Est\u00e1ndar: raíz cuadrada de la media aritmética de las desviaciones cuadr\u00e1ticas con relaci\u00f3n 
  a la media (Salazar et al; 2017). "),
  p(" • Varianza: es el cuadrado de la desviaci\u00f3n est\u00e1ndar (Salazar et al; 2017). "),
  p(" • Coeficiente de variaci\u00f3n: es la raz\u00f3n porcentual entre la desviaci\u00f3n est\u00e1ndar y la media aritmética (Salazar et al; 2017). ")
  
  
  
  )

})

output$ttest_ayuda <- renderUI({ 
  
  tagList(
  h2("Prueba T-Test"),
  p(" La prueba 't' de Student es un tipo de estadística deductiva. 
    Se utiliza para determinar si hay una diferencia significativa 
    entre las medias de dos grupos (Janssen, 2005)."),
  h6(" Prueba t para muestras Agrupamientos  "),
  
  p("se refiere a la diferencia entre los promedios de dos poblaciones. 
    Un ejemplo sería comparar cuentas matem\u00e1ticas de un grupo 
    experimental con un grupo de control (Csörgő, 2014)."),
  
  h5("F\u00f3rmula de la distribuci\u00f3n de Student"),
  p("La f\u00f3rmula general para la T de Student es la siguiente: "),
  
  withMathJax(
    helpText("$$ t=\\frac{X-\\mu }{s/\\sqrt{n}} $$ ")),
  
  p(" Si pudiera expresar en un cierto número de pasos 
    para resolver un problema de t de student tendría 
    que declarar los siguientes: "),
  p(HTML(" <b> Paso 1.  </b> Plantear las hip\u00f3tesis nulas  \\(H_0\\)
         y la hip\u00f3tesis alternativa \\(H_1\\). La hip\u00f3tesis 
         alternativa plantea matem\u00e1ticamente lo que 
         queremos demostrar, en tanto que la hip\u00f3tesis 
         nula plantea exactamente lo contrario (Wang & Yang, 2016). ")),
  p(HTML(" <b> Paso 2. </b> Determinar el nivel de significancia 
         (rango de aceptaci\u00f3n de la hip\u00f3tesis alternativa).")),
  br(),
  p(" Se considera un nivel alfa de: 0.05 para proyectos de 
    investigaci\u00f3n; 0.01 para aseguramiento de la calidad; 
    y 0.10 para estudios o encuestas de mercadotecnia (Lachos et al., 2019).  "),
  
  p(HTML(" <b> Paso 3. </b> Evidencia muestral, se calcula 
         la media y la desviaci\u00f3n est\u00e1ndar a partir de la muestra.")),
  br(),
  
  p(HTML(" <b> Paso 4. </b>  Se aplica la distribuci\u00f3n 
         T de Student para calcular la probabilidad de error por 
         medio de la f\u00f3rmula general presentada al principio y se
         contrasta con el valor T obtenido de la tabla 
         correspondiente (Pangemanan, 2020).  ")),
  br(),
  
  p(HTML(" <b> Paso 5. </b>  En base a la evidencia disponible 
         se acepta o se rechaza la hip\u00f3tesis alternativa. 
         Si la probabilidad de error (p) es mayor que el nivel 
         de significancia se rechaza la hip\u00f3tesis alternativa. 
         Si la probabilidad de error (p) es menor que el nivel 
         de significancia se acepta la hip\u00f3tesis alternativa (Flores et al., 2017).    ")),

  
  )
  
  })


output$contingencia_ayuda <- renderUI({
  
  tagList(
    h2("Tablas de contingencia"),
    p(" Una tabla de contingencia es una distribuci\u00f3n en filas y columnas en la
      que los individuos de una poblaci\u00f3n se clasifican en funci\u00f3n de variables.
      Éstas pueden ser de 2 x 2 o de n x n variable. Las filas y las columnas 
      pueden ser definidas de diferentes formas, según el tipo de diseño 
      estadístico (\u00c1lvarez, Pérez 2008). "),
    h3(" Chi-Cuadrado"),
    
    p("La X2 es una prueba de libre distribuci\u00f3n (no paramétrica)
    que mide la discrepancia entre una distribuci\u00f3n de frecuencias 
    observadas y esperadas. Dentro de sus características generales, 
    la prueba X2 toma valores entre cero e infinito y no tiene valores 
    negativos porque es la suma de valores elevados al cuadrado  (Mendivelso, Rodríguez 2018)."),
    
    p("Formula: "),
    
    withMathJax(
      helpText(" $$\\chi^{2} = \\sum \\frac{(f_o - f_t)^{2}}{f_t}$$ ")),
    
    h3("Prueba de Fisher "),
    
    p("Permite analizar la asociaci\u00f3n entre dos variables dicot\u00f3micas 
      cuando no se cumplen las condiciones necesarias para la aplicaci\u00f3n 
      (Molina,2021)."),
    
    p(" Fisher demostr\u00f3 que podía calcular la probabilidad de cualquiera 
      de las tablas de contingencia utilizando para ello la distribuci\u00f3n 
      de probabilidad hipergeométrica (Molina,2021)."),
    
    p("La prueba de Fisher calcula las probabilidades de todas las posibles 
      tablas y suma las de aquellas que tengan valores de p (Lobo,2020)."),
    
    )
  
  

  
})

output$correlacion_ayuda <- renderUI({ 
  
  tagList(
    h2("Tabla de Contingencia"),
    p("Es un tipo de asociaci\u00f3n entre dos variables numéricas, 
      específicamente evalúa la tendencia (creciente o decreciente) 
      en los datos (Weissbrod et al., 2018)."),
    h3("Pearson’s r"),
    p("Es una prueba que mide la relaci\u00f3n estadística entre dos variables 
      continuas. Si la asociaci\u00f3n entre los elementos no es lineal, 
      entonces el coeficiente no se encuentra representado adecuadamente 
      (Edelmann et al., 2021)."),
    p("Formula :"),
    
    withMathJax(
      helpText("$$ r_{xy} = \\frac{\\sum Z_xZ_y}{N} $$ ")),
    p(" 'x' es igual a la variable número uno, 'y' pertenece a la variable 
      número dos, 'zx' es la desviaci\u00f3n est\u00e1ndar de la variable uno, 'zy' 
      es la desviaci\u00f3n est\u00e1ndar de la variable dos y 'N' es en número de datos.  "),
    h3('Spearman’s '),
    
    p(" Es una medida de dependencia no paramétrica en la cual se calcula la 
      +jerarquía media de las observaciones, se hace el cuadrado a las diferencias 
      y se incorpora en la f\u00f3rmula (Rovetta, 2020). "),
    
    withMathJax(
      helpText("$$ r_s = 1-\\frac{6\\sum d_{i}}{n^{3}-n} $$ ")),
    
  h3("Kendall’s tau"),
  
  p("Mide el grado de asociaci\u00f3n entre varios conjuntos (k) de N entidades. 
    Es útil para determinar el grado de acuerdo entre varios jueces, o la 
    asociaci\u00f3n entre tres o m\u00e1s variables (Akoglu, 2018)."),
  
  
  p("Formula: "),
  
  
  
  
  
  
  
  )
  })

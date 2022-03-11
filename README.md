
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RoStatistics

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/CarlosGuerreroG/RoStatisticsGolem/branch/master/graph/badge.svg)](https://app.codecov.io/gh/CarlosGuerreroG/RoStatisticsGolem?branch=master)
[![R-CMD-check](https://github.com/CarlosGuerreroG/RoStatisticsGolem/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CarlosGuerreroG/RoStatisticsGolem/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Descripción

**RoStatictics** un paquete de software gráfico para realizar métodos
estadísticos básicos para universitarios desarrollado por el PADLAB de
la Universidad Catolica de Cuenca

Características:

-   GUI sencillo de usar.
-   Análisis estadísticos básicos Descriptivos, Pruebas- T, One way
    Anova, Regresión.
-   Corre en el navegador sin la necesidad de instalarlo o como paquete
    para correrlo dentro de R.
-   Ofrece Resultados en tablas con formato APA.
-   Opción de descargar los datos en formato tiff(Importar en Ms
    Windows).

## Instalación:

Abrir RStudio e instalar remotes con el comando:

``` r
install.packages("remotes")
```

Iniciar Remotes e instalar la versión inicial con los siguientes
comandos:

``` r
library(remotes)

remotes::install_github("PAD-LAB/Ro-Statistics")
```

## Ejemplo

Este ejemplo básico muestra como correr el paquete:

``` r
library(RoStatistics)
run_app()
## basic example code
```

\#\#\#Nota: Versión en desarrollo, se podrian presentar algunos
problemas.

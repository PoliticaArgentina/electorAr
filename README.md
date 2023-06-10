
## Datos y herramientas `elector`ales de `Ar`rgentina usando `R` <a><img src="man/figures/logo.png" width="200" align="right" /></a>

`{electorAr}` brinda herramientas que facilitan el acceso y el trabajo
con datos electorales de Argentina desde `R`.

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/electorAr)](https://CRAN.R-project.org/package=electorAr)
[![R-CMD-check](https://github.com/PoliticaArgentina/electorAr/workflows/R-CMD-check/badge.svg)](https://github.com/PoliticaArgentina/electorAr/actions)

<!-- badges: end -->

------------------------------------------------------------------------

### INSTALACIÓN

### Versión en desarrollo (*Development version*)

``` r
# install.packages('devtools') si no tiene instalado devtools

devtools::install_github("politicaargentina/electorAr")
```

## Ejemplo de uso (*Usage*)

``` r
library(electorAr)

# Elecciones disponibles según dos fuente de datos
# 'results' para datos agregados y 'data' para escrutinios provisorios 

show_available_elections(source = "results")
```

    ## # A tibble: 728 x 5
    ##    district category round  year  NOMBRE   
    ##    <chr>    <chr>    <chr>  <chr> <chr>    
    ##  1 arg      presi    balota 2015  ARGENTINA
    ##  2 arg      presi    gral   1946  ARGENTINA
    ##  3 arg      presi    gral   1951  ARGENTINA
    ##  4 arg      presi    gral   1958  ARGENTINA
    ##  5 arg      presi    gral   1963  ARGENTINA
    ##  6 arg      presi    gral   1983  ARGENTINA
    ##  7 arg      presi    gral   1989  ARGENTINA
    ##  8 arg      presi    gral   1995  ARGENTINA
    ##  9 arg      presi    gral   1999  ARGENTINA
    ## 10 arg      presi    gral   2003  ARGENTINA
    ## # ... with 718 more rows

``` r
# Filtro eleciones de interés
(gobernadores_tucuman <- show_available_elections(source = "results") %>% 
  dplyr::filter(NOMBRE == "TUCUMAN", 
                category == "gober"))
```

    ## # A tibble: 10 x 5
    ##    district category round year  NOMBRE 
    ##    <chr>    <chr>    <chr> <chr> <chr>  
    ##  1 tucuman  gober    gral  1983  TUCUMAN
    ##  2 tucuman  gober    gral  1987  TUCUMAN
    ##  3 tucuman  gober    gral  1991  TUCUMAN
    ##  4 tucuman  gober    gral  1995  TUCUMAN
    ##  5 tucuman  gober    gral  1999  TUCUMAN
    ##  6 tucuman  gober    gral  2003  TUCUMAN
    ##  7 tucuman  gober    gral  2007  TUCUMAN
    ##  8 tucuman  gober    gral  2011  TUCUMAN
    ##  9 tucuman  gober    gral  2015  TUCUMAN
    ## 10 tucuman  gober    gral  2019  TUCUMAN

``` r
# Descargo multiples elecciones seleccionadas

(elecciones <- get_multiple_elections(gobernadores_tucuman,
                                      source = 'results' ))
```

    ## # A tibble: 10 x 2
    ## # Groups:   id [10]
    ##    id                      election           
    ##    <glue>                  <list>             
    ##  1 tucuman_gober_gral_1983 <spc_tbl_ [19 x 9]>
    ##  2 tucuman_gober_gral_1987 <spc_tbl_ [16 x 9]>
    ##  3 tucuman_gober_gral_1991 <spc_tbl_ [12 x 8]>
    ##  4 tucuman_gober_gral_1995 <spc_tbl_ [11 x 8]>
    ##  5 tucuman_gober_gral_1999 <spc_tbl_ [9 x 8]> 
    ##  6 tucuman_gober_gral_2003 <spc_tbl_ [10 x 8]>
    ##  7 tucuman_gober_gral_2007 <spc_tbl_ [15 x 8]>
    ##  8 tucuman_gober_gral_2011 <spc_tbl_ [17 x 8]>
    ##  9 tucuman_gober_gral_2015 <spc_tbl_ [9 x 8]> 
    ## 10 tucuman_gober_gral_2019 <spc_tbl_ [11 x 8]>

``` r
# Seleciono la elección a Gobernador de TUCUMAN en 1991
(eleccion91 <- elecciones$election[[3]])
```

    ## # A tibble: 12 x 8
    ##    category round  year codprov name_prov listas                   votos elect~1
    ##    <chr>    <chr> <int> <chr>   <chr>     <chr>                    <dbl>   <dbl>
    ##  1 gober    gral   1991 23      TUCUMAN   Frente De La Esperanza  284479  771330
    ##  2 gober    gral   1991 23      TUCUMAN   Fuerza Republicana      247802  771330
    ##  3 gober    gral   1991 23      TUCUMAN   Union Civica Radical     23424  771330
    ##  4 gober    gral   1991 23      TUCUMAN   Laborista De Tucuman      3382  771330
    ##  5 gober    gral   1991 23      TUCUMAN   Frente Por El Progreso~    886  771330
    ##  6 gober    gral   1991 23      TUCUMAN   Obrero                     755  771330
    ##  7 gober    gral   1991 23      TUCUMAN   Movimiento Al Socialis~    724  771330
    ##  8 gober    gral   1991 23      TUCUMAN   Comunista                  511  771330
    ##  9 gober    gral   1991 23      TUCUMAN   Federal                    460  771330
    ## 10 gober    gral   1991 23      TUCUMAN   Defensa Provincial Ban~    325  771330
    ## 11 gober    gral   1991 23      TUCUMAN   Votos En Blanco           9145  771330
    ## 12 gober    gral   1991 23      TUCUMAN   Votos Nulos               3162  771330
    ## # ... with abbreviated variable name 1: electores

``` r
# Calculo indicadores

# Competitividad
compute_competitiveness(eleccion91)
```

    ## # A tibble: 1 x 5
    ##   codprov competitividad  year category round
    ##   <chr>            <dbl> <int> <chr>    <chr>
    ## 1 23               0.936  1991 gober    gral

``` r
# Concentración
compute_concentration(eleccion91)
```

    ## # A tibble: 1 x 5
    ##   codprov concentration  year category round
    ##   <chr>           <dbl> <int> <chr>    <chr>
    ## 1 23               0.93  1991 gober    gral

``` r
# Número Efectivo de Partidos (NEP)
compute_nep(eleccion91)
```

    ## # A tibble: 2 x 6
    ##   value index             year category round codprov
    ##   <dbl> <chr>            <int> <chr>    <chr> <chr>  
    ## 1  2.16 Golosov           1991 gober    gral  23     
    ## 2  2.31 Laakso-Taagepera  1991 gober    gral  23

## `{electorAr}` es parte del universo de paquetes **polAr**

![](https://github.com/PoliticaArgentina/data_warehouse/raw/master/hex/collage.png)<!-- -->

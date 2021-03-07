#' \code{electorAr} package
#'
#' Caja de Herramientas para el procesamiento de discursos presidenciales de Argentina
#' See the README on
#' \href{https://github.com/politicaargentina/electorAr/blob/master/README.md}{Github}
#'
#' @docType package
#' @name electorAr
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines

if(getRversion() >= "2.15.1")  utils::globalVariables(
                                          c(".",
                                          "DIVISOR",
                                          "NOMBRE",
                                          "ORDER",
                                          "PARTY",
                                          "QUOTIENTS",
                                          "V1",
                                          "VOTES",
                                          "blancos",
                                          "category",
                                          "coddepto",
                                          "codprov",
                                          "election",
                                          "electores",
                                          "grillas_geofacet",
                                          "id",
                                          "index",
                                          "lista",
                                          "listas",
                                          "listas_fct",
                                          "mesa",
                                          "n",
                                          "name",
                                          "name_prov",
                                          "nombre_lista",
                                          "nulos",
                                          "parDenominacion",
                                          "pct",
                                          "seats_pct",
                                          "seccion",
                                          "secciones_pba",
                                          "vot_parCodigo",
                                          "vot_proCodigoProvincia",
                                          "votes_pct",
                                          "votos",
                                          "year"))

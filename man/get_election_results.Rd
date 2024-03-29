% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_election_results.R
\name{get_election_results}
\alias{get_election_results}
\title{Descarga resultados electorales
(\emph{Download electoral results})}
\usage{
get_election_results(
  district = NULL,
  category = NULL,
  round = NULL,
  year = NULL
)
}
\arguments{
\item{district}{un character con código para Argentina y las 24 provincias

(\emph{a named character with code for Argentina and the 24 provinces}).}

\item{category}{un character para la categoría electoral: diputado \code{dip}, senador  \code{sen} o  presidente \code{presi}

(\emph{a character with a name for the electoral category: deputy \code{dip}, senator \code{sen} or president \code{presi}}).}

\item{round}{un character para tipo de elección: primaria \code{paso} o general  \code{gral}

(\emph{a character with a name for the election round: primary \code{paso} or general \code{gral}}).}

\item{year}{un integer para el año de eleccion

(\emph{an integer for the year of choice}).}
}
\value{
devuelve un tibble con \code{class "spec_tbl_df", "tbl_df","tbl", "data.frame"} con los resultados de una elección determinada

(\emph{returns a tibble with electoral results of interest with \code{class "spec_tbl_df", "tbl_df","tbl", "data.frame"}}).
}
\description{
Función que descarga resultados electorales agregados, nacionales y provinciales desde 2007

(\emph{Function that downloads national electoral data since 2007})
}
\examples{
 get_election_data(district = "caba",
                   category = "dip",
                   round = "paso",
                   year = 2011,
                   level = "provincia",
                   long = TRUE, raw = FALSE)
}

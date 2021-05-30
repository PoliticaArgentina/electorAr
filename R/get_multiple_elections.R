#' Descarga resultados de múltiples elecciones
#'  (\emph{Download multiple election electoral data})
#'
#' @description
#' Esta función permite descargar resultados de una multiplicidad de elecciones al mismo tiempo.
#'
#'  (\emph{Function that downloads multiple national electoral data with one call})
#'
#' @param data  data.frame con tantas filas como elecciones se quiere descargar y cuatro columnas con las siguientes variables: \emph{district}, \emph{category}, \emph{round}, \emph{year}
#'
#'  (\emph{data.frame with as many rows as elections you want to download and four columns with the following variables:\emph{district}, \emph{category}, \emph{round}, \emph{year}}).
#'
#' @param source Fuente de los datos. Las opciones son 'data' para datos\code{\link{get_election_data}} y 'results' para \code{\link{get_election_results}}
#'
#' @param unnest  un boleano que devuelve los datos anidados cuando \code{TRUE} agrupando cada elección o un data.frame cuando es \code{FALSE} que incluye una variable de id de la elección
#'
#'  (\emph{a boolean that returns nested data when \code{TRUE}, grouping each election or a data.frame when \code{FALSE} that includes an election id variable}).
#'
#' @param level  parámetro para definir el nivel de agregación de los datos que se quieren descargar ('provincia', 'departamento', 'circuito'). Por defecto es provincia
#'
#'   (\emph{parameter to define the level of aggregation of the data to be downloaded ('province', 'department', 'circuit'). Default is province}).
#' @return devuelve un tibble con \code{class "grouped_df", "tbl_df","tbl", "data.frame"} con los resultados de las elección seleccionadas, con tantas
#' filas como elecciones se consultaron y dos columnas: \emph{id} de la elección construido como concatenación de los parámetros
#' \code{year_category_round_year}; \emph{election} contiene un listado de tibbles con los resultados agregados a nivel provincial para cada elección
#'
#'  (\emph{returns a tibble of \code{class "grouped_df", "tbl_df", "tbl", "data.frame"} with as many rows as elections requested and two columns:
#'  \emph{id} of the election build as a concatenation of the parameters \code{year_category_round_year}; \emph{election} contains a list of tibbles with
#'  electoral results aggregated at the provincial level for each each row}).
#'
#' @examples
#' \donttest{
# this is a long running example
#'  electorAr::show_available_elections(source = 'data') %>%
#'  dplyr::filter(district == "caba",
#'               category == "dip",
#'               round == "paso") -> caba_paso_diputados
#'
#' caba_paso_diputados
#'
#'  get_multiple_elections(data = caba_paso_diputados, source = 'data')
#'  }
#'@seealso  \code{\link{get_election_data}}
#'
#'
#' @export


get_multiple_elections <- function(data,
                                   source = NULL,
                                   unnest = FALSE,
                                   level = "provincia") {
  ## Check for internet coection
  attempt::stop_if_not(
    .x = curl::has_internet(),
    msg = "Internet access was not detected. Please check your connection //
No se detecto acceso a internet. Por favor chequear la conexion."
  )

  # level check

  assertthat::assert_that(
    is.character(level),
    msg = "'level' must be a character string. Options = c('provicina', 'departamento', 'circuito') //
'level' tiene que ser un character string. Opciones = c('provincia', 'departamento', 'circuito')"
  )

  assertthat::assert_that(
    level %in%  c('provincia', 'departamento', 'circuito'),
    msg = "Please select a correct 'level'. Check them with 'show_available_elections'() //
Por favor seleccione un 'level' correcto. Compruebelos con 'show_available_elections()'"
  )

  # Data source check

  assertthat::assert_that(!is.null(source),
                          msg = "You must provide valid character parameters for source type. Options are 'results' or 'data'")

  assertthat::assert_that(is.character(source),
                          msg = "'source' must be an character string. Please select a correct option: 'results' or 'data'")

  assertthat::assert_that(source %in% c("results", "data"),
                          msg = "Please select a correct 'source'. Options are 'results' or 'data'")


  # NEST ELECTIONS
  nested <- data %>%
    dplyr::select(-NOMBRE) %>%
    dplyr::mutate(
      id = glue::glue("{district}_{category}_{round}_{year}"),
      year = as.integer(year),
      level = level
    ) %>%
    dplyr::group_by(id) %>%
    tidyr::nest()

  # ITERATE CALL FOR EVERY ELECTION ROW IN THE NESTED DATA FRAME


  # GET DATA

  # Set default value for try()

  default <- NULL

  nested_election <-  if(source == "data") {

    base::suppressWarnings(base::try(default <- nested %>%
                                       dplyr::mutate(
                                         election = purrr::map2(
                                           .x = data,
                                           .y = id,
                                           .f = ~ electorAr::get_election_data(
                                             district = .x$district,
                                             category = .x$category,
                                             round = .x$round,
                                             year = .x$year,
                                             level = .x$level
                                           )
                                         )
                                       ))
                        )

                    } else {

      base::suppressWarnings(base::try(default <- nested %>%
                                         dplyr::mutate(
                                           election = purrr::map2(
                                             .x = data,
                                             .y = id,
                                             .f = ~ electorAr::get_election_results(
                                               district = .x$district,
                                               category = .x$category,
                                               round = .x$round,
                                               year = .x$year
                                             )
                                           )
                                         ))
      )



 }


  # NESTED OR UNNESTED OUTPUT

  if (unnest == TRUE)
  {
    nested_election %>%
      dplyr::ungroup() %>%
      tidyr::unnest(cols = c(election)) %>%
      dplyr::select(-c(data))




  } else{
    nested_election %>%
      dplyr::select(-c(data))

  }

}

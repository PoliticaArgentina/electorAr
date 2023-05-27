#' Diccionario de elecciones disponibles
#'  (\emph{Elections collection})
#'
#' @description
#' Función que devuelve un tibble con los parámetros necesarios para llamar elecciones con \code{\link{get_election_data}} o \code{\link{get_election_results}}
#'  (\emph{Function that returns a tibble with the necessary parameters to call elections with} \code{\link{get_election_data}} or \code{\link{get_election_results}}).
#'
#' @param source Fuente de consulta. Las opciones son 'data' para \code{\link{get_election_data}} y 'results' para \code{\link{get_election_results}}
#'
#' @param viewer Por defecto es \code{FALSE}. Cuando \code{TRUE} devuelve una tabla en el \emph{Viewer} de \emph{RStudio}
#'  (\emph{The default is \code{FALSE}. When \code{TRUE} it returns a table in \emph{RStudio Viewer}}).
#'
#' @return El objeto de salida es un data set con los id de elecciones necesarios como parámetros para usar
#'  con \code{\link{get_election_data}}. Cuando el parámetro es \code{viewer = FALSE}, devuelve un tibble con \code{class "tbl_df","tbl","data.frame"}, y
#'  cuando es \code{viewer = TRUE} devuelve un objeto con \code{class "datatables","htmlwidget"}
#'  (\emph{The output is a data set with elections id needed as parameters in \code{\link{get_election_data}}.
#'  When parameter is set to \code{viewer = FALSE} it returns a tibble and when it is \code{viewer = TRUE} it returns an
#'  object of \code{class "datatables","htmlwidget"}}).
#'
#' @examples
#'
#'  show_available_elections(source= 'data', viewer = FALSE)
#'
#'
#' @export

show_available_elections <- function(source = NULL,
                                     viewer = FALSE){


  # Check for internet coection
  attempt::stop_if_not(.x = curl::has_internet(),  # from eph package
                       msg = "No se detecto acceso a internet. Por favor checkea tu conexion.")


  # Data source check

  assertthat::assert_that(!is.null(source),
                          msg = "You must provide valid character parameters for source type. Options are 'results' or 'data'")

  assertthat::assert_that(is.character(source),
                          msg = "'source' must be an character string. Please select a correct option: 'results' or 'data'")

  assertthat::assert_that(source %in% c("results", "data"),
                          msg = "Please select a correct 'source'. Options are 'results' or 'data'")


  # Get list of files from github data repo

  url_definitivos <- 'https://github.com/PoliticaArgentina/data_warehouse/tree/master/electorAr/data/escrutinios_definitivos'

  url_provisorios <- 'https://github.com/PoliticaArgentina/data_warehouse/tree/master/electorAr/data/escrutinios_provisorios'

          #CHOOSE SOURCE OF DATA

          url <- if(source == "results"){

            url_definitivos

          } else {

            url_provisorios

            }

          # DEFINE SCRAPPING SOURCE URL

          type <- dplyr::case_when(source == "results"~ "definitivos",
                                   source == "data" ~ "provisorios")

  # GET DATA

  # Set default value for try()

  default <- NULL

  df <- base::suppressWarnings(base::try(default <- xml2::read_html(url),
                                         silent = TRUE))

  if(is.null(default)){

    df <- base::message("Fail to download data. Source is not available // La fuente de datos no esta disponible")

  } else {

    pg <- df


    # Get files list from data_warehouse github repo


  pg <- xml2::read_html(url)


  filelist <- rvest::html_nodes(pg, "a") %>%
    rvest::html_attr(name = "href" ) %>%
    stringr::str_match('.*csv') %>%
    stats::na.omit() %>%
    base::as.data.frame()  %>%
    dplyr::rename(name = V1) %>%
    dplyr::mutate(name = stringr::str_remove(name, pattern = glue::glue("/PoliticaArgentina/data_warehouse/blob/master/electorAr/data/escrutinios_{type}/"))) %>%
    tidyr::separate(col = name, into = c("district", "category", "round"),
             sep = "\\_", remove = T) %>%
    dplyr::mutate(year = stringr::str_remove_all(round, "\\D"),
           round = stringr::str_remove_all(round, "\\d")) %>%
    dplyr::mutate(round = stringr::str_remove_all(round, ".csv")) %>%
    tibble::as_tibble()


  ### Hardcode San Luis dirtict name
  # in results original file name doesnt match style (sanluis instead of sluis)
  # hardcoding get_election_results() function too

  filelist <- filelist %>%
    dplyr::mutate(district = dplyr::case_when(
      district == "sanluis" ~ "sluis",
      TRUE ~ district
    ))

  #### province character code and names ######

  df <- filelist %>%
    dplyr::mutate(NOMBRE = dplyr::case_when(
      district =="arg" ~    "argentina",
      district =="caba" ~     "caba",
      district =="catamarca" ~ "catamarca",
      district =="chaco" ~    "chaco",
      district =="chubut" ~   "chubut",
      district =="cordoba" ~  "cordoba",
      district =="corrientes" ~ "corrientes",
      district =="erios" ~    "entre rios",
      district =="formosa" ~  "formosa",
      district =="jujuy" ~    "jujuy",
      district =="mendoza" ~  "mendoza",
      district =="misiones" ~ "misiones",
      district =="neuquen" ~  "neuquen",
      district =="pampa" ~  "la pampa",
      district =="pba" ~    "buenos aires",
      district =="rioja" ~  "la rioja",
      district =="rnegro" ~   "rio negro",
      district =="salta" ~  "salta",
      district =="santiago" ~ "santiago del estero",
      district =="scruz" ~  "santa cruz",
      district =="sfe" ~    "santa fe",
      district =="sjuan" ~  "san juan",
      district =="sluis" ~  "san luis",
      district =="tdf" ~  "tierra del fuego",
      district =="tucuman" ~ "tucuman")) %>%
    dplyr::mutate(NOMBRE = stringr::str_to_upper(NOMBRE))





  if(viewer == TRUE){

     df <-  filelist %>%
      DT::datatable(options = list(
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))%>%
       DT::formatStyle(
         'category',
         target = 'cell',
         backgroundColor = DT::styleEqual(c("sen","presi", "dip"),
                                      c("#91bfdb","#ffffbf","#fc8d59")))%>%
       DT::formatStyle(
         'round',
         target = 'cell',
         backgroundColor = DT::styleEqual(c("paso","gral"),
                                          c("#f1a340","#998ec3")))

     print(df)


     } else {
      df
  }


    }
  }


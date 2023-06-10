
#' Descarga resultados electorales
#' (\emph{Download electoral results})
#'
#' @description
#' Función que descarga resultados electorales agregados, nacionales y provinciales desde 2007
#'
#'  (\emph{Function that downloads national electoral data since 2007})
#'
#' @param district un character con código para Argentina y las 24 provincias
#'
#'  (\emph{a named character with code for Argentina and the 24 provinces}).
#'
#' @param category un character para la categoría electoral: diputado \code{dip}, senador  \code{sen} o  presidente \code{presi}
#'
#'  (\emph{a character with a name for the electoral category: deputy \code{dip}, senator \code{sen} or president \code{presi}}).
#'
#' @param year un integer para el año de eleccion
#'
#' (\emph{an integer for the year of choice}).
#'
#' @param round un character para tipo de elección: primaria \code{paso} o general  \code{gral}
#'
#'  (\emph{a character with a name for the election round: primary \code{paso} or general \code{gral}}).
#'
#'@return devuelve un tibble con \code{class "spec_tbl_df", "tbl_df","tbl", "data.frame"} con los resultados de una elección determinada
#'
#'  (\emph{returns a tibble with electoral results of interest with \code{class "spec_tbl_df", "tbl_df","tbl", "data.frame"}}).
#'
#' @examples
#'  get_election_data(district = "caba",
#'                    category = "dip",
#'                    round = "paso",
#'                    year = 2011,
#'                    level = "provincia",
#'                    long = TRUE, raw = FALSE)
#' @export

get_election_results <- function(district = NULL ,
                                 category = NULL,
                                 round = NULL ,
                                 year = NULL){


  ## Check for internet conection
  attempt::stop_if_not(.x = curl::has_internet(),
                       msg = "Internet access was not detected. Please check your connection //
No se detecto acceso a internet. Por favor chequear la conexion.")

  ## Check params

  # year check

  assertthat::assert_that(!is.null(year),
                          msg = "You must provide valid character parameters for 'district', 'category' and 'round', and an integer parameter for 'year'. Check them with 'show_available_elections()' //
Debe proporcionar parametros character validos para 'district', 'category' y 'round', y un parametro entero para 'year'. Compruebelos con 'show_available_elections()'")

  assertthat::assert_that(is.numeric(year),
                          msg = "'year' must be an integer. Please select a correct option.  Check them with 'show_available_elections(source = 'results')' //
'year' debe ser un integer. Por favor seleccione una opcipn correcta. Verifiquelos con 'show_available_elections(source = 'results')'")

  # category check

  assertthat::assert_that(!is.null(category),
                          msg = "You must provide valid character parameters for 'district', 'category' and 'round', and an integer parameter for 'year'. Check them with show 'show_available_elections()' //
 Debe proporcionar parametros character validos para 'district', 'category' y 'round', y un parametro entero para 'year'. Compruebelos con 'show show_available_elections()'")

  assertthat::assert_that(is.character(category),
                          msg = "'category' must be an character string Please select a correct option.  Check them with 'show_available_elections(source = 'results')' //
'category' debe ser un character string Por favor seleccione una opcipn correcta. Verifiquelas con 'show_available_elections(source = 'results')'")

  assertthat::assert_that(category %in% c("gober", "presi"),
                          msg = "Please select a correct 'category'. Check them with 'show_available_elections(source = 'results')' //
Por favor seleccione una 'catgeory' correcta. Compruebelas con 'show_available_elections(source = 'results')'")


  # district check

  assertthat::assert_that(!is.null(district),
                          msg = "You must provide valid character parameters for 'district', 'category' and 'round', and an integer parameter for 'year'. Check them with 'show_available_elections()' //
Debe proporcionar parametros character validos para 'district', 'category' y 'round', y un parametro integer para 'year'. Compruebelos con  'show_available_elections()'")



  assertthat::assert_that(is.character(district),
                          msg = "'district' must be a character string. Check options with 'show_available_elections(source = 'results')' //
Por favor seleccione un 'district' correcto. Compruebelos con 'show_available_elections(source = 'results')')")

  assertthat::assert_that(district %in% c("arg",
                                          "caba",
                                          "catamarca",
                                          "chaco",
                                          "chubut",
                                          "cordoba",
                                          "corrientes",
                                          "erios",
                                          "formosa",
                                          "jujuy",
                                          "mendoza",
                                          "misiones",
                                          "neuquen",
                                          "pampa",
                                          "pba",
                                          "rioja",
                                          "rnegro",
                                          "salta",
                                          "santiago",
                                          "scruz",
                                          "sfe",
                                          "sjuan",
                                          "sluis",
                                          "tdf",
                                          "tucuman"),
                          msg = "Please select a correct 'district.' Check them with 'show_available_elections()' //
Por favor seleccione un 'district' correcto. Compruebelos con 'show_available_elections(source = 'results')")

  # round check

  assertthat::assert_that(!is.null(round),
                          msg = "You must provide valid character parameters for 'district', 'category' and 'round', and an integer parameter for 'year'. Check them with 'show_available_elections()' //
Debe proporcionar parametros character validos para 'district', 'category' y 'round', y un parametro integer para 'year'. Compruebelos con  'show_available_elections()'")


  assertthat::assert_that(is.character(round),
                          msg = "'round' must be a character string. Options = c('paso', 'gral', 'balota') //
'round' debe ser un character string. Opciones = c('paso', 'gral', 'balota')")

  assertthat::assert_that(round %in% c("paso", "gral", "balota"),
                          msg = "Please select a correct 'round' Check them with 'show_available_elections(source = 'results')' //
Por favor seleccione una 'round' correcta. Compruebelas con 'show_available_elections(source = 'results')'")


  # Check available elections


  check_elections <-   electorAr::show_available_elections(source = 'results') %>%
    dplyr::mutate(elections =paste0(district, category, round, year))


  assertthat::assert_that(glue::glue({district}, {category}, {round}, {year}) %in% check_elections$elections,
                          msg = "Please choose a valid election. Check them with 'show_available_elections(source = 'results')' //
Por favor seleccione una eleccipn valida. Consultelas con 'show_available_elections(source = 'results')'")


  ### Provincial Electoral codes # INDRA


  codProv <- tibble::tibble(name_prov =
                              c("CABA", "CATAMARCA", "CHACO", "CHUBUT", "CORDOBA", "CORRIENTES","ENTRE RIOS","FORMOSA",
                                "JUJUY", "LA PAMPA", "LA RIOJA", "MENDOZA", "MISIONES", "NEUQUEN","BUENOS AIRES","RIO NEGRO",
                                "SALTA", "SANTA CRUZ", "SANTA FE", "SANTIAGO DEL ESTERO", "SAN JUAN", "SAN LUIS", "TIERRA DEL FUEGO", "TUCUMAN"),
                            codprov =
                              c('01','03','06','07','04','05','08','09',
                                '10','11','12','13','14','15','02','16',
                                '17','20','21','22','18','19','24','23'))


    ### SAN LUIS DISTRICT HARDCODING

  district <- if(district == "sluis"){

       "sanluis"} else{

         district
     }

            ## ELECTION URL
            url <- paste0("https://raw.githubusercontent.com/PoliticaArgentina/data_warehouse/master/electorAr/data/escrutinios_definitivos/",
                          district, "_",
                          category, "_",
                          round,
                          year, ".csv")

            # GET DATA

            # Set default value for try()

            default <- NULL

            df <- base::suppressWarnings(base::try(default <- vroom::vroom(file = url,
                                                                           col_types = vroom::cols(),
                                                                           progress = FALSE),
                                                   silent = TRUE))


            # FAIL SAFELY WITH ERROR MESSAGE
            if(is.null(default)){

              df <- base::message("Fail to download data. Source is not available // La fuente de datos no esta disponible")

            } else {


              df <- df %>%
                dplyr::mutate(category = category,
                              round = round,
                              year = year,
                              district = district)

              #add district names from districts id

              df <-  df %>%
                dplyr::mutate(name_prov = dplyr::case_when(
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
                  district =="sanluis" ~  "san luis",
                  district =="tdf" ~  "tierra del fuego",
                  district =="tucuman" ~ "tucuman")) %>%
                dplyr::mutate(name_prov = stringr::str_to_upper(name_prov)) %>%
                dplyr::select(-district)


              # JOIN DISTRCIT CODE

               df <-  df %>%
                dplyr::left_join(codProv, by = "name_prov") %>%
                dplyr::rename(listas = lista) %>%
                dplyr::select(category, round, year, codprov, name_prov, dplyr::everything())

            }

            return(df)

  } # CLOSE FUNCTION


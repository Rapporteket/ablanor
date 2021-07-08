#' Get hospital name from db
#'
#' Get hospital name from db based on resh id
#'
#' @param resh_id Integer resh id
#'
#' @return Character string with hospital name
#' @export

get_hospital_name <- function(resh_id) {

  query <- paste0("
SELECT
  CENTRESHORTNAME
FROM
  friendlycentre
WHERE
  ID = ", resh_id, ";")

  name <- rapbase::loadRegData("ablanor", query)[1, ]

  if (length(name) > 1) {
    warning(paste("Resh ID", resh_id, "have multiple names.",
                  "Only the first will be returned!"))
    name <- name[1]
  }

  if (is.na(name)) {
    warning(paste("Resh ID", resh_id, "did not match any names!"))
  }

  name
}

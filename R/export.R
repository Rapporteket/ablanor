#' Tools for exporting data
#'
#' @param repo Character string providing github repository name
#' @param user Character string with github username
#' @param pubkey Character vector with public keys
#'
#' @return Some data structure with info obtained from the github api
#' @name export
#' @aliases getRepoContributors getUserPubkey selectListPubkey
#' dumpCommand dumpFile
NULL

#' @rdname export
#' @export
getRepoContributors <- function(repo) {

  endpoint <- paste0("/repos/rapporteket/", repo, "/contributors")
  vapply(
    gh::gh(endpoint),
    "[[", "", "login"
  )
}

#' @rdname export
#' @export
getUserPubkey <- function(user) {

  endpoint <- paste0("/users/", user, "/keys")
  vapply(gh::gh(endpoint), "[[", "", "key")
}

#' @rdname export
#' @export
selectListPubkey <- function(pubkey) {

  listName <- substr(pubkey, nchar(pubkey) - 7, nchar(pubkey))
  listName <- paste0(substr(pubkey, 1, 8), "...", listName)
  names(pubkey) <- listName

  pubkey

}

#' @rdname export
#' @export
dumpCommand <- function(registryName) {

  conf <- rapbase::getConfig()[[registryName]]
  bb <- "mysqldump --no-tablespaces --single-transaction --add-drop-database "
  paste0(bb, "-B -u ", conf$user, " -p", conf$pass, " -h ", conf$host,
         " ", conf$name)
}

#' @rdname export
#' @export
dumpFile <- function(registryName) {

  file <- tempfile(pattern = registryName, fileext = ".sql")
  cmd <- paste(dumpCommand(registryName), ">", file)
  out <- system(cmd)

  invisible(file)
}

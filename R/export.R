#' Tools for exporting data
#'
#' @param what Character string defining the api endpoint. Currently
#' one of \code{c("contributors", "keys")}.
#' @param value Character string specifying what to collect from the given
#' endpoint. For "contributors" this should be the name of the repository and
#' for "keys" this should be a github user name.
#' @param .token Character string providing av valid token that will be used if
#' the api call requires authentication.
#' @param repo Character string providing github repository name
#' @param user Character string with github username
#' @param pubkey Character vector with public keys
#' @param compress Logical if export data is to be compressed (using gzip).
#' FALSE by default.
#' @param session Shiny session object
#'
#' @return Some data structure with info obtained from the github api
#' @name export
#' @aliases getRepoContributors getUserPubkey selectListPubkey
#' dumpCommand dumpFile
NULL

#' @rdname export
#' @export
getGithub <- function(what, value, .token = NULL) {

  stopifnot(what %in% c("contributors", "keys"))

  if (what %in% c("contributors")) {
    endpoint <- paste0("/repos/rapporteket/", value, "/contributors")
    vName <- "login"
  }

  if (what %in% c("keys")) {
    endpoint <- paste0("/users/", value, "/keys")
    vName <- "key"
  }

  vapply(
    gh::gh(endpoint, .token = .token),
    "[[", "", vName
  )
}

#' @rdname export
#' @export
selectListPubkey <- function(pubkey) {

  listName <- substr(pubkey, nchar(pubkey) - 7, nchar(pubkey))
  listName <- paste0(substr(pubkey, 1, 8), "...", listName)
  names(pubkey) <- listName

  as.list(pubkey)

}

#' @rdname export
#' @export
exportDb <- function(registryName, compress = FALSE, session) {

  stopifnot(Sys.which("mysqldump") != "")
  stopifnot(Sys.which("gzip") != "")

  f <- tempfile(pattern = registryName, fileext = ".sql")
  conf <- rapbase::getConfig()[[registryName]]
  cmd <- "mysqldump --no-tablespaces --single-transaction --add-drop-database "
  cmd <- paste0(cmd, "-B -u ", conf$user, " -p", conf$pass, " -h ", conf$host,
         " ", conf$name, " > ", f)
  out <- system(cmd)

  if (compress) {
    inFile <- f
    f <- paste0(inFile, ".gz")
    cmd <- paste("gzip -f", inFile, ">", f)
    out <- system(cmd)
  }

  rapbase::repLogger(session, msg = paste(registryName, "db dump created."))

  invisible(f)
}

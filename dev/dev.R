Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT=TRUE)


devtools::install("../rapbase/.", upgrade = FALSE)

# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/ablanor60c4616c4d0a.sql.gz__20241205_163003.tar.gz", keyfile = "p://.ssh/id_rsa")

devtools::install(upgrade = FALSE)

source("dev/sysSetenv.R")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor

ablanor::run_app(browser = TRUE)

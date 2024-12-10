# Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT=TRUE)
# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/ablanor60c4616c4d0a.sql.gz__20241205_163003.tar.gz", keyfile = "p://.ssh/id_rsa")

devtools::install("../rapbase/.", upgrade = FALSE)

devtools::install(upgrade = FALSE)

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor

ablanor::run_app(browser = TRUE)

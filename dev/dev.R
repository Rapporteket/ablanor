# Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT=TRUE)
# dekoding av database-dump

sship::dec(
  "c://Users/ast046/Downloads/ablanor149065672.sql.gz__20250516_093327.tar.gz",
  keyfile = "p://.ssh/id_rsa",
  target_dir = "c://Users/ast046/Downloads/"
  )

devtools::install(upgrade = FALSE)

devtools::install("../rapbase/.", upgrade = FALSE)

source("dev/sysSetenv.R")

ablanor::run_app(browser = TRUE)

# For å kjøre tester

Sys.setenv(MYSQL_DB_DATA = "ablanor")
Sys.setenv(MYSQL_HOST = "localhost")
Sys.setenv(MYSQL_USER = "root")
Sys.setenv(MYSQL_PASSWORD = "root")
Sys.setenv(R_RAP_INSTANCE = "DEV")
devtools::test()

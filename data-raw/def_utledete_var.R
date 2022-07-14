## code to prepare `def_utledete_var` dataset goes here

def_utledete_var <-read.csv2(file = "data-raw/def_utledete_var.txt",
                             sep = ";",
                             header = TRUE,
                             encoding = "UTF-8")

usethis::use_data(def_utledete_var, overwrite = TRUE)

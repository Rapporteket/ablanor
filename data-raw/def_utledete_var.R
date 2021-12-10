## code to prepare `def_utledete_var` dataset goes here

def_utledete_var <-read.csv(file = "data-raw/def_utledete_var.csv", sep = ";")

usethis::use_data(def_utledete_var, overwrite = TRUE)

## code to prepare `def_utledete_var` dataset goes here
# MERK: Ved editering av kodeboken, gjør dette i CSV. Åpne filen med
# notepad, velg 'lagre som' og endre til UTF-8. Sende txt filen til
# ssh mappen på docker

def_utledete_var <- read.csv2(file = "data-raw/def_utledete_var.txt",
                             sep = ";",
                             header = TRUE,
                             encoding = "UTF-8")

usethis::use_data(def_utledete_var, overwrite = TRUE)

## code to prepare `kbd` dataset goes here

kbd <- read.csv(file = "data-raw/AblaNor_klokeboken_04.11.2026.csv",
                sep = ";")

usethis::use_data(kbd, overwrite = TRUE)

## code to prepare `kbd` dataset goes here

kbd <-read.csv(file = "data-raw/AblaNor_klokeboken_29.11.2023.csv", sep = ";")

usethis::use_data(kbd, overwrite = TRUE)

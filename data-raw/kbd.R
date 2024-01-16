## code to prepare `kbd` dataset goes here

kbd <-read.csv(file = "data-raw/ablanor_klokeboken_11.01.2024.csv", sep = ";")

usethis::use_data(kbd, overwrite = TRUE)

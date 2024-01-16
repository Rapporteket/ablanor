## code to prepare `kbd` dataset goes here

kbd <-read.csv(file = "data-raw/ablanor_klokeboken_16.01.2024_v002.csv", sep = ";")

usethis::use_data(kbd, overwrite = TRUE)

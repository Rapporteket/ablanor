## code to prepare `kbd` dataset goes here

kbd <-read.csv(file = "data-raw/klokeboken.csv", sep = ";")

usethis::use_data(kbd, overwrite = TRUE)

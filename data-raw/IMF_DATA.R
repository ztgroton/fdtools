## code to prepare `IMF_DATA` dataset goes here

IMF_DATA <- read.csv(system.file("extdata/csv/IMF_DATA_SAMPLE.csv", package = 'fdtools'))

usethis::use_data(IMF_DATA, overwrite = TRUE)

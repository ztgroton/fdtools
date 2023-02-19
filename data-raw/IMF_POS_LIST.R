## code to prepare `IMF_POS_LIST` dataset goes here

IMF_POS_LIST <- pos_list(IMF_DATA)

usethis::use_data(IMF_POS_LIST, overwrite = TRUE)

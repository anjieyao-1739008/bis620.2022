## code to prepare `DATASET` dataset goes here

ukb_accel <- readRDS("accel.rds")
usethis::use_data(ukb_accel, overwrite = TRUE)

newdata <- read.csv("data3.csv")

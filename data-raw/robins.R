## code to prepare `robins` dataset goes here
robins <- data.frame(site = c("Walker_Creek", "Knobs_Flat"),
                     aug_05 = c(1.67, 1.64),
                     aug_06 = c(1.50, 1.76),
                     aug_07 = c(1.80, 2.33),
                     aug_08 = c(1.60, 3.00),
                     aug_09 = c(1.38, 2.40))

usethis::use_data(robins, overwrite = TRUE)

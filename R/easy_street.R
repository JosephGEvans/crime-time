# This script is meant to save time for the casual user of this project.
# This allows the project to be executed easily without a Socrata account.
# It is only secure enough to slow down or stop bots that scrape public repos.
# If you want your own key just request a free account at:
#   https://data.cityofchicago.org/profile/edit/developer_settings
# Then log in and create an app token.
# That will be faster and easier than reverse engineering my plastic padlock.

library(sodium)

#' Make someone's life easier
#' 
#' Trust me, bro.
#' 
#' @return a useful piece of text for downloading data from Socrata
easy_street <- function() {
  
  useless_pattern <- readRDS("remote.bin")
  meaningless_data <- readBin("auto.bin", 
                              "raw", 
                              n = file.info("auto.bin")$size
                              )
  unimportant_number <- meaningless_data[1:24]
  automobile <- meaningless_data[25:length(meaningless_data)]
  parts <- data_decrypt(automobile, useless_pattern, unimportant_number)
  scrap <- rawToChar(parts)
  
  return(scrap)
}
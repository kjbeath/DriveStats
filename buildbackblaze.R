# turns data files into one large data file
# this will take a while
# for RStudio set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(survival)

# get names of directories at top level of data
dir.list <- list.files(path = "./data", pattern = NULL, all.files = FALSE,
                        full.names = TRUE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = TRUE, no.. = FALSE)


# first summarise the data in each period for each drive with just start and end date in that period
dataset <- do.call("rbind", lapply(dir.list, FUN = function(thedir) {
  file.list <- list.files(path = thedir , pattern = NULL, all.files = FALSE,
                          full.names = TRUE, recursive = TRUE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
    temp <- do.call("rbind", lapply(file.list, FUN = function(thefile) {
# print file name to show progress
       print(thefile)
         temp <- read.csv(thefile)[,1:5]
        temp$date <- lubridate::as_date(temp$date)
        temp
    }))
    temp <- temp %>%
      group_by(serial_number, model) %>%
      summarise(firstdate =min(date), lastdate=max(date), status=max(failure), capacity_bytes=max(capacity_bytes))
    temp
 }))

# now combine all the periods to have start and end date
# and convert periods to years

sumdata <- dataset %>%
  group_by(serial_number, model) %>%
  summarise(firstdate =min(firstdate), lastdate=max(lastdate), status=max(status), capacity_bytes=max(capacity_bytes)) %>%
  dplyr::mutate(time=as.numeric(lastdate-firstdate)/365.25) %>%
  dplyr::mutate(year=lubridate::year(firstdate))

write.csv(sumdata,file="sumbackblaze.csv")

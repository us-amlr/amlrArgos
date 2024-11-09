library(dplyr)
library(readr)

downloads.folder <- "../Downloads"

csv.files <- list.files(downloads.folder, pattern = ".csv")
csv.files

x <- read_csv(file.path(downloads.folder, tail(csv.files, 1)))
table(x$`Platform ID No.`)

x.fish <- x %>% filter(`Platform ID No.` %in% c(232865, 232866, 232867))

x %>%
  group_by(`Platform ID No.`) %>%
  summarise(n_records = n(),
            date_min = min(`Msg Date`),
            date_max = max(`Msg Date`)) %>%
  View()

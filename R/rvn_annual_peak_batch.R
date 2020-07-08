#I don't know how to do this top part - SAG

library(tidyverse)
library(reshape2)
library(lubridate)

rvn_annual_peak <- function(hyd,
                            exclude = NA,
                            add_line = T,
                            add_r2 = F,
                            add_eqn = F,
                            rplot = F) {

  df <- data.frame(date=index(hyd$hyd), coredata(hyd$hyd)) %>%
    select(-precip) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"))

  data.type <- case_when(hyd$units == "m3/s" ~ "Discharge",
                         TRUE ~ "Other") #need to check what inputs could be read here

  df.tall <- melt(df, id = "date") %>%
    mutate(Subbasin = as.character(variable)) %>%
    mutate(Type = ifelse(grepl("_obs", Subbasin)== TRUE, "Obs", "Sim"))%>%
    mutate(Subbasin = gsub("_obs", "", Subbasin))

  all.basins <- unique(df.tall$Subbasin)

  keep <- subset(all.basins, !(all.basins %in% exclude))

  df.tall.filter <- df.tall %>% filter(Subbasin %in% keep)

  df.max <- df.tall.filter %>%
    mutate(Year = year(date),
           Month = month(date)) %>%
    mutate(WYear = ifelse(Month <=9, Year, Year + 1))
    group_by(Subbasin, Type) %>%
    summarise(Max.Q = )


}

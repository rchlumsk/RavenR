#I don't know how to do this top part - SAG, don't add equation, too small with
#facet plot, can use individual one if someone wants it

library(tidyverse)
library(reshape2)
library(lubridate)

rvn_annual_peak_batch <- function(hyd,
                            exclude = NA,
                            add_line = T,
                            add_r2 = F,
                            rplot = T) {

  #convert from xts to data frame; assumes hyd is a hydrograph file from read
  #hydrograph function

  df <- data.frame(date=index(hyd$hyd), coredata(hyd$hyd)) %>%
    select(-precip) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"))

  data.type <- case_when(hyd$units == "m3/s" ~ "Discharge",
                         TRUE ~ "Other") #need to check what inputs could be read here

  #convert from wide data to tall data

  df.tall <- melt(df, id = "date") %>%
    mutate(Subbasin = as.character(variable)) %>%
    mutate(Type = ifelse(grepl("_obs", Subbasin)== TRUE, "Obs", "Sim"))%>%
    mutate(Subbasin = gsub("_obs", "", Subbasin))

  all.basins <- unique(df.tall$Subbasin)

  keep <- subset(all.basins, !(all.basins %in% exclude))

  #filter out any basisns user specified not to include in plots

  df.tall.filter <- df.tall %>% filter(Subbasin %in% keep) %>%
    mutate(Year = year(date),
           Month = month(date)) %>%
    mutate(WYear = ifelse(Month <=9, Year, Year + 1))

  #Calculate max Q for each water year

  df.max <- df.tall.filter %>%
    group_by(Subbasin, Type, WYear) %>%
    summarise(Max.Q = max(value, na.rm = TRUE))

  #extract max date from Max Q

  find.max.date <- function(Sub, Ty, Yr) {

    df <- df.tall.filter %>%
      filter(Subbasin == Sub) %>%
      filter(Type == Ty) %>%
      filter(WYear == Yr)

    df.max <- df.max %>%
      filter(Subbasin == Sub) %>%
      filter(Type == Ty) %>%
      filter(WYear == Yr)


    max.date <- df$date[which(df$value == df.max$Max.Q)]

    df.max2 <- data.frame(Subbasin = Sub,
                          Type = Ty,
                          WYear = Yr,
                          Max.Q = df.max$Max.Q,
                          Max.Date = as.Date(max.date, format = "%Y-%m-%d"))
    return(df.max2)
  }

  max.list <- expand.grid(Sub = all.basins,
                          Ty = unique(df.max$Type),
                          Yr = unique(df.max$WYear))

  df.max2 <- pmap_df(max.list, find.max.date)

  #local function to calc r2 for each subbasin

    calc.r2 <- function(Sub){
      df <- df.max2 %>% filter(Subbasin == Sub)
      df.obs <- df %>% filter(Type == "Obs")
      df.sim <- df %>% filter(Type == "Sim")

      max.obs.mean <- mean(df.obs$Max.Q)
      ss.err <- sum((df.sim$Max.Q - df.obs$Max.Q)^2)
      ss.tot <- sum((df.obs$Max.Q - max.obs.mean)^2)
      r2 <- 1 - ss.err/ss.tot

      r2.df <- data.frame(Subbasin = Sub,
                          r2 = r2)

      return(r2.df)
    }

    if(add_r2){
      r2.df <- map_df(all.basins, calc.r2)
    }

    x.lab <- expression("Observed Peak Discharge ("*m^3*"/s)")
    y.lab <- expression("Simulated Peak Discharge ("*m^3*"/s)")

    #local function to calc plot limits for each subbasin

    calc.limits <- function(Sub){
      df <- df.max2 %>% filter(Subbasin == Sub)

      df.obs <- df %>% filter(Type == "Obs")
      df.sim <- df %>% filter(Type == "Sim")

      df.limits <- data.frame(Subbasin = rep(Sub, times = 2),
                              X.lim = c(max(df.obs$Max.Q, df.sim$Max.Q, na.rm = TRUE)*1.1,
                                        min(df.obs$Max.Q, df.sim$Max.Q, na.rm = TRUE)*0.9),
                              Y.lim = c(max(df.obs$Max.Q, df.sim$Max.Q, na.rm = TRUE)*1.1,
                                        min(df.obs$Max.Q, df.sim$Max.Q, na.rm = TRUE)*0.9))
      return(df.limits)

    }

    df.limits <- map_df(all.basins, calc.limits)

    #create data.frame of all required data
    df.obs <- df.max2 %>% filter(Type == "Obs") %>%
      rename(Max.Q.Obs = Max.Q,
             Max.Date.Obs = Max.Date) %>%
      select(-Type)

    df.sim <- df.max2 %>% filter(Type == "Sim")%>%
      rename(Max.Q.Sim = Max.Q,
             Max.Date.Sim = Max.Date) %>%
      select(-Type)

    df.final <- left_join(df.sim, df.obs, by = c("Subbasin", "WYear"))

    df.plot <- data.frame(Subbasin = all.basins) %>%
      rename()

    df.plot <- left_join(df.plot, df.limits, by = "Subbasin")

    if(add_r2){
      df.plot <- left_join(df.plot, r2.df, by = "Subbasin")
    }

    #now plot

    p1 <- ggplot(data = df.final) +
      geom_point(aes(x = Max.Q.Obs, y = Max.Q.Sim, group = Subbasin))+
      facet_wrap(~Subbasin, ncol = 2, scales = "free")+
      geom_blank(data = df.plot, aes(x=X.lim, y = Y.lim))+
      xlab(label = x.lab) +
      ylab(label = y.lab) +
      theme_RavenR()

    if (add_line){
      p1 <- p1 +
        geom_abline(linetype=2)
    }

    if (add_r2){

     df.plot2 <- df.plot %>%
       mutate(label = paste("R^2 == ", round(r2,2)))

     labels <- data.frame(Subbasin= unique(df.plot2$Subbasin),
                          label = unique(df.plot2$label))

     find.label.limits <- function(Sub){
       df <- labels %>% filter(Subbasin == Sub)
       df2 <- df.limits %>% filter(Subbasin == Sub)

       x = max(df2$X.lim)*0.75
       y = min(df2$Y.lim)*1.2

       df3 <- df %>%
         mutate(Max.Q.Obs = x,
                Max.Q.Sim = y,
                Subbasin = as.factor(Subbasin))

       return(df3)
     }

     r.labels <- map_df(all.basins, find.label.limits)

      p1 <- p1 +
        geom_text(data = r.labels,
                  aes( x = Max.Q.Obs,
                       y = Max.Q.Sim,
                       label = label,
                       group = Subbasin),
                  parse = T,
                  vjust = 1,
                  hjust = 0)
    }

    if (rplot) {plot(p1)}

    return(list(df_peak = df.final,p1=p1))

}

require(tidyverse)
require(lubridate)
require(here)


url <-
  "https://raw.githubusercontent.com/opencovid-mr/covid-19_sorveglianza_integrata_italia/main/data/"
fname <- "sesso_eta.csv"

gotosaturday <- function(date){
  offset = 1 - lubridate::wday(date, week_start = 7)
  return(date + offset)
}

ddata <- tibble()
for (i in format(seq(as.Date("2021-01-01"),
                     Sys.Date(),
                     by = 1), "%Y-%m-%d")) {
  t <- tryCatch(
    readr::read_csv(paste0(url, i, "/", fname), show_col_types=FALSE),
    error = function(e) {
      tibble()
    }
  )
  ddata <- union_all(ddata, t)
}


data1 <- ddata %>% 
  select(date = iss_date, age_group = AGE_GROUP, deaths = DECEDUTI) %>%
  filter(age_group != "Non noto") %>% 
  filter(deaths != "<5") %>% 
  group_by(date, age_group) %>% 
  mutate(deaths = as.integer(deaths)) %>% 
  summarise(tdeaths = sum(deaths)) %>%
  ungroup() %>% 
  mutate(date = as.Date(date, "%d/%m/%Y")) %>% 
  arrange(age_group, date) %>% 
  mutate(daily_deaths = as.double(tdeaths - lag(tdeaths))) %>% 
  mutate(week = gotosaturday(date)) %>% 
  mutate(daily_deaths = if_else(daily_deaths < 0, 0, daily_deaths)) %>% 
  arrange(week, age_group) %>% 
  group_by(week, age_group) %>% 
  summarise(total_deaths = sum(daily_deaths)) %>% 
  select(week = week, 
         age_range = age_group,
         total_deaths = total_deaths)
     
txt70 <- data1 %>%
    filter(week >= as.Date("2021-01-01")) %>%
    #filter(week <= as.Date("2021-06-12")) %>% 
    mutate(over70 = if_else(age_range %in% c("70-79","80-89", ">90"), "70+", "<70")) %>% 
    group_by(week, over70) %>%
    summarise(total_deaths = sum(total_deaths)) %>% 
    spread(c(over70), total_deaths) %>% 
    mutate(position = `70+`/(`<70` + `70+`),
           text = as.character(round(position*100, 1)))

txt80 <- data1 %>%
    filter(week >= as.Date("2021-01-01")) %>%
    #filter(week <= as.Date("2021-06-12")) %>% 
    mutate(over80 = if_else(age_range %in% c("80-89", ">90"), "80+", "<80")) %>% 
    group_by(week, over80) %>%
    summarise(total_deaths = sum(total_deaths)) %>% 
    spread(c(over80), total_deaths) %>% 
    mutate(position = `80+`/(`<80` + `80+`),
           text = as.character(round(position*100, 1)))

txt90 <- data1 %>%
    filter(week >= as.Date("2021-01-01")) %>%
    #filter(week <= as.Date("2021-06-12")) %>% 
    mutate(over90 = if_else(age_range == ">90" , "90+", "<90")) %>% 
    group_by(week, over90) %>%
    summarise(total_deaths = sum(total_deaths)) %>% 
    spread(c(over90), total_deaths) %>% 
    mutate(position = `90+`/(`<90` + `90+`),
           text = as.character(round(position*100, 1)))

png(filename = "/Users/runner/work/auto-graphs/auto-graphs/output/iss-deaths-age-rel.png", width = 465, height = 225, units='mm', res = 300)

data1 %>%
    filter(week >= as.Date("2021-01-01")) %>% 
    #filter(week <= as.Date("2021-06-12")) %>% 
    mutate(age_range =case_when(
        age_range %in% 
            c("0-9", "10-19", "20-29", "30-39", "40-49") ~ "< 50",
        age_range == "50-59" ~ "50-59",
        age_range == "60-69" ~ "60-69",
        age_range == "70-79" ~ "70-79",
        age_range == "80-89" ~ "80-89",
        age_range == ">90" ~ "90+"
    )) %>% 
    ggplot(mapping = aes(x = week, y = total_deaths, fill=age_range))  +
    geom_col(position = "fill") + scale_fill_brewer(palette= "YlGnBu") +
    ggthemes::theme_few() + 
    xlab("Settimana") +
    ylab("% decessi")+
    labs(fill = "Fascia d'età") +
    scale_y_continuous(labels = scales::percent)  +
    geom_text(inherit.aes = FALSE,data = txt80,
              mapping = aes(x = week, y = position + 0.03,
                            label = text),
              size =4.1, 
              col="white")+
    geom_text(inherit.aes = FALSE,data = txt90,
              mapping = aes(x = week, y = position + 0.03,
                            label = text),
              size =4.1,
              col="white") +  geom_text(inherit.aes = FALSE,data = txt70,
                                        mapping = aes(x = week, y = position + 0.03,
                                                      label = text),
                                        size =4.1,
                                        col="white")
dev.off()

png(filename = "/Users/runner/work/auto-graphs/auto-graphs/output/iss-deaths-age-abs.png"), width = 465, height = 225, units='mm', res = 300)

data1 %>%
    filter(week >= as.Date("2021-01-01")) %>% 
    #filter(week <= as.Date("2021-06-11")) %>% 
    
    mutate(age_range =case_when(
        age_range %in% 
            c("0-9", "10-19", "20-29", "30-39", "40-49") ~ "< 50",
        age_range == "50-59" ~ "50-59",
        age_range == "60-69" ~ "60-69",
        age_range == "70-79" ~ "70-79",
        age_range == "80-89" ~ "80-89",
        age_range == ">90" ~ "90+"
    )) %>% 
    ggplot(mapping = aes(x = week, y = total_deaths, fill=age_range))  +
    geom_bar(stat = "identity") + scale_fill_brewer(palette= "YlGnBu") +
    ggthemes::theme_few() + 
    xlab("Settimana") +
    ylab("N. decessi")+
    labs(fill = "Fascia d'età")
    
 dev.off()

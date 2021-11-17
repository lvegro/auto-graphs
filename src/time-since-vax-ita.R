library(tidyverse)
library(readr)
library(ggthemes)
library(rjson)

last_update <- fromJSON(file = "https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/last-update-dataset.json")
last_update <- substr(last_update[[1]], 1, 19)
last_update <- gsub("T", " ", last_update)
last_update <- as.POSIXct(last_update) + 2 * 60 * 60

v <- read_csv("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.csv")

s <- v %>% group_by(data_somministrazione, fascia_anagrafica) %>%
mutate(dose_finale = seconda_dose + pregressa_infezione + ifelse(fornitore=="Janssen", prima_dose,0)) %>%
  summarise(dosi = sum(dose_finale)) %>%
  mutate(diffdays = as.integer(Sys.Date() - data_somministrazione)) %>%
  filter(dosi > 0) %>%
  mutate(
    tempo_dal_vaccino = case_when(
      diffdays >= 180 ~ "Più di 6 mesi",
      diffdays < 180 &
        diffdays >= 90 ~ "Tra 3 e 6 mesi",
      diffdays < 90 ~ "< 3 mesi"
    )
  ) %>%
  group_by(fascia_anagrafica, tempo_dal_vaccino) %>%
  summarise(dosi = sum(dosi))

s$tempo_dal_vaccino <-
  factor(s$tempo_dal_vaccino,
         levels = c("< 3 mesi", "Tra 3 e 6 mesi", "Più di 6 mesi"))

png(filename = "/Users/runner/work/auto-graphs/auto-graphs/output/time-since-vax-ita.png", width = 465, height = 225, units='mm', res = 300)

s %>% ggplot(mapping = aes(x = fascia_anagrafica, y = dosi, fill = tempo_dal_vaccino))  +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_few() +
  xlab("Fascia Anagrafica") +
  ylab("% vaccinati") +
  labs(title = "Tempo trascorso da completamento ciclo vaccinale con doppia dose",
    #subtitle = "Sono escluse le persone che hanno ricevuto una sola dose (J&J o pregressa infezione)",
    caption = paste("Fonte dati: Covid-19 Opendata Vaccini | Data aggiornamento:", last_update),
    fill = "Tempo trascorso dal vaccino") +
  theme(legend.position = "bottom")
dev.off()

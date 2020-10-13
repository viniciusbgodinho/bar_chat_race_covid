con <- gzcon(url(paste("https://data.brasil.io/dataset/covid19/caso_full.csv.gz", sep=",")))
txt <- readLines(con)
dados <- read.csv(textConnection(txt))

setwd('E:\\Covid-19\\rede analise\\bar chart race 100k')


library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("devtools")
library("gganimate")
library("gifski")
library("av")

colnames(dados)[9] <-'confirmed'
colnames(dados)[13] <-'deaths'
colnames(dados)[10] <- 'confirmed_per_100k'


dados <- dados %>%
  mutate(deaths_per_100k = deaths/estimated_population_2019*100000) %>%
  arrange(date)



dados$confirmed_per_100k <- round(dados$confirmed_per_100k, digits = 2)
dados$deaths_per_100k <- round(dados$deaths_per_100k, digits = 2)


####capitais



dados_capitais <- dados %>%
  filter(place_type=="city",  city_ibge_code=="1100205" |  city_ibge_code=="1302603" |
           city_ibge_code=="1200401" | city_ibge_code=="5002704"| city_ibge_code=="1600303"|
           city_ibge_code=="5300108"| city_ibge_code=="1400100" | city_ibge_code=="5103403"|
           city_ibge_code=="1721000" | city_ibge_code=="2211001"| city_ibge_code=="3550308"|
           city_ibge_code=="3304557" | city_ibge_code=="1501402" |  city_ibge_code=="2111300" |
           city_ibge_code=="5208707" | city_ibge_code=="2927408" |  city_ibge_code=="2704302"|
           city_ibge_code=="4314902" |city_ibge_code=="4106902" |  city_ibge_code=="4205407"|
           city_ibge_code=="3106200" |  city_ibge_code=="2304400" |  city_ibge_code=="2611606" |
           city_ibge_code=="2507507" |city_ibge_code=="2800308" |city_ibge_code=="2408102" |
           city_ibge_code=="3205309") 



dados_capitais$date <- as.Date(dados_capitais$date)



ggplot2::theme_set(theme_classic())



table_deaths_per_100k_capitais <- dados_capitais %>%
  select(city,deaths_per_100k,date)


# generate top n ranking by year group

anim_table_deaths_per_100k_city <- table_deaths_per_100k_capitais %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_city, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill= 'black'
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19: Óbitos Acumulados por 100 mil Habitantes Capitais", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_capitais.gif"))





###casos confirmados

table_confirmed_per_100k_capitais <- dados_capitais %>%
  select(city,confirmed_per_100k,date)


# generate top n ranking by year group

anim_table_confirmed_per_100k_city <- table_confirmed_per_100k_capitais %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_city, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, colour = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19: Casos Acumulados por 100 mil Habitantes Capitais", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_capitais.gif"))




####municípios por estado


##minas gerais

df_mg <- dados %>%
  filter(place_type == "city", state == "MG", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_mg <- df_mg %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_mg <- table_confirmed_per_100k_mg %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_mg, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Minas Gerais ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_mg.gif"))


##óbitos

table_deaths_per_100k_mg <- df_mg %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_mg <- table_deaths_per_100k_mg %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_mg, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Minas Gerais", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_mg.gif"))


##Tocantins

df_to <- dados %>%
  filter(place_type == "city", state == "TO", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_to <- df_to %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_to <- table_confirmed_per_100k_to %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_to, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Tocantins ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_to.gif"))


##óbitos

table_deaths_per_100k_to <- df_to %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_to <- table_deaths_per_100k_to %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_to, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Tocantins", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_to.gif"))


##Sergipe

df_se <- dados %>%
  filter(place_type == "city", state == "SE", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_se <- df_se %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_se <- table_confirmed_per_100k_se %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_se, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Sergipe ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_se.gif"))


##óbitos

table_deaths_per_100k_se <- df_se %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_se <- table_deaths_per_100k_se %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_se, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Sergipe", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_se.gif"))


##São Paulo

df_sp <- dados %>%
  filter(place_type == "city", state == "SP", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_sp <- df_sp %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_sp <- table_confirmed_per_100k_sp %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_sp, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: São Paulo ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_sp1.gif"))


##óbitos

table_deaths_per_100k_sp <- df_sp %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_sp <- table_deaths_per_100k_sp %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_sp, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: São Paulo", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_sp1.gif"))


##Santa Catarina

df_sc <- dados %>%
  filter(place_type == "city", state == "SC", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_sc <- df_sc %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_sc <- table_confirmed_per_100k_sc %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_sc, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Santa Catarina ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_sc.gif"))


##óbitos

table_deaths_per_100k_sc <- df_sc %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_sc <- table_deaths_per_100k_sc %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_sc, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Santa Catarina", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_sc.gif"))


##Roraima

df_rr <- dados %>%
  filter(place_type == "city", state == "RR", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_rr <- df_rr %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_rr <- table_confirmed_per_100k_rr %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_rr, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Roraima ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_rr.gif"))


##óbitos

table_deaths_per_100k_rr <- df_rr %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_rr <- table_deaths_per_100k_rr %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_rr, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Roraima", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_rr.gif"))


##Rondonia

df_ro <- dados %>%
  filter(place_type == "city", state == "RO", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ro <- df_ro %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ro <- table_confirmed_per_100k_ro %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ro, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Rondônia ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ro.gif"))


##óbitos

table_deaths_per_100k_ro <- df_ro %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ro <- table_deaths_per_100k_ro %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ro, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Rondônia", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ro.gif"))


##Rio Grande do Sul

df_rs <- dados %>%
  filter(place_type == "city", state == "RS", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_rs <- df_rs %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_rs <- table_confirmed_per_100k_rs %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_rs, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Rio Grande do Sul", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_rs.gif"))


##óbitos

table_deaths_per_100k_rs <- df_rs %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_rs <- table_deaths_per_100k_rs %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_rs, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Rio Grande do Sul", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_rs.gif"))


##Rio Grande do Norte

df_rn <- dados %>%
  filter(place_type == "city", state == "RN", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_rn <- df_rn %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_rn <- table_confirmed_per_100k_rn %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_rn, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Rio Grande do Norte ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_rn.gif"))


##óbitos

table_deaths_per_100k_rn <- df_rn %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_rn <- table_deaths_per_100k_rn %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_rn, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Rio Grande do Norte", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_rn.gif"))


##Rio de Janeiro

df_rj <- dados %>%
  filter(place_type == "city", state == "RJ", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_rj <- df_rj %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_rj <- table_confirmed_per_100k_rj %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_rj, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Rio de Janeiro ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_rj.gif"))


##óbitos

table_deaths_per_100k_rj <- df_rj %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_rj <- table_deaths_per_100k_rj %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_rj, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Rio de Janeiro", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_rj.gif"))


##Piauí

df_pi <- dados %>%
  filter(place_type == "city", state == "PI", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_pi <- df_pi %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_pi <- table_confirmed_per_100k_pi %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_pi, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Piauí ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_pi.gif"))


##óbitos

table_deaths_per_100k_pi <- df_pi %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_pi <- table_deaths_per_100k_pi %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_pi, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Piauí", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_pi.gif"))


##Pernambuco

df_pe <- dados %>%
  filter(place_type == "city", state == "PE", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_pe <- df_pe %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_pe <- table_confirmed_per_100k_pe %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_pe, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Pernambuco ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_pe.gif"))


##óbitos

table_deaths_per_100k_pe <- df_pe %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_pe <- table_deaths_per_100k_pe %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_pe, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Pernambuco", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_pe.gif"))


##Paraná

df_pr <- dados %>%
  filter(place_type == "city", state == "PR") %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_pr <- df_pr %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_pr <- table_confirmed_per_100k_pr %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_pr, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Paraná ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_pr.gif"))


##óbitos

table_deaths_per_100k_pr <- df_pr %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_pr <- table_deaths_per_100k_pr %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_pr, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Paraná", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_pr.gif"))


##Paraíba 

df_pb <- dados %>%
  filter(place_type == "city", state == "PB", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_pb <- df_pb %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_pb <- table_confirmed_per_100k_pb %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_pb, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Paraíba ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_pb.gif"))


##óbitos

table_deaths_per_100k_pb <- df_pb %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_pb <- table_deaths_per_100k_pb %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_pb, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Paraíba", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_pb.gif"))


##Pará

df_pa <- dados %>%
  filter(place_type == "city", state == "PA", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_pa <- df_pa %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_pa <- table_confirmed_per_100k_pa %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_pa, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Pará ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_pa.gif"))


##óbitos

table_deaths_per_100k_pa <- df_pa %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_pa <- table_deaths_per_100k_pa %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_pa, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Pará", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_pa.gif"))


##Mato Grosso do Sul

df_ms <- dados %>%
  filter(place_type == "city", state == "MS", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ms <- df_ms %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ms <- table_confirmed_per_100k_ms %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ms, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Mato Grosso do Sul ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ms.gif"))


##óbitos

table_deaths_per_100k_ms <- df_ms %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ms <- table_deaths_per_100k_ms %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ms, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Mato Grosso do Sul", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ms.gif"))


##Mato Grosso

df_mt <- dados %>%
  filter(place_type == "city", state == "MT", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_mt <- df_mt %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_mt <- table_confirmed_per_100k_mt %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_mt, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Mato Grosso ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_mt.gif"))


##óbitos

table_deaths_per_100k_mt <- df_mt %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_mt <- table_deaths_per_100k_mt %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_mt, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Mato Grosso", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_mt.gif"))


##Maranhão

df_ma <- dados %>%
  filter(place_type == "city", state == "MA", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ma <- df_ma %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ma <- table_confirmed_per_100k_ma %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ma, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Maranhão ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ma.gif"))


##óbitos

table_deaths_per_100k_ma <- df_ma %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ma <- table_deaths_per_100k_ma %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ma, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Maranhão", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ma.gif"))


##Goiás

df_go <- dados %>%
  filter(place_type == "city", state == "GO", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_go <- df_go %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_go <- table_confirmed_per_100k_go %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_go, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Goiás ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_go.gif"))


##óbitos

table_deaths_per_100k_go <- df_go %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_go <- table_deaths_per_100k_go %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_go, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Goiás", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_go.gif"))


##Espírito Santo

df_es <- dados %>%
  filter(place_type == "city", state == "ES", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_es <- df_es %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_es <- table_confirmed_per_100k_es %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_es, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Espírito Santo ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_es.gif"))


##óbitos

table_deaths_per_100k_es <- df_es %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_es <- table_deaths_per_100k_es %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_es, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Espírito Santo", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_es.gif"))


##Distrito Federal

df_df <- dados %>%
  filter(place_type == "city", state == "DF") %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_df <- df_df %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_df <- table_confirmed_per_100k_df %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 5) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_df, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Distrito Federal ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

#gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_df.gif"))


##óbitos

table_deaths_per_100k_df <- df_df %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_df <- table_deaths_per_100k_df %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 5) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_df, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Distrito Federal", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

#gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_df.gif"))


##Ceará

df_ce <- dados %>%
  filter(place_type == "city", state == "CE", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ce <- df_ce %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ce <- table_confirmed_per_100k_ce %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ce, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Ceará ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ce.gif"))


##óbitos

table_deaths_per_100k_ce <- df_ce %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ce <- table_deaths_per_100k_ce %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ce, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Ceará", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ce.gif"))


##Bahia

df_ba <- dados %>%
  filter(place_type == "city", state == "BA", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ba <- df_ba %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ba <- table_confirmed_per_100k_ba %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ba, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Bahia ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ba.gif"))


##óbitos

table_deaths_per_100k_ba <- df_ba %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ba <- table_deaths_per_100k_ba %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ba, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Bahia", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ba.gif"))


##Amazonas

df_am <- dados %>%
  filter(place_type == "city", state == "AM", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_am <- df_am %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_am <- table_confirmed_per_100k_am %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_am, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Amazonas", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_am.gif"))


##óbitos

table_deaths_per_100k_am <- df_am %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_am <- table_deaths_per_100k_am %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_am, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Amazonas", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_am.gif"))


##Amapá

df_ap <- dados %>%
  filter(place_type == "city", state == "AP", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ap <- df_ap %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ap <- table_confirmed_per_100k_ap %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ap, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Amapá ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ap.gif"))


##óbitos

table_deaths_per_100k_ap <- df_ap %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ap <- table_deaths_per_100k_ap %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ap, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Amapá", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ap.gif"))


##Acre

df_ac <- dados %>%
  filter(place_type == "city", state == "AC", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ac <- df_ac %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ac <- table_confirmed_per_100k_ac %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ac, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Acre ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ac.gif"))


##óbitos

table_deaths_per_100k_ac <- df_ac %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ac <- table_deaths_per_100k_ac %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ac, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Acre", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ac.gif"))

####municípios por estado

##Alagoas

df_al <- dados %>%
  filter(place_type == "city", state == "AL", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_al <- df_al %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_al <- table_confirmed_per_100k_al %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_al, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Alagoas ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_al.gif"))


##óbitos

table_deaths_per_100k_al <- df_al %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_al <- table_deaths_per_100k_al %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_al, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Alagoas", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_al.gif"))









con <- gzcon(url(paste("https://data.brasil.io/dataset/covid19/caso_full.csv.gz", sep=",")))
txt <- readLines(con)
dados <- read.csv(textConnection(txt))

setwd('E:\\Covid-19\\rede analise\\bar chart race 100k')


library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("devtools")
library("gganimate")
library("gifski")
library("av")

colnames(dados)[9] <-'confirmed'
colnames(dados)[13] <-'deaths'
colnames(dados)[10] <- 'confirmed_per_100k'


dados <- dados %>%
  mutate(deaths_per_100k = deaths/estimated_population_2019*100000) %>%
  arrange(date)



dados$confirmed_per_100k <- round(dados$confirmed_per_100k, digits = 2)
dados$deaths_per_100k <- round(dados$deaths_per_100k, digits = 2)


####capitais



dados_capitais <- dados %>%
  filter(place_type=="city",  city_ibge_code=="1100205" |  city_ibge_code=="1302603" |
           city_ibge_code=="1200401" | city_ibge_code=="5002704"| city_ibge_code=="1600303"|
           city_ibge_code=="5300108"| city_ibge_code=="1400100" | city_ibge_code=="5103403"|
           city_ibge_code=="1721000" | city_ibge_code=="2211001"| city_ibge_code=="3550308"|
           city_ibge_code=="3304557" | city_ibge_code=="1501402" |  city_ibge_code=="2111300" |
           city_ibge_code=="5208707" | city_ibge_code=="2927408" |  city_ibge_code=="2704302"|
           city_ibge_code=="4314902" |city_ibge_code=="4106902" |  city_ibge_code=="4205407"|
           city_ibge_code=="3106200" |  city_ibge_code=="2304400" |  city_ibge_code=="2611606" |
           city_ibge_code=="2507507" |city_ibge_code=="2800308" |city_ibge_code=="2408102" |
           city_ibge_code=="3205309") 



dados_capitais$date <- as.Date(dados_capitais$date)



ggplot2::theme_set(theme_classic())



table_deaths_per_100k_capitais <- dados_capitais %>%
  select(city,deaths_per_100k,date)


# generate top n ranking by year group

anim_table_deaths_per_100k_city <- table_deaths_per_100k_capitais %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_city, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill= 'black'
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19: Óbitos Acumulados por 100 mil Habitantes Capitais", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_capitais.gif"))





###casos confirmados

table_confirmed_per_100k_capitais <- dados_capitais %>%
  select(city,confirmed_per_100k,date)


# generate top n ranking by year group

anim_table_confirmed_per_100k_city <- table_confirmed_per_100k_capitais %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_city, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, colour = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19: Casos Acumulados por 100 mil Habitantes Capitais", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_capitais.gif"))




####municípios por estado


##minas gerais

df_mg <- dados %>%
  filter(place_type == "city", state == "MG", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_mg <- df_mg %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_mg <- table_confirmed_per_100k_mg %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_mg, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Minas Gerais ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_mg.gif"))


##óbitos

table_deaths_per_100k_mg <- df_mg %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_mg <- table_deaths_per_100k_mg %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_mg, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Minas Gerais", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_mg.gif"))


##Tocantins

df_to <- dados %>%
  filter(place_type == "city", state == "TO", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_to <- df_to %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_to <- table_confirmed_per_100k_to %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_to, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Tocantins ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_to.gif"))


##óbitos

table_deaths_per_100k_to <- df_to %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_to <- table_deaths_per_100k_to %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_to, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Tocantins", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_to.gif"))


##Sergipe

df_se <- dados %>%
  filter(place_type == "city", state == "SE", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_se <- df_se %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_se <- table_confirmed_per_100k_se %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_se, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Sergipe ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_se.gif"))


##óbitos

table_deaths_per_100k_se <- df_se %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_se <- table_deaths_per_100k_se %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_se, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Sergipe", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_se.gif"))


##São Paulo

df_sp <- dados %>%
  filter(place_type == "city", state == "SP", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_sp <- df_sp %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_sp <- table_confirmed_per_100k_sp %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_sp, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: São Paulo ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_sp1.gif"))


##óbitos

table_deaths_per_100k_sp <- df_sp %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_sp <- table_deaths_per_100k_sp %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_sp, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: São Paulo", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_sp1.gif"))


##Santa Catarina

df_sc <- dados %>%
  filter(place_type == "city", state == "SC", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_sc <- df_sc %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_sc <- table_confirmed_per_100k_sc %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_sc, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Santa Catarina ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_sc.gif"))


##óbitos

table_deaths_per_100k_sc <- df_sc %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_sc <- table_deaths_per_100k_sc %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_sc, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Santa Catarina", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_sc.gif"))


##Roraima

df_rr <- dados %>%
  filter(place_type == "city", state == "RR", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_rr <- df_rr %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_rr <- table_confirmed_per_100k_rr %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_rr, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Roraima ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_rr.gif"))


##óbitos

table_deaths_per_100k_rr <- df_rr %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_rr <- table_deaths_per_100k_rr %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_rr, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Roraima", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_rr.gif"))


##Rondonia

df_ro <- dados %>%
  filter(place_type == "city", state == "RO", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ro <- df_ro %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ro <- table_confirmed_per_100k_ro %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ro, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Rondônia ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ro.gif"))


##óbitos

table_deaths_per_100k_ro <- df_ro %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ro <- table_deaths_per_100k_ro %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ro, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Rondônia", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ro.gif"))


##Rio Grande do Sul

df_rs <- dados %>%
  filter(place_type == "city", state == "RS", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_rs <- df_rs %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_rs <- table_confirmed_per_100k_rs %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_rs, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Rio Grande do Sul", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_rs.gif"))


##óbitos

table_deaths_per_100k_rs <- df_rs %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_rs <- table_deaths_per_100k_rs %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_rs, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Rio Grande do Sul", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_rs.gif"))


##Rio Grande do Norte

df_rn <- dados %>%
  filter(place_type == "city", state == "RN", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_rn <- df_rn %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_rn <- table_confirmed_per_100k_rn %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_rn, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Rio Grande do Norte ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_rn.gif"))


##óbitos

table_deaths_per_100k_rn <- df_rn %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_rn <- table_deaths_per_100k_rn %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_rn, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Rio Grande do Norte", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_rn.gif"))


##Rio de Janeiro

df_rj <- dados %>%
  filter(place_type == "city", state == "RJ", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_rj <- df_rj %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_rj <- table_confirmed_per_100k_rj %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_rj, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Rio de Janeiro ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_rj.gif"))


##óbitos

table_deaths_per_100k_rj <- df_rj %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_rj <- table_deaths_per_100k_rj %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_rj, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Rio de Janeiro", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_rj.gif"))


##Piauí

df_pi <- dados %>%
  filter(place_type == "city", state == "PI", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_pi <- df_pi %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_pi <- table_confirmed_per_100k_pi %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_pi, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Piauí ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_pi.gif"))


##óbitos

table_deaths_per_100k_pi <- df_pi %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_pi <- table_deaths_per_100k_pi %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_pi, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Piauí", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_pi.gif"))


##Pernambuco

df_pe <- dados %>%
  filter(place_type == "city", state == "PE", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_pe <- df_pe %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_pe <- table_confirmed_per_100k_pe %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_pe, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Pernambuco ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_pe.gif"))


##óbitos

table_deaths_per_100k_pe <- df_pe %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_pe <- table_deaths_per_100k_pe %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_pe, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Pernambuco", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_pe.gif"))


##Paraná

df_pr <- dados %>%
  filter(place_type == "city", state == "PR") %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_pr <- df_pr %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_pr <- table_confirmed_per_100k_pr %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_pr, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Paraná ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_pr.gif"))


##óbitos

table_deaths_per_100k_pr <- df_pr %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_pr <- table_deaths_per_100k_pr %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_pr, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Paraná", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_pr.gif"))


##Paraíba 

df_pb <- dados %>%
  filter(place_type == "city", state == "PB", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_pb <- df_pb %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_pb <- table_confirmed_per_100k_pb %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_pb, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Paraíba ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_pb.gif"))


##óbitos

table_deaths_per_100k_pb <- df_pb %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_pb <- table_deaths_per_100k_pb %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_pb, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Paraíba", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_pb.gif"))


##Pará

df_pa <- dados %>%
  filter(place_type == "city", state == "PA", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_pa <- df_pa %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_pa <- table_confirmed_per_100k_pa %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_pa, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Pará ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_pa.gif"))


##óbitos

table_deaths_per_100k_pa <- df_pa %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_pa <- table_deaths_per_100k_pa %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_pa, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Pará", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_pa.gif"))


##Mato Grosso do Sul

df_ms <- dados %>%
  filter(place_type == "city", state == "MS", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ms <- df_ms %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ms <- table_confirmed_per_100k_ms %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ms, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Mato Grosso do Sul ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ms.gif"))


##óbitos

table_deaths_per_100k_ms <- df_ms %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ms <- table_deaths_per_100k_ms %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ms, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Mato Grosso do Sul", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ms.gif"))


##Mato Grosso

df_mt <- dados %>%
  filter(place_type == "city", state == "MT", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_mt <- df_mt %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_mt <- table_confirmed_per_100k_mt %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_mt, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Mato Grosso ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_mt.gif"))


##óbitos

table_deaths_per_100k_mt <- df_mt %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_mt <- table_deaths_per_100k_mt %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_mt, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Mato Grosso", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_mt.gif"))


##Maranhão

df_ma <- dados %>%
  filter(place_type == "city", state == "MA", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ma <- df_ma %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ma <- table_confirmed_per_100k_ma %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ma, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Maranhão ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ma.gif"))


##óbitos

table_deaths_per_100k_ma <- df_ma %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ma <- table_deaths_per_100k_ma %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ma, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Maranhão", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ma.gif"))


##Goiás

df_go <- dados %>%
  filter(place_type == "city", state == "GO", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_go <- df_go %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_go <- table_confirmed_per_100k_go %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_go, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Goiás ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_go.gif"))


##óbitos

table_deaths_per_100k_go <- df_go %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_go <- table_deaths_per_100k_go %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_go, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Goiás", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_go.gif"))


##Espírito Santo

df_es <- dados %>%
  filter(place_type == "city", state == "ES", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_es <- df_es %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_es <- table_confirmed_per_100k_es %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_es, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Espírito Santo ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_es.gif"))


##óbitos

table_deaths_per_100k_es <- df_es %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_es <- table_deaths_per_100k_es %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_es, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Espírito Santo", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_es.gif"))


##Distrito Federal

df_df <- dados %>%
  filter(place_type == "city", state == "DF") %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_df <- df_df %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_df <- table_confirmed_per_100k_df %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 5) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_df, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Distrito Federal ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

#gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_df.gif"))


##óbitos

table_deaths_per_100k_df <- df_df %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_df <- table_deaths_per_100k_df %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 5) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_df, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Distrito Federal", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

#gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_df.gif"))


##Ceará

df_ce <- dados %>%
  filter(place_type == "city", state == "CE", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ce <- df_ce %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ce <- table_confirmed_per_100k_ce %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ce, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Ceará ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ce.gif"))


##óbitos

table_deaths_per_100k_ce <- df_ce %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ce <- table_deaths_per_100k_ce %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ce, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Ceará", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ce.gif"))


##Bahia

df_ba <- dados %>%
  filter(place_type == "city", state == "BA", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ba <- df_ba %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ba <- table_confirmed_per_100k_ba %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ba, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Bahia ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ba.gif"))


##óbitos

table_deaths_per_100k_ba <- df_ba %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ba <- table_deaths_per_100k_ba %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ba, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Bahia", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ba.gif"))


##Amazonas

df_am <- dados %>%
  filter(place_type == "city", state == "AM", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_am <- df_am %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_am <- table_confirmed_per_100k_am %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_am, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Amazonas", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_am.gif"))


##óbitos

table_deaths_per_100k_am <- df_am %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_am <- table_deaths_per_100k_am %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-04-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_am, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Amazonas", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_am.gif"))


##Amapá

df_ap <- dados %>%
  filter(place_type == "city", state == "AP", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ap <- df_ap %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ap <- table_confirmed_per_100k_ap %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ap, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Amapá ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ap.gif"))


##óbitos

table_deaths_per_100k_ap <- df_ap %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ap <- table_deaths_per_100k_ap %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ap, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Amapá", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ap.gif"))


##Acre

df_ac <- dados %>%
  filter(place_type == "city", state == "AC", estimated_population_2019 >= 20000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_ac <- df_ac %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_ac <- table_confirmed_per_100k_ac %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_ac, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Acre ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_ac.gif"))


##óbitos

table_deaths_per_100k_ac <- df_ac %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_ac <- table_deaths_per_100k_ac %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_ac, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Acre", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_ac.gif"))

####municípios por estado

##Alagoas

df_al <- dados %>%
  filter(place_type == "city", state == "AL", estimated_population_2019 >= 50000) %>%
  arrange(date)




###casos confirmados

table_confirmed_per_100k_al <- df_al %>%
  select(city,confirmed_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_per_100k_al <- table_confirmed_per_100k_al %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-confirmed_per_100k) * 1,
    Value_rel = confirmed_per_100k / confirmed_per_100k[rank == 1],
    Value_lbl = paste0(" ", confirmed_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_per_100k_al, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = confirmed_per_100k / 2,
    height = confirmed_per_100k,
    width = 0.9
  ), alpha = 0.8, color = "black") +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = confirmed_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Casos Acumulados por 100 mil Habitantes: Alagoas ", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_per_100k_al.gif"))


##óbitos

table_deaths_per_100k_al <- df_al %>%
  select(city,deaths_per_100k,date) %>%
  filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_per_100k_al <- table_deaths_per_100k_al %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    rank = row_number(-deaths_per_100k) * 1,
    Value_rel = deaths_per_100k / deaths_per_100k[rank == 1],
    Value_lbl = paste0(" ", deaths_per_100k)
  ) %>%
  filter(date >= "2020-05-01") %>%
  filter(rank <= 20) %>%
  dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_per_100k_al, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = deaths_per_100k / 2,
    height = deaths_per_100k,
    width = 0.9,
    fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = deaths_per_100k, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Covid-19 Óbitos Acumulados por 100 mil Habitantes: Alagoas", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede Análise COVID-19 por Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(date, transition_length = 4, state_length = 1) +
  gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_per_100k_al.gif"))























































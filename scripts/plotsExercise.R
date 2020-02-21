pacman::p_load(readxl, tidyverse, stringi, microbenchmark, leaflet, sf, magrittr, plotly, RColorBrewer)


# load the excel file (with optional processing)
empl_rate <- read_excel("data/file1.xls", skip = 3, n_max = 41)
# %>% mutate_at(vars(matches("\\d")), as.numeric)
# %>% mutate_if(stri_detect_regex(names(.), "\\d"), as.numeric)

# speed test of two approaches (stringi is always faster)
(bench <- microbenchmark(`dplyr_syntax` = read_excel("data/file1.xls", skip = 3, n_max = 41) %>% mutate_at(vars(matches("\\d")), as.numeric),
                         stringi = read_excel("data/file1.xls", skip = 3, n_max = 41) %>% mutate_if(stri_detect_regex(names(.), "\\d"), as.numeric)))
autoplot(bench)

# pacman::p_load(openxlsx, xlsx, XLConnect) # openxlsx is the best for more advanced operations

# readxl shortcuts (doing everything while loading data)
empl_rate_men <- read_excel("data/file1.xls", skip = 3, n_max = 41, col_types = c("text", rep("numeric", 14)))
empl_rate_women <- read_excel("data/file2.xls", skip = 3, n_max = 41, col_types = c("text", rep("numeric", 14)))

# look at the dataset
empl_rate_men
View(empl_rate_men)
glimpse(empl_rate_men)

# shapefile of europe
(world <- sf::st_read("europe"))
View(world)

# test map
leaflet(sf::st_read("europe")) %>%
  addPolygons(color = 'black', opacity = .4, fillOpacity = .8,
              fillColor = "yellow", weight = .4)

# join of our data with the shapefile
names(empl_rate_men)
eurostat <- left_join(empl_rate_men, world, c("geo\\time" = "NAME_ENGL"))
View(eurostat)

# color distributed palette function for values in 2017
pal_2017 <- leaflet::colorBin(RColorBrewer::brewer.pal(10, "RdYlGn"), eurostat$`2017`)

# values from 2017 on a map (joined table as sf into leaflet())
left_join(empl_rate_men, world, c("geo\\time" = "NAME_ENGL")) %>%
  sf::st_as_sf() %>%
  leaflet() %>%
  addPolygons(label = ~stri_paste(`geo\\time`, " - ", `2017`, "%"),
              color = ~pal_2017(`2017`),
              fillOpacity = .6, opacity = 1, weight = .5)

# function that fixes two "France" tuples by taking their mean
fix_france <- function(dataset) {
  men_france_fixed <- filter(dataset, stri_detect_fixed(`geo\\time`, "France")) %>%
    summarise_at(2:15, mean, na.rm = T)

  dataset[13, -1] <- men_france_fixed
  dataset %<>% filter(!stri_detect_regex(`geo\\time`, "France."))

  dataset
}

# fix datasets with the function above
empl_rate_men <- fix_france(empl_rate_men)
empl_rate_women <- fix_france(empl_rate_women)

# palette for 2018
pal_2018 <- leaflet::colorBin(RColorBrewer::brewer.pal(10, "RdYlGn"), eurostat$`2018`)

# map for 2018
left_join(empl_rate_men, world, c("geo\\time" = "NAME_ENGL")) %>%
  sf::st_as_sf() %>%
  leaflet(options = leafletOptions(minZoom = 3, maxZoom = 7)) %>%
  addPolygons(label = ~stri_paste(`geo\\time`, " - ", `2018`, "%"),
              color = ~pal_2018(`2018`),
              fillOpacity = .6, opacity = 1, weight = .5) %>%
  addControl("Title of the map", "topright") %>%
  addLegend("bottomright", pal = pal_2018, values = ~`2018`, na.label = "undefined")

# change names of columns
eurostat %>%
  rename(country = `geo\\time`)

# keep specific columns
eurostat %>%
  select(`geo\\time`, contains("00"))

# change the values of columns
eurostat %>%
  mutate(`2005` = as.character(`2006`))

# change the values of columns and keep only them
eurostat %>%
  transmute(`geo\\time`, `2018`, mean = (`2006` + `2008`) / 2)

# choose same 5 random countries from both datasets
set.seed(666)
random_5_men <- empl_rate_men %>%
  sample_n(5)

set.seed(666)
random_5_women <- empl_rate_women %>%
  sample_n(5)

# view chosen tuples
random_5_men
random_5_women

# join tuples and keep last 2 years
(empl <- inner_join(random_5_men, random_5_women, by = "geo\\time") %>%
  select(Country = `geo\\time`,
         `Men 2017` = `2017.x`,
         `Women 2017` = `2017.y`,
         `Men 2018` = `2018.x`,
         `Women 2018` = `2018.y`))

# plot with many layers
empl %>% plot_ly(x = ~Country) %>%
  add_bars(y = ~`Men 2017`, name = "Men 2017", color = I("red")) %>%
  add_bars(y = ~`Women 2017`, name = "Women 2017", color = I("orange")) %>%
  add_bars(y = ~`Men 2018`, name = "Men 2018", color = I("yellow")) %>%
  add_bars(y = ~`Women 2018`, name = "Women 2018", color = I("lightgreen")) %>%
  layout(title = "Employment rate in EU in years 2017-2018", yaxis = list(title = "Employment rate [%]"))

# plot with one layer
gather(empl, "Category", "Employment rate [%]", -Country) %>%
  mutate(gender = stri_extract_first_regex(Category, "\\w+")) %>%
  ggplot(aes(Country, `Employment rate [%]`, fill = Category)) +
  geom_col(position = "dodge", width = .3) + facet_grid(~gender) +
  theme_minimal() + coord_flip() + ggtitle("Employment rate in EU in years 2017-2018") +
  theme(plot.title = element_text(hjust = .5)) + scale_fill_brewer(palette = "Dark2")
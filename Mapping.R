library(tidyverse)
library(sf)

election2020 <- read.csv("/Users/edwardblaha/DataWrangling/Week12-Maps/2020results.csv")
election2026 <- read.csv("/Users/edwardblaha/DataWrangling/Week12-Maps/midterms_2026_preliminary.csv")

counties <- st_read("/Users/edwardblaha/DataWrangling/Week12-Maps/cb_2024_us_county_500k/cb_2024_us_county_500k.shp")
names(counties)



election2020$state_fips <- sprintf("%02d", election2020$state_fips) # AI code to go from integer to 2-digit text

counties <- counties |>  
  filter(STATEFP <= 56)

states <- counties |>
  group_by(STATEFP, STUSPS) |>
  summarise(.groups = "drop")

election_map_states <- states |>
  left_join(election2020, by = c("STATEFP" = "state_fips"))

election2020_county <- read.csv("/Users/edwardblaha/DataWrangling/Week12-Maps/2020results_county.csv")
election2020_county$county_fips <- sprintf("%05d", election2020_county$county_fips)
election_map_counties <- counties |>
  left_join(election2020_county, by = c("GEOID" = "county_fips"))


#State map 2020 (2024 absent from data drive)
election_map_states <- election_map_states |>
  mutate(winner = if_else(dem > rep, "Democratic", "Republican"))

library(tigris)    # for shift_geometry()
library(ggthemes)  # for theme_map()

party_colors <- c("Democratic" = "#2E74C0",
                  "Republican" = "#CB454A")

election_map_states <- election_map_states |>
  shift_geometry()

State2020Plot <- ggplot(election_map_states) +
  geom_sf(aes(fill = winner), color = "white") +
  scale_fill_manual(values = party_colors) +
  labs(title = "2020 Presidential Election Results by State", fill = NULL) +
  theme_map() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))



#county map 2020
election_map_counties <- election_map_counties |>
  mutate(winner = if_else(votes_dem > votes_gop, "Democratic", "Republican"))


election_map_counties <- election_map_counties |>
  shift_geometry()

County2020Plot <- ggplot(election_map_counties) +
  geom_sf(aes(fill = winner), color = "white") +
  scale_fill_manual(values = party_colors, na.value = "gray80") +
  labs(title = "2020 Presidential Election Results by County", fill = NULL) +
  theme_map() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))



#Population (Vote) Density by County
library(tidycensus)

pop <- get_decennial(
  geography = "county",
  variables = c(total = "P1_001N"),   # total population
  year = 2020,
  output = "wide"
) |>
  select(GEOID, total)

election_map_counties <- election_map_counties |>
  left_join(pop, by = "GEOID")

election_map_counties <- election_map_counties |>
  mutate(sq_miles = ALAND / 2589988,
         pop_per_sqmi = total / sq_miles)

election_map_counties <- election_map_counties |>
  mutate(density_bins = cut(pop_per_sqmi,
                            breaks = c(0, 10, 50, 100, 500, 1000, 5000, Inf),
                            labels = c("0–10", "10–50", "50–100", "100–500",
                                       "500–1,000", "1,000–5,000", "5,000+")))

PopulationDensityPlot <- ggplot(election_map_counties) +
  geom_sf(aes(fill = density_bins), color = "gray60", linewidth = 0.01) +
  scale_fill_brewer(palette = "Greens", na.value = "gray90") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Population Density by County, 2020",
       fill = "People per square mile") +
  theme_map() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

#% black by county
black_pop <- get_decennial(
  geography = "county",
  variables = c(black = "P1_004N"),   # Black alone
  year = 2020,
  output = "wide"
) |>
  select(GEOID, black)

election_map_counties <- election_map_counties |>
  left_join(black_pop, by = "GEOID")

election_map_counties <- election_map_counties |>
  mutate(pct_black = black / total)

election_map_counties <- election_map_counties |>
  mutate(black_bins = cut(pct_black,
                          breaks = c(0, 0.02, 0.05, 0.10, 0.25, 0.50, 1),
                          labels = c("0–2%", "2–5%", "5–10%",
                                     "10–25%", "25–50%", "50%+"),
                          include.lowest = TRUE))

PercentageBlackPlot <- ggplot(election_map_counties) +
  geom_sf(aes(fill = black_bins), color = "gray60", linewidth = 0.01) +
  scale_fill_brewer(palette = "Purples", na.value = "gray90") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Percent Black by County, 2020",
       fill = "Percent Black") +
  theme_map() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
#2026 Senate

election2026$state_fips <- sprintf("%02d", as.integer(election2026$state_fips))

colors_2026 <- c(party_colors,
                 "Tied" = "#8E6BBF",
                 "No results yet" = "gray60",
                 "No race" = "gray90")

senate_2026 <- election2026 |>
  filter(office %in% c("U.S. Senate", "U.S. Senate (special)")) |>
  arrange(desc(vote_pct)) |>
  distinct(state_fips, .keep_all = TRUE) |>
  mutate(winner = if_else(is.na(vote_pct), "No results yet",
                          if_else(party == "D", "Democratic", "Republican"))) |>
  mutate(winner = if_else(state_abbr == "MS", "Tied", winner))

senate_map <- states |>
  left_join(senate_2026, by = c("STATEFP" = "state_fips")) |>
  mutate(winner = if_else(is.na(winner), "No race", winner)) |>
  shift_geometry()

map_senate <- ggplot(senate_map) +
  geom_sf(aes(fill = winner), color = "white") +
  scale_fill_manual(values = colors_2026,
                    breaks = c("Democratic", "Republican", "Tied",
                               "No results yet", "No race")) +
  labs(title = "2026 Senate Results by State (Preliminary)", fill = NULL) +
  theme_map() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

map_senate

#Governor


gov_2026 <- election2026 |>
  filter(office == "Governor") |>
  arrange(desc(vote_pct)) |>
  distinct(state_fips, .keep_all = TRUE) |>
  mutate(winner = if_else(is.na(vote_pct), "No results yet",
                          if_else(party == "D", "Democratic", "Republican")))

gov_map <- states |>
  left_join(gov_2026, by = c("STATEFP" = "state_fips")) |>
  mutate(winner = if_else(is.na(winner), "No race", winner)) |>
  shift_geometry()

map_governor <- ggplot(gov_map) +
  geom_sf(aes(fill = winner), color = "white") +
  scale_fill_manual(values = colors_2026,
                    breaks = c("Democratic", "Republican", "Tied",
                               "No results yet", "No race")) +
  labs(title = "2026 Governor Results by State (Preliminary)", fill = NULL) +
  theme_map() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

map_governor


#Swing: 2020 President to 2026 Senate

senate_swing <- election2026 |>
  filter(office %in% c("U.S. Senate", "U.S. Senate (special)")) |>
  group_by(state_fips) |>
  summarise(dem26 = sum(vote_pct[party == "D"]),
            rep26 = sum(vote_pct[party == "R"])) |>
  filter(dem26 > 0, rep26 > 0) |>
  left_join(election2020, by = "state_fips") |>
  mutate(margin_2026 = 100 * (dem26 - rep26) / (dem26 + rep26),
         margin_2020 = 100 * (dem - rep) / (dem + rep),
         swing = margin_2026 - margin_2020)

senate_swing_map <- states |>
  left_join(senate_swing, by = c("STATEFP" = "state_fips")) |>
  shift_geometry()

map_senate_swing <- ggplot(senate_swing_map) +
  geom_sf(aes(fill = swing), color = "white") +
  scale_fill_gradient2(low = party_colors["Republican"], mid = "white",
                       high = party_colors["Democratic"],
                       limits = c(-25, 25), na.value = "gray90") +
  labs(title = "Swing from 2020 Presidental Election to 2026 Senate Election",
       fill = "Points toward\nDemocrats",
       caption = "Gray = no race, no results, no Democrat vs. Republican election in 2026.\n Using 2020 because data on 2024 is missing from the course drive") +
  theme_map() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

map_senate_swing



#Swing: 2020 President to 2026 Governor

gov_swing <- election2026 |>
  filter(office == "Governor") |>
  group_by(state_fips) |>
  summarise(dem26 = sum(vote_pct[party == "D"]),
            rep26 = sum(vote_pct[party == "R"])) |>
  filter(dem26 > 0, rep26 > 0) |>
  left_join(election2020, by = "state_fips") |>
  mutate(margin_2026 = 100 * (dem26 - rep26) / (dem26 + rep26),
         margin_2020 = 100 * (dem - rep) / (dem + rep),
         swing = margin_2026 - margin_2020)

gov_swing_map <- states |>
  left_join(gov_swing, by = c("STATEFP" = "state_fips")) |>
  shift_geometry()

map_gov_swing <- ggplot(gov_swing_map) +
  geom_sf(aes(fill = swing), color = "white") +
  scale_fill_gradient2(low = party_colors["Republican"], mid = "white",
                       high = party_colors["Democratic"],
                       limits = c(-25, 25), na.value = "gray90") +
  labs(title = "Swing from 2020 President to 2026 Governor",
       fill = "Points toward\nDemocrats",
       caption = "Gray = no race, no results, no Democrat vs. Republican election in 2026.\n Using 2020 because data on 2024 is missing from the course drive") +
  theme_map() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

map_gov_swing

#Syracuse on a map
syracuse <- data.frame(longitude = -76.1352, latitude = 43.0377) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  shift_geometry()

map_syracuse <- ggplot() +
  geom_sf(data = shift_geometry(states), fill = "gray90", color = "white") +
  geom_sf(data = syracuse, shape = 11, size = 3, stroke = 2, color = "#F76900") +
  labs(title = "Syracuse University") +
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5))

map_syracuse

#ACC schools

library(ggrepel)   
# U.S. Department of Education, National Center for Education Statistics, 
#Integrated Postsecondary Education Data System (IPEDS), Institutional Characteristics: 
#Directory Information, 2023 (HD2023). https://nces.ed.gov/ipeds/datacenter/data/HD2023.zip

acc <- data.frame(
  school = c("Boston College", "California", "Clemson", "Duke", "Florida State",
             "Georgia Tech", "Louisville", "Miami", "North Carolina", "NC State",
             "Notre Dame", "Pittsburgh", "SMU", "Stanford", "Syracuse",
             "Virginia", "Virginia Tech", "Wake Forest"),
  longitude = c(-71.169242, -122.260463, -82.835114, -78.937624, -84.295064,
                -84.394832, -85.760412, -80.278662, -79.050969, -78.674517,
                -86.238959, -79.953254, -96.783365, -122.167359, -76.133989,
                -78.503428, -80.423675, -80.277446),
  latitude = c(42.336213, 37.871918, 34.679381, 36.001135, 30.443147,
               33.772420, 38.216862, 25.721260, 35.911769, 35.785111,
               41.703058, 40.444388, 32.843612, 37.429434, 43.036644,
               38.035665, 37.229012, 36.133609),
  color = c("#98002E", "#003262", "#F56600", "#00539B", "#782F40",
            "#B3A369", "#AD0000", "#F47321", "#7BAFD4", "#CC0000",
            "#0C2340", "#003594", "#CC0035", "#8C1515", "#F76900",
            "#E57200", "#861F41", "#9E7E38")
) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  shift_geometry()

map_acc <- ggplot() +
  geom_sf(data = shift_geometry(states), fill = "gray95", color = "gray70") +
  geom_sf(data = acc, aes(color = color), size = 3) +
  geom_text_repel(data = acc,
                  aes(label = school, geometry = geometry, color = color),
                  stat = "sf_coordinates", size = 3.5, fontface = "bold") +
  scale_color_identity() +
  labs(title = "The Atlantic Coast Conference, 2026-27") +
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5))

map_acc



library(patchwork)

#ACC schools zoomed in

election_map_counties <- election_map_counties |>
  mutate(dem_share = votes_dem / (votes_dem + votes_gop))

zoom_map <- function(name) {
  school_point <- acc |> filter(school == name)
  window <- st_buffer(school_point, 120000)
  
  ggplot() +
    geom_sf(data = st_crop(election_map_counties, window),
            aes(fill = dem_share), color = "white", linewidth = 0.1) +
    geom_sf(data = school_point, shape = 21, fill = "white", color = "black",
            size = 2.5, stroke = 1) +
    scale_fill_gradient2(low = party_colors["Republican"], mid = "white",
                         high = party_colors["Democratic"], midpoint = 0.5,
                         limits = c(0, 1), labels = scales::label_percent(),
                         na.value = "gray80") +
    labs(title = name, fill = "Democratic share\nof two-party vote") +
    theme_map() +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, size = 10))
}

school_maps <- lapply(sort(acc$school), zoom_map)

map_acc_zoom <- wrap_plots(school_maps, ncol = 6) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "The 2020 Presidential Vote Around Each ACC School",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16),
                                plot.subtitle = element_text(hjust = 0.5)))

map_acc_zoom




#plot

acc_counties <- acc |>
  st_join(election_map_counties) |>
  st_drop_geometry() |>
  left_join(election2020, by = c("STATEFP" = "state_fips")) |>
  mutate(state_share = dem / (dem + rep))

plot_acc_vote <- ggplot(acc_counties, aes(y = reorder(school, dem_share))) +
  geom_segment(aes(x = state_share, xend = dem_share,
                   yend = reorder(school, dem_share)), color = "gray60") +
  geom_point(aes(x = state_share), color = "gray50", size = 3) +
  geom_point(aes(x = dem_share), color = "black", size = 3) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray70") +
  scale_x_continuous(labels = scales::label_percent()) +
  labs(title = "ACC Host Counties vs. Their States, 2020",
       subtitle = "Gray: statewide Democratic share. Black: the school's county.",
       x = "Democratic share of two-party vote", y = NULL) +
  theme_minimal()

plot_acc_vote
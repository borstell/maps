
# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidylo)
library(sf)
#devtools::install_github("borstell/swemapdata")
library(swemapdata)



# Explore data ------------------------------------------------------------

# Find common (and meaningful) 3-letter endings 
# among "tätorter" (lit. 'dense places')
tatorter |> 
  as_tibble() |> 
  mutate(end3 = str_sub(name, -3, -1)) |> 
  count(end3, sort = T) |> 
  print(n = 40)

# Some relevant ones
endings <- c("vik", "sta", "näs", "sjö", "ryd", 
             "arp", "bro", "red", "löv", "röd")



# Process data ------------------------------------------------------------

# Calculate relative frequency of endings by region (län)
# using log odds ratio from {tidylo}
swedish_endings <- 
  tatorter |> 
  mutate(ending = case_when(
    str_ends(name, "vik") ~ "vik",
    str_ends(name, "sta") ~ "sta",
    str_ends(name, "näs") ~ "näs",
    str_ends(name, "sjö") ~ "sjö",
    str_ends(name, "ryd") ~ "ryd",
    str_ends(name, "arp") ~ "arp",
    str_ends(name, "bro") ~ "bro",
    str_ends(name, "red") ~ "red",
    str_ends(name, "löv") ~ "löv",
    str_ends(name, "röd") ~ "röd",
  )) |> 
  as_tibble() |> 
  select(-geometry) |> 
  
  # Count occurrences
  count(lan_code, ending) |> 
  
  # Calculate log odds
  tidylo::bind_log_odds(lan_code, ending, n, uninformative = T, unweighted = T) |> 
  select(-log_odds_weighted) |> 
  
  # Join with geospatial data and transform to SF
  left_join(lan, by = join_by(lan_code == code)) |> 
  drop_na(ending) |> 
  st_as_sf()



# Plot data ---------------------------------------------------------------

# Filter to positive log odds
swedish_endings |> 
  filter(log_odds > 0) |> 
  
  # Plot data
  ggplot() +
  geom_sf(data = lan, fill = "grey85", color = "transparent") +
  geom_sf(aes(fill = log_odds), color = "transparent") +
  scale_fill_gradient(low = "lightyellow", high = "firebrick", 
                       na.value = "grey85") +
  facet_wrap(~paste0("\n—", ending), ncol = 5) +
  guides(fill = "none") +
  labs(title = "Spatial distribution of endings in Swedish place names", 
       subtitle = "Log odds frequency of 3-letter endings per region (län)",
       caption = "Data: Statistics Sweden (SCB) | Packages: {tidyverse, tidylo, sf, swemapdata} | Visualization: C. Börstell") +
  theme_void(base_size = 15, base_family = "PT Sans Narrow Bold") +
  theme(strip.text = element_text(margin = margin(2, 2, 2, 2, "mm")),
        plot.title = element_text(size = rel(1.3), hjust = .5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "PT Sans Narrow", color = "grey50", hjust = 1.2),
        plot.caption = element_text(family = "PT Sans Narrow", size = rel(.65), hjust = .5, color = "grey40"),
        plot.margin = margin(5, -5, 5, -5, "mm"))

# Save plot
ggsave("swedish_placenames.png", width = 6, height = 6.5, units = "in", dpi = 600, bg = "snow")





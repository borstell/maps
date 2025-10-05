
# Load packages -----------------------------------------------------------

library(tidyverse)
library(rvest)
library(sf)
library(swemapdata)



# Read data ---------------------------------------------------------------

# Read data from Wikipedia
wiki <- 
  rvest::read_html("https://sv.wikipedia.org/wiki/Svenska_ortnamnsefterled")

# Extract endings and make into searchable strings
endings <- 
  wiki |> 
  html_elements("h2") |> 
  html_text() |> 
  tibble(ending = _) |> 
  filter(str_starts(ending, "-")) |> 
  mutate(ending = str_remove_all(ending, " ")) |> 
  mutate(ending = str_replace_all(ending, ",", "/")) |> 
  mutate(end = str_split(ending, "/")) |> 
  unnest(end) |> 
  mutate(end_string = str_remove_all(str_replace_all(end, "/", "|"), "-"))

# Function for matching longest possible ending from data frame
match_end <- function(x, df) {
  matches <- df[which(str_ends(x, df$end_string)), ]$ending
  longest_match <- matches[which.max(nchar(matches))]
  if (length(longest_match) > 0) {
    return(longest_match)
  } else {
    return(NA)
  }
}



# Process data ------------------------------------------------------------

# Calculate relative frequency of endings by region (län)
# using log odds ratio from {tidylo}
swedish_endings <- 
  tatorter |> 
  tibble() |> 
  select(-geometry) |> 
  
  # Get longest possible ending match
  mutate(ending = map_chr(name, \(x) match_end(x, endings))) |> 
  
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
  
  mutate(max_log_odds = max(log_odds), 
         max_n = max(n),
         .by = ending) |> 
  filter(max_log_odds > 1 & max_n > 5) |> 
  mutate(label_len = map_int(str_extract_all(ending, "-"), length)) |> 
  mutate(label = case_when(
    label_len > 2 ~ "bajs",
    .default = ending
  )) |> 
  
  # Plot data
  ggplot() +
  geom_sf(data = lan, fill = "grey85", color = "transparent") +
  geom_sf(aes(fill = log_odds), color = "transparent") +
  scale_fill_gradient(low = "yellow2", high = "firebrick", 
                      na.value = "grey85") +
  facet_wrap(~str_replace_all(ending, "/", "\n"), ncol = 5, strip.position = "right") +
  guides(fill = "none") +
  labs(title = "Spatial distribution of endings in Swedish place names", 
       subtitle = "Log odds frequency of well-known endings per region (län)\n",
       caption = "Data: Statistics Sweden (SCB) & Wikipedia | Packages: {tidyverse, tidylo, sf, swemapdata} | Visualization: C. Börstell") +
  theme_void(base_size = 15, base_family = "PT Sans Narrow Bold") +
  theme(strip.text = element_text(margin = margin(0, 0, 0, -.5, "mm"), hjust = .5, size = rel(.8)),
        plot.title = element_text(size = rel(1.8), hjust = .5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "PT Sans Narrow", 
                                     color = "grey50", hjust = 1.1),
        plot.caption = element_text(family = "PT Sans Narrow", size = rel(.65), hjust = .5, color = "grey40"),
        plot.margin = margin(5, -5, 5, -5, "mm"))

# Save plot
ggsave("swedish_placenames_wiki.png", width = 7.7, height = 10, units = "in", dpi = 600, bg = "snow")



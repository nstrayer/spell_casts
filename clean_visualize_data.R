library(purrr)
library(tidyverse)
library(showtext)
# read in and clean data for modeling

# Download a wefont
font_add_google(name = "Amatic SC", family = "Amatic SC")

showtext.auto()

# get list of CSVs
all_files <- list.files('gather_data')
csv_locations <- paste0('gather_data/', all_files[grepl('.csv', all_files)])

# load all data into a dataframe
data <- csv_locations %>% 
  map_df(read_csv) %>% 
  mutate(recording_num = gsub('gesture ', '', recording_num))

ggplot(data, aes(x = time, y = accel, color = direction, group = recording_num)) +
  geom_line(alpha = 0.15) +
  facet_wrap(~label) +
  theme_void() +
  theme(strip.text = element_text(family = "Amatic SC"))

data %>% write_csv('spell_data.csv')

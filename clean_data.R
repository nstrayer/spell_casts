library(tidyverse)

gestures_1 <- read_csv('my_gestures_1.csv') %>% 
  select(-X1) %>% 
  mutate(
    recording = gsub('gesture ', '', recording_num),
    label = case_when(
      recording == '21' | label == 'Episkey' ~ 'Episkey', 
      label == 'Alohamora' ~ 'Accio',
      label == 'Alohamoratrue' ~ 'Alohamora')) %>% 
  filter(!(recording %in% c(5))) %>% 
  group_by(recording_num) %>% 
  select(-recording) %>% 
  mutate(
    time = time - first(time),
    batch = 1 ) %>% 
  ungroup() %>% 
  distinct()

gestures_2 <- read_csv('my_gestures_2.csv') %>% mutate(batch = 2) %>% select(-X1)
gestures_3 <- read_csv('my_gestures_3.csv') %>% mutate(batch = 3) %>% select(-X1)

all_gestures <- rbind(
  gestures_1,
  gestures_2,
  gestures_3
) %>% 
  group_by(recording_num, batch) %>% 
  mutate(
    time = time - first(time),
    id = paste0(gsub('gesture ', '', recording_num), '_', batch)
  ) %>% 
  group_by(time, direction, id) %>% 
  summarise(
    accel = mean(accel),
    spell = last(label)
  ) 

write_csv(all_gestures, 'spell_data.csv')

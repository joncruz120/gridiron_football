library(tidyverse)
library(ggrepel)
library(magrittr)
library(tidyverse)
library(scales)
library(labeling)
library(readxl)
library(shiny)
library(devtools)
library(gganimate)
library(moderndive)
library(na.tools)
library(ggimage)

# Read csv into R from github: 2019 play by play (weeks 1-17)

pbp_2019_original <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2019.csv"))

# Removing plays that don't have values for Expected Points Added (EPA)
pbp1 <- pbp_2019_original %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp1 %>% select(posteam, desc, play_type) %>% head(n=10)  

# Correcting "no plays" that were actually unsuccessful plays
pbp2 <- pbp1 %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp2 %>% filter(play_type=="no_play") %>% select(pass, rush, desc)  %>% head

#Pass heavy teams before final 2 minutes of each half with win prob > 20%
pass_over_run <- pbp2 %>%
  filter(wp >= .20 & wp <= .80 & down<=2 & qtr<=2 & half_seconds_remaining>120) %>%
  group_by(posteam) %>%
  summarize(mean_pass=mean(pass), plays=n()) %>%
  arrange(mean_pass)
 
pass_over_run_plot <- pass_over_run %>%
  ggplot(aes(x = reorder(posteam, -mean_pass), y = mean_pass, label = posteam)) + geom_point(aes(label = posteam)) +labs(x = "Teams", y = "Average Pass Play")

show(pass_over_run_plot)

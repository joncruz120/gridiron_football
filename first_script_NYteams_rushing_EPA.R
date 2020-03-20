# Read csv into R from github: 2019 play by play (weeks 1-17)

pbp_2019_original <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2019.csv"))

# Save original csv back into excel
write.table(pbp_2019, file="pbp_2019.csv", sep = ",")

# View structure of table
str(pbp_2019)

# Removing plays that don't have values for Expected Points Added (EPA)
pbp1 <- pbp_2019_original %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp1 %>% select(posteam, desc, play_type) %>% head(n=10)  

# Correcting "no plays" that were actually unsuccessful plays
pbp1 <- pbp1 %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp1 %>% filter(play_type=="no_play") %>% select(pass, rush, desc)  %>% head

pbp1 <- pbp2 %>% filter(pass==1 | rush==1)  

# Giants who rushed the ball more than 10x with an EPA
NYG_rushing <- pbp1 %>%
  filter(posteam == "NYG", rush == 1, down<=4) %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>10)

#Giants plot
NYG_rushing_plot <- NYG_rushing %>%
  ggplot(aes(x = success_rate, y = mean_epa)) + geom_point(color = "blue", size = 3) + geom_label_repel(
    aes(label = rusher_player_name)) + labs(x = "Rate of Success", y = "Average Expected Points Added")

show(NYG_rushing)
show(NYG_rushing_plot)

# Jets who rushed the ball more than 10x with an EPA
NYJ_rushing <- pbp1 %>%
  filter(posteam == "NYJ", rush == 1, down<=4) %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>10)

# Jets plot
NYJ_rushing_plot <- NYJ_rushing %>%
  ggplot(aes(x = success_rate, y = mean_epa)) + geom_point(color = "blue", size = 3) + geom_label_repel(
    aes(label = rusher_player_name)) + labs(x = "Rate of Success", y = "Average Expected Points Added")

show(NYJ_rushing_plot)

# Combined NY rushing to see all rushers
combined_NY_rushing<- full_join(NYG_rushing, NYJ_rushing, by = c("rusher_player_name", "mean_epa", "success_rate", "ypc", "plays"))

combined_NY_plot <- combined_NY_rushing %>%
  ggplot(aes(x = success_rate, y = mean_epa)) + geom_point(aes(color = "red", size = plays)) + scale_x_log10()  + geom_label_repel(
    aes(label = rusher_player_name)) + labs(title = "Who is the most efficient RB in New York?", x = "Rate of Success", y = "Average Expected Points Added")

show(combined_NY_plot)


> 
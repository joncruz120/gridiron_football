# QB Drop back success rate vs EPA plots

#Get number of dropbacks aka # of passes and EPA per DB
chart_data <- pbp2 %>%
  filter(pass == 1) %>%
  group_by(posteam) %>%
  summarise(
    num_db = n(),
    epa_per_db = sum(epa) / num_db,
    success_rate = sum(epa > 0) / num_db
  )

nfl_logos <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")

chart2 <- chart_data %>% left_join(nfl_logos, by = c("posteam" = "team_code"))

chart2 %>%
  ggplot(aes(x = success_rate, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(
    x = "Success rate",
    y = "EPA per play",
    caption = "Data from nflscrapR",
    title = "Dropback success rate & EPA/play",
    subtitle = "2019"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12)
  )


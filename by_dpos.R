by_pos <- no_outliers |>
  group_by(tacklerPos) |>
  summarise(meanForce = mean(overExpected)) 

by_pos |>
  ggplot(aes(x = reorder(tacklerPos, desc(meanForce)), y = meanForce)) +
  geom_col(fill = 'gray13') +
  theme_bw() +
  labs(x = "Position",
       y = "Average Over Expected Force",
       title = "Average Over Expected Force by Defensive Position",
       caption = "Owen Yoo | Ben Weber | Eliana Detata | MSAS") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('by_dpos.png', width = 10, height = 10, dpi = "retina")

  
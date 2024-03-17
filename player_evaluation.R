filtered_players <- no_outliers |>
  group_by(tacklerName) |>
  summarize(count = n()) |>
  filter(count >= 10)

tacklers <- filtered_players$tacklerName

top_force <- no_outliers |>
  group_by(tacklerName) |>
  summarize(count = n(),
            force = mean(overExpected)) |>
  filter(count >= 10) |>
  arrange(desc(force)) |>
  head(5) 

top_tackle <- no_outliers |>
  group_by(tacklerName) |>
  summarize(count = n(),
            force = mean(overExpected)) |>
  filter(count >= 10) |>
  arrange(desc(count)) |>
  head(5) 

dpoy <- no_outliers |>
  group_by(tacklerName) |>
  summarize(count = n(),
            force = mean(overExpected)) |>
  filter(tacklerName %in% c('Nick Bosa', 'Chris Jones', 'Micah Parsons')) |>
  arrange(desc(count))


top_players <- rbind(top_force, top_tackle) |>
  pull(tacklerName)


# Define your 10 specific tacklers and their desired colors
specific_colors <- c('red', 'red', 'red', 'red', 'red',
                     'blue', 'blue', 'blue', 'blue', 'blue')

# Create a named vector for the colors, setting the default color for the rest of the tacklers
color_values <- setNames(c(specific_colors, rep('lightgrey', length(unique(no_outliers$tacklerName)) - length(top_players))),
                         c(top_players, setdiff(unique(no_outliers$tacklerName), top_players)))

no_outliers |>
  filter(tacklerName %in% tacklers) |>
  ggplot(aes(x = overExpected, color = tacklerName)) +
  geom_density() +
  scale_color_manual(values = color_values) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = 'Over Expected Generated Force',
       y = 'Density',
       title = 'Distribution of Over Expected Generated Force',
       subtitle = 'Red = Top 5 in Avg. Over Expected Force | Blue = Top 5 in tackles in our Dataset',
       caption = "Owen Yoo | Ben Weber | Eliana Detata | MSAS") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave('distribution.png', width = 14, height = 10, dpi = "retina")


gt(top_force) |>
  tab_header(
    title = "Top Avg. Over Expected Force",
    subtitle = "Minimum of 10 Tackles to be considered"
  )

gt(top_tackle) |>
  tab_header(title = "Top Tacklers")
  
  


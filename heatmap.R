#Do it by 10 so the graph comes out clean
heatmap_df <- no_outliers |>
  mutate(dividedX = cut(tacklerX, breaks = c(-Inf, 12, 24, 36, 48, 60, 72, 84, 
                                             96, 108, 120),
                        labels = c('0-12', '12-24', '24-36', '36-48', '48-60',
                                   '60-72', '72-84', '84-96', '96-108', '108-120')),
         dividedY = cut(tacklerY, breaks = c(-Inf, 10, 20, 30, 40, Inf),
                       labels = c('<0-10', '10-20', '20-30', '30-40', '40-50+'))) 

field <- image_read("field.jpg")

heatmap_df |>
  group_by(dividedX, dividedY) |>
  summarize(meanForce = mean(overExpected)) |>
  ggplot(aes(x = dividedX, y = dividedY, fill = meanForce)) +
  annotation_raster(field, xmin = 0.2, xmax = 10.75, ymin = 0.3, ymax = 5.7) +
  geom_tile(alpha = 0.66) +
  scale_fill_gradient(low = "red", high = "green") +
  theme_bw() +
  labs(x = "X Coordinate",
       y = "Y Coordinate",
       fill = "Avg. Over Expected Force",
       title = "The Average Over Expected Force per Field Location",
       caption = "Owen Yoo | Ben Weber | Eliana Detata | MSAS") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

ggsave('field_heatmap.png', width = 14, height = 10, dpi = "retina")
  


  




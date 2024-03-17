over_expected <- read_csv("over_expected.csv")

outliers <- boxplot(over_expected$overExpected, plot=FALSE)$out

no_outliers <- over_expected[-which(over_expected$overExpected %in% outliers),]

plotting_t <- as.data.frame(tvalue)

plot_t <- data.frame(variable = rownames(plotting_t), 
           tvalue = plotting_t$tvalue) |>
  mutate(tvalue = abs(tvalue)) |>
  arrange(desc(tvalue))

better_vip <- head(plot_t, 10)

better_vip |>
  ggplot(aes(x = tvalue, y = reorder(variable, tvalue))) +
  geom_col(fill = 'gray13') +
  theme_bw() +
  labs(x = 'Importance (Determined by Absolute Value of t-value)',
       y = 'Variable',
       title = "Variable Importance Plot for Expected Force Regression",
       caption = "Owen Yoo | Ben Weber | Eliana Detata | MSAS") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('tackle_vip.png', width = 14, height = 10, dpi = "retina")



  


  



newpbp <- read_csv("data bowl CSV/newpbp.csv")

newpbp <- newpbp |>
  mutate(distFromEachOther = sqrt((tacklerX - ballCarrierX)^2 + (tacklerY - ballCarrierY)^2),
         forceGeneratedSpotTackle = if_else(event == 'tackle', forceGeneratedSpotTackle, NA)) |>
  filter(!is.na(tacklerName),
         tackle == 1,
         !(ballCarrierPos == 'QB'),
         !(ballCarrierDisplayName == 'Taysom Hill'))

index <- newpbp |>
  group_by(playId, gameId) |>
  summarize(count = n())

onlyDoubleRealReal <- subset(index, count == 2)

only_one <- newpbp |>
  filter(playId %in% onlyDoubleRealReal$playId & gameId %in% onlyDoubleRealReal$gameId) |>
  group_by(playId, gameId) |>
  summarize(count = n()) |>
  filter(count != 2)

newer_pbp <- newpbp |>
  filter(!(playId %in% only_one$playId) | !(gameId %in% only_one$gameId),
         !grepl("pushed ob", playDescription),
         !(ballCarrierPos == 'CB'),
         !(ballCarrierPos == 'OLB')) |>
  left_join(temp, by = c('gameId', 'playId')) |>
  filter(is.na(count))


temp<-newer_pbp |>
  group_by(gameId, playId) |>
  summarize(count = n()) |>
  filter(!(count == 2))

write.csv(newer_pbp, "data bowl CSV/.csv", row.names = FALSE)
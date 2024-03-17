library(tidyverse)

pbp <- read_csv("pbp.csv")

pbp <- pbp |>
  mutate(differenceAngularDirection = if_else(differenceAngularDirection > 180, 
                                              abs(differenceAngularDirection - 360),
                                              differenceAngularDirection))

fumbleId <- pbp |>
  filter(event == 'fumble') 

fumbleId <- fumbleId$playId

tackle <- pbp |>
  filter(event == 'tackle' | event == 'fumble') |>
  filter(!(playId %in% fumbleId)) |>
  select(gameId, playId, ballCarrierId, tacklerNflId, ballCarrierDisplayName, ballCarrierPos, 
         tacklerName, tacklerPos, quarter, down,  yardsToGo, possessionTeam, defensiveTeam, 
         playDescription, playResult, diffInOrientationVsDirTackler, forceGeneratedSpotTackle, tackleAngle, 
         disFromFirstDown, tacklerAcc, ballCarrierrAcc, tacklerX, tacklerY) |>
  rename('DIOVDT_T' = diffInOrientationVsDirTackler,
         'tacklerAcc_T' = tacklerAcc,
         'ballCarrierAcc_T' = ballCarrierrAcc)

ballRecieved <- pbp |>
  filter(event == 'handoff' | event == 'pass_outcome_caught') |>
  filter(!(playId %in% fumbleId)) |>
  select(playDescription, distFromEachOther, tacklerMass, ballCarrierMass, passProbability, tackleAngle,
         tacklerAcc, ballCarrierrAcc, tackleDisFromCenter, diffInOrientationVsDirTackler,
         differenceAngularDirection) |>
  rename('DIOVDT_BR' = diffInOrientationVsDirTackler,
         'tacklerAcc_BR' = tacklerAcc,
         'ballCarrierAcc_BR' = ballCarrierrAcc,
         'ballRecievedAngle' = tackleAngle) 

tackle_df <- tackle |>
  left_join(ballRecieved, by = 'playDescription') |>
  filter(DIOVDT_T <= 180,
         DIOVDT_BR <= 180) |>
  filter(!is.na(passProbability))



tackleRegression <- lm(forceGeneratedSpotTackle ~ DIOVDT_T + tackleAngle + playResult + ballRecievedAngle +
                       disFromFirstDown + tacklerAcc_T + ballCarrierAcc_T +
                         distFromEachOther + tacklerMass + ballCarrierMass +
                         passProbability + tacklerAcc_BR + ballCarrierAcc_BR +
                         tackleDisFromCenter + DIOVDT_BR + differenceAngularDirection
                       ,data = tackle_df)

tackle_df$expForce <- predict(tackleRegression, data = tackle_df, type = 'response')

tackle_df <- tackle_df |>
  mutate(overExpected = forceGeneratedSpotTackle - expForce)

write.csv(tackle_df, "C:/Users/owenh/OneDrive/Desktop/MSAS/over_expected.csv")
  

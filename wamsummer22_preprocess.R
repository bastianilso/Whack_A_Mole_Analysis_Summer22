library(gsheet)
library(tidyverse)
source("utils/loadrawdata.R")

options("digits.secs"=6)

# Load data from directories
# For some reason we only have original data from Participant 1-10 in the folder.
# So load the data from the RDA file instead!
D <- LoadFromDirectory("testdata3/")
save(D, file = 'data_wam_raw.rda', compress=TRUE)

D <- D %>% mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>% 
  arrange(Timestamp) %>%  select(Timestamp, Event, everything())


D <- D %>% rename(Condition = i3, Participant = i2) 

D <-D %>% mutate(Participant = as.numeric(Participant),
                 Framecount = as.numeric(Framecount),
                 GameTimeSpent = as.numeric(GameTimeSpent),
                 MolePositionWorldX = as.numeric(MolePositionWorldX),
                 MolePositionWorldY = as.numeric(MolePositionWorldY),
                 MolePositionWorldZ = as.numeric(MolePositionWorldZ),
                 RightControllerLaserPosWorldX = as.numeric(RightControllerLaserPosWorldX),
                 RightControllerLaserPosWorldY = as.numeric(RightControllerLaserPosWorldY),
                 RightControllerLaserPosWorldZ = as.numeric(RightControllerLaserPosWorldZ))


#############
# Clean events before and after "Game Started"
#############

# Create a "PlayPeriod" column to indicate: "PreGame", "Game" and "PostGame" 
D = D %>% group_by(Participant, Condition) %>%
  mutate(indication = ifelse(Event == "Game Started", 1, 0),
         indication = ifelse(Event %in% c("Game Finished","Game Stopped"), 1, indication),
         indication = cumsum(indication),
         indication = ifelse(Event %in% c("Game Finished","Game Stopped"), 1, indication),
         PlayPeriod = ifelse(indication < 1, "PreGame", "Game"),
         PlayPeriod = ifelse(indication > 1, "PostGame", PlayPeriod),
         indication = NULL)

# Create a time_delta column, which measures the duration of each row by Timestamp.
D = D %>% 
  mutate(Time_delta = Timestamp - lag(Timestamp),
         Time_delta = as.numeric(Time_delta),
         Time_delta = ifelse(is.na(Time_delta), 0, Time_delta))

# Create a Hz column - Measures sample rate (samples per second).
D = D %>% mutate(second = format(Timestamp, format = "%Y-%m-%d %H:%M:%S")) %>% 
  group_by(second) %>%
  mutate(Hz = sum(Event=="Sample")) %>%
  ungroup() %>%
  select(-second)

# Re-calculate GameTimeSpent - so it also covers Sample Events
# GameTimeSpent in Unity can only easily be provided on Events.

D = D %>% group_by(Participant, Condition) %>%
  mutate(
  GameStarted = paste(Timestamp[Event == "Game Started"],collapse=' '),
  GameStarted = as.POSIXct(GameStarted, format = "%Y-%m-%d %H:%M:%OS"),
  GameFinished = paste(Timestamp[Event %in% c("Game Finished","Game Stopped")],collapse=' '),
  GameFinished = as.POSIXct(GameFinished, format = "%Y-%m-%d %H:%M:%OS"),
  GameTimeSpent = as.numeric(difftime(Timestamp, GameStarted)),
  GameTimeLeft = as.numeric(difftime(GameFinished,Timestamp))
)

#############
# Save to RDA
#############
# Split into 4 

save(D, file = 'data_wam.rda', compress=TRUE)


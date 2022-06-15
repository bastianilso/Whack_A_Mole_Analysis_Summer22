library(plotly)
library(tidyverse)
library(lubridate)
library(sf)
options("digits.secs"=6)

fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

load('data_wam.rda')

#############
# Summaries
#############

St <- D %>% group_by(Participant, Condition) %>% filter(PlayPeriod == "Game") %>%
  summarise(MoleHitCount = sum(Event == "Mole Hit"),
            PlayTime = difftime(Timestamp[Event %in% c("Game Finished", "Game Stopped")],Timestamp[Event == "Game Started"]),
            PlayTime_delta = sum(Time_delta), # PlayTime based on summing up time_delta
            MeanGaze_x =  mean(as.numeric(WorldGazeHitPositionX), na.rm=TRUE),
            MeanGaze_y =  mean(as.numeric(WorldGazeHitPositionY), na.rm=TRUE),
            MeanGaze_z =  mean(as.numeric(WorldGazeHitPositionZ), na.rm=TRUE),
            MeanGazeConf = mean(as.numeric(GazeConfidence), na.rm=TRUE),
            MeanHz = mean(Hz),
            MolesRight = sum(Event == "Mole Spawned" & MolePositionWorldX > 0),
            MolesLeft = sum(Event == "Mole Spawned" & MolePositionWorldX < 0)
            )

# Summary of Moles
Sm <- D %>% filter(Event == "Mole Spawned", PlayPeriod == "Game") %>% 
  select(MoleId, MolePositionWorldX, MolePositionWorldY, MoleSpawnOrder, PatternSegmentLabel) %>%
  st_as_sf(coords = c("MolePositionWorldX", "MolePositionWorldY"), crs = NA, remove=F) %>%
  mutate(geometry_lag = lag(geometry),
         geometry_lead =lead(geometry))

Sm <- Sm %>% rowwise() %>% mutate(distance_lag = st_distance(geometry, geometry_lag, which="Euclidean"))

Sm <- Sm %>% ungroup() %>% 
  mutate(travelLeadHor = ifelse(MolePositionWorldX > lead(MolePositionWorldX), "Right", "Left"),
         travelLeadHor = ifelse(MolePositionWorldX == lead(MolePositionWorldX), "None", travelLeadHor),
        travelLeadVer = ifelse(MolePositionWorldY < lead(MolePositionWorldY), "Down", "Up"),
        travelLeadVer = ifelse(MolePositionWorldY == lead(MolePositionWorldY), "None", travelLeadVer))

Sm %>% group_by(PatternSegmentLabel) %>% 
  summarise(dist = sum(distance_lag),
            left = sum(travelLeadHor == "Left"),
            right = sum(travelLeadHor == "Right"))

# Legal "actions" to take with segments:
# Reaction time based on equal euclidian distance travelled
# If differences are found, it can be because of: travel direction, placement of moles, the person's attention.
# It's not fair to look at just "travel direction" in its basic form. The assumption is that even if a mole is left/right to another mole, if its adjacent enough, the head and eyes and controller dont really travel to left/right.
# Instead, we should analyze by column and measure by column. How are the travels from left column to right column distributed.

# So where we are now: The segments are: 1) equally distance, 2) includes the types of travel we want to measure
# So where does that leave us.
# What happens when a patient performs worse in Segment D than in Segment A?
# 1) It could be just 1 mole, that affects the average reaction time. Fx. missing a mole or not being able to find a mole.
# 2) The segments' purposes are exclusively just to provide something we can shuffle around.
# 3) The segments' purpose is not to be possible to analyze, or to have certain characteristics. Instead, we have certain travels we are interested in analyzing.

# Why should each segment cover an equal distance?
# It allows for: Using only half the amount of segments.
# It allows for: Swopping segments around?
# It allows for: Working 10 moles at a time..
# Segments cover "10 moles". By having equal segments, we can also say that for each 10 moles, they cover the same distance.
# So the segments are sort of "balanced". ?


#############
# Visual Check for Duplicate Moles / Missing Moles
#############
fig %>%
    add_trace(name="Spawn Points", data=D %>% filter(PlayPeriod == "Game") %>% filter(Participant == 1) %>% ungroup(),
              x=~MolePositionWorldX, y=~MolePositionWorldY, type='scatter', color=~PatternSegmentLabel,
              mode='markers',symbol=I('circle'),marker=list(size=32, opacity=.5), 
              hoverinfo='text',text=~paste(MoleId,MolePositionWorldX,MolePositionWorldY))

measureMoles = c('0705','0104','0505','0701','0902','0501','0205','0401','0303','0201','0605','0803')

Sm = Sm %>% mutate(measured = ifelse(MoleId %in% measureMoles, T,F))

fig %>% add_trace(data=Sm,x=~as.numeric(distance_lag), y=~as.integer(MoleSpawnOrder), symbol=I('o'), marker=list(size=24)) %>%
  add_trace(data=Sm %>% filter(measured),x=~as.numeric(distance_lag), y=~as.integer(MoleSpawnOrder), symbol=I('circle'), color=~PatternSegmentLabel, marker=list(size=24)) %>%
  add_trace(data=Sm %>% filter(measured),x=~as.numeric(distance_lag), y=~as.integer(MoleSpawnOrder), mode='text', text=~MoleId) %>%
  layout(xaxis=list(title='Travel Distance'), yaxis=list(title='MoleOrder'))
  
  
#############
# Visualize Moles Animated
#############

# Select Condition and Participant for playback.
v_Condition = "Test Condition 2"
v_Participant = 0


# MoleTemplate contains template data, used to generate the plotly animation data frame.
MoleTemplate <- D %>% group_by(Participant, Condition) %>% filter(!is.na(MoleId)) %>%
  filter(Participant == v_Participant, Condition == v_Condition) %>%
  rename(x = MolePositionWorldX, y = MolePositionWorldY, size = MoleSize, id = MoleId) %>%
  select(Participant, Condition, x,y, id) %>%
  distinct(id, .keep_all=T) %>% mutate(size = NA, name="Active Mole")

# MoleTemplate contains template data, used to generate the plotly animation data frame.
MoleData <- D %>% filter(Event %in% c("Mole Hit", "Mole Expired","Mole Spawned")) %>%
  filter(Participant == v_Participant, Condition == v_Condition) %>%
  rename(x = MolePositionWorldX, y = MolePositionWorldY, size = MoleSize, id = MoleId) %>%
  select(Timestamp, Participant, Condition, Event, x,y, id) %>%
  mutate(size = ifelse(Event %in% c("Mole Spawned"), 1, 0),
         second = as.POSIXct(paste(Timestamp), format = "%Y-%m-%d %H:%M:%S"),
         name = "Active Mole") %>% select(-Timestamp)

# MoleData contains all the actual mole activity during the condition.
#MoleData = MoleData %>% filter(Participant == v_Participant, Condition == v_Condition) %>%
#  group_by(second) %>%
#  group_modify(~left_join(MoleTemplate %>% filter(Participant == v_Participant, Condition == v_Condition),.x)) %>%
#  mutate(size = ifelse(is.na(size), 0, size))

LaserTemplate <- D %>% filter(Participant == v_Participant, Condition == v_Condition) %>% 
  slice(1) %>% select(Participant, Condition) %>%
  mutate(x = NA,
         y = NA,
         size = 0,
         id = "laser",
         name = "Laser")

LaserData <- D %>% filter(PlayPeriod == "Game", Participant == v_Participant, Condition == v_Condition) %>%
  rename(x = RightControllerLaserPosWorldX, y = RightControllerLaserPosWorldY) %>%
  select(Timestamp, Participant, Condition, Event, x,y) %>%
  mutate(second = as.POSIXct(paste(Timestamp), format = "%Y-%m-%d %H:%M:%S"),
         has_hit = ifelse(Event == "Mole Hit", T, F)) %>% select(-Timestamp) %>%
  group_by(second) %>% 
  summarize(has_hit = length(has_hit[has_hit==TRUE]) > 0,
            y =  ifelse(has_hit, y[Event == "Mole Hit"], last(y)),
            x = ifelse(has_hit, x[Event == "Mole Hit"], last(x)),
            id = "laser",
            name = "Laser",
            size = 1,
            Condition = unique(Condition),
            Participant = unique(Participant))

fig %>% add_trace(data=LaserData, x= ~second, y=~x) %>%
  add_trace(data=MoleData, x=~second, y=~x)


# First we generate all seconds in the animation.
Time = D %>% filter(Participant == v_Participant, Condition == v_Condition) %>%
  summarize(time_start = as.POSIXct(paste(Timestamp[Event == "Game Started"]), format = "%Y-%m-%d %H:%M:%S"),
            time_end = as.POSIXct(paste(Timestamp[Event %in% c("Game Stopped", "Game Finished")]), format = "%Y-%m-%d %H:%M:%S"),
            total_time = difftime(Timestamp[Event %in% c("Game Stopped", "Game Finished")], 
                                  Timestamp[Event == "Game Started"], units="secs"),
            total_time = as.integer(total_time),
            second = 0)
TimeAnimate= Time %>% uncount(total_time, .remove=F, .id="second") %>% bind_rows(Time) %>% arrange(second)

TimeAnimate = TimeAnimate %>%
  mutate(second = format(second, format = "%Y-%m-%d %H:%M:%S"),
         time_start = as.POSIXct(paste(time_start), format = "%Y-%m-%d %H:%M:%S"),
         second = seconds(second) + time_start) %>%
  select(second) %>%
  distinct(second, .keep_all=T)

DataTemplate = MoleTemplate %>%
  bind_rows(LaserTemplate)

# Then we generate the MoleTemplate for each second listed in MoleAnimate.
DataAnimate = TimeAnimate %>%  group_by(second) %>%
  group_modify(~left_join(DataTemplate %>% filter(Participant == v_Participant, Condition == v_Condition),.x))

# Remove rows in the template, for which we have real data, then bind the real data.
DataAnimate = DataAnimate %>% anti_join(MoleData %>% select(second, id)) %>% bind_rows(MoleData)

# Moles should appear big in size until they need to be scaled down.
DataAnimate = DataAnimate %>% arrange(second,id) %>% group_by(id) %>%
  fill(size, .direction="down") %>%
  mutate(size = ifelse(is.na(size),0,size))

DataAnimate = DataAnimate %>% anti_join(LaserData %>% select(second, id)) %>% bind_rows(LaserData)
DataAnimate = DataAnimate %>% arrange(second, id) %>%
  mutate(second = as.numeric(difftime(second,Time$time_start), units = "secs"))
# Remove any entries which are newer/older than the designated ending timestamp
DataAnimate = DataAnimate %>% filter(second <= Time$total_time)

# Plot the animation with static Spawn Points and animated active moles.
fig %>%
  add_trace(name="Spawn Points", data=D %>% filter(PlayPeriod == "Game") %>% filter(Participant == v_Participant, Condition == v_Condition) %>% ungroup() %>% distinct(MoleId,.keep_all=T),
            x=~MolePositionWorldX, y=~MolePositionWorldY, type='scatter',
            mode='markers',symbol=I('o'),marker=list(size=32), 
            hoverinfo='text',text=~paste(MoleId,MolePositionWorldX,MolePositionWorldY)) %>%
  add_trace(data=DataAnimate %>% filter(name =="Active Mole"),
            x=~x, y=~y, frame=~second, type='scatter',mode='markers',frame=~second, marker=list(size=~size*32),
            hoverinfo='text',text=~paste(id,x,y)) %>%
  add_trace(data=DataAnimate %>% filter(name =="Laser"),
            x=~x, y=~y, frame=~second, type='scatter',mode='markers',frame=~second, marker=list(size=~size*32),
            hoverinfo='text',text=~paste(id,x,y)) %>%
  layout(xaxis = list(range=c(min(D$MolePositionWorldX,na.rm=T)-1, max(D$MolePositionWorldX,na.rm=T)+1)),
         yaxis = list(range=c(min(D$MolePositionWorldY,na.rm=T)-1, max(D$MolePositionWorldY,na.rm=T)+1))
  ) %>%
  animation_opts(
    1000, easing= 'linear', redraw = FALSE
  )

# Todo: Add pointer
add_trace(name="Laser Pointer R", data=LaserAnimate,
          x=~RightControllerLaserPosWorldX, y=~RightControllerLaserPosWorldY,
          type='scatter',mode='markers',frame=~second) %>%
  
  

#############
# Visualize Sample Rate
#############

plot_ly(D %>% filter(PlayPeriod == "Game"), color=~paste(Participant, Condition), x = ~GameTimeSpent, y = ~Hz, type = "scatter", mode="markers")

             
             
#             accInput = sum(TrialResult == "AccInput", na.rm=T),
#             posTrial = sum(TrialResult == "OverrideInput" | TrialResult == "AccInput" | TrialResult == "AugSuccess" | TrialResult == "ExplicitSham" | TrialResult == "AssistSuccess", na.rm=T),
#             assistInput = sum(TrialResult %in% c("AssistSuccess", "AugSuccess"), na.rm=T),
#             explicitSham = sum(TrialResult %in% c("ExplicitSham", "OverrideInput"), na.rm=T),
#             mitigateFail = sum(TrialResult %in% c("AssistFail","MitigateFail"), na.rm=T),
#             totalTrials2 = sum(!is.na(TrialResult), na.rm=T),
#             totalTrials = rejInput+accInput+assistInput+explicitSham+mitigateFail,
#             #fishCaught = sum(FishEvent == "FishCaught", na.rm=T),
#             #fishCaught2 = sum(Event == "GameDecision" & fishFeedback == 1, na.rm=T),
#             fishCaught = sum(TrialFeedback == "FishCaught", na.rm=T),
#             fishReel = sum(TrialFeedback == "Reel", na.rm=T),
#             fishStay = sum(TrialFeedback == "Stay", na.rm=T),
#             fishUnreel = sum(TrialFeedback == "Unreel", na.rm=T),
#             fishLost = sum(TrialFeedback == "FishLost", na.rm=T),
#             notFishCaught = sum(Event == "GameDecision" & lead(Event) != "FishEvent"),
#             reel = sum(Event == "GameDecision" & lead(Event) != "FishEvent" & !(TrialResult %in% c("RejInput", "AssistFail", "MitigateFail"))),
#             escape = sum(Event == "GameDecision" & lead(Event) != "FishEvent" & (TrialResult %in% c("RejInput"))),
#             trial_rate_accept = (accInput + assistInput) / totalTrials,
#             trial_rate_reject = rejInput / totalTrials,
#             trial_rate_assist = assistInput / totalTrials,
#             trial_rate_sham = explicitSham / totalTrials,
#             trial_rate_mitigate = mitigateFail / totalTrials,
#             trial_rate_positive = (accInput+assistInput+explicitSham) / (totalTrials-mitigateFail),
#             pam_rate = 0,
#             pam_rate = unique(ifelse(Condition == "AS", trial_rate_assist, pam_rate)),
#             pam_rate = unique(ifelse(Condition == "IO", trial_rate_sham, pam_rate)),
#             pam_rate = unique(ifelse(Condition == "MF", trial_rate_mitigate, pam_rate)),
#             time_total = sum(time_delta),
#             PercNormalized = unique(PercNormalized),
#             Gender = unique(Gender),
#             Age = unique(Age),
#             FrustNormalized = unique(FrustNormalized),
#             Order = unique(Order),
#             Hardest = unique(Hardest),
#             Easiest = unique(Easiest))

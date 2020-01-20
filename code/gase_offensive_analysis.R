##########################################################
#  
#    Line Chart: Cumulative EPA over Multiple Seasons
#    @NYJAnalytics
#    Originally Created: 10/24/2019
#
##########################################################

# Load libraries ----
library(nflscrapR)
library(dplyr)
library(na.tools)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggimage)
library(ggthemes)

# Read in team colors ----
team_colors <- read_csv("https://raw.githubusercontent.com/NYJetsAnalytics/NYJetsAnalytics/master/data_sets/teamcolorsdf.csv")

# Pull out Jets primary color and Dolphins colors, to be used in the plot ----
nyj_color <- team_colors %>%
  filter(team == "NYJ") %>%
  pull(color)

mia_color <- team_colors %>%
  filter(team == "MIA") %>%
  pull(color)

mia_color2 <- team_colors %>%
  filter(team == "MIA") %>%
  pull(color2)

mia_color3 <- team_colors %>%
  filter(team == "MIA") %>%
  pull(color3)

# Pull play-by-play data for seasons where Gase was a head coach ----
season_2019 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv")
season_2018 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
season_2017 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv")
season_2016 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2016.csv")

# Create new data frames, filter by Gase coached teams where epa is not NA ----
nyj_2019 <- season_2019 %>%
  filter(posteam=="NYJ" & !is.na(epa))

mia_2018 <- season_2018 %>%
  filter(posteam=="MIA" & !is.na(epa))

mia_2017 <- season_2017 %>%
  filter(posteam=="MIA" & !is.na(epa))

mia_2016 <- season_2016 %>%
  filter(posteam=="MIA" & !is.na(epa))

# Define new field for cumulative EPA. Index plays over the course of a season. Add field "season" ----
#do this for every season individually

nyj_2019[,"cum. epa"] <- cumsum(nyj_2019$epa)
nyj_2019$play_num <- seq.int(nrow(nyj_2019))
nyj_2019$season <- "2019"

mia_2018[,"cum. epa"] <- cumsum(mia_2018$epa)
mia_2018$play_num <- seq.int(nrow(mia_2018))
mia_2018$season <- "2018"

mia_2017[,"cum. epa"] <- cumsum(mia_2017$epa)
mia_2017$play_num <- seq.int(nrow(mia_2017))
mia_2017$season <- "2017"

mia_2016[,"cum. epa"] <- cumsum(mia_2016$epa)
mia_2016$play_num <- seq.int(nrow(mia_2016))
mia_2016$season <- "2016"

# Bind all season data frames to a new data frame (gase) ----
gase <- rbind(nyj_2019, mia_2018, mia_2017, mia_2016)

# BEN BALDWIN MUTATIONS ----

#filter by plays where play type is "no play", "pass", or "rush". remove plays where epa is NA
pbp_2019 <- gase %>%
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

#account for plays that are nullified by penalties add new variables - pass, rush, success
pbp_2019 <- pbp_2019 %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  )

#text scrape play descriptions to get player names
pbp_players <- pbp_2019 %>% 
  mutate(
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, 
                                              "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name)
  )

# Rename final data frame to gase_df ----
gase_df <- pbp_players

# Plot gase_df ----
gase_df %>%
  ggplot(aes(x=play_num, y=`cum. epa`, color=season))+
  geom_hline(yintercept=0, size=1, color="black")+
  geom_line(size=1.25)+
  scale_color_manual(values=c("2019"=nyj_color, "2018"=mia_color, "2017"=mia_color2,"2016"=mia_color3), 
                     labels=c("2019"="2019 - NYJ (7-9)", "2018"="2018 - MIA (7-9)", "2017"="2017 - MIA (6-10)","2016"="2016 - MIA (10-6)"))+
  scale_x_continuous(breaks=seq(0,1500,100), expand=c(.005,0))+
  scale_y_continuous(breaks=seq(-150,50, 25), expand = c(.05,0))+
  labs(x = "Number of Plays",
       y = "Total EPA",
       caption = "Plot by @NYJAnalytics | Data from @nflscrapR",
       title = "Head Coach Gase: Offensive Guru",
       subtitle = "2016-2019 Seasons, Cumulative EPA on Offense")+
  ggthemes::theme_fivethirtyeight()+
  theme(plot.title=element_text(color= nyj_color),  # title
        plot.caption=element_text(color = nyj_color, 
                                  face ="bold")
  )


# Save png file to your machine, diminsions optimized for twitter posting ----
ggsave("Cumulative EPA over Multiple Seasons.png", path="%local path here%",width=12, height = 7.5, dpi = 850, units= "in", device="png")

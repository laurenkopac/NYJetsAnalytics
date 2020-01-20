##########################################################
#  
#    Bar Chart: EPA/Dropback Analysis
#    @NYJAnalytics
#    Originally Created: 11/7/2019
#
##########################################################

# Load Libraries ----
library(nflscrapR)
library(dplyr)
library(na.tools)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggimage)
library(ggthemes)

# Read in logos, colors, headshots from Github ----
logos <- read_csv("https://raw.githubusercontent.com/NYJetsAnalytics/NYJetsAnalytics/master/data_sets/logosdf.csv")
team_colors <- read_csv("https://raw.githubusercontent.com/NYJetsAnalytics/NYJetsAnalytics/master/data_sets/teamcolorsdf.csv")
qb_headshots <- read_csv("https://raw.githubusercontent.com/NYJetsAnalytics/NYJetsAnalytics/master/data_sets/2019_qb_headshots.csv")

# Pull out Jets' primary color (optional, used in title color) ----
nyj_color <- team_colors %>%
  filter(team == "NYJ") %>%
  pull(color)

# Read in play-by-play data. 2019 regular season (hosted on Ryan Yurko's github) used in this chart ----
season_2019 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv")

# BEN BALDWIN MUTATIONS ----

#filter by plays where play type is "no play", "pass", or "rush". remove plays where epa is NA
pbp_2019 <- season_2019 %>%
  filter(!is.na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

#account for plays that are nullified by penalties add new variables - pass, rush, success
pbp_2019 <- pbp_2019 %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  )

#text scrape play descriptions to get player names
pbp_players <- pbp_2019 %>% 
  filter(!is.na(posteam)) %>% 
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

# Clean QB Records ----

#Gardner Minshew and Josh Allen appear as two different names, consolidate with the following
pbp_players$passer_player_name[pbp_players$passer_player_name=="G.Minshew II"] <- "G.Minshew"
pbp_players$rusher_player_name[pbp_players$rusher_player_name=="G.Minshew II"] <- "G.Minshew"

pbp_players$passer_player_name[pbp_players$passer_player_name=="Jos.Allen"] <- "J.Allen"
pbp_players$rusher_player_name[pbp_players$rusher_player_name=="Jos.Allen"] <- "J.Allen"


# Create new data frame, qbs ----
qbs <- pbp_players%>% 
  mutate(
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    rusher = rusher_player_name,
    receiver = receiver_player_name,
    play = 1
  ) %>%
  group_by(name, posteam) %>%
  summarize (
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    n_plays = sum(play),
    epa_per_play = sum(epa)/n_plays,
    success_per_play =sum(success)/n_plays
  ) %>%
  ungroup()%>%
  inner_join(team_colors,by=c("posteam"="team"))%>%
  left_join(qb_headshots, by=c("name"="passer_player_name"))%>%
  left_join(logos, by=c("posteam"="team")) %>% 
  arrange(name) %>%
  filter(n_dropbacks>=70) #minimum 70 drop backs

# Add variables for logo and headshot mapping ----
qbs <- qbs %>% 
  mutate(
    headshot_cord = ifelse(epa_per_play<0, epa_per_play-.03, epa_per_play+.03), #add headshot above bar if EPA/play is positive, else below bar
    url_cord = -.8 #add team logo below plot
  )

# Plot qbs ----
qbs %>% 
  ggplot(aes(fill=name, reorder(x=posteam, -epa_per_play), y=epa_per_play))+ #group by team, order by epa/play descending
  geom_bar(position = "dodge", stat="identity", show.legend = FALSE, width =.5)+
  geom_image(aes(image=headshot, y=qbs$headshot_cord), size = 0.05, by = "width", asp =1.8)+
  geom_image(aes(image=url, y=qbs$url_cord), size = 0.03, by = "width", asp =1.8)+
  scale_fill_manual(values = qbs$color)+
  scale_y_continuous(breaks=seq(-.75,.5,.25), limits=c(-.8,.5))+
  ggthemes::theme_fivethirtyeight()+
  theme(plot.title=element_text(color= nyj_color),  
        plot.caption=element_text(color = nyj_color, 
                                  face ="bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank()
        )+
  labs(x = "",
       y = "EPA/Dropback",
       caption = "Plot: @NYJAnalytics | Data: @nflscrapR",
       title = "Year of the Backup: EPA/Dropback",
       subtitle = "2019 Regular Season, Minimum 70 Dropbacks")


# Export high quality png ----
ggsave("Year of the Backup 2019 Bar Chart.png", path="%local path here%",width = 12, height = 7, unit="in", device="png",dpi = 850)

##############################################################
#  
#    Line Chart: Multi-Team Offensive Efficiency Analysis
#    @NYJAnalytics
#    Originally Created: 12/30/2019
#
#############################################################

#load packages
library(nflscrapR)
library(dplyr)
library(na.tools)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggimage)

#read in team logos and colors from NYJetsAnalytics Github
logos <- read_csv("https://raw.githubusercontent.com/NYJetsAnalytics/NYJetsAnalytics/master/data_sets/logosdf.csv")
team_colors <- read_csv("https://raw.githubusercontent.com/NYJetsAnalytics/NYJetsAnalytics/master/data_sets/teamcolorsdf.csv")

#pull out Jets' primary color (optional, I use it for the title color)
nyj_color <- team_colors %>%
  filter(team == "NYJ") %>%
  pull(color)

#read in play-by-play data. 2019 regular season (hosted on Ryan Yurko's github) used in this chart
#this method is faster than the fuction scrape_season_play_by_play()
season_2019 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv")

## BEGIN BEN BALDWIN MUTATIONS ##
{
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
}
## END OF BEN BALDWIN MUTATIONS ##

#create vector containing all team names
teams <- unique(pbp_players$posteam)

#define empty data frame
df <- data.frame()

#for loop to create data frame, df, containing all teams, create sum_epa for each team's cumulative EPA for the season
#yes, i in teams was intentional
for (i in teams){
   df <- rbind(df, assign(paste0(tolower(i)),
    pbp_players %>%
    filter(posteam==i)%>%
    mutate(
        sum_epa = cumsum(epa)
        ) %>% 
    select(posteam, sum_epa))
   )
}

df <- df %>% 
  group_by(posteam) %>% 
  mutate(
    play_num = seq_along(posteam)
  ) 

logos_df <- df %>%
  group_by(posteam) %>%
  summarise(
    tot_epa = last(sum_epa),
    last_play = max(play_num)
    ) %>%
  left_join(logos, by=c("posteam"="team"))

#define new plot: off_efficiency
off_efficiency <- ggplot()+
  geom_hline(yintercept=0, size=1, color="black")+
  geom_line(aes(x=df$play_num, y=df$sum_epa,color=df$posteam,alpha=ifelse(df$posteam=="NYJ"|df$posteam=="BUF"|df$posteam=="NE"|df$posteam=="MIA", 1, .8)),size=1.25, show.legend = FALSE)+
  geom_image(aes(x=logos_df$last_play, y=logos_df$tot_epa, image = logos_df$url), size = 0.025, by = "width", asp =1.8)+
  scale_color_manual(values=c("ARI"="#97233f", "ATL"="#a71930", "BAL"="#241773", "BUF"="#00338d", "CAR"="#0085ca","CHI"="#0b162a","CIN"="#000000","CLE"="#fb4f14",
                              "DAL"="#002244", "DEN"="#002244","DET"="#005a8b", "GB"="#203731","HOU"="#03202f","IND"="#002c5f", "JAX"="#000000","KC"="#e31837",
                              "LAC"="#002244","LA"="#002244","MIA"="#008e97","MIN"="#4f2683","NE"="#002244","NO"="#9f8958","NYG"="#0b2265","NYJ"="#125740","OAK"="#a5acaf",
                              "PHI"="#004953","PIT"="#000000","SD"="#002244","SF"="#aa0000","SEA"="#69be28","STL"="#002244","TB"="#d50a0a","TEN"="#4095d1","WAS"="#773141")) +
  scale_x_continuous(breaks=seq(0,1500,100), expand=c(.01,0))+
  scale_y_continuous(breaks=seq(-200,300,50), expand=c(.01,0))+
  ggthemes::theme_fivethirtyeight()+
  theme(plot.title=element_text(color= nyj_color),  
        plot.caption=element_text(color = nyj_color, 
                                  face ="bold"),
        axis.title = element_text(face = "bold"))+
  labs(x = "Number of Plays",
       y = "Cumulative EPA",
       caption = "Plot: @NYJAnalytics | Data: @nflscrapR",
       title = "Offensive Efficiency by Team: AFC East",
       subtitle = "2019 Regular Season, Cumulative EPA")

#plot off_efficiency
plot(off_efficiency)

#export high quality png file to your machine, diminsions optimized for twitter posting
ggsave("2019 Offensive Efficiency: AFC East.png", path="%local path here%", width=12, height = 7.5, dpi = 850, units= "in", device="png")

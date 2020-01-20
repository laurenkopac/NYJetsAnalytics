# Cumulative EPA Analysis

* [The Offensive Guru](#adam-gase)
* [Analysis](#analysis)
* [R Code](#r-code)
  * [Libraries](#libraries)
  * [Data Sets](#data-sets)
  * [Manipulate the Data](#manipulate-the-data)
  * [Clean Up](#clean-up)
  * [Plot](#plot)
* [Results](#results)

## Adam Gase

If there's one thing (anything) that Jets fans loved in 2019, it was putting down newly acquired head coach, Adam Gase. The fan based pulled out the pitch forks after some brutal loses, including a shut out on MNF against the Patriots. 

Despite the struggles in Miami that led to his firing and his losing inaugural season in New York, he's been described by some outside the organization as an "offensive guru." I decieded to take a closer look at his offenses.

## Analysis

For this analysis, we'll only look at teams that Adam Gase has head coached. This leaves us with four teams:

* Miami Dolphins, 2016-18
* New York Jets, 2019

For each team, we're going to measure offensive preformance using [Expected Points Added (EPA)](https://www.espn.com/nfl/story/_/id/8379024/nfl-explaining-expected-points-metric) per play over the course of the entire season. 

## R Code

Assuming you have R installed on your machine, everything you'll need to analyize Gase's head coaching tenure is available [here](https://github.com/NYJetsAnalytics/NYJetsAnalytics/blob/master/code/gase_offensive_analysis.R).

Now that you've got the code, let's plot. 

### Libraries

If you're in a new R session, you'll likely need to load the following libraries:

````R
library(nflscrapR)
library(dplyr)
library(na.tools)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggimage)
library(ggthemes)
````

### Data Sets

For this plot, we'll need to import and manipulate play-by-play data from the 2016-2019 regular seasons. 

````R
# Pull play-by-play data for seasons where Gase was a head coach ----
season_2019 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv")
season_2018 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
season_2017 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv")
season_2016 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2016.csv")
````

Additionally, to differentiate between seasons, we'll need to import and utilze the [team colors data set](https://github.com/NYJetsAnalytics/NYJetsAnalytics/blob/master/DATASETS.md#team-colors). Let's `pull()` the primary color of the Jets for 2019, and three distinct Dolphins colors for 2016-2018.

````R
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
````
### Manipulate the Data

Now that we have the play-by-play data for each season, let's `filter()` to get the data we want. For each season data frame, filter by Gase's teams. We'll also exclude plays where EPA is `NA`, since we can't measure them. 

````R
# Create new data frames, filter by Gase coached teams where epa is not NA ----
nyj_2019 <- season_2019 %>%
  filter(posteam=="NYJ" & !is.na(epa))

mia_2018 <- season_2018 %>%
  filter(posteam=="MIA" & !is.na(epa))

mia_2017 <- season_2017 %>%
  filter(posteam=="MIA" & !is.na(epa))

mia_2016 <- season_2016 %>%
  filter(posteam=="MIA" & !is.na(epa))
````
This plot will rely on fields that aren't in our data frames yet, so let's calculate them. 

Using EPA provided by nflscrapR, we can determine cumulative EPA over the course of a season with `cumsum()`. Next, index each play with `seq.int(nrow())`. Finally, add a field that simply lists the season year. This will need to be done for each season data frame. 

**Note: This isn't the most efficient way to create/manipulate multiple data frames. If I were to rewrite this code, I would likely use a For Loop, similar to the one in my [NFL Wide Cumulative EPA Analysis code](https://github.com/NYJetsAnalytics/NYJetsAnalytics/blob/master/code/season_long_cumulative_epa.R).**

````R
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
````

Combine each season into a new data frame:

````R
# Bind all season data frames to a new data frame (gase) ----
gase <- rbind(nyj_2019, mia_2018, mia_2017, mia_2016)
````

### Clean Up

Whenever I work with nflscrapR play-by-plays, I make a point to clean the data using Ben Baldwin's mutations. More details on his process can be found in his [tutorial](https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701). 

````R
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
````

Rename the final dataset to `gase_df`. 

### Plot

Using `ggplot`, let's generate a `geom_line` chart with the following parameters:
* x: `play_num`, number of plays over the course of the season
* y: `cum. epa`, cumulative EPA 
* color: `season`, color will be determined based on the season field
  * define colors using `scale_color_manual` and our color variables defined in an earlier section

````R
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
        plot.caption=element_text(color = nyj_color, #caption
                                  face ="bold")
  )
````

## Results
Here we have it! We can now see that Gase's last three seasons as a head coach left a lot to be desired in terms of consistent offensive efficiency. 

![Image of Gase Plot](https://i.imgur.com/24GvOaH.png)

For more examples of this application of EPA, and to see how the 2019 Jets faired compared to the rest of NFL using this metric (poorly), check out my NFL Wide Cumulative EPA Analysis Plot on [Twitter](https://twitter.com/NYJetsAnalytics/status/1211798960676986881). 


# Data Sets

Below are the data sets frequently used in my code.

* [Team Logos](#team-logos)
* [Team Colors](#team-colors)
* [Quarterback Headshots](#qb-headshots)

## Team Logos

This is a data set of team logos as transparent images. View the full [csv](https://github.com/NYJetsAnalytics/NYJetsAnalytics/blob/master/data_sets/logosdf.csv) online. 

Import to R:

````R
logos <- read_csv("https://raw.githubusercontent.com/NYJetsAnalytics/NYJetsAnalytics/master/data_sets/logosdf.csv")
````
Fields:
* `team`: NFL team abbrivation (i.e. NYJ - New York Jets).
* `url`: The image address of the team logo. 

## Team Colors

This data set contains hex codes for each team's color palette. All teams have at least a *primary* and *second* color. Most teams have four colors in their palette. View the full [csv](https://github.com/NYJetsAnalytics/NYJetsAnalytics/blob/master/data_sets/teamcolorsdf.csv) online. 

Import to R:

````R
team_colors <- read_csv("https://raw.githubusercontent.com/NYJetsAnalytics/NYJetsAnalytics/master/data_sets/teamcolorsdf.csv")
````
Fields:
* `team`: NFL team abbrivation (i.e. NYJ - New York Jets).
* ` color`: Primary color. **I've made "action green" the Seahawks' primary color when it is officially listed as their secondary.**
* `color2`: Secondary color.
* `color3`: Third color (if exists). 
* `color4`: Fourth color (if exists). 

## QB Headshots

This is a manually managed data set of quarterback headshots provided from ESPN. View the full [csv](https://github.com/NYJetsAnalytics/NYJetsAnalytics/blob/master/data_sets/2019_qb_headshots.csv) online. 

Import to R:

````R
qb_headshots <-read_csv("https://raw.githubusercontent.com/NYJetsAnalytics/NYJetsAnalytics/master/data_sets/2019_qb_headshots.csv")
````

Fields:
* `passer_player_name`: Quarterback name. This field follows the player naming convention set by nflscrapR data. All player names will be `"first name initial.lastname"` (i.e. 'S.Darnold' for Sam Darnold). 
* `headshot`: The image address of the headshot, taken from ESPN. 

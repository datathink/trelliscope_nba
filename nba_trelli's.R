# Old Trelliscope ---------------------------------------------------------

## Libraries & Data --------------------------------------------------------
# devtools::install_github("hafen/trelliscopejs", force=TRUE)
library(trelliscopejs)
library(tidyverse)
library(arrow)
library(gapminder)

nba_dat_logs <- read_parquet("nba_game_logs.gz.parquet") 
nba_dat_season <- read_parquet("nba_player_season_summary.gz.parquet") 

## Wrangling & Analysis ----

nba_dat_logs <- nba_dat_logs |>
  # filter(year_season %in% c(2018, 2019, 2021)) |> 
  mutate(is_pacific = case_when(
    name_team %in% c("Sacramento Kings", "LA Clippers", "Phoenix Suns", 
                     "Golden State Warriors", "Los Angeles Lakers") ~ TRUE,
    TRUE ~ FALSE)) |> 
  filter(is_pacific == TRUE)


nba_dat_season <- nba_dat_season |> 
  filter(year_season == 2019)

# mylm <- lm(fgm ~ minutes, data = nba_dat_logs)
# summary(mylm)

## Plotting ----------------------------------------------------------------
### Product 1 ---------------------------------------------------------------

plot1 <- ggplot(nba_dat_logs |> filter(year_season == 2018, name_team %in% c("Sacramento Kings", "LA Clippers", "Golden State Warriors", "Phoenix Suns", "Los Angeles Lakers")), aes(minutes, fgm, color = "pink"))+
  geom_jitter(alpha = 3/5, size = 5, show.legend = FALSE)+
  geom_smooth(linetype = "dashed", color = "purple", method = "lm", formula = y~x, se=FALSE)+
  labs(x="Minutes Played", y="FG Made", subtitle="NBA Pacific Division: 2019")+
  theme_minimal()+
  facet_trelliscope(~name_team + name_player, 
                    ncol = 4, nrow = 2, 
                    path = "product1",
                    name = "Points by Time",
                    self_contained = TRUE); tr_charm(plot1, password = "NBA")

### Product 2 ---------------------------------------------------------------

nba_dat_logs |> 
  # distinct(name_player, .keep_all = TRUE) |> 
  filter(name_team %in% c("Golden State Warriors"),
         # name_player %in% c("Nick Young", "Patrick McCaw", "David West", "Draymond Green", "Omri Casspi", "Jordan Bell", "Kevin Durant", "Kevon Looney", "Klay Thompson", "Stephen Curry", "Corey Brewer", "Kyle Kuzma", "Alex Caruso", "Brandon Ingram", "Luol Deng", "Larry Nance Jr", "Lonzo Ball", "Julius Randle", "Andrew Bogut")
         ) |> 
  arrange(name_team, desc(plusminus), name_player)|> 
  select(Player=name_player, Team=name_team, Season=year_season, plusminus) |> #view()
  ggplot(aes(plusminus, fill = plusminus, group = plusminus))+
  geom_histogram(bins=15, show.legend = F)+
  scale_x_continuous(n.breaks = 8)+ #, limits = c(-15, 15)
  scale_y_continuous(n.breaks = 8)+ #, limits = c(0, 15)
  scale_fill_gradient(low = "red", high = "green")+
  labs(x="Plus Minus")+
  # facet_wrap(~Team + Player)
  facet_trelliscope(~Team + Player, 
                    ncol = 4, nrow = 2, 
                    path = "product2",
                    name="plusminus",
                    self_contained = TRUE)

# gganimate?


### Product 3 ---------------------------------------------------------------

nba_dat_logs |> 
  # filter(name_team %in% c("Tri-Cities Blackhawks", "St. Louis Bombers", "Philadelphia 76ers", "Kansas City-Omaha Kings", "New Orleans/Oklahoma City Hornets", "Sacramento Kings")) |>
  # summarise(min(year_season), max(year_season))
  group_by(id_game, slug_season, name_team) |> 
  summarise(pts_total = sum(pts)) |> 
  ggplot(aes(pts_total))+
  geom_histogram()+
  # facet_wrap(~slug_season)
  facet_trelliscope(~slug_season + name_team, 
                    ncol = 4, nrow = 2, 
                    path = "product3",
                    name = "Distribution of Points by Season",
                    self_contained = TRUE,
                    views = view_list(
                      view_item(name = "Anderson Packers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Anderson%20Packers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Atlanta Hawks", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Atlanta%20Hawks&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Baltimore Bullets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Baltimore%20Bullets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Boston Celtics", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Boston%20Celtics&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Brooklyn Nets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Brooklyn%20Nets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Buffalo Braves", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Buffalo%20Braves&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Capital Bullets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Capital%20Bullets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Charlotte Bobcats", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Charlotte%20Bobcats&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Charlotte Hornets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Charlotte%20Hornets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Chicago Bulls", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Chicago%20Bulls&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Chicago Packers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Chicago%20Packers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Chicago Stags", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Chicago%20Stags&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Chicago Zephyrs", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Chicago%20Zephyrs&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Cincinnati Royals", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Cincinnati%20Royals&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Cleveland Cavaliers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Cleveland%20Cavaliers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Dallas Mavericks", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Dallas%20Mavericks&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Denver Nuggets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Denver%20Nuggets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Detroit Pistons", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Detroit%20Pistons&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Ft. Wayne Zollner Pistons", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Ft.%20Wayne%20Zollner%20Pistons&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Golden State Warriors", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Golden%20State%20Warriors&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Houston Rockets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Houston%20Rockets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Indiana Pacers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Indiana%20Pacers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Indianapolis Olympians", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Indianapolis%20Olympians&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Kansas City Kings", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Kansas%20City%20Kings&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Kansas City-Omaha Kings", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Kansas%20City-Omaha%20Kings&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "LA Clippers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:LA%20Clippers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Los Angeles Clippers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Los%20Angeles%20Clippers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Los Angeles Lakers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Los%20Angeles%20Lakers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Memphis Grizzlies", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Memphis%20Grizzlies&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Miami Heat", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Miami%20Heat&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Milwaukee Bucks", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Milwaukee%20Bucks&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Milwaukee Hawks", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Milwaukee%20Hawks&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Minneapolis Lakers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Minneapolis%20Lakers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Minnesota Timberwolves", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Minnesota%20Timberwolves&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "New Jersey Nets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:New%20Jersey%20Nets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "New Orleans Hornets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:New%20Orleans%20Hornets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "New Orleans Jazz", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:New%20Orleans%20Jazz&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "New Orleans Pelicans", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:New%20Orleans%20Pelicans&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "New Orleans/Oklahoma City Hornets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:New%20Orleans%2FOklahoma%20City%20Hornets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "New York Knicks", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:New%20York%20Knicks&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "New York Nets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:New%20York%20Nets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Oklahoma City Thunder", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Oklahoma%20City%20Thunder&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Orlando Magic", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Orlando%20Magic&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Philadelphia 76ers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Philadelphia%2076ers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Philadelphia Warriors", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Philadelphia%20Warriors&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Phoenix Suns", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Phoenix%20Suns&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Portland Trail Blazers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Portland%20Trail%20Blazers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Rochester Royals", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Rochester%20Royals&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Sacramento Kings", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Sacramento%20Kings&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "San Antonio Spurs", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:San%20Antonio%20Spurs&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "San Diego Clippers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:San%20Diego%20Clippers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "San Diego Rockets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:San%20Diego%20Rockets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "San Francisco Warriors", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:San%20Francisco%20Warriors&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Seattle SuperSonics", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Seattle%20SuperSonics&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Sheboygan Redskins", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Sheboygan%20Redskins&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "St. Louis Bombers", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:St.%20Louis%20Bombers&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "St. Louis Hawks", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:St.%20Louis%20Hawks&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Syracuse Nationals", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Syracuse%20Nationals&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Toronto Raptors", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Toronto%20Raptors&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Tri-Cities Blackhawks", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Tri-Cities%20Blackhawks&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Utah Jazz", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Utah%20Jazz&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Vancouver Grizzlies", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Vancouver%20Grizzlies&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Washington Bullets", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Washington%20Bullets&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Washington Capitols", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Washington%20Capitols&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Washington Wizards", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Washington%20Wizards&sidebar=1&fv=name_team&staticView="),
                      view_item(name = "Waterloo Hawks", 
                                hash = "&nrow=2&ncol=4&arr=row&pg=1&labels=slug_season,name_team&sort=slug_season;desc,name_team;desc&filter=var:name_team;type:select;val:Waterloo%20Hawks&sidebar=1&fv=name_team&staticView=")))


## nba_dat_logs Data Dictionary ---------------------------------------------------------

# "year_season": , 
# "name_team": ,
# "name_player": ,
# "slug_team": ,
# "slug_opponent": ,
# "slug_team_winner": ,
# "outcome_game": , 
# "fgm": "field goals made",
# "fga": "field goals attempted",
# "pct_fg": "percent field goals", 
# "pct_ft": "ratio of made to attempted",
# "minutes": "minutes played",
# "ftm": "free throws made"
# "fta": "free throws attempted",
# "ast": "assists made",
# "pf": "personal fouls",
# "pts": "points",
# "treb": "total rebounds",
# "oreb": "offensive rebounds", 
# "dreb": "deffensive rebounds",
# "tov": "turnovers",
# "stl": "steals",
# "blk": "blocks"


# [1] "year_season" - 
# [2] "slug_season" - 
# [3] "slug_league" - 
# [4] "type_season" - 
# [5] "date_game" - 
# [6] "id_game" - 
# [7] "number_game_team_season" - 
# [8] "name_team" - 
# [9] "id_team" - 
# [10] "is_b2b" - 
# [11] "is_b2b_first" - 
# [12] "is_b2b_second" - 
# [13] "location_game" - 
# [14] "slug_matchup" - 
# [15] "slug_team" - 
# [16] "count_days_rest_team" - 
# [17] "count_days_next_game_team" - 
# [18] "slug_opponent" - 
# [19] "slug_team_winner" - 
# [20] "slug_team_loser" - 
# [21] "outcome_game" - 
# [22] "name_player" - 
# [23] "number_game_player_season" - 
# [24] "count_days_rest_player" - 
# [25] "count_days_next_game_player" - 
# [26] "id_player" - 
# [27] "is_win" -
# [28] "fgm" - field goals made
# [29] "fga" - field goals attempted
# [30] "pct_fg" - percent of field goals made
# [31] "pct_ft" - percent of field throws made
# [32] "has_video" - 
# [33] "minutes" - minutes played
# [34] "ftm" - Free Throw Made
# [35] "fta" - Free Throws Attempted
# [36] "ast" - The number of assists (passes that lead directly to a made basket) by a player
# [37] "pf" - personal fouls
# [38] "pts" - points
# [39] "url_team_season_logo" -
# [40] "url_player_stats" -
# [41] "url_player_thumbnail" - 
# [42] "url_player_headshot" - 
# [43] "url_player_action_photo" - 
# [44] "url_player_photo" - 
# [45] "treb"– the total number of rebounds, Total Reb = Off Reb + Def Reb
# [46] "oreb"– the number of offensive rebounds
# [47] "dreb"- the number of defensive rebounds
# [48] "tov" - turnovers
# [49] "stl" – the number of steals by a defensive player or team
# [50] "blk" – the number of blocks by a defensive player or team
# [51] "fpts" - fantasy points??? 
# [52] "fg3m" - how many 3's made
# [53] "fg3a" - how many 3's attempted 
# [54] "pct_fg3" - percent 3's made
# [55] "fg2m" - how many 2's made
# [56] "fg2a" - how many 2's attempted
# [57] "pct_fg2" - percent 2's made
# [58] "plusminus" - points team scored while on the court minus point opposing team scored while on the court. Better metric for season(s) rather than single games 
# [59] "pts_ft" - total points from free throws made 
# [60] "pts_fg2m" - total points from 2 pointed field goals
# [61] "pts_fg3m" - total points from 3 pointed field goals
# [62] "has_incomplete_box_score" - 
# [63] "is_pacific" - 

# Fouled & giver free throw = 1pt each basket made
# field goals within three-point arc (including layups, dunks, within the key) = 2pts
# field goals outside three-point arc = 3pts
# paint & 3 second rule






## nba_dat_season Data Dictionary ---------------------------------------------------------

# "year_season": 
# "id_player": 
# "name_player": 
# "url_player_headshot":
# "minutes_total":
# "pts_total":
# "fg2a_total":
# "fg2m_total":
# "ftm_total":
# "fta_total":
# "fg3a_total":
# "fg3m_total":
# "ast_total":
# "pf_total":
# "stl_total":
# "blk_total":
# "tov_total":
# "oreb_total":
# "dreb_total":
# "count_id_game_distinct":
# "name_team_unique":
# "pts_total_per_minutes_total": 
# "fg2a_total_per_minutes_total": 
# "fg2m_total_per_minutes_total": 
# "ftm_total_per_minutes_total": 
# "fta_total_per_minutes_total": 
# "fg3a_total_per_minutes_total": 
# "fg3m_total_per_minutes_total": 
# "ast_total_per_minutes_total": 
# "pf_total_per_minutes_total": 
# "stl_total_per_minutes_total": 
# "blk_total_per_minutes_total": 
# "tov_total_per_minutes_total": 
# "oreb_total_per_minutes_total": 
# "dreb_total_per_minutes_total": 



# Code Graveyard 1 ----

# Tri-Cities Blackhawks
# Denver Nuggets
# Chicago Stags
# New York Knicks
# Sheboygan Redskins
# Rochester Royals
# St. Louis Bombers
# Indianapolis Olympians
# Baltimore Bullets
# Philadelphia Warriors
# Minneapolis Lakers
# Washington Capitols
# Waterloo Hawks
# Syracuse Nationals
# Ft. Wayne Zollner Pistons
# Anderson Packers
# Boston Celtics
# Milwaukee Hawks
# St. Louis Hawks
# Detroit Pistons
# Cincinnati Royals
# Los Angeles Lakers
# Chicago Packers
# Chicago Zephyrs
# San Francisco Warriors
# Philadelphia 76ers
# Chicago Bulls
# Seattle SuperSonics
# San Diego Rockets
# Atlanta Hawks
# Milwaukee Bucks
# Phoenix Suns
# Cleveland Cavaliers
# Buffalo Braves
# Portland Trail Blazers
# Houston Rockets
# Golden State Warriors
# Kansas City-Omaha Kings
# Capital Bullets
# New Orleans Jazz
# Washington Bullets
# Kansas City Kings
# Indiana Pacers
# San Antonio Spurs
# New York Nets
# New Jersey Nets
# San Diego Clippers
# Utah Jazz
# Dallas Mavericks
# Los Angeles Clippers
# Sacramento Kings
# Charlotte Hornets
# Miami Heat
# Minnesota Timberwolves
# Orlando Magic
# Toronto Raptors
# Vancouver Grizzlies
# Washington Wizards
# Memphis Grizzlies
# New Orleans Hornets
# Charlotte Bobcats
# New Orleans/Oklahoma City Hornets
# Oklahoma City Thunder
# Brooklyn Nets
# New Orleans Pelicans
# LA Clippers

# Tri-Cities%20Blackhawks 
# Denver%20Nuggets
# Chicago%20Stags
# New%20York%20Knicks
# Sheboygan%20Redskins
# Rochester%20Royals
# St.%20Louis%20Bombers
# Indianapolis%20Olympians
# Baltimore%20Bullets
# Philadelphia%20Warriors
# Minneapolis%20Lakers
# Washington%20Capitols
# Waterloo%20Hawks
# Syracuse%20Nationals
# Ft.%20Wayne%20Zollner%20Pistons
# Anderson%20Packers
# Boston%20Celtics
# Milwaukee%20Hawks
# St.%20Louis%20Hawks
# Detroit%20Pistons
# Cincinnati%20Royals
# Los%20Angeles%20Lakers
# Chicago%20Packers
# Chicago%20Zephyrs
# San%20Francisco%20Warriors
# Philadelphia%2076ers
# Chicago%20Bulls
# Seattle%20SuperSonics
# San%20Diego%20Rockets
# Atlanta%20Hawks
# Milwaukee%20Bucks
# Phoenix%20Suns
# Cleveland%20Cavaliers
# Buffalo%20Braves
# Portland%20Trail%20Blazers
# Houston%20Rockets
# Golden%20State%20Warriors
# Kansas%20City-Omaha%20Kings
# Capital%20Bullets
# New%20Orleans%20Jazz
# Washington%20Bullets
# Kansas%20City%20Kings
# Indiana%20Pacers
# San%20Antonio%20Spurs
# New%20York%20Nets
# New%20Jersey%20Nets
# San%20Diego%20Clippers
# Utah%20Jazz
# Dallas%20Mavericks
# Los%20Angeles%20Clippers
# Sacramento%20Kings
# Charlotte%20Hornets
# Miami%20Heat
# Minnesota%20Timberwolves
# Orlando%20Magic
# Toronto%20Raptors
# Vancouver%20Grizzlies
# Washington%20Wizards
# Memphis%20Grizzlies
# New%20Orleans%20Hornets
# Charlotte%20Bobcats
# New%20Orleans%2FOklahoma%20City%20Hornets
# Oklahoma%20City%20Thunder
# Brooklyn%20Nets
# New%20Orleans%20Pelicans
# LA%20Clippers)

# qplot(year, lifeExp, data = gapminder) +
#   xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
#   facet_trelliscope(~ country + continent, 
#                     nrow = 2, ncol = 7, width = 300, 
#                     path = file.path(path, "NBA"), 
#                     self_contained = TRUE)
# 
# 
# 
# nba_dat |>
#   select(year_season, name_team, name_player, pts) |>
#   filter(year_season %in% c("2017", "2018", "2019"),
#          name_team %in% c("Golden State Warriors", "Atlanta Hawks")) |> 
#   mutate(name_player = fct_reorder(name_player, pts, .fun = mean, na.rm = TRUE), 
#          name_team = factor(name_team)) |>
#   arrange(-year_season, -pts) |> 
#   ggplot(aes(x=pts, y=name_player, fill = pts))+
#   # stat_density_ridges(quantile_lines = TRUE)+
#   geom_boxplot()+
#   labs(x = "Points", y = "Player")+
#   # facet_wrap(~name_team + name_player, drop = TRUE)
#   facet_trelliscope(~ name_player, ncol = 4, nrow = 2)


# nba_dat_season |> 
#   select(contains("id")) |> head() |> view()

# combined_nba <- inner_join(nba_dat_logs, nba_dat_season, by = "id_player", keep = FALSE) |> 
#   select(-ends_with(".y")) |> 
#   rename_at(
#     vars(ends_with(".x")),
#     ~str_replace(., "\\..$","")) 
# 
# combined_nba |> view()


# nba_dat_logs |> 
#   # distinct(name_player, .keep_all = TRUE) |> 
#   filter(name_team == "Los Angeles Lakers") |> 
#   group_by(name_player, name_team, year_season) |> 
#   summarise(pm_mean = mean(plusminus)) |> 
#   # arrange(name_team, desc(plusminus), name_player)|>
#   # select(Player=name_player, Team=name_team, Season=year_season, plusminus) |> #view()
#   ggplot(aes(pm_mean, fill = pm_mean, group = pm_mean))+
#   geom_histogram(bins=15, show.legend = F)+
#   # scale_x_continuous(n.breaks = 8, limits = c(-15, 15))+
#   # scale_y_continuous(n.breaks = 8, limits = c(0, 15))+
#   scale_fill_gradient(low = "red", high = "green")+
#   labs(x="Plus Minus")+
#   # facet_wrap(~name_team + name_player)
#   facet_trelliscope(~name_team + name_player, 
#                     ncol = 4, nrow = 2, 
#                     # path = "product2",
#                     name="pm_mean", 
#                     self_contained = TRUE)


# select(all_of(cols)) |>
# cols <- c("id_player", "year_season", "name_team", "name_player", "slug_team", "slug_opponent", "slug_team_winner", "outcome_game", "fgm", "fga", "pct_fg", "pct_ft", "minutes", "ftm", "fta", "ast", "pf", "pts", "treb", "oreb", "dreb", "tov", "stl", "blk")


# New Trelliscope ---------------------------------------------------------

## Libraries -------------------------------------------------------------

# install.packages("devtools")
devtools::install_github("trelliscope/trelliscope")

library(trelliscope)
suppressPackageStartupMessages(
  library(tidyverse, warn.conflicts = FALSE) # for ggplot2, tidyr, dplyr, purrr
)


## Practices ---------------------------------------------------------------

visdf <- gapminder |>
  tidyr::nest(data = !dplyr::one_of(c("continent", "country"))) |>
  dplyr::mutate(
    mean_lifeexp = purrr::map_dbl(data, ~ mean(.x$lifeExp)),
    panel = trelliscope::map_plot(data,
                                  ~ (ggplot2::ggplot(ggplot2::aes(year, lifeExp), data = .x)) +
                                    ggplot2::geom_point())
  )

trelliscope(visdf, name = "life expectancy") |> write_display()

p <- ggplot(aes(year, lifeExp), data = gapminder) +
  geom_point() +
  facet_trelliscope(~ continent + country)

visdf <- build_panels(p)
visdf

visdf <- build_panels(p, as_plotly = TRUE)
visdf


disp <- (ggplot(aes(year, lifeExp), data = gapminder) +
           geom_point() +
           facet_trelliscope(~ continent + country)) |>
  build_panels() |>
  mutate(
    mean_lifeexp = purrr::map_dbl(data, ~ mean(.x$lifeExp)),
    min_lifeexp = purrr::map_dbl(data, ~ min(.x$lifeExp)),
    mean_gdp = purrr::map_dbl(data, ~ mean(.x$gdpPercap)),
    wiki_link = paste0("https://en.wikipedia.org/wiki/", country)
  ) |>
  trelliscope(name = "life expectancy")

disp

disp <- disp |>
  write_panels(width = 800, height = 500, format = "svg")

disp <- disp |>
  add_meta_defs(
    meta_number("mean_gdp",
                label = "Mean of annual GDP per capita (US$, inflation-adjusted)",
                digits = 2),
    meta_href("wiki_link", label = "Wikipedia country page")
  )

disp

disp <- disp |>
  add_meta_labels(
    mean_lifeexp = "Mean of annual life expectancies",
    min_lifeexp = "Lowest observed annual life expectancy"
  )

disp

disp <- disp |>
  set_default_labels(c("country", "continent", "wiki_link"))

disp <- disp |>
  set_default_layout(nrow = 3, ncol = 5)

disp <- disp |>
  set_default_sort(c("continent", "mean_lifeexp"), dir = c("asc", "desc"))

disp <- disp |>
  set_default_filters(
    filter_string("continent", values = "Africa"),
    filter_range("mean_lifeexp", max = 50)
  )

disp <- disp |>
  add_view(
    name = "Countries with high life expectancy (min >= 60)",
    filter_range("min_lifeexp", min = 60),
    state_sort("min_lifeexp", dir = "desc")
  )

disp <- disp |>
  add_inputs(
    input_text(name = "comments", label = "Comments about this panel",
               width = 100, height = 6),
    input_radio(name = "looks_correct",
                label = "Does the data look correct?", options = c("no", "yes"))
  )

write_display(disp)

disp |> as_json()


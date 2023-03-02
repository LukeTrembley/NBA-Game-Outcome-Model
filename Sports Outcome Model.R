library(rvest)
library(plyr)
library(dplyr)
library(janitor)
library(sqldf)
library(RSQLite)
library(DBI)

visitingteam = "Cleveland Cavaliers"
hometeam = "Boston Celtics"
visitinginjuries = c("Danilo Gallinari")
homeinjuries = c("Isaiah Livers", "Isaiah Stewart", "Jalen Duren","Cade Cunningham")

months = c("october", "november","december", "january", "february")
urls = list()
for (i in seq_along(months)) {
  urls[i] = paste0("https://www.basketball-reference.com/leagues/NBA_2023_games-",months[i],".html")
}
 tbl = list()
 month = 1
 x = 1
 for (x in seq_along(urls)) {
   tbl[[x]] = urls[[x]] %>%
     read_html() %>%
     html_nodes("table") %>%
     html_table()
   tbl[[x]]$month = month
   x = x+1
   month = month+1
 }
 
GameScores1 = ldply(tbl, data.frame)
GameScores2 = GameScores1[,-11]
GameScores = na.omit(GameScores2)
 
url = paste0("https://www.basketball-reference.com/leagues/NBA_2023_ratings.html")

tbl2 = url %>%
read_html() %>%
html_nodes("table") %>%
html_table()

TeamStats = ldply(tbl2, data.frame)

games = 0
h2h = data.frame(matrix(ncol = 2, nrow = 0))
count = 1
for (i in 1:nrow(GameScores)) {
  homegame <- GameScores[i,5]
  visitgame <- GameScores[i,3]
  if(homegame == hometeam && visitgame == visitingteam){ 
    h2h[count,] = GameScores[i,c(6,4)]
    count = count + 1
  }
  if(visitgame == hometeam && homegame == visitingteam){
    h2h[count,] = GameScores[i,c(4,6)]
    count = count + 1
  }
}

hometeamtotal = data.frame(matrix(ncol = 2, nrow = 0))
hometeamhome = data.frame(matrix(ncol = 2, nrow = 0))
hometeamaway = data.frame(matrix(ncol = 2, nrow = 0))
count1 = 1
count2 = 1
count3 = 1
for (i in 1:nrow(GameScores)) {
  homegame <- GameScores[i,5]
  visitgame <- GameScores[i,3]
  if(homegame == hometeam){
    hometeamtotal[count1,] = GameScores[i,c(6,4)]
    hometeamhome[count2,]= GameScores[i,c(6,4)]
    count1 = count1 +1
    count2 = count2 +1
  }
  if(visitgame == hometeam){
    hometeamtotal[count1,] = GameScores[i,c(4,6)]
    hometeamaway[count3,] = GameScores[i,c(4,6)]
    count1 = count1 +1
    count3 = count3 +1
  }
}
hometeamtotal = na.omit(hometeamtotal)
hometeamhome = na.omit(hometeamhome)
hometeamaway = na.omit(hometeamaway)

visitingteamtotal = data.frame(matrix(ncol = 2, nrow = 0))
visitingteamhome = data.frame(matrix(ncol = 2, nrow = 0))
visitingteamaway = data.frame(matrix(ncol = 2, nrow = 0))
count1 = 1
count2 = 1
count3 = 1
for (i in 1:nrow(GameScores)) {
  homegame <- GameScores[i,5]
  visitgame <- GameScores[i,3]
  if(homegame == visitingteam){
    visitingteamtotal[count1,] = GameScores[i,c(6,4)]
    visitingteamhome[count2,] = GameScores[i,c(6,4)]
    count1 = count1 +1
    count2 = count2 +1
  }
  if(visitgame == visitingteam){
    visitingteamtotal[count1,] = GameScores[i,c(4,6)]
    visitingteamaway[count3,] = GameScores[i,c(4,6)]
    count1 = count1 +1
    count3 = count3 +1
  }
}
visitingteamtotal = na.omit(visitingteamtotal)
visitingteamhome = na.omit(visitingteamhome)
visitingteamaway = na.omit(visitingteamaway)

wins = function(df){
  w = 0
  for (i in 1:nrow(df)) {
    if(df[i,1] > df[i,2]){
      w = w + 1
    }
  }
  return(w)
}
#total win loss ratio
hwl = wins(hometeamtotal)/nrow(hometeamtotal)
vwl = wins(visitingteamtotal)/nrow(visitingteamtotal)

#adjusted win loss ratio for homecourt advantage
hhwl = wins(hometeamhome)/nrow(hometeamhome)
vvwl = wins(visitingteamaway)/nrow(visitingteamaway)

#head to head win loss ratio
if(nrow(h2h)>0){
hh2h = wins(h2h)/nrow(h2h)
vh2h = 1-hh2h
}else{
  hh2h = 0
  vh2h = 0
}
#calculating win loss of last five games
last5 = function(df){
  x = data.frame(matrix(ncol = 2, nrow = 0))
  count = 1
  for (i in (nrow(df)-4):nrow(df)) {
    x[count,] = df[i,]
    count = count+1
  }
  return(x)
}

hlast5 = last5(hometeamtotal)
hlast5wl = wins(hlast5)/5
vlast5 = last5(visitingteamtotal)
vlast5wl = wins(hlast5)/5

#Calculating Team Stats
ORtg = function(x){
  o = " "
  for (i in 1:nrow(TeamStats)) {
    if(TeamStats[i,2] == x){
      o = TeamStats[i,9]
    }
  }
  return(o)
}
DRtg = function(x){
  d = " "
  for (i in 1:nrow(TeamStats)) {
    if(TeamStats[i,2] == x){
      d = TeamStats[i,10]
    }
  }
  return(d)
}

hor = as.numeric(ORtg(hometeam))
vor = as.numeric(ORtg(visitingteam))
hdr = as.numeric(DRtg(hometeam))
vdr = as.numeric(DRtg(visitingteam))

#Offensive vs defensive ratio
hratio = hor/vdr
vratio = vor/hdr

playerRaptor = sqldf('SELECT player_name, mp, raptor_offense, raptor_defense, raptor_total, war_total FROM modern_RAPTOR_by_player WHERE season = 2021 OR season = 2022')

url = paste0("https://basketball.realgm.com/nba/players")

tbl3 = url %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table()
players = ldply(tbl3, data.frame)
colnames(players)[26] = 'Team'

currentTeam = sqldf('SELECT Player, Team FROM players')
currentTeam = na.omit(currentTeam)
colnames(currentTeam)[1] = "player_name"

playerstats = merge.data.frame(playerRaptor, currentTeam, by = "player_name")

vquery = fn$identity("SELECT * FROM playerstats WHERE Team='$visitingteam'")
visitingPlayers = sqldf(vquery)
visitingPlayers_new <- visitingPlayers[!(visitingPlayers$player_name %in% visitinginjuries), ]

hquery = fn$identity("SELECT * FROM playerstats WHERE Team='$hometeam'")
homePlayers = sqldf(hquery)
homePlayers_new <- homePlayers[!(homePlayers$player_name %in% homeinjuries), ]

vroffense = pnorm(sum(visitingPlayers_new$raptor_offense)/nrow(visitingPlayers_new), mean = 0, sd = 1)
vrdefense = pnorm(sum(visitingPlayers_new$raptor_defense)/nrow(visitingPlayers_new), mean = 0, sd = 1)
vrtotal = pnorm(sum(visitingPlayers_new$raptor_total)/nrow(visitingPlayers_new), mean = 0, sd = 1)
vwartotal = pnorm(sum(visitingPlayers_new$war_total)/nrow(visitingPlayers_new), mean = 0, sd = 2)

hroffense = pnorm(sum(homePlayers_new$raptor_offense)/nrow(homePlayers_new), mean = 0, sd = 1)
hrdefense = pnorm(sum(homePlayers_new$raptor_defense)/nrow(homePlayers_new), mean = 0, sd = 1)
hrtotal = pnorm(sum(homePlayers_new$raptor_total)/nrow(homePlayers_new), mean = 0, sd = 1)
hwartotal = pnorm(sum(homePlayers_new$war_total)/nrow(homePlayers_new), mean = 0, sd = 2)


homescore = hratio*((hwl+hhwl+0.5*(hlast5wl)+1.25*(hh2h))/4 + hrtotal)*(hwartotal/vwartotal)
visitingscore = vratio*((vwl+vvwl+0.5*(vlast5wl)+1.25*(vh2h))/4 + vrtotal)*(hwartotal/vwartotal)

if(homescore == visitingscore){
  print("Both teams have an equal chance of winning")
}
if(homescore > visitingscore){
  prob = homescore/(homescore+visitingscore)*100
  percentage = round(prob, 2)
  outcome = paste(hometeam," is projected to beat ", visitingteam," with a probability of ",percentage,"%")
  print(outcome)
}
if(homescore < visitingscore){
  prob = visitingscore/(homescore+visitingscore)*100
  percentage = round(prob, 2)
  outcome = paste(visitingteam," is projected to beat ", hometeam," with a probability of ",percentage,"%")
  print(outcome)
}

library(tm)

three <- read.csv("2013NCAAResults.csv")
three <- three[,c(1,2,4,3,5,6)]
names(three)[3] <- 'teamScore'
names(three)[5] <- 'opponentScore'
names(three)[6] <- 'result'
four <- read.csv("2014NCAAResults.csv")
names(four)[3] <- 'teamScore'
names(four)[5] <- 'opponentScore'
names(four)[6] <- 'result'

teams3 <- aggregate(teamScore ~ Team, data=three, FUN=length)
teams4 <- aggregate(teamScore ~ Team, data=four, FUN=length)
teams <- rbind(teams3, teams4)
teams <- aggregate(teamScore ~ Team, data=teams, FUN=sum)
teams <- teams[order(-teams$teamScore),]
teams[(teams$teamScore) < 50, 'elo'] <- 400
teams[is.na(teams$elo), 'elo'] <- 1000
teams$Team <- as.character(teams$Team)
teams$index <- 1:nrow(teams)

x <- 1
while(x <= nrow(three)){
team1 <- as.character(three$Team[x])
team2 <- as.character(three$Opponent[x])
t1 <- subset(teams, Team == team1)
t1elo <- t1$elo[1]
t1index <- t1$index[1]
t2elo <- subset(teams, Team == team2)
t2elo <- t2elo$elo[1]
result <- as.character(three$result[x])
if(result == 'Win'){
  score <- 1
} else {
  score <- 0
}
t1new <- t1elo + 150*(score - (1/(1+(10^(((t2elo-t1elo)/1000))))))

teams$elo[t1index] <- t1new

x <- x + 1
}

x <- 1
while(x <= nrow(four)){
  team1 <- as.character(four$Team[x])
  team2 <- as.character(four$Opponent[x])
  t1 <- subset(teams, Team == team1)
  t1elo <- t1$elo[1]
  t1index <- t1$index[1]
  t2elo <- subset(teams, Team == team2)
  t2elo <- t2elo$elo[1]
  result <- as.character(four$result[x])
  if(result == 'Win'){
    score <- 1
  } else {
    score <- 0
  }
  t1new <- t1elo + 150*(score - (1/(1+(10^(((t2elo-t1elo)/1000))))))
  
  teams$elo[t1index] <- t1new
  
  x <- x + 1
}

teams <- teams[order(-teams$elo),]

write.csv(teams, "teamRankings.csv")

matchup <- function(team1, team2){
  t1 <- subset(teams, Team == team1)
  t1elo <- t1$elo[1]
  t2elo <- subset(teams, Team == team2)
  t2elo <- t2elo$elo[1]
 t1pct <- round(100*(1/(1+(10^(((t2elo-t1elo)/1000))))))
 result <- round(mean(sample(100,3,replace=TRUE)))
 if(result <= t1pct){
   winner <- team1
 } else {
   winner <- team2
 }
 return(c(winner, t1pct, team1))
}


# teams <- read.csv("schools.csv")
# teams <- teams[,c(2,3)]
# names(teams)[1] <- 'school'
# names(teams)[2] <- 'conference'
# teams <- subset(teams, school != 'School')
# teams <- subset(teams, school != '')
# 
# 
# teams$school <- gsub("State", "St", teams$school)
# teams$school <- gsub("North", "N", teams$school)
# teams$school <- gsub("South", "S", teams$school)
# teams$school <- gsub("East", "E", teams$school)
# teams$school <- gsub("West", "W", teams$school)
# teams$school <- gsub("N Carolina-", "UNC ", teams$school)
# teams$school <- gsub("California-", "UC ", teams$school)
# teams$school <- gsub("Tennessee-", "TN ", teams$school)
# teams$school <- gsub("Saint", "St", teams$school)
# teams$school <- gsub("International", "Intl", teams$school)
# teams$school <- gsub("Nevada-Las Vegas", "UNLV", teams$school)
# teams$school <- gsub("Sern California", "USC", teams$school)
# teams$school <- gsub("Texas Christian", "TCU", teams$school)
# teams$school <- gsub("Pennsylvania", "Penn", teams$school)
# teams$school <- gsub("Sern Mississippi", "Sern Miss", teams$school)
# teams$school <- gsub("N Carolina St", "NC State", teams$school)
# teams$school <- gsub("Bowling Green St", "Bowling Green", teams$school)
# 
# 
# 
# 
# three$Team <- gsub("South", "S", three$Team)
# three$Team <- gsub("North", "N", three$Team)
# three$Team <- gsub("East", "E", three$Team)
# three$Team <- gsub("West", "W", three$Team)
# three$Team <- gsub("WI ", "", three$Team)
# three$Team <- gsub("W ", "Wern ", three$Team)
# three$Team <- gsub("E ", "Eern ", three$Team)
# three$Team <- gsub("NY", "(NY)", three$Team)
# three$Team <- gsub("FL", "(FL)", three$Team)
# three$Team <- gsub("OH", "(OH)", three$Team)
# three[(three$Team) == 'Wern Virginia', 'Team'] <- 'W Virginia'
# 
# 
# 
# matches <- merge(three, teams, by.x='Team', by.y='school', all.x=TRUE)
# aggregate(teamScore ~ Team, data=subset(matches, is.na(conference)), FUN=length)



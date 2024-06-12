library(DT)
source("C:/Users/bcruey/OneDrive - HDR, Inc/R/Functions/Useful Functions.r")
 

Leaderboard_Main <- data.frame()
Leaderboard_All <- data.frame()
source("C:/Users/bryce/OneDrive/Documents/_Poker/JHPL/R/scripts/Useful Functions.r")
Player_Summary <-Player_Summary_Build(Events_Summary, JHPL_Accounts)
Tee_Fee<- Return_TFee_Total(Player_Summary$TFee_Summary)

#Build All Data Summary



Leaderboard_All <- Player_Summary$JHPL_LeaderBoard %>%
  filter(!is.na(ROI))%>%
  arrange(desc(Total_Points))

df<- Leaderboard_All %>% select(-c(7,8,9,10,12,13))

datatable(Leaderboard_All,colnames = c('Name', 'Player', 'Games Played', 'Cashes', 'Bubble','KOs','Paid','Bounties Won','Tournament Winnings','Points','ITM%','ROI%'),options = list(pageLength = 20))
datatable(df,colnames = c('Name', 'Player', 'Games Played', 'Cashes', 'Bubble','KOs','Points'),options = list(pageLength = 20))

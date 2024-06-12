# Function to return a value from a column based on indexing another column
#Author - Bryce Cruey, PE, CFM
#HDR
#
#exe:
#df = Some Dataframe with Columns A,B,C 
#index_value = a value you want to use to index from one of the columns
#index_column = the column that contains the Value
#target_column = The column with the value you want information from
#
#answer <- get_value_by_index(df,index_column,index_value,target_column)
#-------------------------------------------------------------------------------------

get_value_by_index <- function(dataframe, index_column, index_value, target_column) {
  # Find the row index where the index_column matches the index_value
  row_index <- which(dataframe[[index_column]] == index_value)
  
  # Return the value from the target column at the identified row
  return(dataframe[[target_column]][row_index])
}


extract_and_sum_parentheses <- function(text) {
  matches <- gregexpr("\\(([^)]+)\\)", text)
  total <- 0
  for (match in regmatches(text, matches)[[1]]) {
    numbers <- unlist(strsplit(gsub("[()]", "", match), "\\+"))
    numbers <- as.numeric(numbers)
    total <- total + sum(numbers, na.rm = TRUE)
  }
  return(total)
}

PointFunction <- function(Nent,Place,buyin,rebuy,tfee){
  Points <- 2*sqrt(Nent/Place)*(1+log10(tfee/Nent+0.25))^2/(log10(buyin+rebuy+0.25))
  return(Points)
}

Build_Summary_Table <- function(dflist,Aname, Pname){
All_Games_long <- dflist$Points_Summary %>%
  pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
All_Cashes_long <- dflist$Prize_Summary %>%
  pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
All_Buyin_long <- dflist$Buyin_Summary %>%
  pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
All_TFee_long <- dflist$TFee_Summary %>%
  pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
All_Rebuy_long <- dflist$Rebuy_Summary %>%
  pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
All_Bubble_long <- dflist$Bubble_Summary %>%
  pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
All_Points_long <- dflist$Points_Summary %>%
  pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
All_KO_long <- dflist$KO_Summary %>%
  pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
All_Prize_long <- dflist$Prize_Summary %>%
  pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
All_Bounty_long <- dflist$Bounty_Summary %>%
  pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")

All_KO_Summary<-All_KO_long %>%
  group_by(Player) %>%
  summarize(Bounty = sum(Score, na.rm = TRUE), .groups = 'drop')

All_Points_Summary<-All_Points_long %>%
  group_by(Player) %>%
  summarize(Bounty = sum(Score, na.rm = TRUE), .groups = 'drop')  

All_Bounty_Summary<-All_Bounty_long %>%
  group_by(Player) %>%
  summarize(Bounty = sum(Score, na.rm = TRUE), .groups = 'drop')

All_Prize_Summary<-All_Prize_long %>%
  group_by(Player) %>%
  summarize(Winnings = sum(Score, na.rm = TRUE), .groups = 'drop')
All_Paid_long <- dflist$TPaid_Summary %>%
  pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
All_Paid_Summary<-All_Paid_long %>%
  group_by(Player) %>%
  summarize(Paid = sum(Score, na.rm = TRUE), .groups = 'drop')

df_summary<-data.frame(Name=Aname,Player=Pname)
#return(Pname)
for (j in 1:length(Pname)){
  for (i in 1:length(Pname)){
    #return(c(j,i))
    #ifelse(df_summary$Player[j]==All_Prize_Summary$Player[i],TotalPrize<-All_Prize_Summary$Winnings[i],NA)
    #ifelse(df_summary$Player[j]==All_Paid_Summary$Player[i],TotalPaid==All_Paid_Summary$Paid[i],NA)
    #ifelse(df_summary$Player[j]==All_Bounty_Summary$Player[i],TotalBounty==All_Bounty_Summary$Bounty[i],NA)
    #ifelse(df_summary$Player[j]==All_KO_Summary$Player[i],TotalKO==All_KO_Summary$Bounty[i],NA)
    #ifelse(df_summary$Player[j]==All_Points_Summary$Player[i],TotalPoints==All_Points_Summary$Bounty[i],NA)
    #return(All_Prize_Summary$Winnings)
    if(df_summary$Player[j]==All_Prize_Summary$Player[i]){
      TotalPrize<-All_Prize_Summary$Winnings[i]
      TotalPaid <-  All_Paid_Summary$Paid[i]
      TotalBounty<- All_Bounty_Summary$Bounty[i]
      TotalKO <- All_KO_Summary$Bounty[i]
      TotalPoints <- All_Points_Summary$Bounty[i]
    }
  }
  #return(TotalPrize)
  df_summary$GamesPlayed[j]<- sum(!is.na(All_Games_long$Score) & All_Games_long$Player==df_summary$Player[j]) 
  df_summary$Cashes[j] <- sum(!is.na(All_Cashes_long$Score) & All_Cashes_long$Score != 0 & All_Cashes_long$Player==df_summary$Player[j])
  #
  df_summary$Bubble[j] <- sum(!is.na(All_Bubble_long$Score) & All_Bubble_long$Player==df_summary$Player[j])
  df_summary$KOs[j] <- TotalKO
  
  df_summary$Paid[j] <- TotalPaid
  df_summary$Bounties[j] <- TotalBounty
  
  df_summary$Winnings[j] <- TotalPrize
  df_summary$Points[j] <- round(TotalPoints,2)
  
  
  df_summary$ITM[j] <- round(df_summary$Cashes[j]/df_summary$GamesPlayed[j]*100,2)
  df_summary$ROI[j] <- round((TotalPrize+TotalBounty-TotalPaid)/TotalPaid*100,2)
} 

#df_summary_sorted <- df_summary %>%
#  filter(!is.na(ROI))%>%
#  arrange(desc(Points))

return(df_summary)

}

Return_TFee_Total <- function(df){
  df_long <- df %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "TFee")
  x <- sum(df_long$TFee,na.rm = TRUE)
  return(x)
}

Events_Summary_Build<-function(tf,ttyp){
  
  lines <- readLines(tf)
  general_info <- list()
  player_info <- list()
  
  
  
  # Loop through each line and extract information
  for (line in lines) {
    # Split the line into key and value using the '=' delimiter
    key_value <- strsplit(line, "=")[[1]]
    
    # Check if the split result is valid (has both key and value)
    if (length(key_value) == 2) {
      key <- key_value[1]
      value <- key_value[2]
      
      if(key=="Entrants"){
        Entries<-as.numeric(value)
      }
      # Check if the key starts with "Place" to identify player info
      if (startsWith(key, "Place")) {
        # Extract player information
        Player_Place<-as.numeric(gsub(".*?([0-9]+).*", "\\1", key_value[1]))
        player_details <- strsplit(value, " ")[[1]]
        player_name <- gsub("\\(.*\\)", "", player_details[1])
        
        #winnings <- as.numeric(gsub("[^0-9]", "", player_details[2]))
        name_numbers <- sub(".*=", "", line)
        name <- sub("\\s*\\(.*", "", name_numbers)
        names <- c(names, name)     
        
        winnings <- extract_and_sum_parentheses(name_numbers)
        
        
        paid <- as.numeric(gsub("[^0-9]", "", gsub("Paid:", "", player_details[3])))
        if (ttyp!="Freezeout"){
          rebuys <- as.numeric(gsub("[^0-9]", "", gsub("Rebuys:", "", player_details[4])))
          ko <- gsub("KO:", "", player_details[5])
          pbounty<-NA
          if (rebuys==1){
            prake=20
            buyinf=40
            rebuyf=40
            feein<-rebuyf+buyinf
          }
          else{
            prake=10
            buyinf=40
            rebuyf=0
            feein<-rebuyf+buyinf
          }
        }
        else {
          rebuys <- NA
          prake<-5
          pbounty<-10
          buyinf<-40
          feein<-buyinf+pbounty
          ko <- gsub("KO:", "", player_details[4])
        }
        
        
        
        player_info[[key]] <- data.frame(
          Place = Player_Place,
          Player = player_name,
          Winnings = winnings,
          Paid = paid,
          Rebuys = rebuys,
          KO = ko,
          TFee = prake,
          Buyin = buyinf,
          Rebuy = rebuyf,
          Bounty = pbounty,
          InPlay = feein,
          stringsAsFactors = FALSE
        )
      } else {
        # Add the key-value pair to the general info list
        general_info[[key]] <- value
      }
    }
  }
  
  # Convert the general info list to a dataframe
  general_df <- as.data.frame(general_info, stringsAsFactors = FALSE)
  
  # Combine all player info dataframes into a single dataframe
  player_df <- do.call(rbind, player_info)
  
  tbuyin<-sum(player_df$InPlay)
  
  for (i in 1:Entries){
    tpoints <- PointFunction(Entries,player_df$Place[i],player_df$Buyin[i],player_df$Rebuy[i],tbuyin)
    
    player_df$Points[i]<-tpoints
  }
  
  
  
  flist<-list("General information" = general_info, "Player Information" = player_df)
  #If you need to delete an entry, type:: Event_Summary${Entry}<-NULL
  
  return(flist)
}


Player_Summary_Build<-function(flist,adf){
  library(dplyr)
  library(scales)
  library(tidyr)
  
  fplist<-list()
  start_year<-2020
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  Tnames<-names(flist)
  namesP<-adf$Name
  playerP<-adf$Pm.Name
  Member_Count<-length(adf$Name)
  
  #Build Individual Player Information Lists
  for (i in namesP){
    fplist$Individual[[i]] <- list(ScreenNAme=adf$Pm.Name[i],Email=adf$Email[i],Phone=adf$Phone.No[i])
    
  }
  
  #Season Stats
  for (i in namesP){
    for (j in start_year:current_year){
      newList<-paste("Y",j,"Season",sep="")
      fplist$Individual[[i]][[newList]] <- list()
      
    }
  }
  
  #Build the summary dataframes from scratch
  fplist$Points_Summary<-data.frame(Name=namesP,Player=playerP)
  fplist$Bubble_Summary<-data.frame(Name=namesP,Player=playerP)
  fplist$KO_Summary<-data.frame(Name=namesP,Player=playerP)
  fplist$Prize_Summary<-data.frame(Name=namesP,Player=playerP)
  fplist$NoCashes_Summary<-data.frame(Name=namesP,Player=playerP)
  fplist$GamesPlayed_Summary<-data.frame(Name=namesP,Player=playerP)
  fplist$MainE_LeaderBoard<-data.frame(Name=namesP,Player=playerP)
  fplist$JHPL_LeaderBoard<-data.frame(Name=namesP,Player=playerP)
  fplist$Buyin_Summary<-data.frame(Name=namesP,Player=playerP)
  fplist$Rebuy_Summary<-data.frame(Name=namesP,Player=playerP)
  fplist$TFee_Summary<-data.frame(Name=namesP,Player=playerP)
  fplist$TPaid_Summary<-data.frame(Name=namesP,Player=playerP)
  fplist$Bounty_Summary<-data.frame(Name=namesP,Player=playerP)
  
  #Build Points Summary table from all data
  for (i in Tnames){
    fplist$Points_Summary[[i]]<- merge(flist[[i]]$`Player Information`[c("Player","Points")],fplist$Points_Summary,by=c("Player"),all=TRUE)$Points
  }
  add_scores <- function(df) {
    library(dplyr)
    df %>%
      rowwise() %>%
      mutate(
        total_score = sum(c_across(starts_with(c('Main','Deep'))), na.rm = TRUE), # Sum all event scores
        top_five_scores = sum(sort(c_across(starts_with('Main')), decreasing = TRUE)[1:5], na.rm = TRUE) # Sum top five scores
      ) %>%
      ungroup()
  }
  
  fplist$Points_Summary <- add_scores(fplist$Points_Summary)
  
  #build number of KOs summary dataframe
  for (i in Tnames){
    for (j in 1:length(playerP)){
      fplist$KO_Summary[[i]][j]<-sum(flist[[i]]$`Player Information`$KO==playerP[j])
    }
  }
  
  #create bounty summary dataframe
  
  for (i in Tnames){
    
    for (j in 1:length(playerP)){
      if(as.numeric(flist[[i]]$`General information`$Bounty)>0){
        fplist$Bounty_Summary[[i]][j]<-fplist$KO_Summary[[i]][j]*10
      }
      else{
        fplist$Bounty_Summary[[i]][j]<-0
      }
    }
  }
  
  #Build total prize money dataframe
  
  for (i in Tnames){
    fplist$Prize_Summary[[i]]<- merge(flist[[i]]$`Player Information`[c("Player","Winnings")],fplist$Prize_Summary,by=c("Player"),all=TRUE)$Winnings
    for (j in 1:length(playerP)){
      if(as.numeric(flist[[i]]$`General information`$Bounty)>0){
        fplist$Prize_Summary[[i]][j]<-fplist$Prize_Summary[[i]][j]-fplist$Bounty_Summary[[i]][j]
      }
    }
  }
  
  
  #Build number of cashes summary dataframe
  df_scores_long <- fplist$Prize_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  
  for (j in 1:length(playerP)){
    
    fplist$NoCashes_Summary$Total[j] <- sum(!is.na(df_scores_long$Score) & df_scores_long$Score != 0 & df_scores_long$Player==fplist$NoCashes_Summary$Player[j])
  }
  
  
  
  
  
  
  
  #for (i in Tnames){
  
  #  for (j in 1:length(playerP)){
  #    if(as.numeric(flist[[i]]$`General information`$Bounty)>0){
  #      fplist$Bounty_Summary[[i]][j]<-fplist$KO_Summary[[i]][j]*10
  #      fplist$Prize_Summary[[i]][j]<-fplist$Prize_Summary[[i]][j]-fplist$Bounty_Summary[[i]][j]
  #    }
  #    else{
  #      fplist$Bounty_Summary[[i]][j]<-0
  #    }
  #  }
  #}
  
  
  #Build bubble boy summary df
  
  for (i in Tnames){
    
    Bubble_Boy <- get_value_by_index(flist[[i]]$`Player Information`,"Place", 4, "Player")
    t=0
    for (j in playerP){
      
      t=t+1
      
      if(j==Bubble_Boy){
        
        fplist$Bubble_Summary[[i]][t] <- 1
      }
      else{
        fplist$Bubble_Summary[[i]][t] <- NA
      }
      
    }  
    
  }
  
  #Build buyin summary df
  
  for (i in Tnames){
    if(as.numeric(flist[[i]]$`General information`$Bounty)>0){
      df_long <- flist[[i]]$`Player Information` %>%
        pivot_longer(cols = starts_with(c("Buyin","Bounty")), names_to = "Type", values_to = "Score")
      summary_df <- df_long %>%
        group_by(Player) %>%
        summarize(Buyin = sum(Score, na.rm = TRUE), .groups = 'drop')
      fplist$Buyin_Summary[[i]]<-merge(summary_df[c("Player","Buyin")],fplist$Buyin_Summary,by=c("Player"),all=TRUE)$Buyin
    }
    else{
      fplist$Buyin_Summary[[i]]<- merge(flist[[i]]$`Player Information`[c("Player","Buyin")],fplist$Buyin_Summary,by=c("Player"),all=TRUE)$Buyin
    }
  }
  
  #Build total tournament fee summary
  for (i in Tnames){
    fplist$TFee_Summary[[i]]<- merge(flist[[i]]$`Player Information`[c("Player","TFee")],fplist$TFee_Summary,by=c("Player"),all=TRUE)$TFee
  }
  
  #Build total rebuy summary
  for (i in Tnames){
    fplist$Rebuy_Summary[[i]]<- merge(flist[[i]]$`Player Information`[c("Player","Rebuy")],fplist$Rebuy_Summary,by=c("Player"),all=TRUE)$Rebuy
  }
  
  #Build total ammount paid summary
  for (i in Tnames){
    for (j in 1:length(playerP)){
      fplist$TPaid_Summary[[i]][j]<-fplist$Rebuy_Summary[[i]][j]+fplist$TFee_Summary[[i]][j]+fplist$Buyin_Summary[[i]][j]
    }
    
  }
  
  
  #j=0
  #for (i in namesP){
  #  j=j+1
  #  fplist[[i]]$`Y 2024 Season`$Tournaments_Played<-fplist$JHPL_LeaderBoard$GamesPlayed[j]
  #  fplist[[i]]$`Y 2024 Season`$Cashes<-fplist$JHPL_LeaderBoard$Cashes[j]
  #  fplist[[i]]$`Y 2024 Season`$Bubble_Finishes<-fplist$JHPL_LeaderBoard$Bubble[j]
  #  fplist[[i]]$`Y 2024 Season`$Knockouts<-fplist$JHPL_LeaderBoard$KOs[j]
  #  fplist[[i]]$`Y 2024 Season`$Ammount_Paid<-dollar(fplist$JHPL_LeaderBoard$Paid[j])
  #  fplist[[i]]$`Y 2024 Season`$Bounties_Won<-dollar(fplist$JHPL_LeaderBoard$Bounties[j])
  #  fplist[[i]]$`Y 2024 Season`$Tournament_Winnings<-dollar(fplist$JHPL_LeaderBoard$Winnings[j])
  #}
  fplist$JHPL_LeaderBoard<-NULL
  fplist$JHPL_LeaderBoard<-data.frame(Name=namesP,Player=playerP)
  fplist[["JHPL_LeaderBoard"]]
  #Current TFee Pool
  #df_summary<-data.frame(Name=namesP,Player=playerP)
  #return(Pname)
  All_Games_long <- fplist$Points_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  All_Cashes_long <- fplist$Prize_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  All_Buyin_long <- fplist$Buyin_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  All_TFee_long <- fplist$TFee_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  All_Rebuy_long <- fplist$Rebuy_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  All_Bubble_long <- fplist$Bubble_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  All_Points_long <- fplist$Points_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  All_KO_long <- fplist$KO_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  All_Prize_long <- fplist$Prize_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  All_Bounty_long <- fplist$Bounty_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  
  All_KO_Summary<-All_KO_long %>%
    group_by(Player) %>%
    summarize(Bounty = sum(Score, na.rm = TRUE), .groups = 'drop')
  
  All_Points_Summary<-All_Points_long %>%
    group_by(Player) %>%
    summarize(Bounty = sum(Score, na.rm = TRUE), .groups = 'drop')  
  
  All_Bounty_Summary<-All_Bounty_long %>%
    group_by(Player) %>%
    summarize(Bounty = sum(Score, na.rm = TRUE), .groups = 'drop')
  
  All_Prize_Summary<-All_Prize_long %>%
    group_by(Player) %>%
    summarize(Winnings = sum(Score, na.rm = TRUE), .groups = 'drop')
  All_Paid_long <- fplist$TPaid_Summary %>%
    pivot_longer(cols = starts_with(c("Main","Deep")), names_to = "Game", values_to = "Score")
  All_Paid_Summary<-All_Paid_long %>%
    group_by(Player) %>%
    summarize(Paid = sum(Score, na.rm = TRUE), .groups = 'drop')
  
  
  for (j in 1:length(playerP)){
    
    TopFive <- fplist$Points_Summary$top_five_scores[j]
    TotalPoints <- fplist$Points_Summary$total_score[j]
    
    for (i in 1:length(playerP)){
      #return(c(j,i))
      #ifelse(df_summary$Player[j]==All_Prize_Summary$Player[i],TotalPrize<-All_Prize_Summary$Winnings[i],NA)
      #ifelse(df_summary$Player[j]==All_Paid_Summary$Player[i],TotalPaid==All_Paid_Summary$Paid[i],NA)
      #ifelse(df_summary$Player[j]==All_Bounty_Summary$Player[i],TotalBounty==All_Bounty_Summary$Bounty[i],NA)
      #ifelse(df_summary$Player[j]==All_KO_Summary$Player[i],TotalKO==All_KO_Summary$Bounty[i],NA)
      #ifelse(df_summary$Player[j]==All_Points_Summary$Player[i],TotalPoints==All_Points_Summary$Bounty[i],NA)
      #return(All_Prize_Summary$Winnings)
      
      
      
      if(fplist$JHPL_LeaderBoard$Player[j]==All_Prize_Summary$Player[i]){
        TotalPrize<-All_Prize_Summary$Winnings[i]
        TotalPaid <-  All_Paid_Summary$Paid[i]
        TotalBounty<- All_Bounty_Summary$Bounty[i]
        TotalKO <- All_KO_Summary$Bounty[i]
        #TotalPoints <- All_Points_Summary$Bounty[i]
        
      }
    }
    #return(TotalPrize)
    fplist$JHPL_LeaderBoard$GamesPlayed[j]<- sum(!is.na(All_Games_long$Score) & All_Games_long$Player==fplist$JHPL_LeaderBoard$Player[j]) 
    fplist$JHPL_LeaderBoard$Cashes[j] <- sum(!is.na(All_Cashes_long$Score) & All_Cashes_long$Score != 0 & All_Cashes_long$Player==fplist$JHPL_LeaderBoard$Player[j])
    #
    fplist$JHPL_LeaderBoard$Bubble[j] <- sum(!is.na(All_Bubble_long$Score) & All_Bubble_long$Player==fplist$JHPL_LeaderBoard$Player[j])
    fplist$JHPL_LeaderBoard$KOs[j] <- TotalKO
    
    fplist$JHPL_LeaderBoard$Paid[j] <- TotalPaid
    fplist$JHPL_LeaderBoard$Bounties[j] <- TotalBounty
    
    fplist$JHPL_LeaderBoard$Winnings[j] <- TotalPrize
    fplist$JHPL_LeaderBoard$Top5_Points[j] <- round(TopFive,2)
    fplist$JHPL_LeaderBoard$Total_Points[j] <- round(TotalPoints,2)
    
    
    fplist$JHPL_LeaderBoard$ITM[j] <- round(fplist$JHPL_LeaderBoard$Cashes[j]/fplist$JHPL_LeaderBoard$GamesPlayed[j],4)
    fplist$JHPL_LeaderBoard$ROI[j] <- round((TotalPrize+TotalBounty-TotalPaid)/TotalPaid,4)
  } 
  
  return(fplist)
  
}



add_top_five_scores_sum <- function(df) {
  library(dplyr)
  # Calculate the sum of the top five scores for each player
  top_five_sums <- df %>%
    group_by(Player) %>%
    arrange(Player, desc(Score)) %>%
    slice_head(n = 5) %>%
    summarize(top_five_sum = sum(Score))%>%
    ungroup()
  
  # Merge the sums back into the original dataframe
  df <- df %>%
    left_join(top_five_sums, by = "Player")
  
  return(df)
}

add_scores <- function(df) {
  library(dplyr)
  df %>%
    rowwise() %>%
    mutate(
      total_score = sum(c_across(starts_with(c('Main','Deep'))), na.rm = TRUE), # Sum all event scores
      top_five_scores = sum(sort(c_across(starts_with('Main')), decreasing = TRUE)[1:5], na.rm = TRUE) # Sum top five scores
    ) %>%
    ungroup()
}
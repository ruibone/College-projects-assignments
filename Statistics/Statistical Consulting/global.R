rm(list=ls())


Salary <- read.csv("NBA_Salary.csv")
Pitch <- read.csv("Pitch.csv")
All_data <- read.csv("NBA_all.csv")
colnames(All_data) <- c("Team","Salary","Year","FG","FG%","3P","3P%","2P","2P%","FT","FT%","ORB","DRB","AST","STL","BLK","TOV","PF","PTS","Age","PW","PL","SOS","SRS","ORtg","DRtg","NRtg","Pace","TS%","W","Conference")
Feature_importance <- read.csv("Feature_importance.csv")
correlation <- read.csv("correlation.csv",row.names = "X")
colnames(correlation) <- rownames(correlation)
MAE <- read.csv("MAE.csv")
index <- c("Year","FG","FG%","3P","3P%","2P","2P%","FT","FT%","ORB","DRB","AST","STL","BLK","TOV","PF","PTS","PW","PL","SOS","SRS","ORtg","DRtg","NRtg","Pace","TS%")
fundamental <- c("Team","ORB","DRB","AST","STL","BLK","TOV","PF","PTS","Year")
Shooting <- c("Team","FG%","2P%","3P%","FT%","TS%","Year")
fundamental_dat <- as.data.frame(All_data[,fundamental])
Shooting_dat <- as.data.frame(All_data[,Shooting])
model <- c("Lasso","Ridge","RandomForest")
Team_dat <- read.csv("Team.csv")
Position_dat <- read.csv("Position.csv")
dat <- read.csv("linear_df.csv")
scatter_Salary <- read.csv("scatter_Salary.csv")



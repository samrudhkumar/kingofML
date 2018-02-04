
###############################
## King of ML Hackathon DOTA ##
###############################

setwd("C:/Users/Sam/Desktop/King_of_ML/DataCode")

##Load training dataset 
library(readr)
library(tidyverse)
library(stringr)

##########################################
## REad in data and transform variables ##
##########################################

train.9 <- read.csv("train9.csv")
train.1 <- read.csv("train1.csv")
hero <- read.csv("hero_data.csv")


train.hero.9 <- left_join(train.9, hero, by = "hero_id")
train.hero.1 <- left_join(train.1, hero, by = "hero_id")


train.hero.9$game_id <- rep(1:9, nrow(train.hero.9)/9)
train.hero.1$game_id <- rep(10, nrow(train.hero.1))

###Variable win.ratio
train.hero.9$win.ratio <- train.hero.9$num_wins/train.hero.9$num_games
#train.hero.9$death.ratio <- (train.hero.9$num_games- train.hero.9$num_wins)/train.hero.9$num_games

###Fill trainhero1 with variables for win.ratio
train.hero.1$win.ratio <- -999
#train.hero.1$death.ratio <- -999
train.hero.1$rank <- -999
##ranking variable 
train.hero.9 %>% 
  group_by(user_id) %>% 
  mutate(rank = dense_rank(kda_ratio)) -> train.hero.9

train.full <- rbind(as.data.frame(train.hero.9), train.hero.1)

#train.full$attack.range.rate <- base::log(train.full$attack_range*train.full$attack_range)

###Cleaning and data trx 
train.full$primary_attr_int <- ifelse(train.full$primary_attr == "int", 1,0)
train.full$primary_attr_agi <- ifelse(train.full$primary_attr == "agi", 1,0)
train.full$primary_attr_str <- ifelse(train.full$primary_attr == "str", 1,0)

train.full$attack_type_ranged <- ifelse(train.full$attack_type == "Ranged", 1,0)
train.full$attack_type_melee <- ifelse(train.full$attack_type == "Melee", 1,0)

####Create binary columns for each attack type
uniq.roles <- unique(unlist(str_split(train.full$roles, pattern = ":")))


for (i in 1:length(uniq.roles)) {
  train.full[paste("role_", uniq.roles[i], sep = "")] <- 0
}

for (i in 1:nrow(train.full)) {
  col_name <- paste("role_", unique(unlist(str_split(train.full$roles[i], pattern = ":"))), sep = "")
  train.full[i,col_name] <- sapply(train.full[i,col_name], function(x) ifelse(x==0, 1,0))
}
#########################
## Data transformation ##
#########################

### taking log transform of kda_ratio convert back after predictions
train.full$kda_ratio.log <- base::log(train.full$kda_ratio)

###Drop columns with char and constant values 
drop_cols <- c("primary_attr", "attack_type", "roles", 
               "base_health", "base_mana", "base_mana_regen")
train.full <- train.full[,!names(train.full) %in% drop_cols]

z <- 1:10
train.wide <- as.data.frame(NULL)
test<- train.full[order(train.full$user_id, train.full$game_id),]
for (i in 10:1) {
  set.seed(i)
  z <- as.numeric(lag(z,default = i))
  test<- train.full[order(train.full$user_id, train.full$game_id),]
  test$game_id <- rep(z, nrow(test)/10)
  #test <- test[!test$kda_ratio == 0,]
  temp <- reshape(test, idvar = "user_id", timevar = "game_id", direction = "wide")
  train.wide <- rbind(train.wide, temp)
}
rm(test)

if(FALSE){
  ###create null dataframe
  temp_df <- data.frame(NULL)
  ##create more observations by cyclically moving kth game to the 10 position
  for (q in unique(train.wide$user_id)){
    abc <- train.wide[train.wide$user_id == q,] ##select a user ID 
    temp_user <- abc
    test_list <- list()
    for (i in 1:9) {
      game_no <- paste(".", i, sep = "")
      test_list[[i]] <- list(temp_user[,base::colnames(temp_user)[grep(game_no, base::colnames(temp_user))]])
    }
    ###store the variable to be moved into temp 
    for (m in 1:9){
      temp_user <- abc
      game_no <- paste(".", m, sep = "")
      ##move 10th variable to 9th
      temp_user[,base::colnames(temp_user)[grep(game_no, base::colnames(temp_user))]] <- temp_user[,base::colnames(temp_user)[grep(".10", base::colnames(temp_user))]]
      temp_user[,base::colnames(temp_user)[grep(".10", base::colnames(temp_user))]] <- unlist(test_list[[m]])
      
      temp_df <- rbind(temp_df,temp_user)
    }
  }
  save(temp_df, file = "extra_data.Rda")
}
#temp_df <- temp_df[!temp_df$user_id == 2010,]
#load(file = "extra_data.Rda")
#train.wide <- rbind(train.wide, temp_df)

##calculate avgkda in 9 games
train.wide$avg_kda <- sapply(1:nrow(train.wide), function(x) mean(as.numeric(train.wide[x,paste("kda_ratio.", 1:9, sep = "")])))
train.wide$avg_kda_rank <- rank(train.wide$avg_kda, ties.method = "min")

###avg no of wins 
train.wide$avg_wins <- sapply(1:nrow(train.wide), function(x) mean(as.numeric(train.wide[x,paste("num_wins.", 1:9, sep = "")])))
train.wide$avg_wins_rank <- rank(train.wide$avg_wins, ties.method = "min")

drop_col <- c(paste("id.", 1:10, sep = ""), paste("hero_id.", 1:10, sep = ""), 
              paste("user_id.", 1:10, sep = ""), 'user_id', "num_wins.10", "num_games.10", "win.ratio.10", "death.ratio.10", "rank.10")
train.wide <- train.wide[,!names(train.wide) %in% drop_col]


num_wins <- paste("num_wins.", 1:9, sep = "")

train.wide %>% 
  mutate_at(.funs= funs(kda_wins_ratio = avg_kda/.), num_wins) -> train.wide



#names(train.wide)
###Move the Predicted variable to first position 
col_idx <- grep("kda_ratio.10", names(train.wide))
train.wide <- train.wide[, c(col_idx, (1:ncol(train.wide))[-col_idx])]
#names(train.wide)
col_idx <- grep("kda_ratio.log.10", names(train.wide))
train.wide <- train.wide[, c(col_idx, (1:ncol(train.wide))[-col_idx])]
#names(train.wide)

##################################
## Load test data and Transform ##
##################################

###read data 
test.9 <- read.csv("test9.csv")
test.1 <- read.csv("test1.csv")

test.hero.9 <- left_join(test.9, hero, by = "hero_id")
test.hero.1 <- left_join(test.1, hero, by = "hero_id")

###Variable win.ratio
test.hero.9$win.ratio <- test.hero.9$num_wins/test.hero.9$num_games
#test.hero.9$death.ratio <- (test.hero.9$num_games- test.hero.9$num_wins)/test.hero.9$num_games

###Fill trainhero1 with variables for win.ratio
test.hero.1$win.ratio <- -999
#test.hero.1$death.ratio <- -999
test.hero.1$rank <- -999
##ranking variable 
test.hero.9 %>% 
  group_by(user_id) %>% 
  mutate(rank = dense_rank(kda_ratio)) -> test.hero.9
##create num_wins column in test.hero.1
test.hero.1$num_wins <- -999
##add column kda_ratio to test.hero.1 fill with -999
test.hero.1$kda_ratio <- -999

test.hero.9$game_id <- rep(1:9, nrow(test.hero.9)/9)
test.hero.1$game_id <- rep(10, nrow(test.hero.1))

test.full <- rbind(as.data.frame(test.hero.9), test.hero.1)

#test.full$attack.range.rate <- base::log(test.full$attack_range*test.full$attack_range)


###Cleaning and data trx 
test.full$primary_attr_int <- ifelse(test.full$primary_attr == "int", 1,0)
test.full$primary_attr_agi <- ifelse(test.full$primary_attr == "agi", 1,0)
test.full$primary_attr_str <- ifelse(test.full$primary_attr == "str", 1,0)

test.full$attack_type_ranged <- ifelse(test.full$attack_type == "Ranged", 1,0)
test.full$attack_type_melee <- ifelse(test.full$attack_type == "Melee", 1,0)

####Create binary columns for each attack type
uniq.roles <- unique(unlist(str_split(test.full$roles, pattern = ":")))


for (i in 1:length(uniq.roles)) {
  test.full[paste("role_", uniq.roles[i], sep = "")] <- 0
}

for (i in 1:nrow(test.full)) {
  col_name <- paste("role_", unique(unlist(str_split(test.full$roles[i], pattern = ":"))), sep = "")
  test.full[i,col_name] <- sapply(test.full[i,col_name], function(x) ifelse(x==0, 1,0))
}
#########################
## Data transformation ##
#########################
###one value with KDA ratio 0 which will be removed
test.full <- test.full[!test.full$kda_ratio == 0,]
### taking log transform of kda_ratio convert back after predictions
test.full$kda_ratio.log <- base::log(test.full$kda_ratio)

summary(test.full)
###Drop columns with char and constant values 
drop_cols <- c("primary_attr", "attack_type", "roles", 
               "base_health", "base_mana", "base_mana_regen")
test.full <- test.full[,!names(test.full) %in% drop_cols]

test.wide <- reshape(test.full, idvar = "user_id", timevar = "game_id", direction = "wide")
##calculate avgkda in 9 games
test.wide$avg_kda <- sapply(1:nrow(test.wide), function(x) mean(as.numeric(test.wide[x,paste("kda_ratio.", 1:9, sep = "")])))
test.wide$avg_kda_rank <- rank(test.wide$avg_kda, ties.method = "min")

###avg no of wins 
test.wide$avg_wins <- sapply(1:nrow(test.wide), function(x) mean(as.numeric(test.wide[x,paste("num_wins.", 1:9, sep = "")])))
test.wide$avg_wins_rank <- rank(test.wide$avg_wins, ties.method = "min")

drop_col <- c(paste("id.", 1:9, sep = ""), paste("hero_id.", 1:10, sep = ""), 
              paste("user_id.", 1:10, sep = ""), 'user_id', "num_wins.10", "num_games.10", 
              "kda_ratio.log.10","kda_ratio.10", "win.ratio.10", "death.ratio.10", "rank.10")
test.wide <- test.wide[,!names(test.wide) %in% drop_col]



test.wide %>% 
  mutate_at(.funs= funs(kda_wins_ratio = avg_kda/.), num_wins) -> test.wide


names(test.wide)
##############################
## Model building using h2o ##
##############################

library(h2o)

localH2O <- h2o.init(nthreads = -1) ## initialize h20 local instance to use all CPU cores

train.h2o <- as.h2o(train.wide)
test.h2o <- as.h2o(test.wide)


colnames(train.h2o)
colnames(test.h2o)


splits <- h2o.splitFrame(
  data = train.h2o, 
  ratios = c(0.7,0.1),
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 12345
)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]

#dependent variable (Response)
y.dep <- 1

#independent variables (dropping ID variables)
x.indep <- c(3:dim(train.h2o)[2])

#########################
## GBM training & test ##
#########################
gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = h2o.rbind(train, valid), 
                     ntrees = 100, learn_rate = 0.01, seed = 12345, max_depth = 14,
                     learn_rate_annealing = 0.99, nfolds = 5,
                     ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
                     stopping_rounds = 5,
                     stopping_tolerance = 1e-4,
                     stopping_metric = "RMSE"
)

h2o.performance(gbm.model, test)
gbm.model@model$cross_validation_metrics_summary


predict.gbm <- as.data.frame(h2o.predict(gbm.model, test))
out_gbm <- data.frame(actual = as.data.frame(test[["kda_ratio.10"]]), predicted = exp(predict.gbm$predict))

(rmse <- sqrt(mean((out_gbm$kda_ratio - out_gbm$predicted)^2)))

df<-as.data.frame(h2o.varimp(gbm.model))


###########################################
## Training & testing RandomForest model ##
###########################################
gbm.model.randf <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = h2o.rbind(train, valid), 
                                    ntrees = 1000, seed = 12345, max_depth = 15, nfolds = 5,
                                    ## early stopping once the validation RMSE doesn't improve by at least 0.01% for 5 consecutive scoring events
                                    stopping_rounds = 5,
                                    stopping_tolerance = 1e-4,
                                    stopping_metric = "RMSE"
)

h2o.performance(gbm.model.randf, test)
gbm.model.randf@model$cross_validation_metrics_summary

df<-as.data.frame(h2o.varimp(gbm.model.randf))
predict.gbm <- as.data.frame(h2o.predict(gbm.model.randf, test))
out_gbm <- data.frame(actual = as.data.frame(test[["kda_ratio.10"]]), predicted = exp(predict.gbm$predict))

(rmse <- sqrt(mean((out_gbm$kda_ratio - out_gbm$predicted)^2)))

##Save model to wdir
h2o.saveModel(gbm.model.randf, path = getwd(), force = T)

##Load model
gbm.model.randf <- h2o.loadModel("C:\\Users\\Sam\\Desktop\\King_of_ML\\DataCode\\DRF_model_R_1517178630703_8")




####################
## Testing AutoML ##
####################

aml <- h2o.automl(x = x.indep, y = y.dep,
                  training_frame = h2o.rbind(train, valid),
                  validation_frame = test,
                  max_runtime_secs = 1000, stopping_metric = "RMSE", stopping_rounds = 5)
##RMSE 0.143
#ML leaderboard
lb <- aml@leaderboard
lb

h2o.saveModel(aml@leader, path = getwd(), force = T)

h2o.performance(aml@leader, test)

best_model <- h2o.loadModel("StackedEnsemble_AllModels_0_AutoML_20180130_024511")
predict.gbm <- as.data.frame(h2o.predict(best_model, test))
out_gbm <- data.frame(actual = as.data.frame(test[["kda_ratio.10"]]), predicted = exp(predict.gbm$predict))

(rmse <- sqrt(mean((out_gbm$kda_ratio.10 - out_gbm$predicted)^2, na.rm = T)))

###Make a prediction and write for submission 
predict.gbm <- as.data.frame(h2o.predict(abc_modle, test.h2o))

out_gbm <- data.frame(id = test.wide$id.10, kda_ratio = exp(predict.gbm$predict))

write.csv(out_gbm, file = "out_sub.csv", row.names = F)

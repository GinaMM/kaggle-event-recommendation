## R version 2.15.2(64bit) on Windows 7
## My PC  CPU:i5 2410m, MEMORY:16G
## Editor: Meadow + ess
## Meadow: Multilingual enhancement to gnu Emacs with ADvantages Over Windows

## Function Definitions begin

## init() probably needs 8GB or more of memory.
init <- function(){
  ## train.csv, test.csv
  dTrain <- read.csv("train.csv", as.is = TRUE)
  dTest <- read.csv("test.csv", as.is = TRUE)
  publicSol <- read.csv("public_leaderboard_solution.csv")

  publicIdx <- which(dTest$user %in% publicSol$User)
  publicIdxUnique <- which(unique(dTest$user) %in% publicSol$User)

  numTrain <- nrow(dTrain) + length(publicIdx)
  numAll <- nrow(dTrain) + nrow(dTest)

  ## d1[1:numTrain,] : train.csv + publicSol
  ## 
  d1 <- rbind(dTrain[,-(5:6)], dTest[publicIdx,], dTest[-publicIdx,])

  allUsers <- d1$user
  allEvents <- d1$event
  uniqueUsers <- unique(allUsers)
  numUser <- length(uniqueUsers)
  numUserTrain <- length(unique(d1[1:numTrain,]$user))

  ## Alternative user id
  ## INDEX for tapply
  ## the values of tapply(X, INDEX, FUN) are sorted by INDEX
  allUsersIdx <- rep(1, numAll)
  for(i in 2:numAll){
    if(allUsers[i-1] != allUsers[i]){
      allUsersIdx[i] <- allUsersIdx[i-1] + 1
    }else{
      allUsersIdx[i] <- allUsersIdx[i-1]
    }
  }

  userIdx <- tapply(1:nrow(d1), allUsersIdx, identity)

  ## mapping from alternative user id to event
  user2event <- tapply(allEvents, allUsersIdx, identity)

  ## the same timestamp users
  user1event <- (1:numUser)[sapply(userIdx,
                                   function(x)
                                   all(d1[x[1],]$timestamp == d1[x[-1],]$timestamp))]

  a <- (length(unique(dTrain$user))+1)
  b <- a + nrow(publicSol) - 1
  targetPublic <- unlist(mapply(
                                function(x, y){
                                  result <- rep(0, length(x))
                                  result[x == y] <- 1
                                  result
                                },
                                user2event[a:b],
                                publicSol$Events))

  ## dTrain$interested + public solution
  target <- c(dTrain$interested, targetPublic)

  
  users <- read.csv("users.csv", as.is = TRUE)
  event <- read.csv("events.csv.gz", as.is = TRUE)
  eventA <- read.csv("event_attendees.csv.gz",as.is = TRUE) 
  userF <- read.csv("user_friends.csv.gz", as.is = TRUE)

  ## mapping rows of d1 to rows of above 4 files
  userid2num <- sapply(allUsers, function(x)which(users$user_id == x))
  eventid2num <- sapply(allEvents, function(x)which(event$event_id == x))
  eventA2num <- sapply(allEvents, function(x)which(eventA$event == x))
  userF2num <- sapply(allUsers, function(x)which(userF$user == x))

  ## for non us environment
  Sys.setlocale("LC_TIME", "us")
  
  ## train.csv, test.csv
  d1.month = as.integer(substr(d1$timestamp, 6,7))
  d1.date = as.integer(substr(d1$timestamp, 9,10))
  d1.hour = as.integer(substr(d1$timestamp, 12,13))
  d1.week <- factor(weekdays(as.Date(d1$timestamp), abbreviate=TRUE),
                    levels = c("Sun", "Sat", "Mon", "Tue", "Wed", "Thu", "Fri"))

  d2row <- rep(0, numAll)
  for(i in 1:numAll){
    d2row[i] <- which(uniqueUsers == d1[i,]$user)
  }
  e.u <- tapply(d1$event, d2row, identity)
  e.len <- sapply(e.u, length)

  ## users.csv
  ## Warning message:NAs introduced by coercion
  u.birthyear <- as.integer(users$birthyear)
  u.birthyear[is.na(u.birthyear)] <- 3000

  user.month = as.integer(substr(users[userid2num,]$joinedAt, 6,7))
  user.date = as.integer(substr(users[userid2num,]$joinedAt, 9,10))
  user.hour = as.integer(substr(users[userid2num,]$joinedAt, 12,13))
  user.week <- factor(weekdays(as.Date(users[userid2num,]$joinedAt), abbreviate=TRUE),
                      levels = c("Sun", "Sat", "Mon", "Tue", "Wed", "Thu", "Fri"))


  ## events.csv
  event.year <- as.integer(substr(event$start_time, 1,4))[eventid2num]
  event.month <- as.integer(substr(event$start_time, 6,7))[eventid2num]
  event.date <- as.integer(substr(event$start_time, 9,10))[eventid2num]
  event.hour <- as.integer(substr(event$start_time, 12,13))[eventid2num]
  event.minite <- as.integer(substr(event$start_time, 15,16))[eventid2num]
  event.week <- factor(weekdays(as.Date(event[eventid2num,]$start_time), abbreviate=TRUE),
                       levels = c("Sun", "Sat", "Mon", "Tue", "Wed", "Thu", "Fri"))

  event.city <- event[eventid2num,]$city
  event.state <- event[eventid2num,]$state
  event.zip <- event[eventid2num,]$zip
  event.country <- event[eventid2num,]$country

  event_latlng<- as.matrix(event[eventid2num,8:9])
  event_c <- as.matrix(event[eventid2num,10:110])

  set.seed(12345)
  kModel <- kmeans(t(apply(event_c, 1, l2_normalize)), 20)

  ## event_attendees.csv
  eventA.yes <- strsplit(eventA$yes, split = " ")
  eventA.maybe <- strsplit(eventA$maybe, split = " ")
  eventA.invited <- strsplit(eventA$invited, split = " ")
  eventA.no <- strsplit(eventA$no, split = " ")

  population.yes <- sapply(eventA.yes, length)[eventA2num]
  population.maybe <- sapply(eventA.maybe, length)[eventA2num]
  population.invited <- sapply(eventA.invited, length)[eventA2num]
  population.no <- sapply(eventA.no, length)[eventA2num]

  ## user_friend.csv, event_attendees.csv
  userF.friend <- lapply(strsplit(userF$friends, split = " "), as.double)

  friend.yes <- rep(0, numAll)
  friend.maybe <- rep(0, numAll)
  friend.invited <- rep(0, numAll)
  friend.no <- rep(0, numAll)
  for(i in 1:numAll){
    friend.yes[i] <- length(intersect(userF.friend[[userF2num[i]]],
                                      eventA.yes[[eventA2num[i]]]))
    friend.maybe[i] <- length(intersect(userF.friend[[userF2num[i]]],
                                        eventA.maybe[[eventA2num[i]]]))
    friend.invited[i] <- length(intersect(userF.friend[[userF2num[i]]],
                                          eventA.invited[[eventA2num[i]]]))
    friend.no[i] <- length(intersect(userF.friend[[userF2num[i]]],
                                     eventA.no[[eventA2num[i]]]))
  }


  #### After reading the dolaameng's post
  ## begin
  e.d.dif_time <- as.integer(as.Date(event[eventid2num,]$start_time) - as.Date(d1$timestamp))
  e.u.dif_time <- as.integer(as.Date(event[eventid2num,]$start_time) -
                             as.Date(substr(users[userid2num,]$joinedAt, 1,10)))
  u.d.dif_time <- as.integer(as.Date(substr(users[userid2num,]$joinedAt, 1,10)) - as.Date(d1$timestamp))
  
  allcities <- unique(event.city)
  allstates <- unique(event.state)
  allcountries <- unique(event.country)
  temp <- strsplit(users$location, split="  ")
  numUsers <- nrow(users)
  u.city <- rep("", numUsers)
  u.state <- rep("", numUsers)
  u.country <- rep("", numUsers)

  for(i in 1:numUsers){
    temp2 <- temp[[i]]
    temp2.city <- temp2[temp2 %in% allcities]
    temp2.state <- temp2[temp2 %in% allstates]
    temp2.country <- temp2[temp2 %in% allcountries]
    u.city[i] <- if(length(temp2.city) > 0) temp2.city[1] else ""
    u.state[i] <- if(length(temp2.state) > 0) temp2.state[1] else ""
    u.country[i] <- if(length(temp2.country) > 0) temp2.country[1] else ""
  }

  is_user_in_event_city <- rep(FALSE, numAll)
  is_user_in_event_city[u.city[userid2num] == event.city] <- TRUE

  is_user_in_event_country <- rep(FALSE, numAll)
  is_user_in_event_country[u.country[userid2num] == event.country] <- TRUE

  IsCreaterFriend <- vector(length = numAll)
  for(i in 1:numAll){
    IsCreaterFriend[i] <- event[eventid2num[i],]$user_id %in% userF.friend[[userF2num[i]]]
  }
  ## end

  ## this contains all the variables I tried.
  d3 <- data.frame(## train
                   ## event = FactorTopkFrequentVals(d1$event, 400),
                   invited = d1$invited,
                   d.month = d1.month,
                   d.date = d1.date,
                   d.hour = d1.hour,
                   d.week = d1.week,
                   e.len = e.len[d2row],
                   ## user
                   u.locale = factor(users[userid2num,]$locale),
                   u.birthyear = u.birthyear[userid2num],
                   u.gender = factor(users[userid2num,]$gender),
                   u.month = user.month,
                   u.date = user.date,
                   u.hour = user.hour,
                   u.week = user.week,
                   u.location = FactorTopkFrequentVals(users[userid2num,]$location, 20),
                   u.timezone = users[userid2num,]$timezone,
                   ## user friend, event attendees
                   f.yes = friend.yes,
                   f.maybe = friend.maybe,
                   f.invited = friend.invited,
                   f.no = friend.no,
                   ## event
                   e.year = event.year,
                   e.month = event.month,
                   e.date = event.date,
                   e.wday = event.week,
                   e.city = FactorTopkFrequentVals(event.city, 31),
                   e.state = FactorTopkFrequentVals(event.state, 31),
                   e.zip = FactorTopkFrequentVals(event.zip, 31),
                   e.country = FactorTopkFrequentVals(event.country, 31),
                   e.latlng = event_latlng,
                   event_c,
                   e.clus = factor(kModel$cluster),
                   ## event attendees
                   p.yes.may=population.yes/(population.maybe+1),
                   p.yes.inv=population.yes/(population.invited+1),
                   p.yes.no=population.yes/(population.no+1),
                   p.may.inv=population.maybe/(population.invited+1),
                   p.may.no=population.maybe/(population.no+1),
                   p.inv.no=population.invited/(population.no+1),
                   p.yes = population.yes,
                   p.maybe = population.maybe,
                   p.invited = population.invited,
                   p.no = population.no,
                   ## after the dolaameng's post
                   e.d.dif_time = e.d.dif_time,
                   e.u.dif_time = e.u.dif_time,
                   u.d.dif_time = u.d.dif_time,
                   u.city = FactorTopkFrequentVals(u.city[userid2num], 31),
                   u.state = FactorTopkFrequentVals(u.state[userid2num], 31),
                   u.country = FactorTopkFrequentVals(u.country[userid2num], 31),
                   is_user_in_event_country = factor(is_user_in_event_country),
                   is_user_in_event_city = factor(is_user_in_event_city),
                   isCreaterFriend = factor(IsCreaterFriend)
                   )

  Index <- data.frame(target, allUsersIdx[1:numTrain])
  actual<- tapply(d1[1:numTrain,]$event, Index, identity)[1:numUserTrain* 2]
  list(d3 = d3, dTrain = dTrain, dTest = dTest, target = target,
       user2event = user2event, userIdx = userIdx, actual = actual,
       user1event = user1event, allUsersIdx = allUsersIdx, allEvents = allEvents,
       numUserTrain = numUserTrain, uniqueUsers = uniqueUsers)
}

## factor v only top k frequent vals are used
## k + 1 levels:k most frequent values and "others"
FactorTopkFrequentVals <- function(v, k = 20){
  tbl <- sort(table(v), decreasing = TRUE)[1:k]
  vals <- names(tbl)
  v[! v %in% vals] <- "others"
  factor(v)
}

numeric2events <- function(probs, events){
  len <- length(probs)
  if(len != length(events)) return(FALSE)
  events[order(probs, decreasing = TRUE)]
}

l2_normalize <- function(a){
  s2 <- crossprod(a, a)[1,1]
  if(s2 == 0){
    return(a)
  }else{
    return(a / sqrt(s2))
  }
}

## 
makePrediction <- function(xTrain, yTrain, xTest, uTest, varix1, varix2, eventTest,
                           mb = 8, ratio1 = 0.05, ratio2 = 0.32,
                           tc1 = 8, nt1 = 1000,
                           tc2 = 13, nt2 = 1000,
                           seed1, seeds){
  set.seed(seed1)
  model1 <- gbm(yTrain~., xTrain[,varix1],
                distribution = "gaussian",
                interaction.depth = tc1,
                n.tree = nt1,
                n.minobsinnode = mb,
                shrinkage = 0.01,
                bag.fraction = 0.8,
                train.fraction = 1,
                verbose = F)
  pred1 <- predict(model1, xTrain[,varix1], n.trees = nt1)
  pred1_test <- predict(model1, xTest[,varix1], n.trees = nt1)
  y2 <- yTrain - ratio1 * exp(exp(pred1))
  pred2All <- foreach(seed = seeds, .combine = cbind,
        .packages = c('gbm')) %dopar%{
    set.seed(seed)
    model2 <- gbm(y2~. , xTrain[,varix2],
                  distribution = "gaussian",
                  interaction.depth = tc2,
                  n.tree = nt2,
                  n.minobsinnode = mb,
                  shrinkage = 0.01,
                  bag.fraction = 0.8,
                  train.fraction = 1,
                  verbose = F)
    predict(model2, xTest[,varix2], n.trees = nt2)
  }
  pred2 <- rowMeans(pred2All)
  pred <- pred2 + ratio2 * pred1_test
  predprobs <- tapply(pred, uTest, identity)
  predevents <- mapply(numeric2events, predprobs, eventTest)
  list(predevents = predevents, pred2 = pred2, pred1 = pred1_test)
}

## Function Definitions end

## My PC  CPU:i5 2410m, MEMORY:16G
## Time: 1728s
system.time(d4 <- init())

## memory.size()
## 4356.13

d3 <- d4$d3
target <- d4$target
numTrain <- length(d4$target)
user2event <- d4$user2event
user1event <- d4$user1event
numUserTrain <- d4$numUserTrain
userIdx <- d4$userIdx
allUsersIdx <- d4$allUsersIdx
actual <- d4$actual
uniqueUsers = d4$uniqueUsers
allEvents <- d4$allEvents

rm(d4)
gc()

## memory.size()
## 166.5


## vars for the first model
## e.len, e.state, e.zip, e.country,e.latlng.lat, e.latlng.lng
## c_1 .. c_100, c_other, is_user_in_event_country
## is_user_in_event_city, isCreaterFriend
varix1 <- c(6,25:130,148:150)

## vars for the second model
## u.locale     u.birthyear  u.location   u.timezone   f.yes       
## f.maybe      f.invited    f.no         e.city       e.clus      
## p.yes.may    p.yes.inv    p.yes.no     p.may.inv    p.may.no    
## p.inv.no     p.yes        p.maybe      p.invited    p.no        
## e.d.dif_time
varix2 <- c(7,8,14:19,24,131:142)

## users for Private Leaderboard
userTest <- setdiff(user1event, 1:numUserTrain)
## Indices for Private Leaderboard
idxTest <- unlist(userIdx[userTest])

library(gbm)
library(foreach)
library(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

## Time:360s
## Private Score:0.68610
model005 <- makePrediction(d3[1:numTrain,], target, 
                           d3[idxTest,], allUsersIdx[idxTest],
                           varix1, varix2, user2event[userTest],
                           nt1 = 1000, nt2 = 1000, seed1 = 123432, seeds=1:4)

pred005 <- data.frame(User = uniqueUsers[userTest],
                      Events = sapply(model005$predevents,
                        function(x)paste(x, collapse = " ")))
write.csv(pred005, "pred005.csv", row.names = FALSE)

## Time:614s
## Private Score:0.68237 == my final score
model006 <- makePrediction(d3[1:numTrain,], target, 
                           d3[idxTest,], allUsersIdx[idxTest],
                           varix1, varix2, user2event[userTest],
                           nt1 = 1000, nt2 = 1000, seed1 = 123432, seeds=101:112)

pred006 <- data.frame(User = uniqueUsers[userTest],
                      Events = sapply(model006$predevents,
                        function(x)paste(x, collapse = " ")))
write.csv(pred006, "pred006.csv", row.names = FALSE)

length(which(sapply(model005$predevents, function(x)x[1]) ==
             sapply(model006$predevents, function(x)x[1])))
## 575


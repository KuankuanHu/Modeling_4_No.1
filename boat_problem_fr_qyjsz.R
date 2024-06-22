catch <- function(x, act) {
  if (x < 0 | act < 0) {
    print("ERROR")
  }
  while (x > 0 & act > 0) {
      act = act - 1
      x = x - 1
  }
  if (x == 0 & act > 0){
    act = act - 1
  }
  return(c(x, act))
  } 

boats <- c(0, 1, 1, 3)
boats_sec <- c(0, 1, 2, 2)
names(boats) <- c("boat1", "boat2", "boat3", "boat4")
names(boats_sec) <- c("boat1", "boat2", "boat3", "boat4")
choose_from <- c("boat1", "boat2", "boat3", "boat4")
move = 6

while (action > 0) {
  choice <- sample(choose_from, 1)
  progress <- catch(boats_sec[choice], action)
  print(progress)
  action = progress[2]
  boats_sec[choice] = progress[1]
  choose_from <- choose_from[!(choose_from %in% choice)]
}
print(sum(boats_sec))

#########################
survive_rate_0113 <- c()
for (i in 1:500) {
  boats <- c(0, 1, 1, 3)
  names(boats) <- c("boat1", "boat2", "boat3", "boat4")
  choose_from <- c("boat1", "boat2", "boat3", "boat4")
  action <- 6
  while (action > 0) {
    choice <- sample(choose_from, 1)
    progress <- catch(boats[choice], action)
    action = progress[2]
    boats[choice] = progress[1]
    choose_from <- choose_from[!(choose_from %in% choice)]
  }
  survive_rate_0113 <- c(survive_rate_0113, sum(boats))
}

survive_rate_0122 <- c()
for (i in 1:500) {
  boats <- c(0, 1, 2, 2)
  names(boats) <- c("boat1", "boat2", "boat3", "boat4")
  choose_from <- c("boat1", "boat2", "boat3", "boat4")
  action <- move
  while (action > 0) {
    choice <- sample(choose_from, 1)
    progress <- catch(boats[choice], action)
    action = progress[2]
    boats[choice] = progress[1]
    choose_from <- choose_from[!(choose_from %in% choice)]
  }
  survive_rate_0122 <- c(survive_rate_0122, sum(boats))
}

survive_rate_0005 <- c()
for (i in 1:500) {
  boats <- c(0, 0, 0, 5)
  names(boats) <- c("boat1", "boat2", "boat3", "boat4")
  choose_from <- c("boat1", "boat2", "boat3", "boat4")
  action <- move
  while (action > 0) {
    choice <- sample(choose_from, 1)
    progress <- catch(boats[choice], action)
    action = progress[2]
    boats[choice] = progress[1]
    choose_from <- choose_from[!(choose_from %in% choice)]
  }
  survive_rate_0005 <- c(survive_rate_0005, sum(boats))
}

survive_rate_1112 <- c()
for (i in 1:500) {
  boats <- c(1, 1, 1, 2)
  names(boats) <- c("boat1", "boat2", "boat3", "boat4")
  choose_from <- c("boat1", "boat2", "boat3", "boat4")
  action <- move
  while (action > 0) {
    choice <- sample(choose_from, 1)
    progress <- catch(boats[choice], action)
    action = progress[2]
    boats[choice] = progress[1]
    choose_from <- choose_from[!(choose_from %in% choice)]
  }
  survive_rate_1112 <- c(survive_rate_1112, sum(boats))
}

################
mean(survive_rate_0113)
table(survive_rate_0113)

mean(survive_rate_0122)
table(survive_rate_0122)

mean(survive_rate_0005)
table(survive_rate_0005)

mean(survive_rate_1112)
table(survive_rate_1112)


#################
survive_ran_sel <- c()
for (i in 1:500) {
  boats <- c(0, 0, 0, 5)
  names(boats) <- c("boat1", "boat2", "boat3", "boat4")
  choose_from <- c("boat1", "boat2", "boat3", "boat4")
  action = 6
  while (action > 0) {
    choice <- sample(choose_from, 1)
    if (boats[choice] == 0) {
      choose_from <- choose_from[!(choose_from %in% choice)]
      action = action - 1
    } else if (boats[choice] > 0) {
      boats[choice] = boats[choice] - 1
      action = action - 1
    } else {
      print("ERROR")
    }
  }
  survive_ran_sel <- c(survive_ran_sel, sum(boats))
}


mean(survive_ran_sel)
table(survive_ran_sel)

##################
library(ggplot2)


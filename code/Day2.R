dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2024git/data/data2.txt", header = FALSE)

#The levels are either all increasing or all decreasing.
#Any two adjacent levels differ by at least one and at most three.
safe <- function(x){
  return((all(diff(as.numeric(strsplit(x," ")[[1]])) > 0) | all(diff(as.numeric(strsplit(x," ")[[1]])) < 0)) & max(abs(diff(as.numeric(strsplit(x," ")[[1]])))) <= 3)
}

sum(apply(dat, 1, safe))
# 369

###################################
#Part 2
###################################
x <- as.numeric(strsplit(dat[1,]," ")[[1]])
safe <- function(x){
  return((all(diff(x) > 0) | all(diff(x) < 0)) & max(abs(diff(x))) <= 3)
}
safe(x)

safe2 <- function(x){
  temp <- as.numeric(strsplit(x," ")[[1]])
  for (i in 1:length(temp)){
    if (safe(temp[-i])){return(TRUE)}     
  }
  return(FALSE)
}

sum(apply(dat, 1, safe2))
#428

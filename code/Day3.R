#Day 3
dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2024git/data/data3.txt", header = FALSE)

####################################
#Part 1
####################################

#Ugh. Regular expressions
summ <- c()
for (j in 1:6){print(j)
ind <- gregexpr("mul\\(\\d{1,3},\\d{1,3}\\)",dat[j,])[[1]]
len <- attr(ind,"match.length")

prods <- c()
for (i in 1:length(ind)){
  muls <- substring(dat[j,],ind[i],ind[i] + len[i] - 1)
  muls <- gsub("mul\\(","",muls)
  muls <- gsub("\\)","",muls)
  prods[i] <- prod(as.numeric(strsplit(muls,",")[[1]]))
}
summ[j] <- sum(prods)
}

sum(summ)
#183788984



####################################
#Part 2
####################################
#Ugh. Regular expressions
dat <- read.delim("/Users/gregorymatthews/Dropbox/AOC2024git/data/data3oneline.txt", header = FALSE)
summ <- c()

  ind <- gregexpr("mul\\(\\d{1,3},\\d{1,3}\\)",dat$V1)[[1]]
  len <- attr(ind,"match.length")
  
  ind_do <- gregexpr("do\\(\\)",dat$V1)[[1]]
  ind_dont <- gregexpr("don't\\(\\)",dat$V1)[[1]]
  
  remove <- c()
  for (k in 1:length(ind)){
    
    if(min((ind[k] - ind_do)[ind[k] - ind_do > 0]) > min((ind[k] - ind_dont)[ind[k] - ind_dont > 0])){
      remove <- c(remove,k)
    }
    
  }
  
  ind <- ind[-c(remove)]
  len <- len[-c(remove)]
  
  
  prods <- c()
  for (i in 1:length(ind)){
    muls <- substring(dat$V1,ind[i],ind[i] + len[i] - 1)
    muls <- gsub("mul\\(","",muls)
    muls <- gsub("\\)","",muls)
    prods[i] <- prod(as.numeric(strsplit(muls,",")[[1]]))
  }
  summ <- sum(prods)
  
#incorrect: 74210088
#correct: 62098619



















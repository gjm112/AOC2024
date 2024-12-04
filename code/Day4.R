#Day 4

#############################################
## Part 1
#############################################
dat <- readLines("/Users/gregorymatthews/Dropbox/AOC2024git/data/data4.txt")

mat <- list()
for (i in 1:length(dat)){
mat[[i]] <- strsplit(dat[i],"")[[1]]
}

mat <- do.call(rbind,mat)

#Now search this nonsense. 

#Search rows
count <- 0

#Check Rows
for (i in 1:length(dat)){
  for (j in 1:length(dat)){
    if (i <= 140 & all(j + 0:3 <=140)){
    if (all(mat[i,j + 0:3] == c("X","M","A","S"))){count <- count + 1}
    if (all(mat[i,j + 0:3] == c("S","A","M","X"))){count <- count + 1}
    }
  }
}

#Check Columns
for (i in 1:length(dat)){
  for (j in 1:length(dat)){
    if (j <= 140 & all(i + 0:3 <=140)){
      if (all(mat[i + 0:3,j ] == c("X","M","A","S"))){count <- count + 1}
      if (all(mat[i + 0:3,j ] == c("S","A","M","X"))){count <- count + 1}
    }
  }
}



#Check diag
for (i in 1:length(dat)){
  for (j in 1:length(dat)){
    if (all(i + 0:3 <= 140) & all(j + 0:3 <=140)){
      if (all(mat[cbind(i + 0:3,j + 0:3)] == c("X","M","A","S"))){count <- count + 1}
      if (all(mat[cbind(i + 0:3,j + 0:3)] == c("S","A","M","X"))){count <- count + 1}
    }
  }
}

#Check diag
for (i in 1:length(dat)){
  for (j in 1:length(dat)){
    if (all(i + 0:3 <= 140) & all(j + 3:0 <= 140)){
      if (all(mat[cbind(i + 0:3,j + 3:0)] == c("X","M","A","S"))){count <- count + 1}
      if (all(mat[cbind(i + 0:3,j + 3:0)] == c("S","A","M","X"))){count <- count + 1}
    }
  }
}

count


#Wrong: 1794
#Second wrong guess: 1769
#Also not correct: 3551
#Correct: 2685
#I'm an idiot


#############################################
## Part 2
#############################################
count <- 0
for (i in 1:length(dat)){
  for (j in 1:length(dat)){
    if (all(i + -1:1 <= 140) & all(j + -1:1 <= 140)){
      if (all(mat[cbind(i + -1:1,j + -1:1)] == c("M","A","S")) | all(mat[cbind(i + -1:1,j + -1:1)] == c("S","A","M"))){
        if (all(mat[cbind(i + -1:1,j + 1:-1)] == c("M","A","S")) | all(mat[cbind(i + -1:1,j + 1:-1)] == c("S","A","M"))){
        count <- count + 1  
        }
      }
    }
  }
}

count

#2048


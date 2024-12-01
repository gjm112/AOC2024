dat <- read.table("/Users/gregorymatthews/Dropbox/AOC2024/data/data1.txt")

########################
### Part 1
########################
sum(abs(apply(cbind(sort(dat$V1),sort(dat$V2)),1,diff)))
#2344935

########################
### Part 2
########################
dat$V1 %*% sapply(dat$V1,function(x){sum(x == dat$V2)})
#27647262




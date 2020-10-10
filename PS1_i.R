#HW1 Haixiang Zeng 12032760

#1.flowchart

Print_values <- function(a, b, c){
  a <- runif(1,1,100)
  b <- runif(1,1,100)
  c <- runif(1,1,100)
  
  if(a>b && b>c){
    cat(a, b, c)
  }
  if(a<b && b<c){
    cat(c, b, a)
  }
  if(a>b && b<c){
    if(a>c){
      cat(a, c, b)
    }else{
      cat(c, a, b)
    }
  }
  if(a<b && b>c){
    if(a>c){
      cat(b, a, c)
    }else{
      cat(b, c, a)
      }
  }
}
Print_values(a, b, c)

#can`t print different variables in one row by using "print"
#After reading https://cloud.tencent.com/developer/ask/45564, use "cat" instead. 

#2.matrix multiplication
#2.1
vector1 <- c(runif(50,0,50))
vector2 <- as.integer(vector1)
M1 <- matrix(vector2, nrow = 5, ncol = 10)
vector3 <- c(runif(50,0,50))
vector4 <- as.integer(vector3)
M2 <- matrix(vector4, nrow = 10, ncol = 5)
M1
M2
#2.2
M3 <- array(0, dim = c(5,5))
Matrix_multip <- function(M1, M2){
  for (i in 1:5) {
    for (j in 1:5) {
    M3[i,j] <- sum(M1[i,]*M2[,j])
   
    }
  }
  return(M3)
}
Matrix_multip(M1,M2)

#the way to verify.
Matrix_verify <- function(M1, M2){
  M3 <- M1%*%M2 
  print(M3)
}
Matrix_verify(M1,M2)

#3
install.packages("gtools")
library(gtools)
Pascal_triangle <- function(k){
  print(1)
  if(k > 1){
    for(n in 1:(k-1)){
      print(nrow(combinations(k-1,n)))
    }
  }
}
Pascal_triangle(100)
Pascal_triangle(200)


#4
Least_moves <- function(x){
  n <- 0
  while(x > 1){
    # if it`s an odd
    if(x %%2==0){
      x <- x/2
      #n <- n+1
    }
    # or single?
    else{
      x <- x-1
      #n <- n+1
    }
    n <- n+1
  }
  #if(x == 1) print(n)
    return(n)
}


#5
#5.1
install.packages("gtools")
library(gtools)
Find_expression <- function(x1){
  x1 <- as.integer(runif(1,1,100))
  print(x1)
  sym <- c("+", "-", "")
  symC <- permutations(3, 8, sym,repeats.allowed = T)
  symM <- cbind(symC, "")
  Srow <- nrow(symM)
  num <- seq(1, 9, 1)
  numM <- matrix(num, 9, Srow)
  numT <- t(numM)
  ans <- matrix("", Srow, 1)
  for(row in 1:Srow){
    sym1 <- c(symM[row,])
    num1 <- c(numT[row,])
    ans[row,] <- paste(num1, sym1, sep = "",collapse = "")
  }
  n <- 0
  for( row in 1:Srow){
    ans1 <- eval(parse(text = ans[row]))
    if (ans1 == x1){
      print (ans[row])
      n <- n+1
    }
  }
  return(n)
}
#5.2
Total_solutions <- matrix(0,100,1)
for(i1 in 0:100){
  Total_solutions[i1,] <- Find_expression(i1)
}
#?plot
plot(x=1:100, y=Total_solutions, type="l")
cat(which(Total_solutions == max(Total_solutions)),"has/have the most total solutions for", max(Total_solutions),".  And" ,
    which(Total_solutions == min(Total_solutions)),"has/have the least total solutions for", min(Total_solutions))

#6
#6.1
Met_data <- read.csv(file = "2281305.csv", header = T)
VIS <- Met_data$VIS
#?data.matrix
VIS_M <- data.matrix(VIS)
VIS_DD <- substr(VIS_M, 1,6)
VIS_dqc <- substr(VIS_M, 8, 8)
VIS_vc <- substr(VIS_M, 10, 10)
VIS_qvc <- substr(VIS_M, 12, 12)
Obs_time <- Met_data$DATE
VIS_DD <- as.numeric(VIS_DD)
VIS_dqc <- as.logical( as.numeric(VIS_dqc) )
VIS_vc  <- as.logical(VIS_vc)
VIS_qvc <- as.logical( as.numeric(VIS_qvc) )
Obs_time1 <- as.Date(Obs_time)
VIS_DD[which(VIS_DD == 999999)] <- NA
VIS_dqc[!which(VIS_dqc == 1)] <- NA
VIS_vc[!which(VIS_vc == "N")] <- NA
VIS_qvc[!which(VIS_qvc == 1)] <- NA
#VIS_M2 <- c(VIS_DD, VIS_dqc, VIS_vc, VIS_qvc)
plot(Obs_time1, VIS_DD, type = "l",col="blue", lwd=0.5)

#6.2
draft <- array(NA,dim = c(0,7))

colnames(draft) <- c("[0,5km)","[5km,10km)","[10km,15km)","[15km,20km)","[20km,25km)","[25km,30km)",">=30km")

for (year in 2010:2020) {
  this_year <- which(substr(Obs_time1,1,4) == year)
  km1 <- 0
  km2 <- 0
  km3 <- 0
  km4 <- 0
  km5 <- 0
  km6 <- 0
  km7 <- 0
  for(vis in this_year){
    if( 0<= VIS_DD && VIS_DD) km1 <- km1+1
    if(5000<= VIS_DD &&  VIS_DD< 10000) km2 <- km2 +1
    if(10000<= VIS_DD &&  VIS_DD< 15000) km3 <- km3 +1
    if(15000<= VIS_DD &&  VIS_DD< 20000) km4 <- km4 +1
    if(20000<= VIS_DD &&  VIS_DD< 25000) km5 <- km5 +1
    if(25000<= VIS_DD &&  VIS_DD< 30000) km6 <- km6 +1
    if(30000<= VIS_DD ) km7 <- km7 +1
  }
  draft <- rbind(draft,c(km1,km2,km3,km4,km5,km6,km7))
}
row.names(draft) <- c(2010:2020)
print(draft)
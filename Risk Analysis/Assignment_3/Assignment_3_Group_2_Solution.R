#Solution to Problem 2
n <- 100 #Number of Simulations to be performed
d <- c(0,1,1,23.6,24) #distances from home
Z_1 <- c() #Initialize vector to store cumulative travel time for case 1
Z_2 <- c() #Initialize vector to store cumulative travel time for case 2

#Simulation Case 1
set.seed(630829)
for (i in 1:n) {
  T0 <- 0 #Start Time when Mr.X is at home
  T1 <-rnorm(1,8,1) #Walking time to the bus stop
  b <- rbinom(1,1,0.03) #3% of the times there is major disturbance in the service
  T2 <- (1-b)*5*runif(1)+b*rexp(1,0.1) #Waiting time at the bus stop given that buses are scheduled, I aim for 5 min
  T3 <- (1-b)*rnorm(1,31,2)+b*(31+rexp(1,0.1))#Driving time for the bus
  T4_1 <- rnorm(1,5,2) #Walking time from bus halt is independent
  
  T_1 <- c(T0, T1, T2, T3, T4_1) #Creating vector to store travel time information
  Z_1<-c(Z_1, c(T_1[1],sum(T_1[1:2]),sum(T_1[1:3]),sum(T_1[1:4]),sum(T_1[1:5])))
}

#Simulation Case 2
set.seed(630829)
for (i in 1:n) {
  T0 <- 0 #Start Time when Mr.X is at home
  T1 <-rnorm(1,8,1) #Walking time to the bus stop
  b <- rbinom(1,1,0.03) #3% of the times there is major disturbance in the service
  T2 <- (1-b)*5*runif(1)+b*rexp(1,0.1) #Waiting time at the bus stop given that buses are scheduled, I aim for 5 min
  T3 <- (1-b)*rnorm(1,31,2)+b*(31+rexp(1,0.1))#Driving time for the bus
  T4_2<-rnorm(1,(6-(sum(T1,T2,T3))/50),2) #Walking time from bus halt is dependent
  
  T_2 <- c(T0, T1, T2, T3, T4_2) #Creating vector to store travel time information
  Z_2<-c(Z_2, c(T_2[1],sum(T_2[1:2]),sum(T_2[1:3]),sum(T_2[1:4]),sum(T_2[1:5])))
}

#Create matrix using Z_1 an Z_2 vectors Separately
mat_time_1 <- matrix(Z_1,5)
mat_time_2 <- matrix(Z_2,5)
#a <- data.frame(mat_time_1,row.names = c('T0','T1','T2','T3','T4'))
#View(a)
#b <- data.frame(mat_time_2,row.names = c('T0','T1','T2','T3','T4'))
#View(b)

#Que_1
exp_travel_time_1 <- mean(mat_time_1[5,]) #Case_1
exp_travel_time_2 <- mean(mat_time_2[5,]) #Case_2

#Que_2
time_variation_1 <- sd(mat_time_1[5,]) #Case_1
time_variation_2 <- sd(mat_time_2[5,]) #Case_2

#Que_3
time_1 <- qnorm(0.99, 8, 1)
time_2_3_normal <- qunif(0.99, 0, 5) + qnorm(0.99, 31, 2)
time_2_3_disturbed <- qexp(0.99, 0.1) + (31+qexp(0.99, 0.1))
time_4_1 <- qnorm(0.99, 5, 2)
time_4_2 <- qnorm(0.99, (6-(time_1 + (0.97*time_2_3_normal) + (0.03*time_2_3_disturbed))/50), 2)

time_99_1 <- time_1 + (0.97*time_2_3_normal) + (0.03*time_2_3_disturbed) + time_4_1
# 63.057 min. Start time: 06:56
time_99_2 <- time_1 + (0.97*time_2_3_normal) + (0.03*time_2_3_disturbed) + time_4_2
# 62.989 min. Start time: 06:57

#Alternative intuitive approach
mat_1_sorted <- sort(mat_time_1[5,])
mat_2_sorted <- sort(mat_time_2[5,])
mat_1_sorted[99] # 64.796 
mat_2_sorted[99] # 64.635
#Considering both cases its almost 65 min, Mr.X should start before 06:55

#Que_4
#Case 1
hist(mat_time_1[5,],xlab="Distance Travelled",ylab="Time in Minutes",main = 'Travel Time Distribution')
plot(d,Z_1[1:5],type="l",xlab="Distance Travelled",ylab="Time in Minutes",
     xlim=c(0,25),ylim=c(0,90))
for (j in 1:(n-1)){
  lines(d,Z_1[(5*j+1):(5*(j+1))],col=min(j,100))
}
plot(ecdf(mat_time_1[5,]))

#Case 2
hist(mat_time_2[5,],xlab="Distance Travelled",ylab="Time in Minutes",main = 'Travel Time Distribution')
plot(d,Z_2[1:5],type="l",xlab="Distance Travelled",ylab="Time in Minutes",
     xlim=c(0,25),ylim=c(0,90))
for (j in 1:(n-1)){
  lines(d,Z_2[(5*j+1):(5*(j+1))],col=min(j,100))
}
plot(ecdf(mat_time_2[5,]))
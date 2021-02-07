# 3+3 design
# Clement Ma
# Revised: November 2, 2020

# Inputs

#prior <- NA
target.tox <- 0.2
number.trials <- 10
true.tox<-c(0.05,0.12,0.20,0.30)
arrival.rate<-15
prop.B<-0.1
#target.crm<-1
#min.cohortB<-0 
cycle.length<-28
#cohort.size<-3
#max.N<-18
start.level<-2

########################

###################################################################
# Helper function - provides recommendations for 3+3 design
recommend.3plus3 <- function(current.dose, true.tox, pt) {
  subset.pt <- pt[pt$d == current.dose,]
  num.tox <- sum(subset.pt$dose.tox)
  num.pts <- length(subset.pt$dose.tox)
  
  if (num.pts==3) {
    if (num.tox>=2) {
      recommend.dose <- ifelse(current.dose==1, current.dose, current.dose - 1)
      study.end<-1
    } else if (num.tox == 1) {
      recommend.dose <- current.dose
      study.end<-0
    } else if (num.tox ==0) {
      recommend.dose <- ifelse(current.dose==length(true.tox), current.dose, current.dose + 1)
      study.end<-0
    }
  } else if (num.pts==6) {
    if (num.tox>=2) {
      recommend.dose <- ifelse(current.dose==1, current.dose, current.dose - 1)
      study.end<-1
    } else if (num.tox <= 1) {
      if (current.dose == length(true.tox)) {
        recommend.dose <- current.dose
        study.end <- 1
      } else {
        recommend.dose <- current.dose+1
        study.end <- 0
      }
    }
  }
  return (list(recommend.dose=recommend.dose, study.end=study.end))
}

#################################################################


# Main simulation function
three.plus.three <- function (target.tox, number.trials, true.tox, arrival.rate, prop.B, cycle.length, start.level) {
  
  start <- Sys.time()
  
  # information of interest
  total.patients <- rep(0, number.trials)
  num.cohortA.patients <- rep(0, number.trials)
  num.cohortB.patients <- rep(0, number.trials)
  #num.group1.patients <- rep(0, number.trials)
  #num.group2.patients <- rep(0, number.trials)
  MTD.selection <- rep(0,number.trials)
  study.duration <- rep(0,number.trials)
  
  observe.tox <- mat.or.vec(nr=length(true.tox), nc=number.trials)
  patient.allocation <- mat.or.vec(nr=length(true.tox), nc=number.trials)
  
  for (i in 1:number.trials) {
    # print (c("Trial ", i))
    study.end <- 0
    current.dose <- start.level # current dose level
    timeline.time <- 0
    
    # Enroll first cohort of 3 patients
    PID <- c(1:3)
    inter.time <- rpois(3, arrival.rate)
    arrive.time <- c(timeline.time+inter.time[1], timeline.time+inter.time[1]+inter.time[2], timeline.time+inter.time[1]+inter.time[2]+inter.time[3])
    cohortB <- rbinom(3, 1, prob=prop.B)	
    d <- rep(current.dose, 3)
    
    # Determine toxicities
    dose.tox <- rbinom(3,1,prob=true.tox[d])
    
    end.time <- ifelse(dose.tox==1, arrive.time+runif(n=1,min=0,max=cycle.length), arrive.time+cycle.length)
    timeline.time <- max(end.time)
    
    pt <- data.frame(PID, arrive.time, end.time, cohortB, d, dose.tox)
    
    recommend <- recommend.3plus3 (current.dose, true.tox, pt)
    current.dose <- recommend$recommend.dose
    study.end <- recommend$study.end
    
    # Conducting the trial BEFORE trial end is triggered
    while(study.end == 0) {
      PID <- PID+3
      inter.time <- rpois(3, arrival.rate)
      arrive.time <- c(timeline.time+inter.time[1], timeline.time+inter.time[1]+inter.time[2], timeline.time+inter.time[1]+inter.time[2]+inter.time[3])
      cohortB <- rbinom(3, 1, prob=prop.B)	
      d <- rep(current.dose, 3)
      
      # Determine toxicities
      dose.tox <- rbinom(3,1,prob=true.tox[d])
      end.time <- ifelse(dose.tox==1, arrive.time+runif(n=1,min=0,max=cycle.length), arrive.time+cycle.length)
      timeline.time <- max(end.time)
      
      
      pt <- rbind(pt,data.frame(PID, arrive.time, end.time, cohortB, d, dose.tox))
      
      recommend <- recommend.3plus3 (current.dose, true.tox, pt) 
      current.dose <- recommend$recommend.dose
      study.end <- recommend$study.end
      
    }
    
    # Trial end (no more dose changes)
    pt.subset <- pt[pt$d==current.dose,]
    num.pts <- length(pt.subset$dose.tox)	
    
    if (num.pts==0) {
      PID <- PID+6
      
      inter.time <- rpois(6, arrival.rate)
      arrive.time <- c(timeline.time+inter.time[1], 
                       timeline.time+inter.time[1]+inter.time[2], 
                       timeline.time+inter.time[1]+inter.time[2]+inter.time[3],
                       timeline.time+inter.time[1]+inter.time[2]+inter.time[3]+inter.time[4],
                       timeline.time+inter.time[1]+inter.time[2]+inter.time[3]+inter.time[4]+inter.time[5],
                       timeline.time+inter.time[1]+inter.time[2]+inter.time[3]+inter.time[4]+inter.time[5]+inter.time[6])
      
      cohortB <- rbinom(6, 1, prob=prop.B)	
      d <- rep(current.dose, 6)
      
      # Determine toxicities
      dose.tox <- rbinom(6,1,prob=true.tox[d])
      end.time <- ifelse(dose.tox==1, arrive.time+runif(n=1,min=0,max=cycle.length), arrive.time+cycle.length)
      timeline.time <- max(end.time)
      
      
      if (sum(dose.tox[1:3])<=1) {
        pt <- rbind(pt,data.frame(PID, arrive.time, end.time, cohortB, d, dose.tox))
      } else if (sum(dose.tox[1:3])>=2) {
        pt <- rbind(pt,data.frame(PID, arrive.time, end.time, cohortB, d, dose.tox)[1:3,])
      }
    } else if (num.pts==3) {
      PID <- PID+3
      inter.time <- rpois(3, arrival.rate)
      arrive.time <- c(timeline.time+inter.time[1], timeline.time+inter.time[1]+inter.time[2], timeline.time+inter.time[1]+inter.time[2]+inter.time[3])
      cohortB <- rbinom(3, 1, prob=prop.B)	
      d <- rep(current.dose, 3)
      
      # Determine toxicities
      dose.tox <- rbinom(3,1,prob=true.tox[d])
      end.time <- ifelse(dose.tox==1, arrive.time+runif(n=1,min=0,max=cycle.length), arrive.time+cycle.length)
      timeline.time <- max(end.time)
      
      pt <- rbind(pt,data.frame(PID, arrive.time, end.time, cohortB, d, dose.tox))
    }
    
    # Declare MTD
    
    MTD.selection[i] <- current.dose
    total.patients[i] <- length(pt$PID)
    num.cohortA.patients[i] <- length(pt$PID[pt$cohortB==0])
    num.cohortB.patients[i] <- length(pt$PID[pt$cohortB==1])
    study.duration[i] <- timeline.time	
    
    for (j in 1:length(true.tox)){ 
      patient.allocation[j,i] <- length(pt$d[pt$d==j])
      observe.tox[j,i] <- length(pt$d[pt$d==j & pt$dose.tox==1])
    }
  }
  
  # Calculate summary statistics
  MTD.selection.table <- table(MTD.selection)
  true.MTD <- which.min(round(abs(target.tox-true.tox),10))
  PCS <- MTD.selection.table[true.MTD] / sum(MTD.selection.table)
  obs.tox.overall <- sum(observe.tox)/sum(patient.allocation)
  mean.obs.N <- mean(colSums(patient.allocation))
  min.obs.N <- min(colSums(patient.allocation))
  max.obs.N <- max(colSums(patient.allocation))
  
  patient.allocation.table <- rowSums(patient.allocation)/sum(patient.allocation)
  
  obs.tox.table <- rowSums(observe.tox)/sum(patient.allocation)
  mean.duration = mean(study.duration)
  sd.duration = sd(study.duration)
  
  df <- data.frame("design"="3+3", "true.tox"=true.tox, "trueMTD"=true.MTD, "MTD"=MTD.selection.table,"patient.allocation.table"=patient.allocation.table, "obs.tox.table"=obs.tox.table)
  
  finish <- Sys.time()
  time.taken <- finish - start
  
  result <- list(df=df, target.tox=target.tox, number.trials=number.trials, true.tox=true.tox, arrival.rate=arrival.rate, 
               prop.B=prop.B, cycle.length=cycle.length, start.level=start.level, 
               
               total.patients=total.patients, num.cohortA.patients=num.cohortA.patients, num.cohortB.patients=num.cohortB.patients,
               MTD.selection=MTD.selection, study.duration=study.duration, observe.tox=observe.tox, patient.allocation=patient.allocation,
               
               MTD.selection.table = MTD.selection.table, true.MTD=true.MTD, PCS=PCS, obs.tox.overall=obs.tox.overall,
               mean.obs.N=mean.obs.N, min.obs.N=min.obs.N, max.obs.N=max.obs.N,
               patient.allocation.table=patient.allocation.table, obs.tox.table=obs.tox.table,
               mean.duration=mean.duration, sd.duration=sd.duration, time.taken=time.taken)
  return(result)
  
}


###################################
# tptOut <- three.plus.three(target.tox=0.2, number.trials=1000, true.tox=c(0.05,0.12,0.20,0.30), arrival.rate=15, prop.B=0.1, cycle.length=28, start.level=2)
# 3+3 design
# Clement Ma
# Revised: November 2, 2020

###################################################################
# Helper function - provides recommendations for 3+3 design
recommend_3_plus_3 <- function(current_dose, true_tox, pt) {
  subset_pt <- pt[pt$d == current_dose,]
  num_tox <- sum(subset_pt$dose_tox)
  num_pts <- length(subset_pt$dose_tox)
  
  if (num_pts==3) {
    if (num_tox>=2) {
      recommend_dose <- ifelse(current_dose==1, current_dose, current_dose - 1)
      study_end<-1
    } else if (num_tox == 1) {
      recommend_dose <- current_dose
      study_end<-0
    } else if (num_tox ==0) {
      recommend_dose <- ifelse(current_dose==length(true_tox), current_dose, current_dose + 1)
      study_end<-0
    }
  } else if (num_pts==6) {
    if (num_tox>=2) {
      recommend_dose <- ifelse(current_dose==1, current_dose, current_dose - 1)
      study_end<-1
    } else if (num_tox <= 1) {
      if (current_dose == length(true_tox)) {
        recommend_dose <- current_dose
        study_end <- 1
      } else {
        recommend_dose <- current_dose+1
        study_end <- 0
      }
    }
  }
  return (list(recommend_dose=recommend_dose, study_end=study_end))
}

#################################################################


# Main simulation function
three_plus_three <- function (target_tox, number_trials, true_tox, arrival_rate, prop_b=0, cycle_length, start_level) {
  
  start <- Sys.time()
  
  # information of interest
  total_patients <- rep(0, number_trials)
  num_cohort_a_patients <- rep(0, number_trials)
  num_cohort_b_patients <- rep(0, number_trials)
  #num_group_1_patients <- rep(0, number_trials)
  #num_group_2_patients <- rep(0, number_trials)
  mtd_selection <- rep(0,number_trials)
  study_duration <- rep(0,number_trials)
  
  observe_tox <- mat.or.vec(nr=length(true_tox), nc=number_trials)
  patient_allocation <- mat.or.vec(nr=length(true_tox), nc=number_trials)
  
  for (i in 1:number_trials) {
    # print (c("Trial ", i))
    study_end <- 0
    current_dose <- start_level # current dose level
    timeline_time <- 0
    
    # Enroll first cohort of 3 patients
    PID <- c(1:3)
    inter_time <- rpois(3, arrival_rate)
    arrive_time <- c(timeline_time+inter_time[1], timeline_time+inter_time[1]+inter_time[2], timeline_time+inter_time[1]+inter_time[2]+inter_time[3])
    cohort_b <- rbinom(3, 1, prob=prop_b)	
    d <- rep(current_dose, 3)
    
    # Determine toxicities
    dose_tox <- rbinom(3,1,prob=true_tox[d])
    
    end_time <- ifelse(dose_tox==1, arrive_time+runif(n=1,min=0,max=cycle_length), arrive_time+cycle_length)
    timeline_time <- max(end_time)
    
    pt <- data.frame(PID, arrive_time, end_time, cohort_b, d, dose_tox)
    
    recommend <- recommend_3_plus_3 (current_dose, true_tox, pt)
    current_dose <- recommend$recommend_dose
    study_end <- recommend$study_end
    
    # Conducting the trial BEFORE trial end is triggered
    while(study_end == 0) {
      PID <- PID+3
      inter_time <- rpois(3, arrival_rate)
      arrive_time <- c(timeline_time+inter_time[1], timeline_time+inter_time[1]+inter_time[2], timeline_time+inter_time[1]+inter_time[2]+inter_time[3])
      cohort_b <- rbinom(3, 1, prob=prop_b)	
      d <- rep(current_dose, 3)
      
      # Determine toxicities
      dose_tox <- rbinom(3,1,prob=true_tox[d])
      end_time <- ifelse(dose_tox==1, arrive_time+runif(n=1,min=0,max=cycle_length), arrive_time+cycle_length)
      timeline_time <- max(end_time)
      
      
      pt <- rbind(pt,data.frame(PID, arrive_time, end_time, cohort_b, d, dose_tox))
      
      recommend <- recommend_3_plus_3 (current_dose, true_tox, pt) 
      current_dose <- recommend$recommend_dose
      study_end <- recommend$study_end
      
    }
    
    # Trial end (no more dose changes)
    pt_subset <- pt[pt$d==current_dose,]
    num_pts <- length(pt_subset$dose_tox)	
    
    if (num_pts==0) {
      PID <- PID+6
      
      inter_time <- rpois(6, arrival_rate)
      arrive_time <- c(timeline_time+inter_time[1], 
                       timeline_time+inter_time[1]+inter_time[2], 
                       timeline_time+inter_time[1]+inter_time[2]+inter_time[3],
                       timeline_time+inter_time[1]+inter_time[2]+inter_time[3]+inter_time[4],
                       timeline_time+inter_time[1]+inter_time[2]+inter_time[3]+inter_time[4]+inter_time[5],
                       timeline_time+inter_time[1]+inter_time[2]+inter_time[3]+inter_time[4]+inter_time[5]+inter_time[6])
      
      cohort_b <- rbinom(6, 1, prob=prop_b)	
      d <- rep(current_dose, 6)
      
      # Determine toxicities
      dose_tox <- rbinom(6,1,prob=true_tox[d])
      end_time <- ifelse(dose_tox==1, arrive_time+runif(n=1,min=0,max=cycle_length), arrive_time+cycle_length)
      timeline_time <- max(end_time)
      
      
      if (sum(dose_tox[1:3])<=1) {
        pt <- rbind(pt,data.frame(PID, arrive_time, end_time, cohort_b, d, dose_tox))
      } else if (sum(dose_tox[1:3])>=2) {
        pt <- rbind(pt,data.frame(PID, arrive_time, end_time, cohort_b, d, dose_tox)[1:3,])
      }
    } else if (num_pts==3) {
      PID <- PID+3
      inter_time <- rpois(3, arrival_rate)
      arrive_time <- c(timeline_time+inter_time[1], timeline_time+inter_time[1]+inter_time[2], timeline_time+inter_time[1]+inter_time[2]+inter_time[3])
      cohort_b <- rbinom(3, 1, prob=prop_b)	
      d <- rep(current_dose, 3)
      
      # Determine toxicities
      dose_tox <- rbinom(3,1,prob=true_tox[d])
      end_time <- ifelse(dose_tox==1, arrive_time+runif(n=1,min=0,max=cycle_length), arrive_time+cycle_length)
      timeline_time <- max(end_time)
      
      pt <- rbind(pt,data.frame(PID, arrive_time, end_time, cohort_b, d, dose_tox))
    }
    
    # Declare mtd
    
    mtd_selection[i] <- current_dose
    total_patients[i] <- length(pt$PID)
    num_cohort_a_patients[i] <- length(pt$PID[pt$cohort_b==0])
    num_cohort_b_patients[i] <- length(pt$PID[pt$cohort_b==1])
    study_duration[i] <- timeline_time	
    
    for (j in 1:length(true_tox)){ 
      patient_allocation[j,i] <- length(pt$d[pt$d==j])
      observe_tox[j,i] <- length(pt$d[pt$d==j & pt$dose_tox==1])
    }
  }
  
  # Calculate summary statistics
  mtd_selection_table <- table(mtd_selection)
  true_mtd <- which.min(round(abs(target_tox-true_tox),10))
  pcs <- mtd_selection_table[true_mtd] / sum(mtd_selection_table)
  obs_tox_overall <- sum(observe_tox)/sum(patient_allocation)
  mean_obs_n <- mean(colSums(patient_allocation))
  min_obs_n <- min(colSums(patient_allocation))
  max_obs_n <- max(colSums(patient_allocation))
  
  patient_allocation_table <- rowSums(patient_allocation)/sum(patient_allocation)
  
  obs_tox_table <- rowSums(observe_tox)/sum(patient_allocation)
  mean_cohort_b <- 0
  sd_cohort_b <- 0
  mean_duration = mean(study_duration)
  sd_duration = sd(study_duration)
  
  df <- data.frame("design"="3+3", "true_tox"=true_tox, "true_mtd"=true_mtd, "mtd"=mtd_selection_table,"patient_allocation_table"=patient_allocation_table, "obs_tox_table"=obs_tox_table)
  
  finish <- Sys.time()
  time_taken <- finish - start
  
  result <- list(df=df, target_tox=target_tox, number_trials=number_trials, true_tox=true_tox, arrival_rate=arrival_rate, 
                 prop_b=prop_b, cycle_length=cycle_length, start_level=start_level, 
                 
                 total_patients=total_patients, num_cohort_a_patients=num_cohort_a_patients, num_cohort_b_patients=num_cohort_b_patients,
                 mtd_selection=mtd_selection, study_duration=study_duration, observe_tox=observe_tox, patient_allocation=patient_allocation,
                 
                 mtd_selection_table = mtd_selection_table, true_mtd=true_mtd, pcs=pcs, obs_tox_overall=obs_tox_overall,
                 mean_obs_n=mean_obs_n, min_obs_n=min_obs_n, max_obs_n=max_obs_n,
                 patient_allocation_table=patient_allocation_table, obs_tox_table=obs_tox_table, mean_cohort_b=mean_cohort_b, sd_cohort_b=sd_cohort_b,
                 mean_duration=mean_duration, sd_duration=sd_duration, time_taken=time_taken)
  return(result)
  
}


# Example: ----------------
# three_plus_three_result <- three_plus_three(target_tox=0.2, number_trials=1000, true_tox=c(0.05,0.12,0.20,0.30), arrival_rate=15, prop_b=0.1, cycle_length=28, start_level=2)
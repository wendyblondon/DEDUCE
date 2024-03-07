library(BOIN)

my_boin <- function(prior, target_tox, number_trials, true_tox, arrival_rate, 
                    prop_b=0, min_cohort_b=0, cycle_length, cohort_size, max_n, 
                    start_level) {
  start <- Sys.time()
  
  # information of interest
  total_patients <- rep(0, number_trials)
  #num_cohort_a_patients <- rep(0, number_trials)
  #num_cohort_b_patients <- rep(0, number_trials)
  #num_group_1_patients <- rep(0, number_trials)
  #num_group_2_patients <- rep(0, number_trials)
  mtd_selection <- rep(0,number_trials)
  study_duration <- rep(0,number_trials)
  #mtd_count <- rep(0, length(true_tox))
  #result_num_dose_changes <- rep(0,number_trials)
  
  observe_tox <- mat.or.vec(nr=length(true_tox), nc=number_trials)
  patient_allocation <- mat.or.vec(nr=length(true_tox), nc=number_trials)
  
  ##############################################
  # Hard coded target_crm parameter = 0.
  target_crm <- 0
  ##############################################
  
  start_time <- proc.time()
  
  # Run the BOIN table first. Only need to do this one time.
  BOINtable <- get.boundary(target=target_tox, ncohort=max_n/cohort_size, 
                            cohortsize=cohort_size, n.earlystop = 100, 
                            p.saf = 0.6*target_tox, p.tox = 1.4*target_tox, 
                            cutoff.eli = 0.95, extrasafe = FALSE, offset = 0.05)
  
  #Function to recommend the dose for the next cohort
  runBOIN <- function(BOINtable, pt, num_dose) {
    num_treated <- length(pt$d[pt$d==current_dose]) #how many patients treated at current dose 
    num_DLT <- sum(pt$dose_tox[pt$d==current_dose]==1) #how many DLTs under current dose 
    
    if(cohort_size==1) {
      BOIN_col <- BOINtable$boundary_tab[, which(BOINtable$boundary_tab[1,]==num_treated)] #select the column for num_treated
    } else {
      BOIN_col <- BOINtable$full_boundary_tab[, which(BOINtable$full_boundary_tab[1,]==num_treated)] #select the column for num_treated
    }
    
    if(num_treated<=2) {
      if(num_DLT <= BOIN_col[2]) { #Escalate        
        current_dose = current_dose+1           
        # Check if dose is eliminated 
        if (elim.dose!=0) {        
          current_dose = current_dose-1        
          #print(c("Unable to escalate because dose is eliminated", elim.dose))
        }
        # Check if already at the highest dose
        if (current_dose > num_dose) {
          current_dose = current_dose-1 
          #print(c("Unable to escalate because already at the highest dose level: ", num_dose))
        }
        d = current_dose
        
        study_end=0
      } else if (num_DLT >= BOIN_col[3]) { #De-escalate   
        current_dose = current_dose-1 
        d = current_dose
        study_end=0
      } else { #Stay   
        current_dose = current_dose
        d = current_dose
        study_end=0
      }
      # Check if the study should end. The study should end if the lowest dose level is eliminated
      if (d == 0) { 
        study_end=1 
        d = NA
        #print(c("The lowest dose level is eliminated."))
      }
      #print(c("Recommended dose:", d))
      return(list(d=d, study_end=study_end, elim.dose=elim.dose))
    } else {
      if(num_DLT <= BOIN_col[2]) { #Escalate        
        current_dose = current_dose+1           
        # Check if dose is eliminated 
        if (elim.dose!=0) {        
          current_dose = current_dose-1        
          #print(c("Unable to escalate because dose is eliminated", elim.dose))
        }
        # Check if already at the highest dose
        if (current_dose > num_dose) {
          current_dose = current_dose-1 
          #print(c("Unable to escalate because already at the highest dose level: ", num_dose))
        }
        d = current_dose
        
        study_end=0
      } else if (num_DLT >= BOIN_col[3] & num_DLT < BOIN_col[4]) { #De-escalate   
        current_dose = current_dose-1 
        d = current_dose
        study_end=0
      } else if (num_DLT >= BOIN_col[4]) { #Eliminate  
        elim.dose = current_dose ## Keep track that the current dose level is eliminated
        #print(c("Eliminated dose", elim.dose))
        current_dose = current_dose-1
        d = current_dose
        study_end=0
      } else { #Stay   
        current_dose = current_dose
        d = current_dose
        study_end=0
      }
      # Check if the study should end. The study should end if the lowest dose level is eliminated
      if (d == 0) { 
        study_end=1 
        d = NA
        #print(c("The lowest dose level is eliminated."))
      }
      #print(c("Recommended dose:", d))
      return(list(d=d, study_end=study_end, elim.dose=elim.dose))
    }
  }
  
  
  for (i in 1:number_trials) {
    #print (c("Trial ", i))
    
    # Enroll first cohort_size patients
    current_dose <- start_level # current dose level
    pid <- 1:cohort_size
    d <- rep(current_dose, cohort_size) #set the starting dose
    # Determine toxicities
    dose_tox <- rbinom(cohort_size,1,prob=true_tox[d[1]])
    timeline_time <- 0
    arrive_time <- rep(0, cohort_size)
    end_time <- rep(0, cohort_size)
    for (k in 1:cohort_size) {
      arrive_time[k] <- timeline_time + rpois(1, arrival_rate)
      timeline_time <- arrive_time[k]
      if(dose_tox[k]==1) {
        end_time[k] <- timeline_time + runif(n=1, min=0, max=cycle_length)
      } else {
        end_time[k] <- timeline_time + cycle_length
      }
    }
    include <- rep(1, cohort_size)
    elim.dose <- 0 # set eliminated dose level to be 0
    pt <- data.frame (pid, arrive_time, d, dose_tox, end_time, include)
    
    # Update counts
    observe_tox[d,i] <- observe_tox[d,i]+dose_tox
    patient_allocation[d,i] <- patient_allocation[d,i]+cohort_size
    
    # Find out the recommend dose after the first cohort of patients
    runBOIN_results <- runBOIN(BOINtable, pt, length(true_tox))
    study_end <- runBOIN_results$study_end
    elim.dose <- runBOIN_results$elim.dose
    current_dose <- runBOIN_results$d
    d <- current_dose
    
    # p = 2:ncohort (counts number of cohorts)
    # j = 1:cohort size (counts each patient within one cohort)
    if (study_end != 1) {
      for (p in 2:(max_n/cohort_size)) {     
        for (j in 1:cohort_size) { # Simulate X patients within a cohort       
          pid <- (p-1)*cohort_size+j       
          dose_tox <- rbinom(1,1,prob=true_tox[d])  
          include <- 1
          arrive_time <- timeline_time + rpois(1, arrival_rate)
          timeline_time <- arrive_time
          if(dose_tox==1) {
            end_time <- timeline_time + runif(n=1, min=0, max=cycle_length)
          } else {
            end_time <- timeline_time + cycle_length
          }
          pt <- rbind(pt, c(pid, arrive_time, d, dose_tox, end_time, include))
          
          # Update counts
          observe_tox[d,i] <- observe_tox[d,i]+dose_tox
          patient_allocation[d,i] <- patient_allocation[d,i]+1
          
        }
        
        # Just completed a cohort, need to find out the next recommended dose level
        runBOIN_results <- runBOIN(BOINtable, pt, length(true_tox))
        study_end <- runBOIN_results$study_end
        elim.dose <- runBOIN_results$elim.dose
        current_dose <- runBOIN_results$d
        d <- current_dose
        if (study_end ==1) { 
          #print(c("Study end", study_end))
          break 
        } # Break out of the for loop if the study_end indicator is 1
      }
    }
    
    mtd_selection[i] <- runBOIN_results$d
    total_patients[i] <- length(pt$pid)
    study_duration[i] <- timeline_time
    # for (x in 1:length(true_tox)) {
    #   mtd_count[x] <- sum(mtd_selection == x, na.rm = TRUE)
    # }
    # num_cohort_a_patients[i] <- length(pt$pid[pt$cohort_b==0])
    # num_cohort_b_patients[i] <- length(pt$pid[pt$cohort_b==1])
    # num_group_1_patients[i] <- length(pt$pid[pt$group==1])
    # num_group_2_patients[i] <- length(pt$pid[pt$group==2])
    # result_num_dose_changes[i] <- num_dose_changes
    
  }
  
  mtd_selection_table <- table(factor(mtd_selection, levels = 1:length(true_tox)))
  true_mtd <- which.min(round(abs(target_tox-true_tox),10))
  pcs <- mtd_selection_table[true_mtd] / sum(mtd_selection_table)
  obs_tox_overall <- sum(observe_tox)/sum(patient_allocation)
  mean_obs_n <- mean(colSums(patient_allocation))
  min_obs_n <- min(colSums(patient_allocation))
  max_obs_n <- max(colSums(patient_allocation))
  
  patient_allocation_table <- rowSums(patient_allocation)/sum(patient_allocation)
  obs_tox_table <- rowSums(observe_tox)/sum(patient_allocation)
  
  mean_cohort_b = NA
  sd_cohort_b = NA
  mean_duration = mean(study_duration)
  sd_duration = sd(study_duration)
  median_duration = median(study_duration)
  q1_duration = quantile(study_duration,0.25)
  q3_duration = quantile(study_duration, 0.75)
  
  # print(c("true_tox:", true_tox))
  # print(c("true_mtd:", true_mtd))
  # print(c("mtd:", mtd_selection_table))
  # print(c("patient_allocation_table:", patient_allocation_table))
  # print(c("obs_tox_table:", obs_tox_table))
  # print(c("patient table:", pt))
  
  df <- data.frame("design"="BOIN", "true_tox"=true_tox, "true_mtd"=true_mtd, 
                   "mtd"=mtd_selection_table, "patient_allocation_table"=patient_allocation_table, 
                   "obs_tox_table"=obs_tox_table)
  
  finish <- Sys.time()
  time_taken <- finish - start
  
  ############ Removed "target_crm" from "results" list.
  
  result <- list(df=df, prior=prior, target_tox=target_tox, number_trials=number_trials, true_tox=true_tox, arrival_rate=arrival_rate, 
                 prop_b=prop_b, min_cohort_b=min_cohort_b, cycle_length=cycle_length, cohort_size=cohort_size, max_n=max_n, start_level=start_level, 
                 total_patients=total_patients, num_cohort_a_patients=NA, num_cohort_b_patients=NA,
                 num_group_1_patients=NA, num_group_2_patients=NA, results_num_dose_changes=NA, mtd_selection=mtd_selection,
                 study_duration=study_duration, observe_tox=observe_tox, patient_allocation=patient_allocation, mean_obs_n=mean_obs_n, min_obs_n=min_obs_n, max_obs_n=max_obs_n,
                 
                 mtd_selection_table = mtd_selection_table, true_mtd=true_mtd, pcs=pcs, obs_tox_overall=obs_tox_overall, patient_allocation_table=patient_allocation_table, obs_tox_table=obs_tox_table,
                 mean_cohort_b=NA, sd_cohort_b=NA, mean_duration=mean_duration, sd_duration=sd_duration, time_taken=time_taken,
                 pt=pt, BOINtable=BOINtable, median_duration=median_duration, q1_duration=q1_duration, q3_duration=q3_duration)
  return(result)
}


# # Example: -------------------------------------
# 
boin_result <- my_boin(prior=c(0.05,0.1,0.2,0.3), target_tox=0.2, number_trials=100,
                       true_tox=c(0.05,0.12,0.20,0.30), arrival_rate=15,
                       cycle_length=28, cohort_size=2, max_n=100, start_level=3)

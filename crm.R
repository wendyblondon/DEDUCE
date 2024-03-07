# Simulation program for novel design
# Clement Ma
# Febrary 24, 2018
# Modified March 4, 2018
# Modified July 3, 2018
# Modified August 31, 2018 - Require full cohort of X patients to complete before proceeding to next cohort.
# Modified January 28, 2019
# Modified May 15, 2019
# Modified September 21, 2020 - for DELPHI app
# Modified November 3, 2020 - for DELPHI app


library(dfcrm)

#####################
# CRM FUNCTION: requires cohort size=3 regardless of intra-patient de-escalation
# Jan 28, 2019: include indicator "target_crm" 
	# target_crm=0: NO enrollment of patients at one dose below
	# target_crm=1: Enrollment of patients at one dose below
	# target_crm=2: Enrollment of patients at current best dose based on available information, cannot be higher than current dose

# May 15, 2019: stratified design "min_cohort_b" - require minimum number of cohort B patients

# Jan 22, 2021: New CRM function based on TARGET-CRM function. Hard coded target_crm parameter = 0.


my_crm <- function(prior, target_tox, number_trials, true_tox, arrival_rate, prop_b=0, min_cohort_b=0, cycle_length, cohort_size, max_n, start_level) {
  start <- Sys.time()
  
# information of interest
total_patients <- rep(0, number_trials)
num_cohort_a_patients <- rep(0, number_trials)
num_cohort_b_patients <- rep(0, number_trials)
num_group_1_patients <- rep(0, number_trials)
num_group_2_patients <- rep(0, number_trials)
mtd_selection <- rep(0,number_trials)
study_duration <- rep(0,number_trials)
result_num_dose_changes <- rep(0,number_trials)

observe_tox <- mat.or.vec(nr=length(true_tox), nc=number_trials)
patient_allocation <- mat.or.vec(nr=length(true_tox), nc=number_trials)

##############################################
# Hard coded target_crm parameter = 0.
target_crm <- 0
##############################################

start_time <- proc.time()
for (i in 1:number_trials) {
	#print (c("Trial ", i))
	# Enroll first patient
	num_slots <- cohort_size # counter to be in the main cohort
	num_waiting <- 0 # counter for number of cohort A patients waiting
	wait_cohort <- numeric(0) # array for the cohort of patients in wait list
	study_end <- 0 # flag to end study
	current_dose <- start_level # current dose level
	timeline_time <- 0 # overall study timeline
	num_dose_changes <- 0
	pid <- 1
	#print(pid)
	arrive_time <- timeline_time + rpois(1, arrival_rate)
	timeline_time <- arrive_time
	cohort_b <- rbinom(1, 1, prob=prop_b)
	d <- current_dose
	# Determine toxicities
	dose_tox <- rbinom(1,1,prob=true_tox[d])
	if(dose_tox==1) {
		end_time <- timeline_time + runif(n=1, min=0, max=cycle_length)
	} else {
		end_time <- timeline_time + cycle_length
	}
	
	include <- 0
	group <- 1
	wait_list <- 0
	prev_cohort_end_time <- 0
	num_slots <- num_slots - 1 # Used up one slot
	pt <- data.frame (pid, arrive_time, cohort_b, d, dose_tox, end_time, group, wait_list, include)
	
	# Update counts
	observe_tox[d,i] <- observe_tox[d,i]+dose_tox
	patient_allocation[d,i] <- patient_allocation[d,i]+1
	
	while (study_end == 0 | length(pt$pid[pt$cohort_b==1]) < min_cohort_b) { # NEW require at least min_cohort_b patients
		# Patient arrives
		#print(pid)
		#print(pt)
		#print(timeline_time)

		if (study_end == 0) {
			pid <- pid+1
			arrive_time <- timeline_time + rpois(1, arrival_rate)
			timeline_time <- arrive_time
			cohort_b <- rbinom(1, 1, prob=prop_b)
		} else if (study_end == 1) { # reached study end but not sufficient cohort B patients
			# print("study end == 1 but not sufficient cohort B patients")
			# sample patients until a cohort B patient arrives
			repeat {
				#print(pid)
				pid <- pid+1
				arrive_time <- timeline_time + rpois(1, arrival_rate)
				timeline_time <- arrive_time
				cohort_b <- rbinom(1, 1, prob=prop_b)
				if (cohort_b == 1) {
					break
				}
			}
		}			

		# Update which patients to include
		pt$include <- ifelse(pt$end_time <= timeline_time, 1, 0) # Modified 8/31/2018 to "<="
	
		# Check if new cohort can be enrolled [only include main cohort timeline]
		if (num_slots == 0 & timeline_time > tail(pt$end_time[pt$group==1],n=1)) {
			num_slots <- cohort_size
		}
	
		if (num_slots == cohort_size) { # start of main cohort
			wait_list <- 0
			# Check for waiting list patients
			if (num_waiting > 0) {
				#print ("Enrolling waitlist patient")
				arrive_time <- prev_cohort_end_time
				timeline_time <- arrive_time
				cohort_b <- wait_cohort[1] # take first wait list patient
				wait_cohort <- wait_cohort[-1] # remove from waitlist
				# Update which patients to include
				pt$include <- ifelse(pt$end_time <= timeline_time, 1, 0) # Modified 8/31/2018 to "<="
				wait_list <- 1
				num_waiting <- num_waiting - 1
			}
	
			if (length(pt$pid[pt$include==1])==0) { # no completed observations yet
				d <- current_dose
			} else {
				run_crm <- crm(prior=prior, target=target_tox, tox=pt$dose_tox, level=pt$d, include=which(pt$include==1))
				#print(c("CRM dose", run_crm$mtd))
				if (run_crm$mtd > current_dose & current_dose < length(prior)) {
					current_dose <- current_dose + 1 # Can only escalate one dose higher
					d <- current_dose
					num_dose_changes <- num_dose_changes+1
				} else {
					if (run_crm$mtd < current_dose) {
						num_dose_changes <- num_dose_changes+1
					}
					current_dose <- run_crm$mtd
					d <- current_dose
					##num_slots <- cohort_size # NEW: if de-escalate, then reset num_slots to cohort_size
				}
				#print(c("Recommended dose", current_dose))
			}
			# Determine toxicities
			dose_tox <- rbinom(1,1,prob=true_tox[d])
			if(dose_tox==1) {
				end_time <- timeline_time + runif(n=1, min=0, max=cycle_length)
			} else {
				end_time <- timeline_time + cycle_length
			}

			group <- 1
			num_slots <- num_slots - 1 # Used up one slot
	
			# Add to pt tracker matrix
			pt <- rbind(pt, c(pid, arrive_time, cohort_b, d, dose_tox, end_time, group, wait_list, include))
			
			# Update counts
			observe_tox[d,i] <- observe_tox[d,i]+dose_tox
			patient_allocation[d,i] <- patient_allocation[d,i]+1
	
		} else if (num_slots > 0 & num_slots < cohort_size) { # enroll in main cohort
			wait_list <- 0
			# Check for waiting list patients
			if (num_waiting > 0) {
				#print ("Enrolling waitlist patient")
				arrive_time <- prev_cohort_end_time
				timeline_time <- arrive_time
				cohort_b <- wait_cohort[1] # take first wait list patient
				wait_cohort <- wait_cohort[-1] # remove from waitlist
				# Update which patients to include
				pt$include <- ifelse(pt$end_time <= timeline_time, 1, 0) # Modified 8/31/2018 to "<="
				wait_list <- 1
				num_waiting <- num_waiting - 1
			}

			if (length(pt$pid[pt$include==1])==0) { # no completed observations yet
				d <- current_dose
			} else {
				# Calculate recommended dose
				run_crm <- crm(prior=prior, target=target_tox, tox=pt$dose_tox, level=pt$d, include=which(pt$include==1))
				if (run_crm$mtd < current_dose) { # Within a cohort, can only stay at same dose or de-escalate
					current_dose <- run_crm$mtd
					#print(c("CRM dose", current_dose))
					d <- current_dose
					num_dose_changes <- num_dose_changes+1
					##num_slots <- cohort_size # NEW: if de-escalate, then reset num_slots to cohort_size
				} else {
					d <- current_dose
				}
				#print(c("Recommended dose", current_dose))
			}
			# Determine toxicities
			dose_tox <- rbinom(1,1,prob=true_tox[d])
			if(dose_tox==1) {
				end_time <- timeline_time + runif(n=1, min=0, max=cycle_length)
			} else {
				end_time <- timeline_time + cycle_length
			}
	
			group <- 1
			num_slots <- num_slots - 1 # Used up one slot
	
			# Add to pt tracker matrix
			pt <- rbind(pt, c(pid, arrive_time, cohort_b, d, dose_tox, end_time, group, wait_list, include))
	
			# Update counts
			observe_tox[d,i] <- observe_tox[d,i]+dose_tox
			patient_allocation[d,i] <- patient_allocation[d,i]+1
	
		} else if (num_slots == 0) { # cohort B patients enrolled on one dose below current
			# Capture previous cohort end time
			prev_cohort_end_time <- tail(pt$end_time[pt$group==1],n=1)
	
			 	# YES capture Cohort B patients at one dose below
				if (cohort_b == 1 & current_dose > 1 & target_crm==1) {
					d <- current_dose-1

					# Determine toxicities
					dose_tox <- rbinom(1,1,prob=true_tox[d])
					if(dose_tox==1) {
						end_time <- timeline_time + runif(n=1, min=0, max=cycle_length)
					} else {
						end_time <- timeline_time + cycle_length
					}
					group <- 2
					wait_list <- 0
					# Add to pt tracker matrix
					pt <- rbind(pt, c(pid, arrive_time, cohort_b, d, dose_tox, end_time, group, wait_list, include))
		
					# Update counts
					observe_tox[d,i] <- observe_tox[d,i]+dose_tox
					patient_allocation[d,i] <- patient_allocation[d,i]+1
			 	# YES capture Cohort B patients at current recommended dose
				} else if (cohort_b == 1 & target_crm==2) { # NEW: allow capture of Cohort B patients even at lowest dose level
					d <- current_dose

					# Determine toxicities
					dose_tox <- rbinom(1,1,prob=true_tox[d])
					if(dose_tox==1) {
						end_time <- timeline_time + runif(n=1, min=0, max=cycle_length)
					} else {
						end_time <- timeline_time + cycle_length
					}
					group <- 2
					wait_list <- 0
					# Add to pt tracker matrix
					pt <- rbind(pt, c(pid, arrive_time, cohort_b, d, dose_tox, end_time, group, wait_list, include))
		
					# Update counts
					observe_tox[d,i] <- observe_tox[d,i]+dose_tox
					patient_allocation[d,i] <- patient_allocation[d,i]+1

				} else { # patients added to waiting list with 50% chance of actual enrollment
					if(rbinom(1,1,0.5)==1) {
						num_waiting <- num_waiting + 1
						wait_cohort <- cbind(wait_cohort, cohort_b) # Capture which cohort it belongs to
						#print (c("Adding patient to waitlist: ", pid))
						#print (c("waitlist cohort: ", wait_cohort))
					}
				}

		} else {
			print (c("Error, number of slots is", numslots))
			break
		}

		# Check for study end
		if (length(pt$pid)==max_n) { 
			study_end<-1 
			# Study end, finish observing remaining patients
			timeline_time <- max(pt$end_time)
			pt$include <- 1
			run_crm_final <- crm(prior=prior, target=target_tox, tox=pt$dose_tox, level=pt$d, include=which(pt$include==1))
			#print(pt)
			#print (c("mtd is", run_crm_final$mtd))
			mtd_selection[i] <- run_crm_final$mtd
			total_patients[i] <- length(pt$pid)
			num_cohort_a_patients[i] <- length(pt$pid[pt$cohort_b==0])
			num_cohort_b_patients[i] <- length(pt$pid[pt$cohort_b==1])
			num_group_1_patients[i] <- length(pt$pid[pt$group==1])
			num_group_2_patients[i] <- length(pt$pid[pt$group==2])
			study_duration[i] <- timeline_time
			result_num_dose_changes[i] <- num_dose_changes
		}
	}
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

mean_cohort_b = mean(num_group_2_patients)
sd_cohort_b = sd(num_group_2_patients)
mean_duration = mean(study_duration)
sd_duration = sd(study_duration)
median_duration = median(study_duration)
q1_duration = quantile(study_duration,0.25)
q3_duration = quantile(study_duration, 0.75)

df <- data.frame("design"="CRM", "true_tox"=true_tox, "true_mtd"=true_mtd, 
                 "mtd"=mtd_selection_table, 
                 "patient_allocation_table"=patient_allocation_table, 
                 "obs_tox_table"=obs_tox_table)

finish <- Sys.time()
time_taken <- finish - start

############ Removed "target_crm" from "results" list.

result <- list(df=df, prior=prior, target_tox=target_tox, number_trials=number_trials, true_tox=true_tox, arrival_rate=arrival_rate, 
prop_b=prop_b, min_cohort_b=min_cohort_b, cycle_length=cycle_length, cohort_size=cohort_size, max_n=max_n, start_level=start_level, 
total_patients=total_patients, num_cohort_a_patients=num_cohort_a_patients, num_cohort_b_patients=num_cohort_b_patients,
num_group_1_patients=num_group_1_patients, num_group_2_patients=num_group_2_patients, results_num_dose_changes=result_num_dose_changes, mtd_selection=mtd_selection,
study_duration=study_duration, observe_tox=observe_tox, patient_allocation=patient_allocation, mean_obs_n=mean_obs_n, min_obs_n=min_obs_n, max_obs_n=max_obs_n,

mtd_selection_table = mtd_selection_table, true_mtd=true_mtd, pcs=pcs, obs_tox_overall=obs_tox_overall, patient_allocation_table=patient_allocation_table, obs_tox_table=obs_tox_table,
mean_cohort_b=mean_cohort_b, sd_cohort_b=sd_cohort_b, mean_duration=mean_duration, sd_duration=sd_duration, time_taken=time_taken, median_duration=median_duration, q1_duration=q1_duration, q3_duration=q3_duration)
return(result)
}

# Example: -------------------------------------

# crm_result <- my_crm(prior=c(0.05,0.1,0.2,0.3), target_tox=0.2, number_trials=100, true_tox=c(0.05,0.12,0.20,0.30), arrival_rate=15, prop_b=0.1, min_cohort_b=0,
# cycle_length=28, cohort_size=5, max_n=18, start_level=4)


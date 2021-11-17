# CONDUCT tab
# TARGET-CRM design
# Clement Ma
# March 1, 2021

library(dfcrm)

# Function to run TARGET-CRM design
target_crm_conduct <- function(prior, target_tox, tox, level, n=length(level), dose_labels=NULL, include=1:n, pid=1:n, cohort_size, num_slots_remain, current_dose) {
  
  # Run CRM model
  out <- crm(prior=prior, target=target_tox, tox=tox, dosename=dose_labels, level=level, n=n, include=include, pid=pid)
  
  if (num_slots_remain == 0) {
    if (out$mtd <= current_dose) {
      recommend_dose <- out$mtd # De-escalation or stay at same dose
    } else if (out$mtd > current_dose) {
      recommend_dose <- current_dose+1 # No dose skipping upon escalation
    }
  } else if (num_slots_remain > 0 & num_slots_remain < cohort_size) {
    if (out$mtd < current_dose) {
      recommend_dose <- out$mtd # Intra-cohort de-escalation allowed
    } else if (out$mtd >= current_dose) {
      recommend_dose <- current_dose # No intra-cohort escalation allowed
    }
  }
  
  # Copy dfcrm mtd for error checking
  dfcrm_mtd <- out$mtd
  
  # Replace out$mtd
  out$mtd <- out$dosename[recommend_dose]
  
  # Create Dataframe 1 for Output Into Shiny
  tbl1 <- data.frame("pid"=out$pid, "level"=out$level, "toxicity"=out$tox)
  
  # Make Included Column for Dataframe True
  tbl1[out$include, "included"] <- TRUE
  tbl1$included <- replace(tbl1$included, is.na(tbl1$included), FALSE)
  
  # Create Dataframe 2 for Output Into Shiny
  tbl2 <- data.frame("dose"=out$dosename, "prior"=out$prior, "ptox"=signif(out$ptox, 3), "lolmt"=signif(out$ptoxL, 3), "uplmt"=signif(out$ptoxU, 3))
  
  # Create lists to append to for getting summary of patients given each dose and experiencing a DLT
  n_list <- tbl1$level[which(tbl1$included==1)]
  total_tox_list <- tbl1$level[which(tbl1$toxicity==1 & tbl1$included==1)]
  
  for (i in as.numeric(rownames(tbl2))) {
    n_list <- c(n_list, i)
    total_tox_list <- c(total_tox_list, i)
  }
  
  tbl2$n <- as.numeric(table(n_list)-1)
  tbl2$total_tox <- as.numeric(table(total_tox_list)-1)
  
  tbl2 <- tbl2[c(1,2,6,7,3:5)]
  
  tbl1$level <- out$dosename[tbl1$level]
  
  output <- list(df1 = tbl1, df2=tbl2, crm_out=out, current_dose=current_dose, cohort_size=cohort_size, num_slots_remain=num_slots_remain, dfcrm_mtd=dfcrm_mtd)
  
}

# Example: -------------------

#target_crm_conduct_result <- target_crm_conduct(prior=c(0.05,0.10,0.2,0.30), target_tox=0.2, tox=c(0,0,0,0), dose_labels=c("100mg","200mg","300mg","400mg"), 
#                           level=c(2,2,2,3), pid=c("C1","C2","C3","C4"), include=c(1,2,3,4), cohort_size=3, num_slots_remain=2, current_dose=3)

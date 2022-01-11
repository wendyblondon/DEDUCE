# CONDUCT tab
# TARGET-CRM design
# Clement Ma
# March 1, 2021


library(dfcrm)


# Function to run TARGET-CRM design
conduct.target.crm <- function(prior, target.tox, tox, level, n=length(level), dose.labels=NULL, include=1:n, pid=1:n, cohort.size, num.slots.remain, current.dose) {

	# Run CRM model
	out <- crm(prior=prior, target=target.tox, tox=tox, dosename=dose.labels, level=level, n=n, include=include, pid=pid)
	
	if (num.slots.remain == 0) {
		if (out$mtd <= current.dose) {
			recommend.dose <- out$mtd # De-escalation or stay at same dose
		} else if (out$mtd > current.dose) {
			recommend.dose <- current.dose+1 # No dose skipping upon escalation
		}
	} else if (num.slots.remain > 0 & num.slots.remain < cohort.size) {
		if (out$mtd < current.dose) {
			recommend.dose <- out$mtd # Intra-cohort de-escalation allowed
		} else if (out$mtd >= current.dose) {
			recommend.dose <- current.dose # No intra-cohort escalation allowed
		}
	}
	
	# Copy dfcrm mtd for error checking
	dfcrm.mtd <- out$mtd

	# Replace out$mtd
	out$mtd <- out$dosename[recommend.dose]
		
	output <- list(crm.out = out, current.dose=current.dose, cohort.size=cohort.size, num.slots.remain=num.slots.remain, dfcrm.mtd=dfcrm.mtd)

}

# Function to print clean report

print.target.crm <- function(crm.out) {

	cat("\nDOSE ESCALATION RECOMMENDATIONS\n\n")
	cat("Trial design: TARGET-CRM\n")
	cat("Software: DEDUCE app version 1.0\n")
	cat("URL: https://bengarski.shinyapps.io/DELPHI/ \n\n")
	print(crm.out$crm.out)
	cat("\nAdditional details:\n")
	cat("Current dose level: ", crm.out$crm.out$dosename[crm.out$current.dose], "\n", sep='')
	cat("Cohort size: ", crm.out$cohort.size, "\n", sep='')
	cat("Number of slots remaining at current dose level: ", crm.out$num.slots.remain, "\n", sep='')

	cat("\nDose escalation rules:\n")
	cat("- No dose skipping upon dose escalation.\n")
	cat("- Intra-cohort dose de-escalation is allowed.\n")
	cat("- Intra-cohort dose escalation is not allowed.\n\n")

}

###########################################
# Example code

test <- conduct.target.crm(prior=c(0.05,0.10,0.2,0.30), target.tox=0.2, tox=c(0,0,0,0), dose.labels=c("100mg","200mg","300mg","400mg"), 
level=c(2,2,2,3), pid=c("C1","C2","C3","C4"), include=c(1,2,3,4), cohort.size=3, num.slots.remain=2, current.dose=3)

print.target.crm(test)

############################################
# 3/2/2021 Example Run

# Patient C1: dose level 20mg, no DLT
out1 <- conduct.target.crm(prior=c(0.05,0.12,0.2,0.30), target.tox=0.2, tox=c(0), 
dose.labels=c("10mg","20mg","30mg","40mg"), level=c(2), pid=c("C1"), include=c(1), 
cohort.size=3, num.slots.remain=2, current.dose=2)
print.target.crm(out1)

# Patient C2: dose level 20mg, no DLT
out2 <- conduct.target.crm(prior=c(0.05,0.12,0.2,0.30), target.tox=0.2, tox=c(0,0), 
dose.labels=c("10mg","20mg","30mg","40mg"), level=c(2,2), pid=c("C1","C2"), include=c(1,2), 
cohort.size=3, num.slots.remain=1, current.dose=2)
print.target.crm(out2)

# Patient C3: dose level 20mg, no DLT
out3 <- conduct.target.crm(prior=c(0.05,0.12,0.2,0.30), target.tox=0.2, tox=c(0,0,0), 
dose.labels=c("10mg","20mg","30mg","40mg"), level=c(2,2,2), pid=c("C1","C2","C3"), include=c(1,2,3), 
cohort.size=3, num.slots.remain=0, current.dose=2)
print.target.crm(out3)

# Patient C4: dose level 30mg, no DLT
out4 <- conduct.target.crm(prior=c(0.05,0.12,0.2,0.30), target.tox=0.2, tox=c(0,0,0,0), 
dose.labels=c("10mg","20mg","30mg","40mg"), level=c(2,2,2,3), pid=c("C1","C2","C3","C4"), include=c(1,2,3,4), 
cohort.size=3, num.slots.remain=2, current.dose=3)
print.target.crm(out4)

# Patient C5: dose level 30mg, DLT, BUT DO NOT INCLUDE
out5 <- conduct.target.crm(prior=c(0.05,0.12,0.2,0.30), target.tox=0.2, tox=c(0,0,0,0,1), 
dose.labels=c("10mg","20mg","30mg","40mg"), level=c(2,2,2,3,3), pid=c("C1","C2","C3","C4","C5"), include=c(1,2,3,4), 
cohort.size=3, num.slots.remain=2, current.dose=3)
print.target.crm(out5)

# Patient C6: dose level 30mg, DLT
out6 <- conduct.target.crm(prior=c(0.05,0.12,0.2,0.30), target.tox=0.2, tox=c(0,0,0,0,1,1), 
dose.labels=c("10mg","20mg","30mg","40mg"), level=c(2,2,2,3,3,3), pid=c("C1","C2","C3","C4","C5","C6"), include=c(1,2,3,4,6), 
cohort.size=3, num.slots.remain=1, current.dose=3)
print.target.crm(out6)

# Patient C7: dose level 30mg, no DLT
out7 <- conduct.target.crm(prior=c(0.05,0.12,0.2,0.30), target.tox=0.2, tox=c(0,0,0,0,1,1,0), 
dose.labels=c("10mg","20mg","30mg","40mg"), level=c(2,2,2,3,3,3,2), pid=c("C1","C2","C3","C4","C5","C6","C7"), include=c(1,2,3,4,6,7), 
cohort.size=3, num.slots.remain=2, current.dose=2)
print.target.crm(out7)
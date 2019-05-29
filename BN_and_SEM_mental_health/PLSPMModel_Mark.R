## Code for Mental Health Service Use comorbidity SEM Model

#First we need our packages with dependencies

install.packages(c("car", "sem", "foreign", "plyr", "dplyr", "semPLS", "semPlot", "plspm", "plsdepot"), dependencies = TRUE)

library(sem)
library(semPlot)
library(plyr)
library(dplyr)
library(foreign)
library(car)
library(plsdepot)
library(plspm)
library(semPLS)

#Now we need data 


mhdata <- data.frame(read.csv("C:/Users/Haoyuan/Desktop/dummy2.csv"))
attach(mhdata)

##Recodings for variables from TH CCG dataset to SEM-usable binary variables (1/0)

# Latent Variable 1: Depression Severity (depSev)

##depressionFlag.fixed <- revalue(depressionFlag, c("NULL"=0))
mhdata$depressionFlag.fixed <- as.numeric(mhdata$depressionflag)
mhdata$depressionCluster <- mhdata$clustercode
library(car)
mhdata$depressionCluster2 <- Recode(mhdata$depressionCluster, '1:5 = 2; 6:14 = 1; 15 = 3; 16:22=1; 23 = 0; 99 = 1; "nocluster"=0')
mhdata$depressionCluster2 <- Recode(mhdata$depressionCluster2, "6:8=1")
mhdata$depressionCluster <-as.numeric(mhdata$depressionCluster)

# Latent Variable 2: MentalHealthTreatment(mhTx)

mhdata$antiDepressantRx <- as.numeric(mhdata$antidepressantrxflag)
mhdata$iaptActivity <- as.numeric(mhdata$IAPTactivity)
mhdata$psychInpatient <- as.numeric(mhdata$psychinpatient)

# Latent Variable 3: Care for Type 2 Diabetes (diabCare)

mhdata$diabCareplanFlag.fixed <- as.numeric(mhdata$diabcareplanflag)
mhdata$diabMDTflag.fixed <- as.numeric(mhdata$diabMDTflag)
mhdata$diabFootExam.fixed <- as.numeric(mhdata$diabfootexamflag)
mhdata$digRetinalflag.fixed <- as.numeric(mhdata$digretinalflag)

#Latent variable 4: Poor Control of Type 2 Diabetes (poorCont)

mhdata$hba1cIfccFlag.fixed <- mhdata$hba1ciffcflag
mhdata$hba1cIfccFlag.fixed[mhdata$hba1ciffcflag> 199] <- NA
mhdata$hba1cIfccFlag.fixed[is.na(mhdata$hba1cIfccFlag.fixed)] <-199
mhdata$hba1cIfccFlag.fixed <- as.numeric(mhdata$hba1cIfccFlag.fixed)
mhdata$diabEyeCompFlag.fixed <- as.numeric(mhdata$diabeyecompflag)
mhdata$diablimbCompFlag.fixed <- as.numeric(mhdata$diablimbcompflag)

# Latent Variable 5: Demographic Risk Factors (demRisk)

mhdata$age <- as.numeric(mhdata$age)
mhdata$gender <- as.numeric(mhdata$gender)
mhdata$whiteEthnic <- as.numeric(mhdata$ethnicity)
mhdata$whiteEthnic <- recode(mhdata$whiteEthnic, "2=0; 3=0")
mhdata$deprivation <- as.numeric(mhdata$deprivation)
mhdata$smoking <- as.numeric(mhdata$smoking)

# Latent Variable 6: Mental Health Comorbidity (mhC)

mhdata$auditScreen <- mhdata$alcoholscreenscore
mhdata$auditScreen[mhdata$alcoholscreenscore <5 ] <- 0
mhdata$auditScreen[mhdata$alcoholscreenscore > 4] <- 1
mhdata$anxiety.fixed <- as.numeric(mhdata$anxietyflag)
mhdata$smi <- as.numeric(mhdata$mentalhealthflag)
mhdata$pd <- as.numeric(mhdata$personalitydisordersflag)

# Latent Variable 7: A&E Attendance(aAndE)

## Data should be numeric so no recodes needed
## this is a fake variable whilst the real values are missing
## mhdata$totalcost <- rnorm(23365, mean=100000, sd=25000)

##  Here is an is an explcitly PLS model using plspm:

# Specifying the Inner Model

#            DR MC DC MT DS PC AE
demRisk =  c(0, 0, 0, 0, 0, 0, 0)
mhComorb = c(1, 0, 0, 0, 0, 0, 0)
diabCare = c(0, 0, 0, 0, 0, 0, 0)
mhTX =     c(0, 1, 0, 0, 0, 0 ,0)
depSev  =  c(1, 1, 0, 1, 0, 0, 0)
poorCont = c(1, 1, 1, 1, 1, 0, 0)
aAndE =    c(1, 1, 0, 0, 1, 1, 0)


#Here's the inner (latent) model:

mh_path = rbind(demRisk, mhComorb, diabCare, mhTX, depSev, poorCont, aAndE)
mh_path

library(plspm)
innerplot(mh_path)

#and the outer model:

mh_blocks = list(
  c("age", "gender", "whiteEthnic", "deprivation", "smoking"),
  c("auditScreen", "anxiety.fixed", "smi", "pd"),
  c("diabCareplanFlag.fixed", "diabMDTflag.fixed", "diabFootExam.fixed", "digRetinalflag.fixed"),
  c("antiDepressantRx", "iaptActivity", "psychInpatient"), 
  c("depressionFlag.fixed", "depressionCluster2"),
  c("hba1cIfccFlag.fixed", "diabEyeCompFlag.fixed", "diablimbCompFlag.fixed"),
  c("totalattendances", "totalcost")
)

# Alternative outer model with low frequencies excluded if these are problematic:

mh_blocks2 = list(
  c("depressionFlag.fixed", "depressionCluster"),
  c("antiDepressantRx", "iaptActivity"),
  c("diabCareplanFlag.fixed", "diabMDTflag.fixed", "diabFootExam.fixed", "digRetinalflag.fixed"),
  c("hba1cIfccFlag.fixed", "diabEyeCompFlag.fixed", "diablimbCompFlag.fixed"),
  c("age", "gender", "whiteEthnic", "deprivation", "smoking"),
  c("totalattendances", "totalcost"),
  c("auditScreen", "anxiety.fixed")
)

# This fits the model

mh_pls = plspm(mhdata, mh_path, mh_blocks, modes=NULL)

summary(mh_pls)
plot(mh_pls)

plot(mh_pls, what="loadings")

mh_pls = plspm(mhdata, mh_path, mh_blocks2, modes=NULL, scaling=NULL, scaled=TRUE)


# This is a model with standarised variables (seems to make no difference)

mh_plsc = plspm(mhdata, mh_path, mh_blocks, modes=NULL, scaling=NULL, scaled=TRUE)

summary(mh_plsc)
plot(mh_plsc)

plot(mh_pls, what="loadings")



# tidying up

mhdata$aAndE <- NULL
mhdata$antidepressantrxflag <- NULL
mhdata$anxietyflag <- NULL
mhdata$depressionflag <-NULL
mhdata$depSev <-NULL

mhdata$diabCare <- NULL
mhdata$anxietyflag <-NULL
mhdata$diabcareplanflag <-NULL
mhdata$diabCont <- NULL
mhdata$diabeyecompflag <-NULL
mhdata$diabfootexamflag <- NULL
mhdata$diablimbcompflag <- NULL
mhdata$diabMDTflag <- NULL
mhdata$digretinalflag <- NULL
mhdata$ethnicity <-NULL
mhdata$hba1ciffcflag <- NULL

mhdata$IAPTactivity <- NULL
mhdata$mentalhealthflag <- NULL
mhdata$mhC <- NULL
mhdata$mhclusteredyes <- NULL
mhdata$mhTx<- NULL
mhdata$personalitydisordersflag <- NULL
mhdata$psychinpatient <- NULL
mhdata$alcoholscreenscore <- NULL
mhdata$clustercode <- NULL


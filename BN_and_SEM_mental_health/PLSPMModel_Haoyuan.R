rm(list=ls())



mhdata$otherEthnic = 1- mhdata$whiteEthnic

mh_blocks3 = list(
  c("depressionFlag.fixed", "depressionCluster"),
  c("antiDepressantRx", "iaptActivity"),
  c("diabCareplanFlag.fixed", "diabMDTflag.fixed", "diabFootExam.fixed", "digRetinalflag.fixed"),
  c("hba1cIfccFlag.fixed", "diabEyeCompFlag.fixed", "diablimbCompFlag.fixed"),
  c("age", "gender", "otherEthnic", "deprivation", "smoking"),
  c("totalattendances", "totalcost"),
  c("auditScreen", "anxiety.fixed")
)

# This fits the model

mh_pls = plspm(mhdata, mh_path, mh_blocks3, modes=NULL)

summary(mh_pls)
plot(mh_pls)

plot(mh_pls, what="loadings")


gpa_perm = plspm.groups(mh_pls, mhdata$IAPTactivity, method = "permutation")


mh_pls = plspm(mhdata, mh_path, mh_blocks2, modes=NULL, scaling=NULL, scaled=TRUE)









library(plspm)
mhdata <- data.frame(read.csv("C:/Users/Haoyuan/Desktop/dummy2.csv"))
mhdata = mhdata[, -1]
mhdata$inverdepressionflag = recode(mhdata$depressionflag, "'no' = 'yes'; 'yes' = 'no'")

#inner (latent) model:
#            MC DC MT DS PC AE
demRisk =  c(0, 0, 0, 0, 0, 0, 0)
mhComorb = c(1, 0, 0, 0, 0, 0, 0)
diabCare = c(1, 0, 0, 0, 0, 0, 0)
mhTX =     c(0, 0, 0, 0, 0, 0 ,0)
depSev  =  c(1, 0, 0, 0, 0, 0, 0)
poorCont = c(0, 0, 1, 1, 1, 0, 0)
aAndE =    c(1, 1, 0, 0, 0, 1, 0)



demRisk =  c(0, 0, 0, 0, 0, 0, 0)
mhComorb = c(1, 0, 0, 0, 0, 0, 0)
diabCare = c(1, 0, 0, 0, 0, 0, 0)
mhTX =     c(0, 0, 0, 0, 0, 0 ,0)
depSev  =  c(1, 0, 0, 0, 0, 0, 0)
poorCont = c(0, 0, 1, 1, 1, 0, 0)
aAndE =    c(1, 1, 0, 0, 0, 1, 0)
mh_path = rbind(demRisk, mhComorb, diabCare, mhTX, depSev, poorCont, aAndE)
innerplot(mh_path)


#outer model:
mh_blocks = list(
  c("age", "gender", "ethnicity", "deprivation", "smoking"),
  c("alcoholscreenscore", "personalitydisordersflag", "mentalhealthflag", "anxietyflag"),
  c("diabcareplanflag", "diabMDTflag", "diabfootexamflag","digretinalflag"),
  c("antidepressantrxflag", "IAPTactivity", "psychinpatient"),
  c("depressionflag", "mhclusteredyes"),
  c("hba1ciffcflag", "diabeyecompflag", "diablimbcompflag"),
  c("totalcost", "totalattendances")
)


# mh_blocks = list(
#   c("ethnicity", "deprivation"),
#   c("alcoholscreenscore", "anxietyflag"),
#   c( "diabMDTflag", "digretinalflag"),
#   c("antidepressantrxflag", "IAPTactivity"),
#   c("inverdepressionflag", "clustercode"),
#   c("diabeyecompflag", "diablimbcompflag"),
#   c("totalcost", "totalattendances")
# )


mh_mode = c("B", rep("A",6))
mh_pls = plspm(mhdata, mh_path, mh_blocks, modes=mh_mode)
mh_pls$gof
plot(mh_pls, what="loadings")




summary(mh_pls)
plot(mh_pls)

plot(mh_pls, what="loadings")






fut_rebus = rebus.pls(mh_pls)


Hmisc::describe(mhdata2) 



mh_pls = plspm(mhdata, mh_path, mh_blocks, modes=NULL, boot.val = TRUE)
mh_pls$boot$paths


antidepressantrxflag
IAPTactivity
mentalhealthflag
mhclusteredyes
personalitydisordersflag
psychinpatient





gpa_perm = plspm.groups(mh_pls, mhdata$smoking, method = "permutation")



gpa_perm1 = plspm(mhdata[mhdata$deprivation =="1",], mh_path, mh_blocks)
gpa_perm2 = plspm(mhdata[mhdata$deprivation =="2",], mh_path, mh_blocks)
gpa_perm3 = plspm(mhdata[mhdata$deprivation =="others",], mh_path, mh_blocks)

plot(gpa_perm1, box.size = 0.14)
plot(gpa_perm2, box.size = 0.14)
plot(gpa_perm3, box.size = 0.14)

gpa_perm3$gof

patient <- data.frame(P_name="harini" , P_age = 25 ,P_id=" i21",P_class = factor(x=NA,levels = c("malignant", "benign")))
rownames(patient) <- patient$P_name
saveRDS(patient , "C:/Users/lakshmi1/Documents/R/firstWebApp/patient.rds")
readRDS("C:/Users/lakshmi1/Documents/R/firstWebApp/patient.rds")


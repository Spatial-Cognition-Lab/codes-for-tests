#Specify folder where results are stored
path.1<- "C:/Users/vongh/Documents/Da salvare/Academia/Progetti/Vestibular system x navigation/Data/Pta"
setwd(path.1)
files.1<- list.files(path.1, pattern = "\\.csv")
PTA.results<-data.frame(Subject = character(),
                       Cond = character(),
                       PTA_DEV = numeric(),
                       PTA_ACC= numeric(),
                       PTA_RT = numeric(),
                       stringsAsFactors = FALSE)
for (i in 1:length(files.1)){
  fname<- files.1[i]
  d<- read.csv2(fname, header= F)
  if (ncol(d)==1) {d<-read.csv(fname, header=F)
  }
  
  d1<-d[8:23,]
  ptadev<- mean(as.numeric(d1$V7))
  
  d1<- subset(d1,as.numeric(d1$V9)<20 & as.numeric(d1$V9)>0.5)
  ptacc<- mean(as.numeric(d1$V10))
  
  d1<- subset(d1,as.numeric(d1$V7)==0)
  
  m<-mean(na.omit(as.numeric(d1$V9)))
  sd<-sd(na.omit(as.numeric(d1$V9)))
  cut<- m+(2.5*sd)
  PTA.rt <- mean(as.numeric(d1$V9)[as.numeric(d1$V9) < cut])
  
  #Extract subject number and condition given that files where saved in this format: subjnr_conditon_prepost.csv
  parts <- strsplit(fname, "_")[[1]]
  sub<- parts[1]
  condition <- parts[2]  # Get the second element
  
  PTA.results <- rbind(PTA.results, data.frame(Subject = sub, Cond = condition, PTA_DEV = ptadev, PTA_ACC = ptacc, PRA_RT = PTA.rt))
}
#specify folder path where you'd like to save the results
setwd("C:/Users/vongh/Documents/Da salvare/Academia/Progetti/Vestibular system x navigation/Data")
write.table(PTA.results, "PTA_results.csv", sep = ',',row.names = F)
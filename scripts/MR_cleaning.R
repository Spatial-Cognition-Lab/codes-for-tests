#Specify folder where results are stored
path<- "C:/Users/vongh/Documents/Da salvare/Academia/Progetti/Vestibular system x navigation/Data/MR"
setwd(path)
files<- list.files(path, pattern = "\\.csv")
MR.results<-data.frame(Subject = character(),
                       Cond = character(),
                       MR_ACC = numeric(),
                       MR_RT = numeric(),
                       stringsAsFactors = FALSE)
for (i in 1:length(files)){
  filename <- files[i]
  df<- read.csv2(filename, header= F)
  if (ncol(df)==1) {df<-read.csv(filename, header=F)
  }
  
  df1<-df[5:41,]
  df1<- subset(df1,as.numeric(df1$V9)==1 & as.numeric(df1$V8)<20 & as.numeric(df1$V8)>0.5)
  acc<- sum(as.numeric(df1$V9))
  
  m<-mean(na.omit(as.numeric(df1$V8)))
  sd<-sd(na.omit(as.numeric(df1$V8)))
  cut<- m+(2.5*sd)
  MR.rt <- mean(as.numeric(df1$V8)[as.numeric(df1$V8) < cut])
  
  #Extract subject number and condition given that files where saved in this format: subjnr_conditon_prepost.csv
  parts <- strsplit(filename, "_")[[1]]
  sub<- parts[1]
  condition <- parts[2]  # Get the second element
  
  MR.results <- rbind(MR.results, data.frame(Subject = sub, Cond = condition, MR_ACC = acc, MR_RT = MR.rt))
}
#specify folder path where you'd like to save the results
setwd("C:/Users/vongh/Documents/Da salvare/Academia/Progetti/Vestibular system x navigation/Data")
write.table(MR.results, "MR_results.csv", sep = ',',row.names = F)
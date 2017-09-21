revice<-function(df){
  for (i in 1:ncol(df)){
    df[,i]<-gsub('"','',df[,i])
    colnames(df)[i]<-gsub('X','',colnames(df)[i])
    colnames(df)[i]<-gsub('.','',colnames(df)[i],fixed = TRUE)
  }
  return(df)
}
Escribe <- function(n) {
  vector<-'fb<- function(x) {'
  for (i in 1:n){
    vector<-append(vector,paste0("x",i," <- x[",i,"]"))
  }
  temporal<-'x1*x1'
  for (i in 2:n){
    temporal<-paste0(temporal,"+x",i,"*x",i)
  }
  vector<-append(vector,temporal)
  vector<-append(vector,"}")
  cat(vector,file="fb.R",sep="\n")
}


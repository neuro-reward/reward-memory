library(Boruta)
library(digest)

readRDS("../data/reward-memory.RDS")->rmem

cfile<-"collected.RDS"
tabfile<-"tabulated.RDS"
cache<-if(file.exists(cfile)) readRDS(cfile) else list()

#Load data
yn<-c("Memory","Correct 1st","Correct 2nd")
xn<-setdiff(names(rmem),yn)
X<-rmem[,xn]
D<-rmem[,yn]

#Plan what to do
Iters<-30
Trees<-50000
Version<-1

Trends<-as.list(D)
names(Trends)<-tolower(gsub(" ","",names(Trends)))

expected_tasks<-c()
for(iteration in 1:Iters){
 for(trend in names(Trends)){
  task<-sprintf("%s_i%s_t%s",trend,iteration,Trees)
  expected_tasks<-c(expected_tasks,task)

  Y<-Trends[[trend]]
  Xc<-X[!is.na(Y),]
  Xch<-digest(Xc,'sha512')
  Yc<-Y[!is.na(Y)]
  Ych<-digest(Yc,'sha512')
  
  if(!is.null(cache[[task]])){
   if(
    identical(Xch,cache[[task]]$Xh) && 
    identical(Ych,cache[[task]]$Yh) && 
    identical(cache[[task]]$seed,iteration) &&
    identical(cache[[task]]$version,Version)){
    next
   }else{
    message("Cache for ",task," stale, re-calculating!")
   }
  }
  message("Calculating ",task,"...")
  set.seed(iteration)
  B<-list()
  try(
   Boruta(
    Xc,Yc,
    ntree=Trees,
    getImp=imputeTransdapter(),
    holdHistory=FALSE,
    maxRuns=200,
    doTrace=1
   )
  )->B
  if(inherits(B,"try-error")){
   message("Error executing ",task," will not save it")
  }else{
   message("Task ",task," done, saving cache.")
   B$seed<-iteration
   B$trend<-trend
   B$Ymask<-Y
   B$Xh<-Xch
   B$Yh<-Ych
   B$task<-task
   B$version<-Version
   cache[[task]]<-B
   saveRDS(cache,file=cfile,compress="xz")
  }
 }
}

setdiff(names(cache),expected_tasks)->surplus
if(length(surplus)>0) 
 message("Surplus tasks in cache: ",paste(surplus,collapse=", "),"; removing now...")
for(e in surplus) cache[[e]]<-NULL
saveRDS(cache,file=cfile,compress="xz")

setdiff(expected_tasks,names(cache))->missing
if(length(missing)>0) stop("Boruta tasks failed: ",paste(missing,collapse=", "))

#Tabulate selection counts
names(X)->xn
lapply(split(cache,sapply(cache,'[[','trend')),lapply,getSelectedAttributes)->S
lapply(lapply(S,sapply,function(x) xn%in%x),rowMeans)->S
data.frame(do.call(cbind,S),row.names=xn)->tab

data.frame(lapply(split(cache,sapply(cache,'[[','trend')),function(x) x[[1]]$Ymask))->masks
attr(tab,"masks")<-masks

saveRDS(tab,file=tabfile,compress="xz")
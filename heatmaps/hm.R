library(pheatmap)
library(ggplot2)

readRDS('../data/reward-memory.RDS')->rmem
readRDS('../boruta/tabulated.RDS')->br

nice_c<-function(x){
 x<-gsub('co-localisation','co-loc.',x)
 x<-gsub('exclusively','excl.',x)
 x
}

bor_sel<-function(case=names(br)[1])
 rownames(br)[br[,case]>=.5]

hm<-function(sel=bor_sel(names(br)[1]),title="",use_anot,...){
 gsub("_","",rownames(rmem))->rownames(rmem)
 S<-apply(rmem[,sel],2,rank,na.last='keep')
 colnames(S)<-nice_c(colnames(S))
 ar<-if(!missing(use_anot)){
  if(!is.null(names(use_anot)))
   setNames(rmem[,use_anot,drop=FALSE],names(use_anot)) else
   rmem[,use_anot,drop=FALSE]
 }else NA
 if(is.data.frame(ar))
  for(e in 1:ncol(ar))
   if(is.numeric(ar[[e]]))
    ar[[e]]<-rank(ar[[e]],na.last="keep")
 
 pheatmap(
  S,
  cluster_rows=TRUE,
  cluster_cols=ncol(S)>3,
  annotation_row=ar,
  main=title,
  silent=TRUE,
  ...
 )
}

#plot(hm_bor(anot=c("Memory","Correct 1st"))$gtable)
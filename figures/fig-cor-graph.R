library(ggraph)
library(pspearman)
readRDS('../data/reward-memory.RDS')->rmem

test_cor<-function(X,th=0.05,method="fdr",key=NA){
 expand.grid(a=1:ncol(X),b=1:ncol(X))->J
 J[J$a<J$b,]->J
 sapply(
  1:nrow(J),
  function(e)
   suppressWarnings(
    spearman.test(X[,J$a[e]],X[,J$b[e]])
   )$p.value
 )->J$p
 sapply(
  1:nrow(J),
  function(e)
   cor(X[,J$a[e]],X[,J$b[e]],method='spearman',use='pair')
 )->J$Correlation
 J$ap<-p.adjust(J$p,method=method)
 J$a<-names(X)[J$a]
 J$b<-names(X)[J$b]
 J$Key<-key
 J[J$ap<th,]
}

test_subset<-function(X,key="both",th=0.05,method="fdr"){
 X[,grepl(sprintf("%s$",key),names(X))]->X
 test_cor(X,method=method,th=th,key=key)->X
 rx<-sprintf(" ?%s$",key)

 X$a<-gsub(rx,"",as.character(X$a))
 X$b<-gsub(rx,"",as.character(X$b))
 X
}

collect<-function(X,keys,th=0.05,method="fdr")
 do.call(
  rbind,
  lapply(
   keys,
   function(key) test_subset(X,key,th=th,method=method)
  )
 )

graph_mosaic<-function(){ 
 collect(
  rmem[,-(1:3)],
  c("both","1st","1st exclusively","2nd exclusively"),
  method="fdr",th=0.05
 )->X
 X$Sign<-factor(ifelse(X$Correlation<0,"-","+"))
 X$Correlation<-abs(X$Correlation)
 X$Key<-gsub("both","Both",gsub("1st","First",gsub("2nd","Second",X$Key)))
 
 ggraph(X,layout='linear',circular=TRUE,offset=2.2)+
  geom_edge_arc(aes(width=Correlation,col=Sign))+
  scale_edge_colour_manual(values=c("-"="#ff000a","+"="#000000"))+
  scale_edge_width_continuous(range=c(0.5,2.5))+
  geom_node_label(aes(label=name),size=4.,label.padding=unit(0.15,'lines'))+
  theme(legend.position="bottom")+
  coord_fixed(clip="off")+
  theme(panel.background=element_blank())+
  facet_wrap(.~Key)
}

render<-function(){
 ggsave('fig-cor-graph.pdf',graph_mosaic(),width=5.5,height=6.5)
}

if(!interactive()) render()
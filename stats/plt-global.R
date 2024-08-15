library(ggplot2)
library(coin)

readRDS("../data/reward-memory.RDS")->rmem

targets<-c("BLA","CeA","LA","CA1","CA3","CPu","DG","IL",
 "NAcc core","NAcc shell","PrL","RSA","RSG","VTA")

do.call(rbind,lapply(targets,function(tn){
 rmem[,sprintf("%s co-localisation",tn)]->val
 data.frame(
  Structure=tn,
  Value=val,
  Var="Co-localisation",
  Group=rmem$Memory
 )
}))->rmem_clc

do.call(rbind,lapply(targets,function(tn){
 rmem[,sprintf("%s any",tn)]->val
 data.frame(
  Structure=tn,
  Value=val,
  Var="Any active",
  Group=rmem$Memory
 )
}))->rmem_any

rbind(rmem_clc,rmem_any)->extract


na.omit(extract)->extract
gsub(' ','\n',extract$Structure)->extract$Structure

bm<-function(x){
 diff(quantile(x,c(1,3)/4))->iqr
 min(median(x)+1.5*iqr,max(x))
}

pv<-sapply(
 split(extract,extract$Var),
 function(x)  pvalue(wilcox_test(Value~Group|factor(Structure),data=x))
)

# Significance bars

tapply(
 extract$Value,
 extract$Var,
 function(x) max(x)+0.1*diff(range(x))
)->ay

meds<-function(x){
 x<-unique(sort(x))
 x[floor(length(x)/2)]
}

#Labels
ad<-data.frame(
 Group=c(NA,NA),
 Structure=rep(meds(extract$Structure),2),
 Value=ay,
 label=sprintf("P=%0.1g",pv),
 Var=names(ay)
)

#Dummy elements to stretch scales
sd<-data.frame(
 Group=c(NA,NA),
 Structure=rep(meds(extract$Structure),2),
 Value=ay*1.03,
 Var=names(ay)
)

#Bars
bd<-data.frame(
 Group=c(NA,NA),
 Structure=rep(min(extract$Structure),2),
 xend=rep(max(extract$Structure),2),
 Value=ay,
 yend=ay,
 Var=names(ay)
)

plt_global<-function()
 ggplot(extract,aes(x=Structure,y=Value))+
 geom_boxplot(outlier.shape=NA,aes(fill=Group))+ #Col also depends?
 facet_grid(Var~.,scales='free_y')+
 geom_segment(data=bd,aes(xend=xend,yend=yend))+
 geom_text(data=ad,aes(label=label),vjust=-.45)+
 geom_point(data=sd,pch='')+
 geom_jitter(pch=21,aes(fill=Group),position=position_jitterdodge())+
 theme_bw()+
 theme(legend.position='bottom',axis.title.x=element_blank())


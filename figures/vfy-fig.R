library(ggplot2)
library(patchwork)
library(superb)

readRDS('../data/suppressed-cea.RDS')->Q
Q$Modification<-Q$Modified

pA<-wilcox.test(`Correct 1st`~Modification,Q)$p.value
pB<-wilcox.test(`USVs in training`~Modification,Q)$p.value
pC<-cor.test(Q$`USVs in training`,Q$`Correct 1st`,method="spearman")

pltA<-ggplot(Q,aes(x=Modification,y=`Correct 1st`,fill=Modification))+
 geom_boxplot(outlier.shape=NA)+geom_jitter(pch=21,width=0.1)+theme_bw()+ylab("Correctness 1st entry")+theme(legend.position="none")+
 showSignificance(1:2,max(Q$`Correct 1st`)*1.1,-max(Q$`Correct 1st`)*0.03,sprintf("P=%0.2g",pA))
pltB<-ggplot(Q,aes(x=Modification,y=`USVs in training`,fill=Modification))+scale_y_log10()+
 geom_boxplot(outlier.shape=NA)+geom_jitter(pch=21,width=0.1)+theme_bw()+ylab("USV episodes in training")+theme(legend.position="none")+
 showSignificance(1:2,max(Q$`USVs in training`)*2,-max(Q$`USVs in training`)*0.3,sprintf("P=%0.1g",pB))
pltC<-ggplot(Q,aes(x=`Correct 1st`,y=`USVs in training`,fill=Modification))+geom_point(pch=21)+theme_bw()+
 xlab("Correctness 1st entry")+ylab("USV episodes in training")+scale_y_log10()+theme(legend.position="none")+
 annotate("text",x=5/8,y=10,label=sprintf("Spearman's r=%0.2f, P=%0.1g",pC$estimate,pC$p.value))

pltCmp<-pltA+pltB+pltC+plot_annotation(tag_levels="A")+plot_layout(widths=c(2,2,3))

if(!interactive())
 ggsave("vfy-fig.pdf",pltCmp,width=12,height=5)

library(patchwork)

source('../heatmaps/hm.R')
source("../shared/pal.R")

hm(
 bor_sel("correct1st"),
 title="Correctness, first entry",
 annotation_colors=ann_color,
 lengend=FALSE,
 annotation_legend=FALSE,
 use_anot=c(Memory="Memory",Correct="Correct 1st")
)->c1_hm

hm(
 bor_sel("correct2nd"),
 title="Correctness, second entry",
 annotation_colors=ann_color,
 use_anot=c(Memory="Memory",Correct="Correct 2nd"),
 lengend=FALSE
 )->c2_hm

plt<-wrap_elements(c1_hm$gtable)+(c2_hm$gtable)+
 plot_annotation(tag_levels="A")+
 plot_layout(widths=c(2.0,1.8))

render<-function()
  ggsave('cmb-fig-cor.pdf',width=10,height=5.5)

if(!interactive()) render()
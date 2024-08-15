library(patchwork)

source("../stats/plt-global.R")
source("../heatmaps/hm.R")
source("../shared/pal.R")

plt_global()->e_plt
e_plt<-e_plt+scale_fill_manual(values=ann_color$Memory)
 
hm(
 bor_sel("memory"),
 title="Group differences",
 annotation_colors=ann_color,
 lengend=FALSE,
 annotation_legend=FALSE,
 use_anot="Memory"
)->e_hm

plt<-e_plt+e_hm$gtable+
 plot_annotation(tag_levels="A")+
 plot_layout(widths=c(1,1))

render<-function()
  ggsave("cmb-fig-rr.pdf",width=10,height=6)

if(!interactive()) render()

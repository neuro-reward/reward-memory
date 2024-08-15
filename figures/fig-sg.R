#This script invokes graphviz

readRDS('../boruta/tabulated.RDS')->A

data.frame(A>=0.5)->A
A[rowSums(A)>0,]->A
cbind(stack(A),rownames(A))->B
B[B$values,]->B
B$values<-NULL
names(B)<-c("Source","Target")

nice_c<-function(x){
 x<-gsub('_1',' 1st',x)
 x<-gsub('_2',' 2nd',x)
 x<-gsub('anot','Recall group',x)
 x
}

nice_r<-function(x){
 x<-gsub('co-localisation','co-loc.',x)
 x<-gsub('exclusively','excl.',x)
 x
}

B$Target<-nice_r(B$Target)
B$Source<-c(correct1st="Correct 1st",correct2nd="Correct 2nd",memory="Recall group")[as.character(B$Source)]

wrap<-function(x)
 gsub(' (co-loc.|both|1st|2nd|1st excl.|2nd excl.|any)$','\n\\1',x)

render<-function(){
 writeLines(
  sprintf("graph {\nmargin=\"0,0\"\n%s\n overlap=\"prism\"\noverlap_scaling=-6.5\nsplines=\"true\"\n}",
  paste(sprintf('"%s" -- "%s"',nice_r(B$Source),wrap(B$Target)),collapse="\n")),
  'fig-sg.dot'
 )
 system('neato fig-sg.dot -Tpdf > fig-sg.pdf')
 
}

if(!interactive()) render()

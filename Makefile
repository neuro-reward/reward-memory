all: figures

boruta/collected.RDS boruta/tabulated.RDS: data/reward-memory.RDS boruta/boruta.R
	cd boruta; Rscript boruta.R

figures/fig-sg.pdf: figures/fig-sg.dot
	neato figures/fig-sg.dot -Tpdf > figures/fig-sg.pdf

figures/fig-sg.dot: figures/fig-sg.R boruta/tabulated.RDS
	cd figures; Rscript fig-sg.R

figures/cmb-fig-cor.pdf: figures/cmb-fig-cor.R boruta/tabulated.RDS heatmaps/hm.R shared/pal.R data/reward-memory.RDS
	cd figures; Rscript cmb-fig-cor.R
	
figures/cmb-fig-rr.pdf: figures/cmb-fig-rr.R boruta/tabulated.RDS heatmaps/hm.R stats/plt-global.R shared/pal.R data/reward-memory.RDS
	cd figures; Rscript cmb-fig-rr.R

figures/fig-cor-graph.pdf: figures/fig-cor-graph.R data/reward-memory.RDS
	cd figures; Rscript fig-cor-graph.R

figures: figures/cmb-fig-cor.pdf figures/cmb-fig-rr.pdf figures/fig-sg.pdf figures/fig-cor-graph.pdf

clean:
	rm -f figures/*.pdf
	rm -f figures/*.dot

deep-clean:
	rm -f figures/*.pdf
	rm -f figures/*.dot
	rm -f boruta/*.RDS

.PHONY: figures deep-clean clean all	

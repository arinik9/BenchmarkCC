# TODO: Add comment
# 
# Author: nejat
###############################################################################








#################################################################
#
# d: density
# k: nb cluster
#
##################################################################
retreive.heuristic.optimality.proportion = function(n, l0, d, prop.mispl, prop.neg, network.no, heur.algo.names, cor.clu.exact.algo)
{
	
	#tlog(16, "start to plot transition with exact algorithms")	
	tlog(20, "optimality proportion => algo.name: ", cor.clu.exact.algo)

    results = c()
    for(heur.algo.name in heur.algo.names){
		tlog(24, "optimality proportion => algo.name: ", cor.clu.exact.algo)

	    for(graph.desc.name in c(SIGNED.UNWEIGHTED.FILE)){ # OPPOSITE.SIGNED.UNWEIGHTED.FILE
		    tlog(28, "optimality proportion => graph.desc.name: ", graph.desc.name)
		    #part.folder = part.folder = get.part.folder.path(n, l0, d, prop.mispl, prop.neg, network.no, heur.algo.name, graph.desc.name)
		    eval.folder = get.eval.folder.path(n, l0, d, prop.mispl, detected.imb=NA, prop.neg, k=ALL, network.no, heur.algo.name, graph.desc.name)

            opt.table.file = paste0(EVAL.HEUR.OPTIMALITY.PROP.FILE.PREFIX,"-",heur.algo.name,"_vs_",cor.clu.exact.algo,".csv")
            print(file.path(eval.folder, opt.table.file))
            if(file.exists(file.path(eval.folder, opt.table.file)))
                prop = as.numeric(read.csv(file.path(eval.folder, opt.table.file), row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)[,HEUR.OPTIMALITY.PROP.COL.NAME])
            else
                prop = NA
		    results = cbind(results, prop)
	    }

	}
    colnames(results) = heur.algo.names
    rownames(results) = paste0("n=", n, ",l0=", l0, ",d=", d, ",prop.mispl=", prop.mispl, ",prop.neg=", prop.neg, ",network.no=", network.no)

	return(results)
}









#################################################################
#
# graph.sizes
# d: density
# k: nb cluster
# prop.mispls
# prop.negs
# cor.clu.heur.algos
# heur.reps: heuristic repetitions. Sequantial integers (1, .., 100)
# cor.clu.exact.algos
# in.rand.g.folders: input random graph folders. Sequantial integers (1, .., 10)
# force
# plot.formats
#
##################################################################
collect.all.heuristic.optimality.proportion = function(graph.sizes, d, l0, prop.mispls, prop.negs, in.rand.net.folders,
		heur.algo.names, cor.clu.exact.algo)
{
	all.data = c()
	
	tlog("starts collecting optimality proportion")
	for(n in graph.sizes){
		tlog(8, "collecting optimality proportion => n: ", n)
		
		for(prop.mispl in prop.mispls){
			tlog(8, "collecting optimality proportion => prop.mispl: ", prop.mispl)
			
		    if(is.na(prop.negs) && d == 1){
		        prop.negs = compute.prop.neg(n, d, l0, prop.mispl)
		    }
			
			for(prop.neg in prop.negs){
				tlog(12, "collecting optimality proportion => prop.neg: ", prop.neg)
				
				for(network.no in in.rand.net.folders){
					tlog(16, "partitioning optimality proportion => network.no: ", network.no)
					
					data = retreive.heuristic.optimality.proportion(n, l0, d, prop.mispl, prop.neg, network.no,
							heur.algo.names, cor.clu.exact.algo)
					if(!is.na(data))
						all.data = rbind(all.data, data)
				}
			}
			
		}
		
	}
	print(all.data)
	if(length(all.data)>0){
		if(!dir.exists(OUTPUT.CSV.FOLDER))
		    dir.create(OUTPUT.CSV.FOLDER, recursive=FALSE, showWarnings=FALSE)
		
		write.csv(file=file.path(OUTPUT.CSV.FOLDER,paste0("heuristic-optimality-proportion-l0=",l0,"-n=",n,"-d=",d,".csv")), x=all.data)
	}
}

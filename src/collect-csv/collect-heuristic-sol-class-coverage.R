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
retreive.heuristic.sol.class.coverage = function(n, l0, d, prop.mispl, prop.neg, network.no,
		heur.algo.names, cor.clu.exact.algo, measure)
{
	
	curr.data =c()
		
	tlog(20, "collecting heuristic sol class coverage => algo.name: ", cor.clu.exact.algo)

    results = c()
    for(heur.algo.name in heur.algo.names){
		tlog(24, "heuristic sol class coverage => algo.name: ", cor.clu.exact.algo)
	
	    for(graph.desc.name in c(SIGNED.UNWEIGHTED.FILE)){ # OPPOSITE.SIGNED.UNWEIGHTED.FILE
		    tlog(24, "heuristic sol class coverage => graph.desc.name: ", graph.desc.name)
		    e.eval.folder = get.eval.folder.path(n, l0, d, prop.mispl, NA, prop.neg, k=ALL, network.no, cor.clu.exact.algo, graph.desc.name)
		    h.eval.folder = get.eval.folder.path(n, l0, d, prop.mispl, NA, prop.neg, k=ALL, network.no, heur.algo.name, graph.desc.name)

            measure.best.k.table.file = file.path(e.eval.folder, paste0(EVAL.BEST.K.FOR.KMEDOIDS,"-",measure,".csv"))

            if(file.exists(measure.best.k.table.file)){
                k = as.integer(read.csv(measure.best.k.table.file, row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)[1,c(BEST.K.FOR.SILH.COL.NAME)])
                if(k>1){
                    cover.table.file = file.path(h.eval.folder, paste0(EVAL.HEUR.KMEDOIDS.COVER.FILENAME,".csv"))
			print(cover.table.file)
                    if(file.exists(cover.table.file)){
                        prop = as.numeric(read.csv(cover.table.file, row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)[1,HEUR.KMEDOIDS.COVER.PROP.COL.NAME])
                        results = cbind(results, prop)
                    }
                    else {
                        print("ERRORR !!!!!!!!!!!!!!!!!!!!!!!!")
		                results = cbind(results, NA)
		            }
                }
                else {
	                results = cbind(results, NA)
	            }

		    } 
            else {
		        results = cbind(results, NA)
		    }
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
# l0: nb cluster
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
collect.all.heuristic.sol.class.coverage = function(graph.sizes, d, l0, prop.mispls, prop.negs, in.rand.net.folders,
		heur.algo.names, cor.clu.exact.algo, comp.measures)
{
    
    for(measure in comp.measures){
        tlog(20, "collecting heuristic sol class coverage => measure: ", measure)
        
    	all.data = c()
    	
    	tlog("starts collecting")
    	for(n in graph.sizes){
    		tlog(8, "heuristic sol class coverage => n: ", n)
    		
    		for(prop.mispl in prop.mispls){
    			tlog(8, "collecting heuristic sol class coverage => prop.mispl: ", prop.mispl)
    			
    		    if(is.na(prop.negs) && d == 1){
    		        prop.negs = compute.prop.neg(n, d, l0, prop.mispl)
    		    }	
    			
    			for(prop.neg in prop.negs){
    				tlog(12, "collecting heuristic sol class coverage => prop.neg: ", prop.neg)
    				
    				for(network.no in in.rand.net.folders){
    					tlog(16, "collecting heuristic sol class coverage => network.no: ", network.no)
    					
    				    curr.data = retreive.heuristic.sol.class.coverage(n, l0, d, prop.mispl, prop.neg, network.no,
    							heur.algo.names, cor.clu.exact.algo, measure)
    					all.data = rbind(all.data, curr.data)
    				}
    				
    			}
    			
    		}
    		
    	}

    	if(length(all.data)>0){
    		if(!dir.exists(OUTPUT.CSV.FOLDER))
    		    dir.create(OUTPUT.CSV.FOLDER, recursive=FALSE, showWarnings=FALSE)
    		write.csv(file=file.path(OUTPUT.CSV.FOLDER,paste0("heuristic-sol-class-coverage-",measure,"-l0=",l0,"-n=",n,"-d=",d,".csv")), x=all.data)
    	}
    }
}

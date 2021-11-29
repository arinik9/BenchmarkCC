

#################################################################
# In this method, we not only handle the first closest partition:
#   In case of non-optimality, a heursitic solution may be less distant (with the same score) to many exact solutions
#   In that case, we look for SECOND.CLOSEST.PARTITION.COL.NAME, THIRD.CLOSEST.PARTITION.COL.NAME, etc.
#
##################################################################
retreive.closest.sol.ids.from.closeness.assoc = function(closeness.assoc.df, kmedoids.clu.result, consider.non.opt=FALSE){
    MAX.NB.COL = 6 # related to SECOND.CLOSEST.PARTITION.COL.NAME, THIRD.CLOSEST.PARTITION.COL.NAME, etc.
    CURR.MAX.NB.COL = ncol(closeness.assoc.df)
    #	MAX.NB.COL = min(ncol(closeness.assoc.df), MAX.NB.COL)
    
    exact.sol.ids = NA
    heur.sol.ids = NA
    if(!consider.non.opt){ # consider only opt sols
        score.vec = closeness.assoc.df[,CLOSEST.SCORE.COL.NAME]
        opt.indx = which(score.vec == 0)
        exact.sol.ids = as.numeric(closeness.assoc.df[opt.indx,CLOSEST.PARTITION.COL.NAME])
        heur.sol.ids = opt.indx-1 # ids start from 0
    } else{ # consider all sols, even non-opt
        # there might be a problem when a heuristic solution is not opt, and it is close to multiple ExCC sols
        # maybe, these ExCC sols are in the same solution class (kmedoids)
        #the idea is the following:
        # 1) check the closest partition, if multiple ExCC sols, check their sol classes. If they are all in the same sol clasess, it is ok
        # 2) otherwise, check the 2nd closest partition, if multiple ExCC sols, check their sol classes. If they are all in the same sol clasess, it is ok
        # 2) and so on ... up to 5th closest partition. If there is still problem, make a choice randomly
        
        
        #		print(closeness.assoc.df)
        mult.closest.indx = which(grepl(",",closeness.assoc.df[,CLOSEST.PARTITION.COL.NAME]))
        if(length(mult.closest.indx) > 0){
            sol.vec = rep(NA, length(mult.closest.indx))
            #col.indx = 1
            for(col.indx in 2:CURR.MAX.NB.COL){
                #while(any(is.na(sol.clu.info.vec)))
                #col.indx = col.indx + 1
                
                # note that col index 2: CLOSEST.PARTITION.COL.NAME, col index 3: SECOND.CLOSEST.PARTITION.COL.NAME, ..
                tlog("col index ", col.indx)
                #print(sol.clu.info.vec)
                for(i in 1:length(mult.closest.indx)){
                    if(is.na(sol.vec[i])){
                        str.indx = closeness.assoc.df[mult.closest.indx[i],col.indx]
                        sol.ids = str.indx
                        #print(i)
                        #print(str.indx)
                        contains = grepl(",",str.indx)
                        if(contains == TRUE){
                            #print(str.indx)
                            sol.ids = as.numeric(unlist(strsplit(str.indx,",")))
                            #print(sol.ids)
                            #print("---")
                            sol.clu.info = kmedoids.clu.result[sol.ids+1] # +1 because sol ids start from 0
                            #print(sol.clu.info)
                            # 1st criteria: look for a fully agreement
                            if(length(unique(sol.clu.info)) == 1)
                                sol.vec[i] = sol.ids[1]
                            else if(length(unique(sol.clu.info)) != 1){ # 2nd criteria: if there is no fully agreement, take the dominant one
                                freq = table(sol.clu.info)
                                mx = max(freq)
                                max.indx = which(freq == mx)
                                if(length(max.indx)==1){
                                    clu.info = names(freq)[max.indx]
                                    sol.vec[i] = sol.ids[which(sol.clu.info == clu.info)][1] # take the first one
                                }
                            } else { # if(col.indx == CURR.MAX.NB.COL)
                                # the worst case: do nothing: do not include into vector
                            }
                            
                            
                        } 
                        #else
                        #	sol.vec[i] = sol.ids
                    }
                }
            }
            
            # so far, we tried to assign a solution class to each value containing ','
            # but, when we can not handle it for some, 'sol.vec' will contain NA values
            uniq.sol.ids = closeness.assoc.df[-mult.closest.indx,CLOSEST.PARTITION.COL.NAME]
            #print(sol.vec)
            rm.indx = which(is.na(sol.vec))
            if(length(rm.indx) > 0){ # there are some NA values
                exact.sol.ids = as.numeric(c(uniq.sol.ids, sol.vec[which(!is.na(sol.vec))]))
                id = mult.closest.indx[rm.indx]
                heur.sol.ids = seq(0,nrow(closeness.assoc.df)-1)[-id]
            } else { # there is not any NA values
                exact.sol.ids = as.numeric(c(uniq.sol.ids, sol.vec))
                heur.sol.ids = seq(0,nrow(closeness.assoc.df)-1)
            }
            
        } else {
            exact.sol.ids = as.numeric(closeness.assoc.df[,CLOSEST.PARTITION.COL.NAME])
            heur.sol.ids = seq(0,nrow(closeness.assoc.df)-1) # all heuristic solutions: ids start from 0
        }
        
    }
    
    res = list()
    res$exact.sol.ids = exact.sol.ids
    res$heur.sol.ids = heur.sol.ids
    
    return(res)
}





#################################################################
# freq.vec: frequency vector from which the variation index is calculated
# for now, it is simple. we only calculate the proportion
#################################################################
calculate.variation.index = function(freq.vec){
    tot.obs = sum(freq.vec)
    prop.vec = freq.vec/tot.obs
    result = prop.vec^2
    # print(variation.index)
    variation.index = sum(result)
    return(variation.index)
}



#################################################################
# m: nb partition
# when is.mtrx.symmetric=TRUE, the distance matrix is created based on only 1 list of partitions
#################################################################
find.closest.partition.id.and.dist.score = function(dist.mtrx, is.mtrx.symmetric, m, by.row=TRUE, by.col=FALSE){
    # get closest solution id(s) (it might be multiple): which solution of algo.name2 is closest to a solution of algo.name1?
    # also, get best distance score
    result = c()
    for(i in seq(1,m)){
        dist.vals = NA
        if(by.col)
            dist.vals = dist.mtrx[,i]
        else 
            dist.vals = dist.mtrx[i,]
        
        if(is.mtrx.symmetric) # dist.values[i]=1 when symmetric, exclude it
            dist.vals = dist.vals[-i]
        
        mn = min(dist.vals) # best dist score
        indx = which(dist.vals == mn) # if multiple index possible
        sol.ids = indx - 1 # since membership files start from 0
        str.sol.ids = paste(sol.ids, collapse=",")
        result = rbind(result, c(mn, str.sol.ids))
    }
    colnames(result) = c(CLOSEST.SCORE.COL.NAME, CLOSEST.PARTITION.COL.NAME)
    return(result)
}




#################################################################
# This method handles only unsymmetric distance matrix, i.e. based on 2 lists of partitions
# process by row: algo1, part.folder1
# process by column: algo2, part.folder2
# Put the algo which has many partitions solutions as 1st algo
# Because, we create the distance matrix in parallel mode by row 
#################################################################
process.algo.comparison.evaluation = function(eval.folder1, part.folder1, algo.name1, eval.folder2, part.folder2, algo.name2, comp.measures, force){
    
    is.mtrx.symmetric = FALSE # this comes by definition of the method	
    mbrshps1 = load.membership.files(part.folder1)
    m1 = length(mbrshps1) # nb partition
    mbrshps2 = load.membership.files(part.folder2)
    m2 = length(mbrshps2) # nb partition




   
    # ---------------------------------------------------------------------
    # distance measures
    tlog(28, "process algo comparison evaluation")
    for(measure in comp.measures){
        tlog(32, "process algo comparison evaluation: distance matrix => measure: ", measure)
        dist.mtrx=NA
        mtrx.file1 = paste0(EVAL.DIST.MATRIX.FILE.PREFIX,"-",measure,"-",algo.name1,"_vs_",algo.name2,".csv")
        mtrx.file2 = paste0(EVAL.DIST.MATRIX.FILE.PREFIX,"-",measure,"-",algo.name2,"_vs_",algo.name1,".csv")
        
        if(algo.name1 %in% COR.CLU.EXACT.ALGOS){

            if((!file.exists(file.path(eval.folder1, mtrx.file1)) && !file.exists(file.path(eval.folder2, mtrx.file2)) || force)){
                par.mode = TRUE
                nb.core = PAR.MODE.NB.CORE.DEFAULT
                chunk.size = PAR.MODE.CHUNK.SIZE.DEFAULT
                if(m1 < PAR.MODE.THRESH.NB.MEM.FILE && m2 < PAR.MODE.THRESH.NB.MEM.FILE){ # if there is just a small amount of files, do it sequantially
                    par.mode = FALSE
                    nb.core = NA
                    chunk.size = NA
                }
                dist.mtrx = create.distance.matrix(measure, part.folder1, part.folder2, nb.part1=m1, nb.part2=m2, algo.name1, algo.name2,
                                                   is.mtrx.symmetric, parallel.mode=par.mode, nb.core=nb.core, chunk.size=chunk.size)
                # record the same matrix into both eval folders
                write.csv(x=dist.mtrx, file=file.path(eval.folder1, mtrx.file1))
                write.csv(x=dist.mtrx, file=file.path(eval.folder2, mtrx.file2))
            }
            
            
            # mean & sd by algo.name1: compute the mean of distance scores for each solution of the algo.name1 with all solutions of the algo.name2
            tlog(28, "process algo comparison evaluation: mean & sd values => ",algo.name1, " vs. ",algo.name2)
            table.file = paste0(EVAL.MEAN.SD.DIST.SCORES.FILE.PREFIX,"-",measure,"-",algo.name1,"_vs_",algo.name2,".csv")
            if(!file.exists(file.path(eval.folder1, table.file)) || force){
                # check if row side of the matrix concerns heurstic or exact method => since by.row = TRUE
                new.dist.mtrx = dist.mtrx
                #if(algo.name1 %in% COR.CLU.HEURISTIC.ALGOS){ # TODO
                #    # do not use 'dist.mtrx' since the method requires file name
                #    mtrx.file = file.path(eval.folder1, mtrx.file1) # doest not matter => file.path(eval.folder2, mtrx.file2)
                #    heur.sols.summary.filename = file.path(eval.folder, paste0(HEURISTIC.SOLS.SUMMARY.FILENAME,".csv"))
                #    new.dist.mtrx = reconstruct.heur.dist.matrix(heur.sols.summary.filename, mtrx.file) # non-square matrix
                #}
                
                result = compute.mean.and.sd.dist.scores(new.dist.mtrx, is.mtrx.symmetric, m1, by.row=TRUE, by.col=FALSE)
                rownames(result) = paste(algo.name1, "sol", seq(0, m1 - 1))
                write.csv(x=result, file=file.path(eval.folder1, table.file), row.names=TRUE)
            }
            
            # mean & sd by algo.name2: compute the mean of distance scores for each solution of the algo.name2 with all solutions of the algo.name1
            tlog(28, "process algo comparison evaluation: mean & sd values => ",algo.name2, " vs. ",algo.name1)
            table.file = paste0(EVAL.MEAN.SD.DIST.SCORES.FILE.PREFIX,"-",measure,"-",algo.name2,"_vs_",algo.name1,".csv")
            if(!file.exists(file.path(eval.folder2, table.file)) || force){
                # check if row side of the matrix concerns heurstic or exact method => since by.col = TRUE
                new.dist.mtrx = dist.mtrx
                #if(algo.name2 %in% COR.CLU.HEURISTIC.ALGOS){
                #    # do not use 'dist.mtrx' since the method requires file name
                #    mtrx.file = file.path(eval.folder1, mtrx.file1) # doest not matter => file.path(eval.folder2, mtrx.file2)
                #    heur.sols.summary.filename = file.path(eval.folder2, paste0(HEURISTIC.SOLS.SUMMARY.FILENAME,".csv"))
                #    new.dist.mtrx = reconstruct.heur.dist.matrix(heur.sols.summary.filename, mtrx.file) # non-square matrix
                #}
                
                result = compute.mean.and.sd.dist.scores(new.dist.mtrx, is.mtrx.symmetric, m2, by.row=FALSE, by.col=TRUE)
                rownames(result) = paste(algo.name2, "sol", seq(0, m2 - 1))
                write.csv(x=result, file=file.path(eval.folder2, table.file), row.names=TRUE)
            }
            # ---------------------------------------------------------------------


            #
            # # ---------------------------------------------------------------------
            # get closest solution id(s) (it might be multiple): which solution of algo.name2 is closest to a solution of algo.name1?
            # also, get best measure score
            tlog(28, "process algo comparison evaluation: closest partition id and dist score => ",algo.name1, " vs. ",algo.name2)
            table.file = paste0(EVAL.CLOSENESS.ASS.INFO.FILE.PREFIX,"-",measure,"-",algo.name1,"_vs_",algo.name2,".csv")
            if(!file.exists(file.path(eval.folder1, table.file)) || force){
                result = find.closest.partition.id.and.dist.score(dist.mtrx, is.mtrx.symmetric, m1, by.row=TRUE, by.col=FALSE)
                rownames(result) = paste(algo.name1, "sol", seq(0, m1 - 1))
                write.csv(x=result, file=file.path(eval.folder1, table.file), row.names=TRUE)
            }
            
            # get closest solution id(s) (it might be multiple): which solution of algo.name1 is closest to a solution of algo.name2?
            # also, get best measure score
            tlog(28, "process algo comparison evaluation: closest partition id and dist score => ",algo.name2, " vs. ",algo.name1)
            table.file = paste0(EVAL.CLOSENESS.ASS.INFO.FILE.PREFIX,"-",measure,"-",algo.name2,"_vs_",algo.name1,".csv")
            if(!file.exists(file.path(eval.folder2, table.file)) || force){
                result = find.closest.partition.id.and.dist.score(dist.mtrx, is.mtrx.symmetric, m2, by.row=FALSE, by.col=TRUE)
                rownames(result) = paste(algo.name2, "sol", seq(0, m2 - 1))
                write.csv(x=result, file=file.path(eval.folder2, table.file), row.names=TRUE)
            }
            # ---------------------------------------------------------------------
            
            
            # new: nb distinct sol and nb distinct opt sol
            # WARNING: actually, it is not clean. We overwrite an existing file
            #if(algo.name2 %in% COR.CLU.HEURISTIC.ALGOS){
            #    
            #    sol.table.file = file.path(eval.folder2, paste0(EVAL.NB.SOL.FILENAME,".csv"))
            #    #if(!file.exists(sol.table.file) || force){
            #    table.file = paste0(EVAL.CLOSENESS.ASS.INFO.FILE.PREFIX,"-",measure,"-",algo.name1,"_vs_",algo.name2,".csv")
            #    df = read.csv(file.path(eval.folder1, table.file), row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)
            #    score.vec = df[,CLOSEST.SCORE.COL.NAME]
            #    m.opt = length(which(score.vec == 0))
            #    mtrx = matrix(c(m2, m.opt), nrow=1, ncol=2)
            #    colnames(mtrx) = c(NB.SOL.COL.NAME, NB.OPT.SOL.COL.NAME)
            #    rownames(mtrx) = algo.name2
            #    write.csv(x=mtrx, file=sol.table.file, row.names=TRUE)
            #    #}
            #}
            
            
            
            
            
            # ---------------------------------------------------------------------
            # heur opt solution variation index
            if(algo.name2 %in% COR.CLU.HEURISTIC.ALGOS){
                tlog(24, "process algo evaluation: heuristic optimal solution variation index")
                var.table.file = file.path(eval.folder2, paste0(EVAL.HEUR.OPT.SOL.VARIATION.INDEX.FILENAME,".csv"))
                
                if(!file.exists(var.table.file) || force){
                    table.file = paste0(EVAL.CLOSENESS.ASS.INFO.FILE.PREFIX,"-",measure,"-",algo.name2,"_vs_",algo.name1,".csv")
                    df = read.csv(file.path(eval.folder2, table.file), row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)
                    score.vec = df[,CLOSEST.SCORE.COL.NAME]
                    opt.ids = which(score.vec == 0)
                    
                    table.file = file.path(eval.folder2, paste0(HEURISTIC.SOLS.SUMMARY.FILENAME,".csv"))
                    df = read.csv(file=table.file, header=TRUE, stringsAsFactors=F)
                    opt.sol.frequency.vec = as.numeric(df[opt.ids,FREQ.COL.NAME])
                    
                    result = calculate.variation.index(opt.sol.frequency.vec)
                    mtrx = matrix(result, 1, 1)
                    colnames(mtrx) = VARIATION.INDEX.COL.NAME
                    rownames(mtrx) = algo.name2
                    write.csv(x=mtrx, file=var.table.file, row.names=TRUE)
                }
            }
            # ---------------------------------------------------------------------


            if(algo.name2 %in% COR.CLU.HEURISTIC.ALGOS){
                # new: heuristic optimality (in ExCC side)
                opt.table.file = paste0(EVAL.HEUR.OPTIMALITY.PROP.FILE.PREFIX,"-",algo.name2,"_vs_",algo.name1,".csv")
                if(!file.exists(file.path(eval.folder1, opt.table.file)) || force){
                    table.file = paste0(EVAL.CLOSENESS.ASS.INFO.FILE.PREFIX,"-",measure,"-",algo.name1,"_vs_",algo.name2,".csv")
                    df = read.csv(file.path(eval.folder1, table.file), row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)
                    score.vec = df[,CLOSEST.SCORE.COL.NAME]
                    all.indx = seq(1, nrow(df))
                    opt.indx = which(score.vec == 0)
                    result = matrix(length(opt.indx)/length(all.indx),1,1)
                    colnames(result) = HEUR.OPTIMALITY.PROP.COL.NAME
                    rownames(result) = algo.name1
                    write.csv(x=result, file=file.path(eval.folder1, opt.table.file), row.names=TRUE)
                }
                
                # new: heuristic optimality (in heuristic side)
                opt.table.file = paste0(EVAL.HEUR.OPTIMALITY.PROP.FILE.PREFIX,"-",algo.name2,"_vs_",algo.name1,".csv")
                if(!file.exists(file.path(eval.folder2, opt.table.file)) || force){
                    table.file = paste0(EVAL.CLOSENESS.ASS.INFO.FILE.PREFIX,"-",measure,"-",algo.name2,"_vs_",algo.name1,".csv")
                    df = read.csv(file.path(eval.folder2, table.file), row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)
                    score.vec = df[,CLOSEST.SCORE.COL.NAME]
                    #	opt.ids = which(score.vec == 0)
                    #	# get solution frequency info from another file
                    #	table.file = file.path(eval.folder2, paste0(HEURISTIC.SOLS.SUMMARY.FILENAME,".csv"))
                    #	df = read.csv(file=table.file, header=TRUE, stringsAsFactors=F)
                    #	nb.opt = sum(as.numeric(df[opt.ids,FREQ.COL.NAME]))
                    #	nb.all = length(HEURISTIC.REPETITIONS)
                    #	result = matrix(nb.opt/nb.all,1,1)
                    all.indx = seq(1, nrow(df))
                    opt.indx = which(score.vec == 0)
                    result = matrix(length(opt.indx)/length(all.indx),1,1)
                    colnames(result) = HEUR.OPTIMALITY.PROP.COL.NAME
                    rownames(result) = algo.name2
                    write.csv(x=result, file=file.path(eval.folder2, opt.table.file), row.names=TRUE)
                }

            }

        } # end if
#        else if(algo.name1 %in% COR.CLU.EXACT.ALGOS){
#            e.imb.table.file = file.path(eval.folder1, paste0(EVAL.IMB.INFO.TABLE.FILENAME,".csv"))
#            e.imb.count = as.numeric(read.csv(e.imb.table.file, row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)[1,IMB.COUNT.COL.NAME])
#
#            if(algo.name2 %in% COR.CLU.HEURISTIC.ALGOS){
#                # new: heuristic optimality (in ExCC side)
#                opt.table.file = paste0(EVAL.HEUR.OPTIMALITY.PROP.FILE.PREFIX,"-",algo.name2,"_vs_",algo.name1,".csv")
#                if(!file.exists(file.path(eval.folder1, opt.table.file)) || force){
#                    h.imb.table.file = file.path(eval.folder2, paste0(EVAL.IMB.INFO.TABLE.FILENAME,".csv"))
#                    h.imb.counts = as.numeric(read.csv(h.imb.table.file, row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)[,IMB.COUNT.COL.NAME])
#                    heur.sols.freqs = as.numeric(read.csv(file.path(eval.folder2, paste0(HEURISTIC.SOLS.SUMMARY.FILENAME,".csv")), row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)[,FREQ.COL.NAME])
#                    nb.success = sum(as.integer(h.imb.counts == e.imb.count)*heur.sols.freqs)
#                    nb.tot = sum(heur.sols.freqs)
#                    result = matrix(nb.success/nb.tot,1,1)
#                    colnames(result) = HEUR.OPTIMALITY.PROP.COL.NAME
#                    rownames(result) = algo.name1
#                    write.csv(x=result, file=file.path(eval.folder1, opt.table.file), row.names=TRUE)
#                }
#                
#                # new: heuristic optimality (in heuristic side)
#                opt.table.file = paste0(EVAL.HEUR.OPTIMALITY.PROP.FILE.PREFIX,"-",algo.name2,"_vs_",algo.name1,".csv")
#                if(!file.exists(file.path(eval.folder2, opt.table.file)) || force){
#                    h.imb.table.file = file.path(eval.folder2, paste0(EVAL.IMB.INFO.TABLE.FILENAME,".csv"))
#                    h.imb.counts = as.numeric(read.csv(h.imb.table.file, row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)[,IMB.COUNT.COL.NAME])
#                       heur.sols.freqs = as.numeric(read.csv(file.path(eval.folder2, paste0(HEURISTIC.SOLS.SUMMARY.FILENAME,".csv")), row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)[,FREQ.COL.NAME])
#                    nb.success = sum(as.integer(h.imb.counts == e.imb.count)*heur.sols.freqs)
#                    print(nb.success)
#                    nb.tot = sum(heur.sols.freqs)
#                    result = matrix(nb.success/nb.tot,1,1)
#                    colnames(result) = HEUR.OPTIMALITY.PROP.COL.NAME
#                    rownames(result) = algo.name2
#                    write.csv(x=result, file=file.path(eval.folder2, opt.table.file), row.names=TRUE)
#                }
#
#            }
#
#
#        }


    }
    
}



#################################################################
# This methods depends on the result of the previous method "evaluate.best.k.in.kmedoids()"
#
##################################################################
evaluate.heur.partitions.with.kmedoid.results = function(e.eval.folder, e.part.folder, e.algo.name, h.eval.folder, h.part.folder, h.algo.name,
                                                         comp.measures, force)
{
    
    #    net.folder = get.input.network.folder.path(n, k, d, prop.mispl, prop.neg, network.no)
    
    tlog(24, "process algo evaluation: heuristic solution variation index AND cover proportion")
    for(measure in comp.measures){
        var.result = NA
        prop.result = NA
        
        cover.table.file = file.path(h.eval.folder, paste0(EVAL.HEUR.KMEDOIDS.COVER.FILENAME,".csv"))
        var.indx.table.file = file.path(h.eval.folder, paste0(EVAL.HEUR.SOL.KMEDOIDS.VARIATION.INDEX.FILENAME,".csv"))
        #force=TRUE
        if(!file.exists(cover.table.file) || !file.exists(var.indx.table.file) || force){
            table.file2 = file.path(e.eval.folder, paste0(EVAL.BEST.K.FOR.KMEDOIDS,"-",measure,".csv"))
            if(file.exists(table.file2)){
                df = read.csv(table.file2, row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)
                kemdoids.best.k = as.numeric(df[,BEST.K.FOR.SILH.COL.NAME])
                
                #print(kemdoids.best.k)
                if(kemdoids.best.k != 1){
                    kmedoids.clu.result = unlist(strsplit(df[,BEST.MEM.FOR.SILH.COL.NAME],","))
                    #print(kmedoids.clu.result)
                    
                    table.file2 = paste0(EVAL.CLOSENESS.ASS.INFO.FILE.PREFIX,"-",measure,"-",h.algo.name,"_vs_",e.algo.name,".csv")
                    df = read.csv(file.path(h.eval.folder, table.file2), row.names = 1, header= TRUE, check.names=FALSE, stringsAsFactors=FALSE)
                    
                    #print(h.eval.folder)
                    # ===================================================================
                    res = retreive.closest.sol.ids.from.closeness.assoc(df, kmedoids.clu.result, consider.non.opt=TRUE)
                    #print("after retreive.closest.sol.ids.from.closeness.assoc()")
                    #print(res)
                    exact.sol.ids = res$exact.sol.ids
                    heur.sol.ids = res$heur.sol.ids
                    # ===================================================================
                    opt.sol.clu.info = kmedoids.clu.result[exact.sol.ids+1] # +1 because sol ids start from 0
                    prop.result = length(unique(opt.sol.clu.info))/kemdoids.best.k
                    #print(prop.result)
                    #print("---")
                    
                    table.file = file.path(h.eval.folder, paste0(HEURISTIC.SOLS.SUMMARY.FILENAME,".csv"))
                    #print(table.file)
                    heur.partitions.ass = read.csv(file=table.file, header=TRUE, stringsAsFactors=F)
                    #print(heur.partitions.ass)
                    freq.values = heur.partitions.ass[heur.sol.ids+1,FREQ.COL.NAME]
                    #print(freq.values)
                    #print(length(opt.sol.clu.info))
                    #print(length(freq.values))
                    values = rep(opt.sol.clu.info, freq.values)
                    var.result = calculate.variation.index(table(values))
                    #print(values)
                    #print(var.result)
                    
	#				# ====================================================
					# based on only optimal solutions
					score.vec = df[,CLOSEST.SCORE.COL.NAME]
					opt.indx = which(score.vec == 0)
					sol.ids = as.numeric(df[opt.indx,CLOSEST.PARTITION.COL.NAME]) + 1 # +1 because sol ids start from 0
					opt.sol.clu.info = kmedoids.clu.result[sol.ids]
					var.result = calculate.variation.index(table(opt.sol.clu.info))
					prop.result = length(unique(opt.sol.clu.info))/kemdoids.best.k
	#				# ====================================================
                    
                    mtrx = matrix(var.result, 1, 1)	
                    colnames(mtrx) = VARIATION.INDEX.COL.NAME
                    rownames(mtrx) = h.algo.name
                    write.csv(x=mtrx, file=var.indx.table.file, row.names=TRUE)
                    #print(mtrx)
                    
                    mtrx = matrix(prop.result, 1, 1)	
                    colnames(mtrx) = HEUR.KMEDOIDS.COVER.PROP.COL.NAME
                    rownames(mtrx) = h.algo.name
                    write.csv(x=mtrx, file=cover.table.file, row.names=TRUE)
                    #print(mtrx)
                }
            }
            
            #				mtrx = matrix(var.result, 1, 1)	
            #				colnames(mtrx) = VARIATION.INDEX.COL.NAME
            #				rownames(mtrx) = h.algo.name
            #				write.csv(x=mtrx, file=var.indx.table.file, row.names=TRUE)
            #				
            #				mtrx = matrix(prop.result, 1, 1)	
            #				colnames(mtrx) = HEUR.KMEDOIDS.COVER.PROP.COL.NAME
            #				rownames(mtrx) = h.algo.name
            #				write.csv(x=mtrx, file=cover.table.file, row.names=TRUE)
        }
    }
    
}







#################################################################
# It evaluates the solutions of a given network based on the considered algorithm name and graph type (weighted or not, etc.).
#
# n: graph size
# l0: number of clusters
# d: density
# prop.mispl: proportion of misplaced links
# prop.neg: proportion of negative links
# network.no: network id (the identifiers start from 1)
# cor.clu.heur.algo: the name of correlation clustering algorithm that has been executed
# force: whether or not the existing files are overwritten by a fresh call of all corresponding methods (e.g partitioning method)
#
##################################################################
evaluate.heuristic.partition.after.solution.classes = function(n, l0, d, prop.mispl, prop.neg, network.no,
                                        e.algo.name, h.algo.name, comp.measures, force)
{
    
    net.folder = get.input.network.folder.path(n, l0, d, prop.mispl, prop.neg, network.no)
    
    tlog(16, "start to evaluate exact solutions")
    for(graph.desc.name in c(SIGNED.UNWEIGHTED.FILE)){ # SIGNED.WEIGHTED.FILE
        tlog(20, "evaluating heuristic partitions => graph.desc.name: ", graph.desc.name)
        tlog(20, "evaluating heuristic partitions => algo.name: ", h.algo.name)
        
        e.part.folder = get.part.folder.path(n, l0, d, prop.mispl, prop.neg, network.no, e.algo.name, graph.desc.name)
        e.eval.folder = get.eval.folder.path(n, l0, d, prop.mispl, detected.imb=NA, prop.neg, k=ALL, network.no, e.algo.name, graph.desc.name)
        
        h.part.folder = get.part.folder.path(n, l0, d, prop.mispl, prop.neg, network.no, h.algo.name, graph.desc.name)
        h.eval.folder = get.eval.folder.path(n, l0, d, prop.mispl, detected.imb=NA, prop.neg, k=ALL, network.no, h.algo.name, graph.desc.name)
        if(!dir.exists(h.eval.folder))
            dir.create(path=h.eval.folder, showWarnings=FALSE, recursive=TRUE)
        
        
        # ------------------
        file.copy(from=file.path(h.part.folder,paste0(HEURISTIC.SOLS.SUMMARY.FILENAME,".csv")),
                  file.path(h.eval.folder,paste0(HEURISTIC.SOLS.SUMMARY.FILENAME,".csv")), overwrite=TRUE)
        # ------------------
        
        tlog(20, "proceed heuristic vs. exact solutions in folders (with distinct heuristics, i.e. after post-processing): ",
             h.eval.folder, " and ", e.part.folder)
        
        evaluate.heur.partitions.with.kmedoid.results(e.eval.folder, e.part.folder, e.algo.name, h.eval.folder, h.part.folder, h.algo.name,
                                                      comp.measures, force)
        
    }
}



#################################################################
# It evaluates the solutions of a given network based on the considered algorithm name and graph type (weighted or not, etc.).
#
# It allows to compute the following statistics:
# - nb distinct solutions
# - distance matrix: exact method vs. heuristic method
# - the mean (and str deviation) of distance scores for each solution of the exact method with all solutions of the heuristic method
# - the mean (and str deviation) of distance scores for each solution of the heuristic method with all solutions of the exact method
# - closest solution id(s) (it might be multiple): which solution of exact method is closest to a solution of heuristic method
# - heuristic optimal solution variation index
# - heuristic optimality porportion
#
# n: graph size
# l0: number of clusters
# d: density
# prop.mispl: proportion of misplaced links
# prop.neg: proportion of negative links
# network.no: network id (the identifiers start from 1)
# cor.clu.heur.algo: the name of correlation clustering algorithm that has been executed
# force: whether or not the existing files are overwritten by a fresh call of all corresponding methods (e.g partitioning method)
#
##################################################################
evaluate.heuristic.partition.before.solution.classes = function(n, l0, d, prop.mispl, prop.neg, network.no,
                                        e.algo.name, h.algo.name, comp.measures, force)
{
    
    net.folder = get.input.network.folder.path(n, l0, d, prop.mispl, prop.neg, network.no)
    
    tlog(16, "start to evaluate exact solutions")
    for(graph.desc.name in c(SIGNED.UNWEIGHTED.FILE)){ # SIGNED.WEIGHTED.FILE
        tlog(20, "evaluating heuristic partitions => graph.desc.name: ", graph.desc.name)
        tlog(20, "evaluating heuristic partitions => algo.name: ", h.algo.name)
        
        e.part.folder = get.part.folder.path(n, l0, d, prop.mispl, prop.neg, network.no, e.algo.name, graph.desc.name)
        e.eval.folder = get.eval.folder.path(n, l0, d, prop.mispl, detected.imb=NA, prop.neg, k=ALL, network.no, e.algo.name, graph.desc.name)
        
        h.part.folder = get.part.folder.path(n, l0, d, prop.mispl, prop.neg, network.no, h.algo.name, graph.desc.name)
        h.eval.folder = get.eval.folder.path(n, l0, d, prop.mispl, detected.imb=NA, prop.neg, k=ALL, network.no, h.algo.name, graph.desc.name)
        if(!dir.exists(h.eval.folder))
            dir.create(path=h.eval.folder, showWarnings=FALSE, recursive=TRUE)
        
        
        # ------------------
        file.copy(from=file.path(h.part.folder,paste0(HEURISTIC.SOLS.SUMMARY.FILENAME,".csv")),
                  file.path(h.eval.folder,paste0(HEURISTIC.SOLS.SUMMARY.FILENAME,".csv")), overwrite=TRUE)
        # ------------------
        
        tlog(20, "proceed heuristic vs. exact solutions in folders (with distinct heuristics, i.e. after post-processing): ",
             h.eval.folder, " and ", e.part.folder)
        
        process.algo.comparison.evaluation(e.eval.folder, e.part.folder, e.algo.name, h.eval.folder, h.part.folder, h.algo.name,
                                            comp.measures, force)
        
    }
}




#################################################################
# It is the starting method in the aim of evaluating the solutions of the considered networks. 
#   It handles all networks by graph.sizes,  prop.mispls, my.prop.negs and in.rand.net.folders
#
# It allows to compute the following statistics:
# - Proportion of covered solution classes (kmedoids)
# - variation index of heuristic solutions w.r.t. solution classes (kmedoids)
#
# graph.sizes: a vector of values regarding graph sizes to be considered
# d: density (it is a single value)
# l0: number of clusters to be considered (it is a single value)
# prop.mispls: a vector of values regarding proportion of misplaced links
# prop.negs: a vector of values regarding proportion of negative links (for now, it is not operational)
# in.rand.net.folders: a vector of values regarding input random graph folders. Sequantial integers (1, .., 10)
# cor.clu.exact.algo: the name of correlation clustering algorithm that has been executed for optimal solutions
# force: whether or not the existing files are overwritten by a fresh call of all corresponding methods (e.g partitioning method)
#
##################################################################
evaluate.heuristic.partitions.after.solution.classes = function(graph.sizes, d, l0, prop.mispls, prop.negs, in.rand.net.folders,
                                                                e.algo.name, cor.clu.heur.algos, comp.measures, force)
{
    tlog("starts evaluating heuristic partitions")
    for(n in graph.sizes){
        tlog(4, "evaluating heuristic partitions => n: ", n)
        
        for(prop.mispl in prop.mispls){
            tlog(8, "evaluating heuristic partitions => prop.mispl: ", prop.mispl)
            
            if(is.na(prop.negs) && d == 1){
                prop.negs = compute.prop.neg(n, d, l0, prop.mispl)
            }		
            
            for(prop.neg in prop.negs){
                tlog(12, "evaluating heuristic partitions => prop.neg: ", prop.neg)
                
                
                net.folder = get.input.network.folder.path(n, l0, d, prop.mispl, prop.neg, network.no=NA)
                if(dir.exists(net.folder)){
                    
                    for(network.no in in.rand.net.folders){
                        tlog(16, "evaluating heuristic partitions => network.no: ", network.no)
                        
                        for(cor.clu.algo in cor.clu.heur.algos){
                            tlog(16, "evaluating heuristic partitions => cor.clu.algo: ", cor.clu.algo)
                            evaluate.heuristic.partition.after.solution.classes(n, l0, d, prop.mispl, prop.neg, network.no,
                                                         e.algo.name, cor.clu.algo, comp.measures, force)
                        }
                        
                    }
                }
                
            }
            
        }
        
    }
    
}



#################################################################
# It is the starting method in the aim of evaluating the solutions of the considered networks. 
#   It handles all networks by graph.sizes,  prop.mispls, my.prop.negs and in.rand.net.folders
#
# graph.sizes: a vector of values regarding graph sizes to be considered
# d: density (it is a single value)
# l0: number of clusters to be considered (it is a single value)
# prop.mispls: a vector of values regarding proportion of misplaced links
# prop.negs: a vector of values regarding proportion of negative links (for now, it is not operational)
# in.rand.net.folders: a vector of values regarding input random graph folders. Sequantial integers (1, .., 10)
# cor.clu.exact.algo: the name of correlation clustering algorithm that has been executed for optimal solutions
# force: whether or not the existing files are overwritten by a fresh call of all corresponding methods (e.g partitioning method)
#
##################################################################
evaluate.heuristic.partitions.before.solution.classes = function(graph.sizes, d, l0, prop.mispls, prop.negs, in.rand.net.folders,
                                                                e.algo.name, cor.clu.heur.algos, comp.measures, force)
{
    tlog("starts evaluating heuristic partitions")
    for(n in graph.sizes){
        tlog(4, "evaluating heuristic partitions => n: ", n)
        
        for(prop.mispl in prop.mispls){
            tlog(8, "evaluating heuristic partitions => prop.mispl: ", prop.mispl)
            
            if(is.na(prop.negs) && d == 1){
                prop.negs = compute.prop.neg(n, d, l0, prop.mispl)
            }		
            
            for(prop.neg in prop.negs){
                tlog(12, "evaluating heuristic partitions => prop.neg: ", prop.neg)
                
                
                net.folder = get.input.network.folder.path(n, l0, d, prop.mispl, prop.neg, network.no=NA)
                if(dir.exists(net.folder)){
                    
                    for(network.no in in.rand.net.folders){
                        tlog(16, "evaluating heuristic partitions => network.no: ", network.no)
                        
                        for(cor.clu.algo in cor.clu.heur.algos){
                            tlog(16, "evaluating heuristic partitions => cor.clu.algo: ", cor.clu.algo)
                            evaluate.heuristic.partition.before.solution.classes(n, l0, d, prop.mispl, prop.neg, network.no,
                                                         e.algo.name, cor.clu.algo, comp.measures, force)
                        }
                        
                    }
                }
                
            }
            
        }
        
    }
    
}

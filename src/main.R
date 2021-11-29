
# =============================================================================
# VARIABLES
#   ==> Remark 1: Do not forget to set CPLEX.BIN.PATH correclty in src/define-algos.R
#   ==> Remark 2: PROP.NEGS should be set to 'NA' when DENSITY=1.0
# =============================================================================


# TODO: when I run a method twice, the second time nothing should be produced by the method, it should check
# if the results are already available, I apply this strategy somewhere, but not everywhere ...




## libraries for parallel processing
#library(foreach)
#library(doParallel)

source("src/define-imports.R")


plot.layout <- c( # ==========> it is not taken into account everywhere !! TODO
		# "kamada.kawai"
		# "fruchterman.reingold"
		# "bn_zheng"
		"circle"
)

#####################
plot.format <- c( # ==========> it is not taken into account everywhere !! TODO
		#PLOT.AS.PDF
		#PLOT.AS.JPEG
		#PLOT.AS.PNG
		JUST.PLOT
)

FORCE = FALSE

keep.algo.log.files = TRUE

#UNUSED.COR.CLU.HEURISTIC.ALGOS = c(
#    get.ils.code(l=1, alpha=0.4, gain=0, perturbation=3, time.limit=3600, iter.nbr=10, rcc=FALSE, vns=TRUE), # VNS metaheursitic
#    get.grasp.code(rcc=FALSE, l=1, k=NA, alpha=0.8, gain=0, time.limit=3600, iter.nbr=-1, oneOptNeig=0), # VOTE-BOEM
#    ###get.NIFTY.code(method="greedy-additive"),
#    ###get.NIFTY.code(method="cgc-qpbo"),
#    ###get.NIFTY.code(method="greedy-additive + cgc-qpbo"),
#    #######get.ZONOCC.code(rank=3)
#    ######## get.SPONGE.sym.CC.code()		
#)
COR.CLU.HEURISTIC.ALGOS = c(
		get.ils.code(l=1, alpha=0.4, gain=0, perturbation=3, time.limit=30, iter.nbr=10, rcc=FALSE),
		get.grasp.code(rcc=FALSE, l=1, k=NA, alpha=0.8, gain=0, time.limit=30, iter.nbr=400, oneOptNeig=1),
		get.MLMSB.code(trial=1,time.limit=30),
		get.SA.CC.code(time.limit=30),
		get.TS.CC.code(time.limit=30),
		get.Brusco.VNS.CC.code(time.limit=30),
		get.ICP.GAEC.KLj.CC.code(time.limit=30), # error with large graphs
		get.GAEC.KLj.CC.code(time.limit=30),
		get.MP.GAEC.KLj.CC.code(time.limit=30)
)
HEURISTIC.REPETITIONS = seq(1, 3, by=1)



COR.CLU.EXACT.ALGOS = c(
		#get.BDCC.code(), # TODO: there is sometimes a bug. We need to contact the corresponding authors
		get.ExCC.code(enum.all=FALSE, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), # reduced ILP according to Miyauchi et al. 2018
		get.ExCC.code(enum.all=FALSE, formulation.type = "vertex", triangle.constr.reduced.ILP.model = TRUE), # full ILP model
		get.ExCC.code(enum.all=FALSE, formulation.type = "edge"), # with lazy callback
		# ENUM ALL - OneTreeCC
		get.ExCC.code(enum.all=TRUE, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), # reduced ILP according to Miyauchi et al. 2018
		get.ExCC.code(enum.all=TRUE, formulation.type = "vertex", triangle.constr.reduced.ILP.model = TRUE), # full ILP model
		get.ExCC.code(enum.all=TRUE, formulation.type = "edge"), # with lazy callback
		# ENUMCC
		get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE),
		get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = TRUE),
		get.EnumCC.code(maxNbEdit=3, formulation.type = "edge")
#
#   get.ExCC.code(enum.all=FALSE), # automatic estimation of the formulation type, from which the exact method will be performed
#	get.ExCC.code(enum.all=TRUE), # automatic estimation of the formulation type, from which the exact enumeration method will be performed
)
#EXACT.REPETITIONS = seq(1, 10, by=1) # for ExCC (a single opt sol at a time)
EXACT.REPETITIONS = NA # This indicates if we need to perform some repetitions for exact methods. 
# For now, we do not need to, but if we include some matheuristics later, it can be useful

COR.CLU.ALGOS = c(COR.CLU.EXACT.ALGOS, COR.CLU.HEURISTIC.ALGOS)



# not all the measures are distance measure, 
# but we will convert all similarity measure into distance in the function 'compare.partition.pair' 
#	in 'evaluate-partitions/compare-partitions.R'.
# IMPORTANT: if you want to calculate EDIT distance, which is not normalized measures,
#           and if you want to compute both the unnormalized and normalized versions:
#           So, write first the unnormalized version, then add the normalized version.
#           If you want to use other distance measures, ordering is not important so.
#           We do this, because calculating EDIT distance might be time consuming, so its normalization should not be
COMP.MEASURES <- c(
		#VI # variation of information
		NVI # according to the paper: Xuan Vinh et al. (2010)
#		EDIT # edit distance
#		EDIT.NORM # normalized edit distance
# HA.ARI, Adjusted Rand Index
)


#K.LIMITS = c(3,3) # LIMIT K-MEDOIDS RESULTS
K.LIMITS = c(NA,NA) # LIMIT K-MEDOIDS RESULTS


##########################
# INPUT GRAPH PARAMETERS
##########################
GRAPH.SIZES = c(32) #c(70,80,90,100) #c(50,60,70,80,90,100) # c(20,24)
L0 = 3 # THIS SHOULD BE A SINGLE VALUE
PROP.MISPLS = c(0.2,0.3) # c(0.2,0.3) #c(seq(0.10, 0.60, by=0.10))
#PROP.MISPLS = 0.2
DENSITY = 0.50 #0.125 # graph density: we can only give a single value (not a vector !!)
INPUT.RANDOM.NETWORKS = seq(1, 2, by=1) # indicates how many replications there are for the same parameter set
#INPUT.RANDOM.NETWORKS = seq(2, 2, by=1)
# ----------------------------------------------
PROP.NEGS = c(0.3,0.5) #  c(0.3, 0.5, 0.7)
#PROP.NEGS = NA
# Note that PROP.NEGS should be 'NA' when DENSITY=1.0
# In the standpoint of a developer, if PROP.NEGS = NA, we should know that prop.neg can be only 1 value and it can be computed from (graph.size, prop.mispl)
# Otherwise, user should be able to give a common range of prop.negs for each pair of (graph.size, prop.mispl)
# ----------------------------------------------




DETECTED.IMB.INTERVALS = c()
DETECTED.IMB.INTERVAL.SEQ.VALS = seq(0.00, 1.00, 0.05) # this vector does not matter. In any case, we check if the folder exists or not
for(i in 1:(length(DETECTED.IMB.INTERVAL.SEQ.VALS)-1)){
	lower.bound = DETECTED.IMB.INTERVAL.SEQ.VALS[i]
	upper.bound = DETECTED.IMB.INTERVAL.SEQ.VALS[i+1]
	desc = paste0("[",lower.bound,",",upper.bound,")")
	DETECTED.IMB.INTERVALS = c(DETECTED.IMB.INTERVALS, desc)
}



##########################################################################
# Another scenario: This is used in the article
##########################################################################
#GRAPH.SIZES = seq(16, 36, by=4)
#PROP.MISPLS = c( seq(0.05, 0.40, by=0.05)) # when l0=4 is selected
#PROP.MISPLS = c( seq(0.05, 0.60, by=0.05)) # when l0=3 is selected
#PROP.MISPLS = c( seq(0.05, 0.85, by=0.05) ) # when l0=2 is selected
#DENSITY = 1 # we can only give a single value (not a vector !!)
#L0 = 2 # OR L0 = 3 OR L0 = 4
#PROP.NEGS = NA
#INPUT.RANDOM.NETWORKS = seq(1, 100, by=1)
# =======================================================================








# =============================================================================
# FUNCTIONS
# =============================================================================

# # #################################################################
# # # POST-PROCESSING INPUT NETWORKS ==> since layout algos are stochastic,
# # #       the obtained layout is used whenever plot is used
# # #################################################################
add.layouts.for.all.networks(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
		UNSIGNED.GRAPH.LAYOUTS, SIGNED.GRAPH.LAYOUTS)


##################################################################
# PARTITION NETWORKS
##################################################################
partition.networks(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
		COR.CLU.HEURISTIC.ALGOS, HEURISTIC.REPETITIONS, COR.CLU.EXACT.ALGOS, EXACT.REPETITIONS,
		keep.algo.log.files, plot.format, plot.layout, FORCE)


#################################################################
# CREATE GEPHI NETWORKS with partition info for all solutions  --- For now, it is only for ExCC-all
#   ==> it is a bit slower (depending on the number of obtained partitions)
#################################################################
#create.gephi.networks(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS, COR.CLU.EXACT.ALGO, FORCE)


#if(FALSE){
	
	
# 
# # ################################################################
# # #POST-PROCESSING FOR HEURISTIC SOLUTIONS
# # ################################################################
	post.processing.heuristic.solutions(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			COR.CLU.HEURISTIC.ALGOS, HEURISTIC.REPETITIONS, TRUE, plot.format)
	
	
	#################################################################
	# EVALUATE PARTITIONS
	#################################################################
	## warning: an exact method name is used inside this function for calculation - to check
	evaluate.partitions(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			COR.CLU.ALGOS, COMP.MEASURES, FORCE)
	
	
	evaluate.EnumCC(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), FORCE)		
	
	evaluate.EnumCC(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = TRUE), FORCE)
	
	evaluate.EnumCC(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "edge"), FORCE)
	
	
	
	
	
	#################################################################
# CLUSTER ANALYSIS
	#################################################################
	K.LIMITS = c(1,20)
#perform.all.cluster.analysis(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
#		get.ExCC.code(enum.all=TRUE), COMP.MEASURES, K.LIMITS, FORCE)
	
	perform.all.cluster.analysis(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), COMP.MEASURES, K.LIMITS, FORCE)
	
	
# # ################################################################
# # # EVALUATE PARTITIONS WITH KMEDOID
# # ################################################################
	evaluate.partitions.with.kmedoid.results(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.ExCC.code(enum.all=TRUE),
			#VI,
			COMP.MEASURES,
			FORCE)
	
	
	
	evaluate.partitions.with.kmedoid.results(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE),
			#VI,
			COMP.MEASURES,
			FORCE)
	
	
	
	
	
# ##############################################################
# # CLUSTER CHARACTERIZATION
# ##############################################################
#perform.all.cluster.characterization(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
#		get.ExCC.code(enum.all=TRUE, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), COMP.MEASURES, K.LIMITS, FORCE)
	
	perform.all.cluster.characterization(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), COMP.MEASURES, K.LIMITS, FORCE)
	
	
# # # #################################################################
# # # # OLD ? - CLUSTER ANALYSIS DIFFERENCES - MEASURES ===> needs multiple comp measures ...
# # # #################################################################
# # # ## COMP.MEASURES <- c(
# # # ##     NMI,
# # # ##     VI
# # # ## )
# # # # show.all.cluster.analysis.difference(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
# # # #         COR.CLU.EXACT.ALGO, COMP.MEASURES, K.LIMITS, FORCE)
# # # 
# # # 
	
	
	
# ##################################################################
# ## EVALUATE PARTITIONS WITH CORE PART ANALYSIS AND REPRESENTATIVES
# ##################################################################
#evaluate.partitions.with.cluster.characterization(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
#		get.ExCC.code(enum.all=TRUE, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE),
#		#VI,
#		COMP.MEASURES,
#		FORCE)
	
	evaluate.partitions.with.cluster.characterization(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE),
			#VI,
			COMP.MEASURES,
			FORCE)
	
	
	
	
# ##################################################################
# ## EVALUATE HEURISTIC PARTITIONS WITH OPTIMALITY AND SOL CLASS
# ##################################################################
#FORCE = TRUE
	evaluate.heuristic.partitions.before.solution.classes(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), 
			COR.CLU.HEURISTIC.ALGOS, COMP.MEASURES, FORCE)
	
	
	evaluate.heuristic.partitions.after.solution.classes(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), 
			COR.CLU.HEURISTIC.ALGOS, COMP.MEASURES, FORCE)
	
	
	
	
	
	###################################################################
	### REORGANIZE
	###################################################################
	reorganize.all.csv.results.by.detected.imbalance(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			COR.CLU.ALGOS, FORCE)
	
	
# # for proportion of misplaced AND detected imbalance intervals
	reorganize.all.csv.results.by.sol.class(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, DETECTED.IMB.INTERVALS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			COR.CLU.ALGOS, COMP.MEASURES, FORCE)
	
	
	
	
	
	###################################################################
	### COMBINE EVALUATED PARTITIONS
	###################################################################
	### for proportion of misplaced  AND  detected imbalance intervals
# warning: we do not combine delay exec times
	take.summary.over.networks(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, DETECTED.IMB.INTERVALS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), COR.CLU.HEURISTIC.ALGOS, COMP.MEASURES, FORCE)
	
	####take.summary.over.networks(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, DETECTED.IMB.INTERVALS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
	####		get.EnumCC.code(maxNbEdit=3, formulation.type = "edge"), COR.CLU.HEURISTIC.ALGOS, COMP.MEASURES, FORCE)
	
	
	
	
	
#-----------------------------------------------------------------------------------------------------------
	##################################################################
	## LAYOUT PLOTS (not tested since last time)
	##################################################################
	##make.layout.plots.by.graph.size.and.imbalance(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, DETECTED.IMB.INTERVALS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
	##	COR.CLU.EXACT.ALGO, COMP.MEASURES, FORCE, plot.format)
	##
	##make.layout.plots.by.graph.size.and.network.no(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, DETECTED.IMB.INTERVALS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
	##	COR.CLU.EXACT.ALGO, COMP.MEASURES, FORCE, plot.format)
	##
	##make.layout.plots.by.imbalance.and.network.no(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, DETECTED.IMB.INTERVALS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
	##	COR.CLU.EXACT.ALGO, COMP.MEASURES, FORCE, plot.format)
	
	
	###################################################################
	### SUMMARY FOLDER - LAYOUT PLOTS
	###################################################################
	make.layout.plots.by.graph.size.and.imbalance(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, DETECTED.IMB.INTERVALS, PROP.NEGS, c(SUMMARY.FOLDER.NAME),
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), COMP.MEASURES, FORCE, plot.format)
	
	
# heuristic methods
# warning: an exact method name is used inside this function for calculation - to check
# TODO: error for >> result.type=paste0(NB.SOLUTION.BY.SOL.CLASSES,"-",measure)
	make.layout.plots.by.graph.size.and.imbalance(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, DETECTED.IMB.INTERVALS, PROP.NEGS, c(SUMMARY.FOLDER.NAME),
			COR.CLU.HEURISTIC.ALGOS, COMP.MEASURES, FORCE, plot.format)
	
	
	
	#################################################################
# WRITE SOME RESULTS INTO CSV FILES
	#################################################################
	
	collect.all.best.k.for.kmedoids(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), COMP.MEASURES, FORCE)
	
	collect.all.nb.opt.solution(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), COMP.MEASURES, FORCE)
	
	collect.core.parts(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), COMP.MEASURES, FORCE)
	
	collect.all.heuristic.optimality.proportion(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			COR.CLU.HEURISTIC.ALGOS, get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE))
	
	collect.all.heuristic.sol.class.coverage(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			COR.CLU.HEURISTIC.ALGOS, get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE),
			COMP.MEASURES)
	
	
	
	
	collect.all.nb.opt.solution(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), FORCE)
	
	algos = c(get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE),
			get.ExCC.code(enum.all=TRUE, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE))
	collect.all.exec.times(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			algos, FORCE)
	
	
	collect.all.delay.exec.time(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), FORCE)
	
	algos = c(get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE))
	collect.all.edit.dist.nb.components(GRAPH.SIZES, DENSITY, L0, PROP.MISPLS, PROP.NEGS, INPUT.RANDOM.NETWORKS,
			algos, FORCE)
	
	plot.delay.exec.times(GRAPH.SIZES, DENSITY, L0, PROP.NEGS,
			INPUT.RANDOM.NETWORKS, get.EnumCC.code(maxNbEdit=3, formulation.type = "vertex", triangle.constr.reduced.ILP.model = FALSE), FORCE)
	
#}

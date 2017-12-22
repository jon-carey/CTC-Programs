rm(list=ls()) #Clear the workspace and load necessary packages
# #############
# #USER INPUTS#
# #############
#  #print as a pdf?
#   ifpdf = FALSE
#  #observed, model-lc, model-totmort
#   catchtype = "model-lc"
# 
#   #CLB1702
#  #print as a pdf?
#   ifpdf = FALSE
#     maxyear = 2016
#  #observed, model-lc, model-totmort
#   catchtype = "model-lc"
#    #input files that YOU provide
#     Dir <- "C:\\data\\Temp\\CTC\\1702clb\\"
#     
#     Paths = list(paste(Dir, "CLB1702auxCatch.txt", sep = ""),
#                  paste(Dir, "9806fisherymap.txt", sep = ""),
#                  paste(Dir, "9806stockmap.txt", sep = ""),
#                  paste(Dir, "1702P_fish_AABM_CCC.csv", sep = ""),
#                  paste(Dir, "1702P_fish_ISBM_CCC.csv", sep = ""),
#                  paste(Dir, "clb1702obsCatch.csv", sep = ""),
#                  paste(Dir, "clb1702fisheryCodes.csv", sep = ""),
#                  paste(Dir, "clb1702stockCodes.csv", sep = ""))
#     
#     auxcat     = read.delim(Paths[[1]],header=TRUE)
#     fisherymap = read.delim(Paths[[2]],header=TRUE)
#     stockmap   = read.delim(Paths[[3]],header=TRUE)
#     aabmcat    = read.csv(Paths[[4]], header=TRUE)
#     isbmcat    = read.csv(Paths[[5]], header=TRUE)
#     obscat     = read.csv(Paths[[6]], header=TRUE)
#     modfishery = read.csv(Paths[[7]], header=TRUE)
#     modstocks  = read.csv(Paths[[8]], header=TRUE)
#     
#     # setwd("C:\\zWork\\C&E\\2017 StockCompStacked\\")
#     # auxcat     = read.delim("CLB1702auxCatch.txt",header=TRUE)
#     # fisherymap = read.delim("9806fisherymap.txt",header=TRUE)
#     # stockmap   = read.delim("9806stockmap.txt",header=TRUE)
#     # #input files from the CLB directory
#     # tmpdir = setwd("C:\\zWork\\C&E\\2017 CLB\\CLB1702")
#     # aabmcat    = read.csv("1702P_fish_AABM_CCC.csv", header=TRUE)
#     # isbmcat    = read.csv("1702P_fish_ISBM_CCC.csv", header=TRUE)
#     # obscat     = read.csv("obsCatch.csv", header=TRUE)
#     # modfishery = read.csv("fisheryCodes.csv", header=TRUE)
#     # modstocks  = read.csv("stockCodes.csv", header=TRUE)
#     
#     comp_1702 = StockCompData(stockmap = stockmap, fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL)
#     cat_1702 = CatchData(fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL)
#     #output directory
#     this_is_the_place <- "C:\\data\\Temp\\CTC"


 #Phase 2 Base  Period Calibration
 #print as a pdf?
  ifpdf = FALSE
    maxyear = 2016
 #observed, model-lc, model-totmort
  catchtype = "model-lc"
   #input files that YOU provide
  Dir <- "C:\\data\\GitHub\\CTC-Programs\\Catch Comparison Figs\\Input Files\\12-2017\\ToGenerateCatFiles\\"
  
  Paths = list(paste(Dir, "bpP2\\bpP2auxCatch.csv", sep = ""),
               paste(Dir, "bpP2\\bpP2fisherymap.txt", sep = ""),
               paste(Dir, "bpP2\\bpP2stockmap.txt", sep = ""),
               paste(Dir, "bpP2\\bpP22017P_fish_AABM_CCC.csv", sep = ""),
               paste(Dir, "bpP2\\bpP22017P_fish_ISBM_CCC.csv", sep = ""),
               paste(Dir, "bpP2\\obsCatch.csv", sep = ""),
               paste(Dir, "bpP2\\fisheryCodes.csv", sep = ""),
               paste(Dir, "bpP2\\stockCodes.csv", sep = ""))
  
  auxcat     = read.csv(Paths[[1]],header=TRUE)
  fisherymap = read.delim(Paths[[2]],header=TRUE)
  stockmap   = read.delim(Paths[[3]],header=TRUE)
  aabmcat    = read.csv(Paths[[4]], header=TRUE)
  isbmcat    = read.csv(Paths[[5]], header=TRUE)
  obscat     = read.csv(Paths[[6]], header=TRUE)
  modfishery = read.csv(Paths[[7]], header=TRUE)
  modstocks  = read.csv(Paths[[8]], header=TRUE)
  
  
   #  setwd("C:\\zWork\\C&E\\2017 StockCompStacked\\")
   #  auxcat     = read.delim("bpP2auxCatch.txt",header=TRUE)
   #  fisherymap = read.delim("bpP2fisherymap.txt",header=TRUE)
   #  stockmap   = read.delim("bpP2stockmap.txt",header=TRUE)
   # #input files from the CLB directory
   #  tmpdir = setwd("C:\\zWork\\C&E\\2017 StockCompStacked\\April2017Ph2AnnualCalibNoTweakingBPC_V1-8")
   #  aabmcat    = read.csv("bpP22017P_fish_AABM_CCC.csv", header=TRUE)
   #  isbmcat    = read.csv("bpP22017P_fish_ISBM_CCC.csv", header=TRUE)
   #  obscat     = read.csv("obsCatch.csv", header=TRUE)
   #  modfishery = read.csv("fisheryCodes.csv", header=TRUE)
   #  modstocks  = read.csv("stockCodes.csv", header=TRUE)
    comp_bpP2 = StockCompData(stockmap = stockmap, fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL)
    cat_bpP2 = CatchData(fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL)
    #output directory
    # this_is_the_place <- "C:\\data\\GitHub\\CTC-Programs\\Catch Comparison Figs\\Input Files\\12-2017\\ToGenerateCatFiles"

#write some output
	write.csv(cat_bpP2, paste(Dir,"bpP2clb - cat file.csv", sep = ""), row.names=FALSE)
	# write.csv(cat_1702, "1702clb - cat file.csv", row.names=FALSE)




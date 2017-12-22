###
#Convert a Catch Matrix to a Catch Data Frame
###
#inputs:
#x - input matrix of auxiliary catch information in the layout of 
	#column 1 year, 
	#column 2:n catch by stock, 
	#column name is numeric, corresponding to the model fishery number
#outputs:
#out - a dataframe of the aux catch data in the layout used elsewhere
ConvertCatchMat <- function(x) {
	#note that the format of aux cat matrix is year, fishery, and obs_catch_nom
	fnames = as.numeric(substr(names(x)[2:ncol(x)],2,3)) #fishery names reformatted - R adds an "X" to numeric variables
	out = data.frame(year = rep(matrix(as.matrix(x[,1])),ncol(x)-1),
	                 fishery = matrix(sapply(fnames, rep, nrow(x))),
	                 obs_catch_nom = matrix(as.matrix(x[,2:ncol(x)])))
	return(out)
  #OLD CODE BELOW
  #if the number of columns greater than 3, implies format 2
  #NOTE, routine ASSUMES fisheries are in numerical order. a must!
    #nauxfish = ncol(auxcat)-1
    #fnumbers = substring(colnames(auxcat)[2:ncol(auxcat)],2,4)
    #fnumbers = sort(rep(as.numeric(fnumbers), nrow(auxcat)))
    #years = rep(auxcat$Year, length(2:ncol(auxcat)))
    #temp = data.frame(year=years, fishery=fnumbers)
    #temp$obs_catch_nom = do.call(c, auxcat[,-1])
    #auxcat = temp
}


###
#StackedAreaData
###
#inputs:
 #stockmap
 #fisherymap
 #auxcat
 #obscat
 #aabmcat
 #isbmcat
 #write_output - to save a file, specify the file name here. ELSE NULL and do nothing. 
#outputs:
 #DataToR - a dataframe of the aux catch data in the layout used elsewhere


StockCompData <- function(stockmap, fisherymap, auxcat, obscat, aabmcat, isbmcat, write_output = NULL) {
 #model fishery
  modfishery = fisherymap[c("fishery","fisheryNameLong","fishery.group")]
 #observed catch data comes in 2 formats.
 #the program only takes one format, so manipulate format 2 to format 1
  if(ncol(auxcat)>3) { #test if format is if #of columns is greater than 2
   auxcat = ConvertCatchMat(auxcat)
  }
 #combine aux cat and obs cat (data from the CM) into one file
  allobscat = rbind(auxcat, obscat)
  allobscat = allobscat[ order(allobscat$year, allobscat$fishery), ]
 #model stock comp calculations
  #prelims - NOTE, i'm truncating model catch to observed catch (hence projection 'catch' is truncated)
   minyear = min(allobscat$year)
   maxyear = max(allobscat$year) 
  #format model catch data
   allcat = rbind(aabmcat, isbmcat) #combine isbm and aabm fishery catch
   allcat$nomcat = rowSums(allcat[,5:6]) # Term.catch + preTerm.catch
   allcat$totcat = rowSums(allcat[,5:20]) # The kitchen sink
   allcat = subset(allcat, year <= maxyear)
  #add stock comp mappings, similar to a 'vlookup' 
   allcat=merge(stockmap, allcat, by = "stock") # Append stock mapping to data frame
  #calculate stock comp of catch by 'whole' fish
   stockcomp.nom.lc.numeric = with(allcat, tapply(nomcat, list(year, fishery, stock.group.order), sum))
  #calculate stock comp of model catch by proportion in catch, with special logic if there's no model catch (e.g. NA handling)
   stockcomp.nom.lc.percent = stockcomp.nom.lc.numeric
   for(i in 1:dim(stockcomp.nom.lc.percent)[2]) stockcomp.nom.lc.percent[,i,] = ifelse(stockcomp.nom.lc.numeric[,i,]==0, 0, stockcomp.nom.lc.numeric[,i,]/rowSums(stockcomp.nom.lc.numeric[,i,]))
 #convert data to the r format & calculate/append catch
  #format: 	FisheryName	FisheryNameLong	Year	StockGroup	Catch	PropinCatch
  #prelims
   nfisheries   = nrow(modfishery)
   nstockgroups = ncol(stockcomp.nom.lc.percent[,1,])
   nyears       = nrow(stockcomp.nom.lc.percent[,1,])
   minyear      = min(allcat$year)
  #FisheryName and FisheryNameLong
   DataToR = data.frame(fishery = sort(rep(modfishery$fishery, nyears*nstockgroups)))
   DataToR = merge(modfishery, DataToR, by="fishery")
  #Year
   DataToR$year = rep(sort(rep(minyear:maxyear, nstockgroups)), nfisheries)
  #StockGroup
   temp = sort(unique(stockmap$stock.group.order))
   temp = stockmap[match(temp, stockmap$stock.group.order),]$stock.group
   DataToR$stock = rep(temp, nfisheries*nyears)
  #Catch
   DataToR = merge(DataToR, allobscat, by=c("year","fishery"), sort=FALSE)
  #PropinCatch
   DataToR$catch = NA
   DataToR$pcatch = NA
   index = 1:nstockgroups
   for(i in 1:nfisheries) {
    for(j in 1:nyears) {
      DataToR[index,]$pcatch = stockcomp.nom.lc.percent[j,i,]
      DataToR[index,]$catch = DataToR[index,]$obs_catch_nom*stockcomp.nom.lc.percent[j,i,]
      index = index + nstockgroups
     }
    }
  #Re-order and drop superfolous columns
   DataToR = DataToR[c("fishery","fisheryNameLong","fishery.group","year","stock","catch","pcatch")]
   names(DataToR) = c("FisheryNum","FisheryName","FisheryGroup","Year","StockGroup","Catch","PropinCatch")
  #write output if so desired
   if(!is.null(write_output)) {
    write.table(x=DataToR, file=write_output, sep="\t", row.names=FALSE)
   }
  return(DataToR)
}


###########
#CatchData#
###########
#description
 #function manipulates data 
#inputs:
 #fisherymap
 #auxcat
 #obscat
 #aabmcat
 #isbmcat
 #write_output - to save a file, specify the file name here. ELSE NULL and do nothing. 
#outputs:
 #DataToR - a dataframe of the aux catch data in the layout used elsewhere
#
CatchData <- function(fisherymap, auxcat, obscat, aabmcat, isbmcat, write_output = NULL) {
 #model fishery
  modfishery = fisherymap[c("fishery","fisheryNameLong","fishery.group")]
 #observed catch data comes in 2 formats.
 #the program only takes one format, so manipulate format 2 to format 1
  if(ncol(auxcat)>3) { #test if format is if #of columns is greater than 2
   auxcat = ConvertCatchMat(auxcat)
  }
 #combine aux cat and obs cat (data from the CM) into one file
  allobscat = rbind(auxcat, obscat)
  allobscat = allobscat[ order(allobscat$year, allobscat$fishery), ]
 #model stock comp calculations
  #prelims - NOTE, i'm truncating model catch to observed catch (hence projection 'catch' is truncated)
   minyear = min(allobscat$year)
   maxyear = max(allobscat$year) 
  #format model catch data
   allcat = rbind(aabmcat, isbmcat) #combine isbm and aabm fishery catch
   allcat$nomcat = rowSums(allcat[,5:6]) # Term.catch + preTerm.catch
   allcat$totcat = rowSums(allcat[,5:20]) # The kitchen sink
   allcat = subset(allcat, year <= maxyear)
  #calculate catch by fishery and type
   nomcat = with(allcat, tapply(nomcat, list(year, fishery), sum))
   totcat = with(allcat, tapply(totcat, list(year, fishery), sum))
  #convert nomcat and totcat
   nomcat2 = ConvertCatchMat(data.frame(year=as.numeric(rownames(nomcat)),data.frame(nomcat)))
   totcat2 = ConvertCatchMat(data.frame(year=as.numeric(rownames(totcat)),data.frame(totcat)))
   names(nomcat2)[3] = "nomcat"
   names(totcat2)[3] = "totcat"
 #convert data to the r format & calculate/append catch
  #format: 	FisheryName	FisheryNameLong	Year	StockGroup	Catch	PropinCatch
  #prelims
   nfisheries   = nrow(modfishery)
   nyears       = nrow(nomcat)
  #FisheryName and FisheryNameLong
   DataToR = data.frame(fishery = sort(rep(modfishery$fishery, nyears)))
   DataToR = merge(modfishery, DataToR, by="fishery")
  #Year
   DataToR$year = rep(sort(rep(minyear:maxyear)), nfisheries)
  #Catch
   DataToR = merge(DataToR, allobscat, by=c("year","fishery"), sort=FALSE)
   DataToR = merge(DataToR, nomcat2, by=c("year","fishery"), sort=FALSE)
   DataToR = merge(DataToR, totcat2, by=c("year","fishery"), sort=FALSE)
  #Re-order and drop superfolous columns
   DataToR = DataToR[c("fishery","fisheryNameLong","fishery.group","year","obs_catch_nom","nomcat","totcat")]
   names(DataToR) = c("FisheryNum","FisheryName","FisheryGroup","Year","Observed_Catch","Model_LCatch","Model_TM")
  #write output if so desired
   if(!is.null(write_output)) {
    write.table(x=DataToR, file=write_output, sep="\t", row.names=FALSE)
   }
  return(DataToR)
}


#######
#StackedAreaFigures
######
#description
 #This program is designed to create stacked area time series figures of Chinook-model derived
 #stock composition of actual landed catch; the R code is fairly straightforward; however, the 
 #prep of modeloutput as needed for the program requires additional work, spearheaded by Gayle in 2012
 #NOTE: This is another somewhat painful use of ggplot, but it gets it done as needed...
 #      ALSO, be sure to relabel as needed the APPENDIX LETTER AND YEAR associated with captions, 
 #      which are written into image files here as a matter of convenience/function
 #code from pete m, the original CTC R using badass
#inputs
 #
#outputs
 #
StackedAreaFigures <- function(x, whichstocks = "all", ifpdf = FALSE, out_dir = NULL) {
#set directory
if(!is.null(out_dir)) {
 odir = getwd()
 setwd(out_dir)
}
#Load necessary packages
require(ggplot2)
require(gridExtra)
#change directory for file writing
 setwd(this_is_the_place)
 
#load data
 modcomp = read.table(modcompfilename,header=T)

figs = max(modcomp$FisheryNum)
i = 1

#This can be written into a single multi-page pdf OR as multiple individual jpgs; 
#the latter imports readily into MS Word but is sloppier for transport/other uses perhaps
#if doing pdf option, uncomment (1) the pdf line, (2) the print line and (3) the dev.off()
#************************************************************************************
if(ifpdf) pdf(file=paste(this_is_the_place,"\\","ModStockCompCLB", substr(maxyear+1,3,4),"XX.pdf",sep=""),height=8,width=13)
while(i<=figs)
{

    #take subset of data for each plot
    sub1<-subset(modcomp,modcomp$FisheryNum==i)
    
    #determine while running each fishery what the axis tick breaks should be
    lim<-max(tapply(sub1$Catch,sub1$Year,sum))
    if(lim <= 50000)
    {tickseq = seq(0,50,5)} else
      if(lim >= 300000)
      {tickseq = seq(0,1000,50)} else
        if(lim >= 200000)
        {tickseq = seq(0,300,30)} else
          if(lim >100000)
          {tickseq = seq(0,200,20)} else
            {tickseq = seq(0,100,10)}
      
    #the figure meat
    image<-ggplot(sub1, aes(x=sub1$Year, y=sub1$Catch/1000, fill=sub1$StockGroup)) + 
      geom_area(colour="black", size=.2, alpha=1) + 
      scale_fill_brewer(palette="Set3",name="Stock Group",
                        breaks=c('ORCST','CR-tule','CR-sp&su','CR-bright','WACST','PSD',
                                 'FR-late','FR-early','GS','WCVI','NCBC','SEAK')) + 
      theme(axis.title.x = element_text(size=20),axis.text.x  = element_text(angle=0, size=15,color="black")) +
      theme(axis.title.y = element_text(size=20),axis.text.y  = element_text(angle=0, size=15,color="black")) +
      labs(title = as.character(sub1$FisheryNameLong[1])) +
      theme(plot.title = element_text(size = rel(2))) +
      ylab("Catch (thousands)")+xlab("")+
      scale_x_continuous(expand=c(0,0),breaks=seq(1980,maxyear,5))+
      scale_y_continuous(expand=c(0,0),breaks=tickseq)+
      theme(legend.title = element_text(size=15, face="bold"))+
      theme(legend.text = element_text(size = 15))+
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ #removes the background grid
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ #removes the background grid
      theme(panel.background = element_blank())+ #removes the gray filled back
      theme(panel.border = element_rect(fill=NA,linetype = "solid",colour = "black")) #Adds a border to plot 
      
      xx<-as.character(sub1$FisheryNameLong)[1]
      Caption<-paste("Appendix E",i,"  Chinook Model estimates of landed catch stock composition for ", xx," 1979-", max(sub1$Year), sep="")
      # the above bit adds a caption to the figures
    
      image <- arrangeGrob(image,sub=textGrob(Caption,x = 0, hjust = 0, vjust=0.1,
                                              gp = gpar( fontsize = 14)))
      #now save each one
      #Sloppy exception handling since file names can't have "\" in them      
      fname<-as.character(sub1$FisheryNameLong)[1]
      if(fname=="Washington/Oregon Troll")
        {fname<-"WashingtonOregon Troll"}
      if(fname=="North/Central BC Sport")
        {fname<-"NorthCentral BC Sport"}  
    
    ggsave(paste(this_is_the_place,"\\",i,fname,".jpg",sep=""),plot=image, width=13, height=8,dpi=800)
    
    if(ifpdf) print(image)
    
    i = i+1
}
dev.off()
#************************************************************************************

setwd(odir)

}


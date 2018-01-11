
# Clear workspace
rm(list=ls(all=TRUE))

########################################################################################
####### Pre-processing; load libraries, set paths, load data, define parameters #######
########################################################################################

# Load any required libraries
library(doBy)
library(ggplot2)
library(gridExtra)
library(scales)

# Source dir
Dir <- "C:\\data\\GitHub\\CTC-Programs\\Catch Comparison Figs\\"

# Set the paths to the necessary input files and output path
paths = list(paste(Dir,"Input Files\\FishLUT.csv",sep=""),
             paste(Dir,"Input Files\\StkLUT_01-2018.csv",sep=""),
             paste(Dir,"Input Files\\01-2018\\1702PESC.CSV",sep=""),
             paste(Dir,"Input Files\\01-2018\\1702PTRM.CSV",sep=""),
             paste(Dir,"Input Files\\01-2018\\BPCV1-21_2017PESC.CSV",sep=""),
             paste(Dir,"Input Files\\01-2018\\BPCV1-21_2017PTRM.CSV",sep=""),
             # paste(Dir,"Input Files\\Nov-2017\\1702P_fish_AABM_CCC.csv",sep=""),
             # paste(Dir,"Input Files\\Nov-2017\\1702P_fish_ISBM_CCC.csv",sep=""),
             paste(Dir,"Input Files\\01-2018\\clb1702obsCatch.csv",sep=""),
             paste(Dir,"Input Files\\01-2018\\CLB1702auxCatch.csv",sep=""),
             # paste(Dir,"Input Files\\Nov-2017\\bpP22017P_fish_AABM_CCC.csv",sep=""),
             # paste(Dir,"Input Files\\Nov-2017\\bpP22017P_fish_ISBM_CCC.csv",sep=""),
             paste(Dir,"Input Files\\01-2018\\BPCV1-21_obsCatch.csv",sep=""),
             paste(Dir,"Input Files\\01-2018\\bpP2auxCatch.csv",sep=""),
             paste(Dir,"Output Files\\",sep=""))


# Read in lookups
FishLUT <- read.csv(paths[[1]])
StkLUT <- read.csv(paths[[2]])

# Read in source data
Esc_OldBP <- read.csv(paths[[3]])
TRun_OldBP <- read.csv(paths[[4]])
Esc_NewBP <- read.csv(paths[[5]])
TRun_NewBP <- read.csv(paths[[6]])
Cat_Obs_OldBP <- read.csv(paths[[7]])
Cat_Aux_OldBP <- read.csv(paths[[8]])
Cat_Obs_NewBP <- read.csv(paths[[9]])
Cat_Aux_NewBP <- read.csv(paths[[10]])

# Designate outfile path
Outfile <- paths[[11]]

# Set max year and trim datasets accordingly
maxyear = 2016
Esc_OldBP <- Esc_OldBP[Esc_OldBP$year <= maxyear, ]
TRun_OldBP <- TRun_OldBP[TRun_OldBP$year <= maxyear, ]
Esc_NewBP <- Esc_NewBP[Esc_NewBP$year <= maxyear, ]
TRun_NewBP <- TRun_NewBP[TRun_NewBP$year <= maxyear, ]


# Identify how you want figures to be saved... 
# Create JPEGs for each figure? (1 = Yes, 0 = No)
CreateJPEG = 0

# Create PDF of all figures? (1 = Yes, 0 = No)
CreatePDF = 1
########################################################################################


########################################################################################
# This isn't necessary unless we want to include observed terminal catches in the figs #
########################################################################################

# # Function to reformat files (from Randy's 'StackedAreaFunLibrary.R')
# ConvertCatchMat <- function(x) {
#     #note that the format of aux cat matrix is year, fishery, and obs_catch_nom
#     fnames = as.numeric(substr(names(x)[2:ncol(x)],2,3)) #fishery names reformatted - R adds an "X" to numeric variables
#     out = data.frame(fishery = matrix(sapply(fnames, rep, nrow(x))),
#                      year = rep(matrix(as.matrix(x[,1])),ncol(x)-1),
#                      obs_catch_nom = matrix(as.matrix(x[,2:ncol(x)])))
#     return(out)
# }
# 
# # Reformat aux files
# Cat_Aux_OldBP <- ConvertCatchMat(Cat_Aux_OldBP)
# Cat_Aux_NewBP <- ConvertCatchMat(Cat_Aux_NewBP)
# 
# # Combine observed and aux catches
# Cat_Obs_OldBP <- rbind(Cat_Aux_OldBP, Cat_Obs_OldBP)
# Cat_Obs_OldBP <- Cat_Obs_OldBP[order(Cat_Obs_OldBP$fishery, Cat_Obs_OldBP$year), ]
# Cat_Obs_NewBP <- rbind(Cat_Aux_NewBP, Cat_Obs_NewBP)
# Cat_Obs_NewBP <- Cat_Obs_NewBP[order(Cat_Obs_NewBP$fishery, Cat_Obs_NewBP$year), ]
# 
# # Set min & max years
# minyear = min(Cat_Obs_NewBP$year)
# maxyear = max(Cat_Obs_NewBP$year)
########################################################################################


########################################################################################
##################################### Work up data ##################################### 
########################################################################################

Esc_NewBP_Names <- colnames(Esc_NewBP)[2:dim(Esc_NewBP)[2]]
Esc_NewBP_Long <- reshape(Esc_NewBP, varying = Esc_NewBP_Names, times = Esc_NewBP_Names,
                          v.names = "Esc", idvar = c("year"), direction = "long")
colnames(Esc_NewBP_Long)[2] <- "Stock"

TRun_NewBP_Names <- colnames(TRun_NewBP)[2:dim(TRun_NewBP)[2]]
TRun_NewBP_Long <- reshape(TRun_NewBP, varying = TRun_NewBP_Names, times = TRun_NewBP_Names,
                           v.names = "TRun", idvar = c("year"), direction = "long")
colnames(TRun_NewBP_Long)[2] <- "Stock"

NewBP <- merge(TRun_NewBP_Long, Esc_NewBP_Long)
NewBP$TermCatch_NewBP <- NewBP$TRun - NewBP$Esc

Esc_OldBP_Names <- colnames(Esc_OldBP)[2:dim(Esc_OldBP)[2]]
Esc_OldBP_Long <- reshape(Esc_OldBP, varying = Esc_OldBP_Names, times = Esc_OldBP_Names,
                          v.names = "Esc", idvar = c("year"), direction = "long")
colnames(Esc_OldBP_Long)[2] <- "Stock"

TRun_OldBP_Names <- colnames(TRun_OldBP)[2:dim(TRun_OldBP)[2]]
TRun_OldBP_Long <- reshape(TRun_OldBP, varying = TRun_OldBP_Names, times = TRun_OldBP_Names,
                           v.names = "TRun", idvar = c("year"), direction = "long")
colnames(TRun_OldBP_Long)[2] <- "Stock"

OldBP <- merge(TRun_OldBP_Long, Esc_OldBP_Long)
OldBP$TermCatch_OldBP <- OldBP$TRun - OldBP$Esc

# # Combine AABM and ISBM catches, separate into pre-terminal and terminal datasets
# Cat_Mod_OldBP <- rbind(Cat_AABM_OldBP, Cat_ISBM_OldBP)
# Cat_Mod_OldBP <- subset(Cat_Mod_OldBP, year <= maxyear)
# Cat_Mod_OldBP_Term <- Cat_Mod_OldBP[Cat_Mod_OldBP$Term.catch > 0, c(1:6)]
# Cat_Mod_OldBP_Term <- summaryBy(Term.catch ~ year + fishery + stock, data = Cat_Mod_OldBP_Term, FUN = sum)
# Cat_Mod_OldBP_PTerm <- Cat_Mod_OldBP[Cat_Mod_OldBP$preTerm.catch > 0, c(1:6)]
# Cat_Mod_OldBP_PTerm <- summaryBy(preTerm.catch ~ year + fishery, data = Cat_Mod_OldBP_PTerm, FUN = sum)
# 
# Cat_Mod_NewBP <- rbind(Cat_AABM_NewBP, Cat_ISBM_NewBP)
# Cat_Mod_NewBP <- subset(Cat_Mod_NewBP, year <= maxyear)
# Cat_Mod_NewBP_Term <- Cat_Mod_NewBP[Cat_Mod_NewBP$Term.catch > 0, c(1:6)]
# Cat_Mod_NewBP_Term <- summaryBy(Term.catch ~ year + fishery + stock, data = Cat_Mod_NewBP_Term, FUN = sum)
# Cat_Mod_NewBP_PTerm <- Cat_Mod_NewBP[Cat_Mod_NewBP$preTerm.catch > 0, c(1:6)]
# Cat_Mod_NewBP_PTerm <- summaryBy(preTerm.catch ~ year + fishery, data = Cat_Mod_NewBP_PTerm, FUN = sum)

# Add stock names to terminal catch data
Stk_oldBP <- na.omit(unique(StkLUT[ ,c(2:3)]))
Stk_newBP <- na.omit(unique(StkLUT[ ,c(5:6)]))
Stk_newold <- StkLUT[,c(5,3)]

OldBP <- merge(OldBP, Stk_oldBP, by.x = "Stock", by.y = "stock.old")
NewBP <- merge(NewBP, Stk_newBP, by.x = "Stock", by.y = "stock.new")
NewBP <- merge(NewBP, Stk_newold, by.x = "Stock", by.y = "stock.new")
########################################################################################


########################################################################################
############################ Create Terminal Catch Figures ############################ 
########################################################################################

p <- list()

# Total Terminal Catch Fig
TermCatch_Old <- summaryBy(TermCatch_OldBP ~ year, data = OldBP, FUN = sum)
TermCatch_Old$Source <- rep("Old Model", dim(TermCatch_Old)[[1]])
colnames(TermCatch_Old) <- c("Year", "Catch", "Source")
TermCatch_New <- summaryBy(TermCatch_NewBP ~ year, data = NewBP, FUN = sum)
TermCatch_New$Source <- rep("New Model", dim(TermCatch_New)[[1]])
colnames(TermCatch_New) <- c("Year", "Catch", "Source")

TermCatch <- rbind(TermCatch_New, TermCatch_Old)

title <- "Total Terminal Catch"

p[[1]] <- ggplot(data = TermCatch, aes(Year, Catch, color = Source))
p[[1]] <- p[[1]] + geom_line(size = 1.0)
p[[1]] <- p[[1]] + scale_color_manual(values=c("#0072B2", "#D55E00", "#000001", "#999999"))
p[[1]] <- p[[1]] + ggtitle(title)
p[[1]] <- p[[1]] + theme(legend.position = "bottom")
p[[1]] <- p[[1]] + scale_x_continuous(breaks = seq(from = 1980, to = 2016, by = 2))
p[[1]] <- p[[1]] + scale_y_continuous(labels = comma)
p[[1]] <- p[[1]] + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# Save
if(CreateJPEG == 1) {
    ggsave(paste(Outfile,title,".jpg",sep=""),p[[1]],height=5,width=7.5)
}

# Individual stock terminal catch figures
# This loop is for stocks with complements between base periods
i=1
for(i in 1:length(unique(Stk_oldBP$stock.old))) {
    TermCatch_Old <- OldBP[OldBP$stockName.old == Stk_oldBP[i,2], ]
    TermCatch_Old <- summaryBy(TermCatch_OldBP ~ year, data = TermCatch_Old, FUN = sum)
    # TermCatch_Old$Source <- rep("Old Model", dim(TermCatch_Old)[[1]])
    TermCatch_New <- na.omit(NewBP[NewBP$stockName.old == Stk_oldBP[i,2], ])
    TermCatch_New <- summaryBy(TermCatch_NewBP ~ year, data = TermCatch_New, FUN = sum)
    # TermCatch_New$Source <- rep("New Model", dim(TermCatch_New)[[1]])
    
    TermCatch <- merge(TermCatch_New, TermCatch_Old, by = "year", all = TRUE)
    TermCatch[is.na(TermCatch)] <- 0
    colnames(TermCatch) <- c("Year", "New Model", "Old Model")
    
    TermCatch <- reshape(TermCatch, varying = c("New Model", "Old Model"), 
                          times = c("New Model", "Old Model"), v.names = "Catch", 
                          idvar = c("Year"), direction = "long")
    colnames(TermCatch)[2] <- "Source"
    
    title <- Stk_oldBP[i,2]
    
    p[[length(p)+1]] <- ggplot(data = TermCatch, aes(Year, Catch, color = Source))
    p[[length(p)]] <- p[[length(p)]] + geom_line(size = 1.0)
    p[[length(p)]] <- p[[length(p)]] + scale_color_manual(values=c("#0072B2", "#D55E00", "#000001", "#999999"))
    p[[length(p)]] <- p[[length(p)]] + ggtitle(title)
    p[[length(p)]] <- p[[length(p)]] + theme(legend.position = "bottom")
    p[[length(p)]] <- p[[length(p)]] + scale_x_continuous(breaks = seq(from = 1980, to = 2016, by = 2))
    p[[length(p)]] <- p[[length(p)]] + scale_y_continuous(labels = comma)
    p[[length(p)]] <- p[[length(p)]] + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
    
    # Save
    if(CreateJPEG == 1) {
        ggsave(paste(Outfile,title,".jpg",sep=""),p[[1]],height=5,width=7.5)
    }
    
}

# This loop is for new base period stocks without old base period counterparts
NewStks <- c(3,4,40)

i=1
for(i in 1:length(NewStks)) {
    TermCatch_New <- NewBP[NewBP$Stock == StkLUT[NewStks[i],5], ]
    TermCatch_New <- summaryBy(TermCatch_NewBP ~ year, data = TermCatch_New, FUN = sum)
    TermCatch_New$Source <- rep("New Model", dim(TermCatch_New)[1])
    
    TermCatch <- TermCatch_New
    colnames(TermCatch) <- c("Year", "Catch", "Source")
    
    title <- StkLUT[StkLUT$stockID.new == NewStks[i], 6]
    
    p[[length(p)+1]] <- ggplot(data = TermCatch, aes(Year, Catch, color = Source))
    p[[length(p)]] <- p[[length(p)]] + geom_line(size = 1.0)
    p[[length(p)]] <- p[[length(p)]] + scale_color_manual(values=c("#0072B2", "#D55E00", "#000001", "#999999"))
    p[[length(p)]] <- p[[length(p)]] + ggtitle(title)
    p[[length(p)]] <- p[[length(p)]] + theme(legend.position = "bottom")
    p[[length(p)]] <- p[[length(p)]] + scale_x_continuous(breaks = seq(from = 1980, to = 2016, by = 2))
    p[[length(p)]] <- p[[length(p)]] + scale_y_continuous(labels = comma)
    p[[length(p)]] <- p[[length(p)]] + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
    
    # Save
    if(CreateJPEG == 1) {
        ggsave(paste(Outfile,title,".jpg",sep=""),p[[1]],height=5,width=7.5)
    }
}
########################################################################################


########################################################################################
############################### Export figures into PDF ################################
########################################################################################
if(CreatePDF == 1) {
    pdf(file=paste(Outfile,"StockSpecificTerm_Catch_Comparisons.pdf",sep=""),height=7,width=10)
    n <- ceiling(length(p)/2)
    
    i=1
    for(i in 1:n) {
        top.plot <- p[[i*2-1]]
        if(i*2 <= length(p)) {
            bottom.plot <- p[[i*2]]
            grid.arrange(top.plot, bottom.plot)
        }
        if(i*2 > length(p)) {
            grid.arrange(top.plot)
        }
    }
    dev.off()
}
########################################################################################

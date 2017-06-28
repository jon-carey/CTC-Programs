
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
Dir <- "C:\\data\\GitHub\\CTC Programs\\Catch Comparison Figs\\"

# Set the paths to the necessary input files and output path
paths = list(paste(Dir,"Input Files\\FishLUT.csv",sep=""),
             # paste(Dir,"StkLUT - OldBP.csv",sep=""),
             # paste(Dir,"StkLUT - NewBP.csv",sep=""),
             paste(Dir,"Input Files\\1702clb - cat file.csv",sep=""),
             paste(Dir,"Input Files\\bpP2clb - cat file.csv",sep=""),
             paste(Dir,"Output Files\\",sep=""))


FishLUT <- read.csv(paths[[1]])
# StkLUT_OldBP <- read.csv(paths[[2]])
# StkLUT_NewBP <- read.csv(paths[[3]])

Cat_OldBP <- read.csv(paths[[2]])
Cat_NewBP <- read.csv(paths[[3]])

Outfile <- paths[[4]]

# Identify how you want figures to be saved... #
# Create JPEGs for each figure? (1 = Yes, 0 = No)
CreateJPEG = 0

# Create PDF of all figures? (1 = Yes, 0 = No)
CreatePDF = 1
########################################################################################


########################################################################################
########################## Create Pre-Terminal Catch Figures ########################### 
########################################################################################

p <- list()

# Cat_NewBP$Mod.Obs.Rat <- Cat_NewBP$Model_LCatch / Cat_NewBP$Observed_Catch
# Cat_OldBP$Mod.Obs.Rat <- Cat_OldBP$Model_LCatch / Cat_OldBP$Observed_Catch

# TOTAL CATCH FIGURE
NewCatch <- summaryBy(Observed_Catch + Model_LCatch ~ Year, data = Cat_NewBP, FUN = sum)
colnames(NewCatch) <- c("Year", "Observed_New", "Model_New")

OldCatch <- summaryBy(Observed_Catch + Model_LCatch ~ Year, data = Cat_OldBP, FUN = sum)
colnames(OldCatch) <- c("Year", "Observed_Old", "Model_Old")

Catch_wide <- merge(NewCatch, OldCatch, by = "Year")
Catch <- reshape(Catch_wide, varying = c("Observed_New", "Model_New", "Observed_Old", "Model_Old"), 
                 times = c("Observed_New", "Model_New", "Observed_Old", "Model_Old"),
                 v.names = "Catch", idvar = c("Year"), direction = "long")
colnames(Catch)[2] <- "Source"

title <- "Total Catch"

p[[1]] <- ggplot(data = Catch, aes(Year, Catch, color = Source))
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


# TOTAL PRE-TERMINAL CATCH FIGURE
Fish_PT <- FishLUT[FishLUT$TerminalID == 0, ]

NewFishIDs <- unique(Fish_PT$FishID_New)
OldFishIDs <- na.omit((unique(Fish_PT$FishID_Old)))

NewCatch <- Cat_NewBP[Cat_NewBP$FisheryNum %in% NewFishIDs, c(1,4:6)]
NewCatch <- summaryBy(Observed_Catch + Model_LCatch ~ Year, data = NewCatch, FUN = sum)
colnames(NewCatch) <- c("Year", "Observed_New", "Model_New")

OldCatch <- Cat_OldBP[Cat_OldBP$FisheryNum %in% OldFishIDs, c(1,4:6)]
OldCatch <- summaryBy(Observed_Catch + Model_LCatch ~ Year, data = OldCatch, FUN = sum)
colnames(OldCatch) <- c("Year", "Observed_Old", "Model_Old")

Catch_wide <- merge(NewCatch, OldCatch, by = "Year")
Catch <-  reshape(Catch_wide, varying = c("Observed_New", "Model_New", "Observed_Old", "Model_Old"), 
                  times = c("Observed_New", "Model_New", "Observed_Old", "Model_Old"),
                  v.names = "Catch", idvar = c("Year"), direction = "long")
colnames(Catch)[2] <- "Source"

title <- "Total Pre-Terminal Catch"

p[[length(p)+1]] <- ggplot(data = Catch, aes(Year, Catch, color = Source))
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

# INDIVIDUAL PRE-TERMINAL FISHERY FIGURES 
i=1
while(i <= dim(Fish_PT)[1]) {
    Fish_PT_i <- Fish_PT[i, ]
    OldFish <- subset(Fish_PT, Fish_PT$FishID_Old == Fish_PT_i[1,2])
    
    title <- Fish_PT[i,6]
    
    # This loop if for fisheries with exactly one ounterpart in each base period
    if(dim(OldFish)[1] == 1) {
        NewCatch <- Cat_NewBP[Cat_NewBP$FisheryNum == Fish_PT$FishID_New[i], c(1,4:6)]
        colnames(NewCatch) <- c("Fishery", "Year", "Observed_New", "Model_New")
        
        OldCatch <- Cat_OldBP[Cat_OldBP$FisheryNum == Fish_PT$FishID_Old[i], c(1,4:6)]
        colnames(OldCatch) <- c("Fishery", "Year", "Observed_Old", "Model_Old")
        
        Catch_wide <- merge(NewCatch[ ,c(2:4)], OldCatch[ ,c(2:4)], by = "Year")
        Catch <- reshape(Catch_wide, varying = c("Observed_New", "Model_New", "Observed_Old", "Model_Old"), 
                         times = c("Observed_New", "Model_New", "Observed_Old", "Model_Old"),
                         v.names = "Catch", idvar = c("Year"), direction = "long")
        colnames(Catch)[2] <- "Source"
        
        i = i + 1
    }
    
    # This loop is for fisheries that were split in the new BP (> 1 new BP fishery for each old BP fishery)
    if(dim(OldFish)[1] > 1) {
        NewFish_i <- unique(OldFish$FishID_New)
        OldFish_i <- unique(OldFish$FishID_Old)
        
        NewCatch <- Cat_NewBP[Cat_NewBP$FisheryNum %in% NewFish_i, c(1,4:6)]
        NewCatch <- summaryBy(Observed_Catch + Model_LCatch ~ Year, data = NewCatch, FUN = sum)
        colnames(NewCatch) <- c("Year", "Observed_New", "Model_New")
        
        OldCatch <- Cat_OldBP[Cat_OldBP$FisheryNum %in% OldFish_i, c(1,4:6)]
        OldCatch <- summaryBy(Observed_Catch + Model_LCatch ~ Year, data = OldCatch, FUN = sum)
        colnames(OldCatch) <- c("Year", "Observed_Old", "Model_Old")
        
        Catch_wide <- merge(NewCatch, OldCatch, by = "Year")
        Catch <-  reshape(Catch_wide, varying = c("Observed_New", "Model_New", "Observed_Old", "Model_Old"), 
                          times = c("Observed_New", "Model_New", "Observed_Old", "Model_Old"),
                          v.names = "Catch", idvar = c("Year"), direction = "long")
        colnames(Catch)[2] <- "Source"
        
        i = i + length(NewFish_i)
    }
    
    # This loop is for new BP fisheries that don't have old BP counterparts
    if(dim(OldFish)[1] < 1) {
        NewCatch <- Cat_NewBP[Cat_NewBP$FisheryNum == Fish_PT$FishID_New[i], c(4:6)]
        colnames(NewCatch) <- c("Year", "Observed_New", "Model_New")
        
        Catch <- reshape(NewCatch, varying = c("Observed_New", "Model_New"), 
                         times = c("Observed_New", "Model_New"),
                         v.names = "Catch", idvar = c("Year"), direction = "long")
        colnames(Catch)[2] <- "Source"
        
        i = i + 1
    }
    
    p[[length(p)+1]] <- ggplot(data = Catch, aes(Year, Catch, color = Source))
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
    pdf(file=paste(Outfile,"PT_Fishery_Catch_Comparisons.pdf",sep=""),height=7,width=10)
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



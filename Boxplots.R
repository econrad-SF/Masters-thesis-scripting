# Significant Differences between Current and Projected Rosy-Finch Distribution
install.packages(c("raster","rgdal","SDMTools","ggplot2"))
install.packages("scales")
install.packages("grid")
library(raster)
library(rgdal)
library(SDMTools)
library(ggplot2)
library(scales)
library(grid)
#################################
#RCP2.6 2061-2080
setwd("~/Documents/University_of_Utah/MSc._Thesis/Models/Maps_Resolution_828x828m/PresenceAbsence")
change <- read.csv("distribution.csv", header=TRUE, sep=",") 
names(change)

brt_26_changePres <- change$ChangePres[1:15]; brt_26_changePres
mean(brt_26_changePres); sd(brt_26_changePres)
rForest_26_changePres <- change$ChangePres[16:30]; rForest_26_changePres
mean(rForest_26_changePres); sd(rForest_26_changePres)

brt_85_changePres
rForest_85_changePres

areaLost <- data.frame("Species Distribution Model"=factor(rep(c("BRT","RF"), each=15)),"Change in Area (Km^2)"=c(brt_26_changePres,brt_85_changePres,rForest_26_changePres,rForest_85_changePres))

#Altered ==============
compact_area <- data.frame("Species Distribution Model"=factor(rep(c("RCP2.6","RCP8.5","RF (RCP2.6)","RF (RCP8.5)"), c(15,17,15,17))),
                "Change in Area (Km^2)"=c(brt_26_changePres,brt_85_changePres,rForest_26_changePres,rForest_85_changePres))
ddplot_area <- ggplot(compact_area, aes(x=Species.Distribution.Model, y=Change.in.Area..Km.2., fill=Species.Distribution.Model)) +
  scale_x_discrete(name="BRT") + scale_y_continuous(name=expression(paste("Change in area ", (km^"2"))), breaks=c(100000,50000,0,-50000,-100000,-150000,-200000), labels=comma) + geom_boxplot() +
  guides(fill=FALSE) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(plot.margin = unit(c(1,1,0,0),"mm")) + theme(axis.text=element_text(size=6)) + theme(axis.title=element_text(size=10))
ddplot_area

# get rid of gridlines: +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# get rid of gray background: +theme_bw() 

compact_percentage <- data.frame("Species Distribution Model"=factor(rep(c("BRT (RCP2.6)","BRT (RCP8.5)","RF (RCP2.6)","RF (RCP8.5)"), c(15,17,15,17))),
                "Change in Percentage"=c(brt_26_percentchangePres,brt_85_percentchangePres,rForest_26_percentchangePres,rForest_85_percentchangePres))
ddplot_percentage <- ggplot(compact_percentage, aes(x=Species.Distribution.Model, y=Change.in.Percentage, fill=Species.Distribution.Model)) +
  scale_x_discrete(name="") + scale_y_continuous(name=expression(paste("Change in percentage")),breaks=c(25,0,-25,-50,-75)) + geom_boxplot() + guides(fill=FALSE) +
  theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + theme(plot.margin=unit(c(1,1,0,0),"mm")) + theme(axis.text=element_text(size=6)) + theme(axis.title=element_text(size=10))  #, text=element_text(size=10)) 
ddplot_percentage
# specify label size: +theme(axis.text=element_text(size=6)) 
myfigure<-multiplot(ddplot_area,ddplot_percentage,cols=2)

ggsave(plot=ddplot_area, filename="Boxplots_area.pdf", path="~/Documents/University_of_Utah/MSc._Thesis/Global_Change_Biology/", width=84.5, height=70, units="mm", dpi=300)
ggsave(plot=ddplot_percentage, filename="Boxplot_percentage.pdf", path="~/Documents/University_of_Utah/MSc._Thesis/Global_Change_Biology/", width=84.5, height=70, units="mm", dpi=300)

# ^^^^^  Altered  ^^^^ ================




#Original
rcp26 <- data.frame("Species Distribution Model"=factor(rep(c("BRT","RF"), each=15)),"Change in Area (Km^2)"=c(brt_26_changePres,rForest_26_changePres))
p1 <- ggplot(rcp26, , aes(x=Species.Distribution.Model, y=Change.in.Area..Km.2., fill=Species.Distribution.Model)) + 
  scale_x_discrete(name="") + scale_y_continuous(name=expression(paste("Change in Area ", (Km^"2")))) + geom_boxplot() + 
  guides(fill=FALSE) + theme(plot.margin=unit(c(1,1,0,0),"mm"))

brt_26_percentchangePres <- change$PercentChangePres[1:15]; brt_26_percentchangePres
mean(brt_26_percentchangePres); sd(brt_26_percentchangePres)
rForest_26_percentchangePres <- change$PercentChangePres[16:30]; rForest_26_percentchangePres
mean(rForest_26_percentchangePres); sd(rForest_26_percentchangePres)

brt_85_percentchangePres
rForest_85_percentchangePres

rcp26pc <- data.frame("Species Distribution Model"=factor(rep(c("BRT","RF"), each=15)),"Change in Percentage"=c(brt_26_percentchangePres,rForest_26_percentchangePres))
p2 <- ggplot(rcp26pc, aes(x=Species.Distribution.Model, y=Change.in.Percentage, fill=Species.Distribution.Model)) + 
  scale_x_discrete(name="") + scale_y_continuous(name="Change in Percentage") + geom_boxplot() + 
  guides(fill=FALSE) + theme(plot.margin=unit(c(1,1,0,0),"mm"))  # top, right, bottom, & left margins.

# Run Multiplot function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot(p1,p2,cols=2)
########################
#RCP8.5 2061-2080

brt_85_changePres <- change$ChangePres[31:47]; brt_85_changePres
mean(brt_85_changePres); sd(brt_85_changePres)
rForest_85_changePres <- change$ChangePres[48:64]; rForest_85_changePres
mean(rForest_85_changePres); sd(rForest_85_changePres)

rcp85 <- data.frame("Species Distribution Model"=factor(rep(c("BRT","RF"), each=17)),"Change in Area (Km^2)"=c(brt_85_changePres,rForest_85_changePres))
p3 <- ggplot(rcp85, aes(x=Species.Distribution.Model, y=Change.in.Area..Km.2., fill=Species.Distribution.Model)) + scale_x_discrete(name="") + 
  scale_y_continuous(labels=comma, name=expression(paste("Change in Area ", (Km^"2")))) + geom_boxplot() + guides(fill=FALSE) + theme(plot.margin=unit(c(1,1,0,0),"mm"))  # top, right, bottom, & left margins.

# to get superscripts in label
#http://stackoverflow.com/questions/19745508/how-to-write-x-axis-title-with-text-and-superscript-ggplot2

# to force labels not to do scientific notation
#http://stackoverflow.com/questions/14563989/force-r-to-stop-plotting-abbreviated-axis-labels-e-g-1e00-in-ggplot2

brt_85_percentchangePres <- change$PercentChangePres[31:47]; brt_85_percentchangePres
mean(brt_85_percentchangePres); sd(brt_85_percentchangePres)
rForest_85_percentchangePres <- change$PercentChangePres[48:64]; rForest_85_percentchangePres
mean(rForest_85_percentchangePres); sd(rForest_85_percentchangePres)

rcp85pc <- data.frame("Species Distribution Model"=factor(rep(c("BRT","RF"), each=17)),"Change in Percentage"=c(brt_85_percentchangePres,rForest_85_percentchangePres))
p4 <- ggplot(rcp85pc, aes(x=Species.Distribution.Model, y=Change.in.Percentage, fill=Species.Distribution.Model)) + scale_x_discrete(name="") + 
  scale_y_continuous(name="Change in Percentage") + geom_boxplot() + guides(fill=FALSE) + theme(plot.margin=unit(c(1,1,0,0),"mm"))  # top, right, bottom, & left margins.

multiplot(p1,p2, cols=2)
multiplot(p3,p4, cols=2)
multiplot(p1,p2,p3,p4, cols=2)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

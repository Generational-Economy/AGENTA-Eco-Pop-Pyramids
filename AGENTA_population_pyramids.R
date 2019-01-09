#####################################################################
# Generate economic population pyramides from AGENTA data
####################################################################

# Code to generate the economic population pyramide figures
# using AGENTA data (www.wittgensteincentre.org/ntadata) and
# Eurostat population data

# Content: 
# 1. Prepare the data (requires your input)
# 2. Bring the data in the required form (no input needed)
# 3. Population pyramide for one selected variable
# 4. Population pyramide for two selected variables

# Required packages
library(ggplot2)

###########################
# 1. Prepare the data (requires your input)
###########################
  
# 1. Set working directory (save also the data ecopyr_long.csv on your local computer)
  setwd("C:/Users/max/Dropbox/AGENTA_projects/Database/eco_pop_pyramids")
  
# 2. Select desired variables and country (you can choose one or two variables)

  var <- c("Labour income (YL)")
  cnt <- "Austria"

  # Options are the follwing NTA and NTTA variables and countries

  # NTTA
    # Countries: Belgium, Bulgaria, Estonia, Finland, France, Germany, Italy, Latvia, Lithuania, Poland, Slovenia,  
    #            Spain, Sweden, United Kingdom,
    # Variables: Education, Leisure, Unpaid work, Personal care, Paid work

  # NTA
    # Countries: Austria, Belgium, Bulgaria, Cyprus, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece,
    #            Hungary, Ireland, Italy, Latvia, Lithuania, Luxembourg, Poland, Portugal, Romania, Slovakia, Slovenia,
    #            Spain, Sweden, United Kingdom
    # Variables: Labour income (YL), Asset income (YA), Consumption (C), Private consumption (CF), Public consumption (CG),
    #            Public health consumption (CGH), Public education consumption (CGE), Public consumption, other (CGX),    
    #            Public transfer benefits (TGI), Public transfer contributions (TGO)  


##############################
# 2. Bring the data in the required form (no input needed)
##############################

  # Read the data
    ecopyr <- read.csv("ecopyr.csv", header=TRUE, sep=",", dec=".")

  # Subset the dataset with selected variable(s) and country
    sel <- ecopyr[ which( (ecopyr$country==cnt)  &  (ecopyr$varname==var[1]) ),]
    
  # Reshape the data from long to wide (variables in columns)
    sel$var1 <- ecopyr$value[ which( (ecopyr$country==cnt)  &  (ecopyr$varname==var[1]) )]
    if (length(var)>=2) {
      sel$var2 <- ecopyr$value[ which( (ecopyr$country==cnt)  &  (ecopyr$varname==var[2]) )]
    }

  # Combine population and economic variables
    sel$var1[is.na(sel$var1)] <- 0 
    sel$varpop1 <- sel$POP*sel$var1/1000000
    
  # Variable that represents the population pyramide (assuming equal distribution of first selected variable) 
    varavg <- sum(sel$varpop1)/sum(sel$POP)*1000000
    sel$varavg <- sel$POP*varavg/1000000
  
  # Different x-axis-titles for NTA and NTTA
    if ("nta2010" %in% unique(sel$source)) {
      labely <- "Million Euros"
    } else {
      labely <- "Million Hours"         
    }
  
  # Corresponding year (input in title)
    year = unique(sel$year)
    
  # Coordinates for text labels "Men" and "Women" and scaling of axis
    varpopmax <-  max(sel$varpop1, na.rm = TRUE)
    
  
###########################################
# 3. Population pyramide for one selected variable
###########################################

if (length(var)==1) {  
  
  # Plot basic bar graph with variables 
  plotpyr <- ggplot(data=sel, aes(x=age)) +
    geom_bar(aes(y=varpop1, fill="A"), col="grey30", stat="identity", width=1, subset(sel, sex=="female")) +
    geom_bar(aes(y=-varpop1), fill="#307A85D2", col="grey30", stat="identity", width=1, subset(sel, sex=="male")) +
    coord_flip() + theme_classic() +
    
  # Add shape of population pyramide
    geom_jitter(aes(y=varavg, fill="C"), stat="identity", subset(sel, sex=="female")) +
    geom_jitter(aes(y=-varavg), stat="identity", subset(sel, sex=="male")) +
    
  # Add titles and labels
    xlab("Age") + ylab(labely) +
    ggtitle(paste0("Economic population pyramid: \n", var[1], ", ", cnt, " ", year)) +
    geom_text(x=98, y=varpopmax/4, label = "Women", size=6) +
    geom_text(x=98, y=-varpopmax/4, label = "Men", size=6) +
    scale_y_continuous(labels = abs, limits = c(-varpopmax-1,varpopmax+1)) +
    scale_x_continuous(labels = abs, limits = c(-1,101), expand=c(0,0)) +
    
  # Colors, look and content of legend
    scale_fill_manual(values=c("A"="#307A8596", "C"="white", aesthetics = "fill"), 
                        name = "",
                        labels=c(paste0(var[1]), "Shape of the population pyramide")) +
    guides(fill = guide_legend(override.aes = list(linetype = c(1,0), shape = c(NA, 16)))) +
    
  # Fine tuning of the plot
    theme(
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      axis.title = element_text(size = 18),
      axis.ticks.length =  unit(0.2, "cm"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey90", size = 0.25),
      plot.margin = unit(c(0,1,0,0), unit = "cm"), #top, right, bottom, left
      plot.title = element_text(colour =  "black", size = 17, hjust = 0.5, lineheight=1.15),
      #legend.key = element_rect(fill = "red", colour = "black"),
      legend.text = element_text(size = 12, colour = "black"),
      legend.position = "bottom"
    )
  
    plotpyr
    
} # end if   
    
################################
# 4. Population pyramide for 2 selected variables
################################
  
if (length(var)>=2) {
  
  # Add second variable
    sel$var2[is.na(sel$var2)] <- 0 
    sel$varpop2 <- sel$POP*sel$var2/1000000
    
  # Coordinates for text labels and scale of x-axis
    varpopmax <-  max(sel$varpop1, sel$varpop2, na.rm = TRUE)
    
  plotpyr <- ggplot(data=sel, aes(age)) +
    
  # Plot of first variable
    geom_bar(aes(y=varpop1, fill="A"), col="grey30", stat="identity", width=1, subset(sel, sex=="female")) +
    geom_bar(aes(y=-varpop1), fill="#307A85DC", col="grey30", stat="identity", width=1, subset(sel, sex=="male")) +
    coord_flip() +
      
  # Plot second variable
    geom_bar(aes(y=varpop2, fill="B"),  col="grey30", stat="identity", width=1, subset(sel, sex=="female")) +
    geom_bar(aes(y=-varpop2), fill="#E9760096",  col="grey30", stat="identity", width=1, subset(sel, sex=="male")) +
      
  # Add shape of population pyramide
    geom_jitter(aes(y=varavg, fill="C"), stat="identity", subset(sel, sex=="female")) +
    geom_jitter(aes(y=-varavg), stat="identity", subset(sel, sex=="male")) +
      
  # Titles and labels
    xlab("Age") + ylab(labely) +
    ggtitle(paste0("Economic population pyramid: ", var[1], " and \n", var[2], ", ", cnt, " ", year)) +
    geom_text(x=98, y=varpopmax/4, label = "Women", size=6) +
    geom_text(x=98, y=-varpopmax/4, label = "Men", size=6) +
    scale_y_continuous(labels = abs, limits = c(-varpopmax-1,varpopmax+1)) +
    scale_x_continuous(labels = abs, limits = c(-1,101), expand=c(0,0)) +
      
  # Colors, look and content of legend
    scale_fill_manual(values=c("A"="#307A8596", "B"="#E9760064", "C"="white", aesthetics = "fill"),
                        name = "",
                        labels=c(var[1], var[2], "Population pyramide (shape)")) +
    guides(fill = guide_legend(override.aes = list(linetype = c(1,1,0),
                                                     shape = c(NA,NA,16)))) +
  # Fine tuning of plot  
    theme(
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length =  unit(0.2, "cm"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey90", size = 0.25),
        plot.margin = unit(c(0,1,0,0), unit = "cm"), #top, right, bottom, left
        plot.title = element_text(colour =  "black", size = 17, hjust = 0.5, lineheight = 1.15),
        #legend.key = element_rect(fill = "red", colour = "black"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.position = "bottom"
      )
    
    plotpyr
    
} # end if
    


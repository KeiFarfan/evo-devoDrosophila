##############################################################################################################
##############################################################################################################
################# A cis-regulatory sequence of the wing selector gene, vestigial,#############################
##################### drives the evolution of wing scaling in Drosophila species #############################
########### Keity J. Farfán-Pira, Teresa I. Martínez-Cuevas, Timothy A. Evans, Marcos Nahmad #################
##############################################################################################################
##############################################################################################################


# Charge packages
library(smatr)
library(tidyverse)
library(ggpmisc)
library(ggpp)
library(ggpubr)

# Set working directory
setwd("C:/Users/keity/Documents/MEGA/DOCTORADO FISIOLOGÍA, BIOFÍSICA Y NEUROCIENCIAS/Lab_things/2022/Septiembre/SMATR/Test_SMATR")

# Create a table with the data and save it in an object (datasets separated for males and females)
females <- read.csv("C:/Users/keity/Documents/MEGA/DOCTORADO FISIOLOGÍA, BIOFÍSICA Y NEUROCIENCIAS/Lab_things/2022/Diciembre/malesHDR_species_test_intercept.csv", h=T)

####### Applying SMATR package

#Comparing to see differences in slopes (Adittionaly used to see 95% Confidence Intervals)

# see if slopes are different to 1
slope_comparison <- sma(wing~tibia*species, log = 'xy', 
                        data = females_WT, slope.test = 1)
summary(slope_comparison)
# see if slopes are different between groups (or species)
group_comparison_slope <- sma(wing~tibia*species, log = 'xy', 
                              data = females_WT, slope.test = 1, 
                              multcomp = TRUE, multcompmethod = "adjust")
summary(group_comparison_slope)
# save the comparison in a matrix, and in a .txt file
multcompmatrix(group_comparison_slope)
capture.output(multcompmatrix(group_comparison_slope), 
               file = "female_comparison_slope_WT.txt")


#ggplot representation of scaling relationships wing/tibia
#Assign colors to species

#Colors for WT species:
#colors <- c("Dmel" = "#c23a32" , "Dsim" = "#206ce6", "Dana" = "#2f9c2a", "Dvir" = "#b80ee3" )

#Color for CRISPR mutant animals:
colors <- c("Dmel_WT" = "#c23a32" , "vgQEmel" = "#edcb09", "vgQEvir" = "#0713f0", "Dvir_WT" = "#b80ee3" )

#Make plot using ggplot2 and SMA adjust
p <-ggplot(females, aes(x=log10(tibia), y=log10(wing), color=species)) +
  geom_point(aes(colour = species)) +
  geom_vline(xintercept = -0.351152292, color = "black", type= "l", lty=2, lwd=1)+ #Include value of median for representation
  stat_ma_line(method = "SMA", se = FALSE)+ 
  stat_ma_eq(method = "SMA", use_label(c("eq", "R2")), size=8)+
  scale_color_manual(values = colors)+
  labs(x = "Log P/D tibia length (mm)", y = "Log P/D wing length (mm)")+
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 30, angle = 0, face = "bold"),
        axis.title.y = element_text(size = 30, angle = 90, face = "bold"),
        axis.text.x = element_text(size = 25, face = "bold"),
        axis.text.y = element_text(size = 25, face = "bold"),
        panel.background = element_rect(color="black", fill = "white"),
        panel.grid.major = NULL,
        panel.grid.minor = NULL,
        legend.position = "none")

#Visualize the plot and save as tiff extension
p
tiff("FemalesWT_WT.tiff", width = 30, height = 20, units = "cm", res = 600)
print(p)
dev.off()


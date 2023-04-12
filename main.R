# load packages 
source("utils/libraries.R")

# load data sets
test1 = read.table("data/DATA_PARDOSA_2.txt", 
                 h = T, 
                 na.string = "NA", 
                 stringsAsFactors = T, 
                 dec = ".") #Tpref data

d_indiv = read.table("data/DONNEES_indiv.txt", 
                   h = T, 
                   na.string = "NA", 
                   stringsAsFactors = T, 
                   dec = ",") #individuals information

length_cephalothorax = read.table("data/length_cephalothorax.txt", 
                                  h = T, 
                                  na.string = "NA", 
                                  stringsAsFactors = T, 
                                  dec = ".") #morphological measurements (body size)

length_legs = read.table("data/length_legs.txt", 
                         h=T, 
                         na.string="NA", 
                         stringsAsFactors = T, 
                         dec=".") #morphological measurements (legs length)

# Tpref dynamic over the 90min experiment
source("scripts/test_dynamic_tpref_90min_with557ind.R") # formatting data and run models
source("scripts/figures_dynamic_Tpref.R") # Create and export figures 

# Effect of factors on Tpref and SD Tpref from the 30th min of the experiment
source("scripts/test_factors_30min_with557ind.R")
source("scripts/figures_factors_Tpref.R")

# Appendix
source("scripts/appendix.R")
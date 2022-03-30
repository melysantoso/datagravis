# Title : Chord Diagram 
# Author : Mely Santoso


# The Hidden Links Between Mental Disorders --------------------------------


# This Inspiration for this cahrt was from Nature article whic can be found 
# here https://www.nature.com/articles/d41586-020-00922-8 #
# The article is actually based on collaborative research by The Brainstorm Consortium
# The article's doi: 10.1126/science.aap8757
# The data containing information about this can be found on suplementary materials
# S7 Disorder-Disorder sheet : here https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6097237/#SM1title 


# Load Library ------------------------------------------------------------


library(readr)
library(circlize)
library(tidyverse)


# Data Wrangling  ---------------------------------------------------------


# The data has been reproceed from original dataset on Excel after being downloaded 
# I don't have any right to push the dataset here 

disorder_dis <- read.csv("disorderdisorder.csv")

# Select the disorder type as viz on nature web 
new_data_2 <-  disorder_dis %>% 
  filter(Phenotype.1 %in% c("ADHD", "Alzheimer's disease","Anorexia nervosa", "Anxiety disorders", 
                            "Autism spectrum disorder", "Bipolar disorder", "Major depressive disorder",
                            "OCD", "PTSD", "Schizophrenia")) %>% 
  filter(Phenotype.2 %in%  c("Alzheimer's disease", "Anorexia nervosa", "Anxiety disorders", "Autism spectrum disorder",
                             "Bipolar disorder", "Major depressive disorder",
                             "OCD", "PTSD", "Schizophrenia", "Tourette Syndrome")) %>% 
  mutate(Phenotype.1 = recode_factor(Phenotype.1, "Alzheimer's disease" = "Alzheimer", 
                                     "Anorexia nervosa" = "Anorexia", 
                                     "Anxiety disorders" = "Anxiety", 
                                     "Autism spectrum disorder" = "Autism", 
                                     "Bipolar disorder" = "Bipolar", 
                                     "Major depressive disorder" = "Depressive"
                                     )) %>% 
  mutate(Phenotype.2 = recode_factor(Phenotype.2, "Alzheimer's disease" = "Alzheimer", 
                                     "Anorexia nervosa" = "Anorexia", 
                                     "Anxiety disorders" = "Anxiety", 
                                     "Autism spectrum disorder" = "Autism", 
                                     "Bipolar disorder" = "Bipolar", 
                                     "Major depressive disorder" = "Depressive", 
                                     "Tourette Syndrome" = "Tourette")) %>% 
  select(-c("Correlation.SE", "P.value")) %>% 
  as_tibble()


# Get the Color for Each Variable -----------------------------------------


grid.col = c(ADHD = "red4", Alzheimer = "turquoise1", 
             Anorexia = "seagreen4",  Anxiety = "violetred4", 
             Autism = "yellow", Bipolar = "grey", 
             Depressive = "orange", OCD = "#EE8EA5", 
             PTSD = "lightblue4", 	Schizophrenia = "turquoise4", 	
             Tourette = "#EFA17F")


# Plot  -------------------------------------------------------------------


chordDiagram(new_data_2, grid.col = grid.col)
circos.clear()

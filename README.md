# nypd-stopfrisk
This project addresses the Stop-And-Frisk Policy by the New York City Policy Department and builds upon the paper "Precinct or prejudice? Understanding racial disparities in New York City's stop-and-frisk policy".

# Code Structure
0) MainFile.R - loads and combines all relevant code snippets in respective order

  1) PreProcessing
      - FirstSteps.R - merging of annual datasets, subsetting with relevant covariates, creation of dependent variable
      - hitRate.R - to be done: calculation of the complex hitRate-Formula
      - Cleaning.R - standardization, outliers, missing values
      
      to be done: transformation of coordinates to longitude/latitude
      
  2) ANALYSIS
      - SGD.R
      - Logit.R
      
    to be done: logistic regression, stochastic gradient descent
    
    
  3) PostProcessing
      - Plots.R - Plots that replicate those on the paper: 
                (1) Grouped barplot to compare the distributions of homicide and stop cases per precinct
                (2) GPS plots of stops on NYC google map to compare with racial distribution in the city
      - FurtherAnalysis.R
      
    to be done: variable importance, geographic maps etc.

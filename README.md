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
    
    to be done: logistic regression, stochastic gradient descent
    
    
  3) PostProcessing
      - Graphs.R - first graphs
      
    to be done: variable importance, geographic maps etc.

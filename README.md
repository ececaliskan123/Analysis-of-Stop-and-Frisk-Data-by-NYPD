# The Stops-And-Frisk Policy by the New York City Policy Department
This project addresses the Stop-And-Frisk Policy by the New York City Policy Department and builds upon the paper "Precinct or prejudice? Understanding racial disparities in New York City's stop-and-frisk policy" (Goel et al., 2016).

# Code Structure
0) MainFile.R - loads and combines all relevant code snippets in respective order

  1) PreProcessing 
      - Merging of annual datasets, subsetting with relevant covariates, creation of dependent variable
      - Calculation of the complex hitRate-Formula
      - Transformation of coordinates to longitude/latitude
  
  2) Cleaning 
      - Standardization, outliers, missing values
 
      - Trends over time, calculate racial compositions
      
  4) Replications 
      - Replications of graphical analysis to re-examine the conclusions in the paper
        - (1) Grouped barplot to compare the distributions of homicide and stop cases per precinct
        - (2) GPS plots of stops on NYC google map to compare with racial distribution in the city
        - (3) Examining cumulative density functions of each race
      
  5) Logit
      - Apply logistic model (without interaction terms) to the data
      
  6) Random Forest 
      - Replication and extension of analysis via Random Forest model
  
  3) Model Assessment 
      - Model check
        - Different hit rates vs Age / Sex / Race
        - Empirical hit rates to predicted values

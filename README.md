# nypd-stopfrisk
This project addresses the Stop-And-Frisk Policy by the New York City Policy Department and builds upon the paper "Precinct or prejudice? Understanding racial disparities in New York City's stop-and-frisk policy".

# Code Structure
0) MainFile.R - loads and combines all relevant code snippets in respective order

  1) PreProcessing (Malte)
      - Merging of annual datasets, subsetting with relevant covariates, creation of dependent variable
      - Calculation of the complex hitRate-Formula
      - Transformation of coordinates to longitude/latitude
  
  2) Cleaning (Ece)
      - Standardization, outliers, missing values
  
  3) Exploratory Analysis (LingKi)
      - Trends over time, calculate racial compositions
      
  4) Replications (LingKi
      - Replications of graphical analysis to re-examine the conclusions in the paper
        - (1) Grouped barplot to compare the distributions of homicide and stop cases per precinct
        - (2) GPS plots of stops on NYC google map to compare with racial distribution in the city
        - (3) Examining cumulative density functions of each race
      
  5) Logit(Ece)
      - Apply logistic model (without interaction terms) to the data
      
  6) Random Forest (Malte)
      - Replication and extension of analysis via Random Forest model
  
  3) Model Assessment (LingKi)
      - Model check
        - Different hit rates vs Age / Sex / Race
        - Empirical hit rates to predicted values

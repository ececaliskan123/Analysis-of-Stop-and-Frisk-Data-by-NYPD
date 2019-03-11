# nypd-stopfrisk
This project addresses the Stop-And-Frisk Policy by the New York City Policy Department and builds upon the paper "Precinct or prejudice? Understanding racial disparities in New York City's stop-and-frisk policy".

# Code Structure
0) MainFile.R - loads and combines all relevant code snippets in respective order

  1) PreProcessing
      - [x] FirstSteps.R - merging of annual datasets, subsetting with relevant covariates, creation of dependent variable
        - [x] Contains **StrToMatch** - to include more CPW cases that are not marked as CPW
      - [x] hitRate.R - to be done: calculation of the complex hitRate-Formula
      - [x] coordinates.R - transformation of coordinates to longitude/latitude
      - [x] Cleaning.R - standardization, outliers, missing values
      
  2) ANALYSIS  #**_TO BE DONE_**
      - [x] Preview.R - An overview of data and potential racism
      - Logit.R
        - First run Logit, then fit Logit with SDG due to large size of variables. 
        - **_p.372 Footnote 6_ package Vowpal Wabbit was used, all parameters adopted their default values in the package.**
      - RF.R 
   
  3) PostProcessing
      - [x] Plots.R - Plots that replicate those on the paper:
        - (1) Grouped barplot to compare the distributions of homicide and stop cases per precinct
        - (2) GPS plots of stops on NYC google map to compare with racial distribution in the city
      - [ ] FurtherAnalysis.R
        - (1) Model check - Hit Rate vs Age / Sex / Race _(Not sure if possible)_ p.389 Appendix A (b). *Or use AUC*
        - (2) 
        

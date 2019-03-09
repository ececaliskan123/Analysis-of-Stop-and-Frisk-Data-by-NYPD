# nypd-stopfrisk
This project addresses the Stop-And-Frisk Policy by the New York City Policy Department and builds upon the paper "Precinct or prejudice? Understanding racial disparities in New York City's stop-and-frisk policy".

# Code Structure
0) MainFile.R - loads and combines all relevant code snippets in respective order

  1) PreProcessing
      - [x] FirstSteps.R - merging of annual datasets, subsetting with relevant covariates, creation of dependent variable
      - [x] hitRate.R - to be done: calculation of the complex hitRate-Formula
      - [x] Cleaning.R - standardization, outliers, missing values
      - [x] coordinates.R - transformation of coordinates to longitude/latitude
      - [ ] **StrToMatch.R** - to include more CPW cases that are not marked as CPW 
      
  2) ANALYSIS  #**_TO BE DONE_**
      - SDG.R - process interaction terms which are used in logic model (next step)  **OR**  perform variable importance analysis and select based on results
      - Logit.R
      - RF.R 
   
  3) PostProcessing
      - [x] Plots.R - Plots that replicate those on the paper:
        - (1) Grouped barplot to compare the distributions of homicide and stop cases per precinct
        - (2) GPS plots of stops on NYC google map to compare with racial distribution in the city
      - [ ] FurtherAnalysis.R
        - (1) Model check - Hit Rate vs Age / Sex / Race _(Not sure if possible)_ p.389 Appendix A (b). *Or use AUC*
        - (2) 
        

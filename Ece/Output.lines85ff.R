#### OUTPUT

glm_model <- parglm(weaponfound ~ ., binomial(), train, control = parglm.control(method = "LINPACK",
                                                                                 +    
                                                                                   +                                                                     
                                                                                   +                                                        nthreads = 2))
> coef(glm_model)
(Intercept)          year      datestop           pct          sexM          sexZ         raceB         raceI 
-1.366045e+03  6.071600e-01  7.006867e-08 -4.127907e-03  1.350159e-01 -6.110531e-01 -9.442264e-01 -2.690995e-01 
raceP         raceQ         raceU         raceW         raceZ        weight           age      trhslocP 
-5.638790e-01 -5.937812e-01 -5.067820e-01  5.634557e-02 -4.950579e-01  1.208378e-03  1.351467e-03  5.164889e-01 
trhslocT        inoutO     cs_objcsY     cs_objcs      cs_objcs1     cs_descrY     cs_descr      cs_descr1 
2.412571e+00 -9.099991e-02  2.425396e+00 -1.563967e+01 -1.633439e+01 -2.660960e-01 -1.676354e-01            NA 
cs_casngY     cs_casng      cs_casng1     cs_lkoutY     cs_lkout      cs_lkout1     cs_clothY     cs_cloth  
-6.505324e-01 -2.597238e-02            NA -3.287128e-01 -2.158290e-01            NA -3.645219e-01 -1.480156e-01 
cs_cloth1     cs_drgtrY     cs_drgtr      cs_drgtr1     cs_furtvY     cs_furtv      cs_furtv1     cs_vcrimY 
NA -2.121067e-01  9.573847e-03            NA -4.125506e-01 -4.738295e-01            NA -1.494065e-01 
cs_vcrim      cs_vcrim1     cs_bulgeY     cs_bulge      cs_bulge1     cs_otherY     cs_other      cs_other1 
-1.918163e-01            NA  7.839711e-01  1.033922e-01            NA  5.030263e-01 -5.810859e-02            NA 
radioY        radio0        radio1        height          long           lat 
-3.393350e-01 -4.730984e-02            NA  2.570422e-04 -1.438903e+00  8.403275e-01 

> plot(glm_model)
Error in lm.influence(model, do.coef = do.coef, ...) : 
  non-NA residual length does not match cases used in fitting
> 
  
  glm_model$fitted ## output too big to copy....

# coefplot -> see folder

> yhat_glm <- predict(glm_model, test, type = "response")
Warning message:
  In predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type ==  :
                                                                    Vorhersage durch Fit ohne vollen Rang mag täuschen
                                                                  > summ(glm_model)
                                                                  Error in summ(glm_model) : could not find function "summ"
                                                                  > summary(glm_model)
                                                                  
                                                                  Call:
                                                                    glm(formula = weaponfound ~ ., family = binomial(), data = train, 
                                                                        control = parglm.control(method = "LINPACK", nthreads = 2), 
                                                                        method = parglm::parglm.fit, singular.ok = FALSE)
                                                                  
                                                                  Deviance Residuals: 
                                                                    Min       1Q   Median       3Q      Max  
                                                                  -2.7642  -0.3138  -0.2280  -0.1694   3.2932  
                                                                  
                                                                  Coefficients: (10 not defined because of singularities)
                                                                  Estimate Std. Error z value Pr(>|z|)    
                                                                  (Intercept) -1.366e+03  1.397e+02  -9.777  < 2e-16 ***
                                                                    year         6.072e-01  6.847e-02   8.868  < 2e-16 ***
                                                                    datestop     7.007e-08  9.156e-09   7.653 1.96e-14 ***
                                                                    pct         -4.128e-03  1.289e-03  -3.203 0.001359 ** 
                                                                    sexM         1.350e-01  1.680e-01   0.804 0.421650    
                                                                  sexZ        -6.111e-01  3.908e-01  -1.564 0.117900    
                                                                  raceB       -9.442e-01  1.943e-01  -4.859 1.18e-06 ***
                                                                    raceI       -2.691e-01  4.926e-01  -0.546 0.584900    
                                                                  raceP       -5.639e-01  2.140e-01  -2.634 0.008427 ** 
                                                                    raceQ       -5.938e-01  1.983e-01  -2.994 0.002754 ** 
                                                                    raceU       -5.068e-01  4.450e-01  -1.139 0.254793    
                                                                  raceW        5.635e-02  2.118e-01   0.266 0.790228    
                                                                  raceZ       -4.951e-01  3.097e-01  -1.599 0.109920    
                                                                  weight       1.208e-03  5.714e-04   2.115 0.034459 *  
                                                                    age          1.351e-03  7.954e-04   1.699 0.089317 .  
                                                                  trhslocP     5.165e-01  9.468e-02   5.455 4.90e-08 ***
                                                                    trhslocT     2.413e+00  1.407e-01  17.147  < 2e-16 ***
                                                                    inoutO      -9.100e-02  9.952e-02  -0.914 0.360503    
                                                                  cs_objcsY    2.425e+00  9.405e-02  25.789  < 2e-16 ***
                                                                    cs_objcs    -1.564e+01  1.339e+03  -0.012 0.990678    
                                                                  cs_objcs1   -1.633e+01  1.661e+03  -0.010 0.992155    
                                                                  cs_descrY   -2.661e-01  8.659e-02  -3.073 0.002120 ** 
                                                                    cs_descr    -1.676e-01  4.240e+02   0.000 0.999685    
                                                                  cs_descr1           NA         NA      NA       NA    
                                                                  cs_casngY   -6.505e-01  1.296e-01  -5.020 5.17e-07 ***
                                                                    cs_casng    -2.597e-02  5.492e+02   0.000 0.999962    
                                                                  cs_casng1           NA         NA      NA       NA    
                                                                  cs_lkoutY   -3.287e-01  1.524e-01  -2.157 0.031039 *  
                                                                    cs_lkout    -2.158e-01  5.981e+02   0.000 0.999712    
                                                                  cs_lkout1           NA         NA      NA       NA    
                                                                  cs_clothY   -3.645e-01  1.438e-01  -2.536 0.011222 *  
                                                                    cs_cloth    -1.480e-01  7.001e+02   0.000 0.999831    
                                                                  cs_cloth1           NA         NA      NA       NA    
                                                                  cs_drgtrY   -2.121e-01  1.810e-01  -1.172 0.241236    
                                                                  cs_drgtr     9.574e-03  9.894e+02   0.000 0.999992    
                                                                  cs_drgtr1           NA         NA      NA       NA    
                                                                  cs_furtvY   -4.126e-01  6.360e-02  -6.486 8.80e-11 ***
                                                                    cs_furtv    -4.738e-01  3.614e+02  -0.001 0.998954    
                                                                  cs_furtv1           NA         NA      NA       NA    
                                                                  cs_vcrimY   -1.494e-01  1.015e-01  -1.473 0.140851    
                                                                  cs_vcrim    -1.918e-01  4.743e+02   0.000 0.999677    
                                                                  cs_vcrim1           NA         NA      NA       NA    
                                                                  cs_bulgeY    7.840e-01  6.618e-02  11.845  < 2e-16 ***
                                                                    cs_bulge     1.034e-01  4.025e+02   0.000 0.999795    
                                                                  cs_bulge1           NA         NA      NA       NA    
                                                                  cs_otherY    5.030e-01  7.414e-02   6.784 1.17e-11 ***
                                                                    cs_other    -5.811e-02  4.051e+02   0.000 0.999886    
                                                                  cs_other1           NA         NA      NA       NA    
                                                                  radioY      -3.393e-01  6.824e-02  -4.973 6.59e-07 ***
                                                                    radio0      -4.731e-02  3.802e+02   0.000 0.999901    
                                                                  radio1              NA         NA      NA       NA    
                                                                  height       2.570e-04  3.825e-03   0.067 0.946417    
                                                                  long        -1.439e+00  4.142e-01  -3.474 0.000512 ***
                                                                    lat          8.403e-01  4.760e-01   1.765 0.077528 .  
                                                                  ---
                                                                    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                                                  
                                                                  (Dispersion parameter for binomial family taken to be 1)
                                                                  
                                                                  Null deviance: 12224.1  on 28564  degrees of freedom
                                                                  Residual deviance:  9545.9  on 28521  degrees of freedom
                                                                  (16 observations deleted due to missingness)
                                                                  AIC: 9633.9
                                                                  
                                                                  Number of Fisher Scoring iterations: 17
                                                                  
                                                                  > 
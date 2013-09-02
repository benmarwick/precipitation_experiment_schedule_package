% Precipitation manipulation experiments -- diagnostics and illustrations

The program which produces the precipitation schedule produces a number of diagnostic plots as well.  This document shows some of these.  It demonstrates how our rainfall schedule reflects natural rainfall, and how our manipulations alter it.



write normal text!!








![This figure shows a frequency distribution of rainfall in every year.  The black dots represent an idealized (average) negative binomial distribution of rainfall amounts.](figure/unnamed-chunk-3.png) 


![This figure shows the negative binomial prediction (as above).  However, it cannot be used directly because it predicts non-integer frequencies for each rainfall amount (and of course we cannot water for a non-integer number of days!).  We came up with an 'integerization' algorithim which approximates this smooth line with the points shown here.](figure/unnamed-chunk-4.png) 



![Each panel represents the distribution of days within one treatment (bromeliad).  Rows are variation in the *mu* parameter, while columns are *k*.](figure/unnamed-chunk-5.png) 



![The temporal pattern of rain in each bromeliad -- equal to the previous figure but with the addition of a sequence of rainfall that approximates the variation natural to the site.](figure/unnamed-chunk-6.png) 


![The control treatment bromeliad (i.e. *mu1k1*, black line) compared with the rainfall data, to demonstrate that they are very similar in temporal pattern](figure/unnamed-chunk-7.png) 


![As the above, but each year (and the control plant) is in a separate panel.](figure/unnamed-chunk-8.png) 



Getting, Cleaning, and Importing Data:
======================================

Github doesn't like to let you download single files, but you can if you
want, you will find it in the repository "YCHHopData.csv."
Unfortunately, my webscraping abilities are not up to the level
presented by this website (or any other website) for that matter, so, I
had to resort to manual entry.

Importing csv as a data frame:

    hops <- read.csv("./HopCluster/YCHHopData.csv")
    str(hops)

    ## 'data.frame':    121 obs. of  25 variables:
    ##  $ Variety              : Factor w/ 121 levels "Admiral","Ahtanum",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Country              : Factor w/ 8 levels "AU","CZ","DE",..: 7 8 8 4 6 8 3 6 7 8 ...
    ##  $ Class                : Factor w/ 3 levels "A","B","D": 2 1 1 1 3 3 1 1 3 2 ...
    ##  $ AALow.pct            : num  13 3.5 7 7.9 7 12 9 3.5 5 15 ...
    ##  $ AAHigh.pct           : num  0 6.5 11 8.3 13 14.5 12 7 7 18 ...
    ##  $ BALow.pct            : num  4 4 5.5 3.8 2.7 4.5 4.5 4 2.3 3.5 ...
    ##  $ BAHigh.pct           : num  6 6 8 4.5 4.4 6 5.5 6.1 3.2 5.5 ...
    ##  $ TOLow.mL100g         : num  0 0.5 1 1.2 0.9 1 1.2 0.7 0.7 2.3 ...
    ##  $ TOHigh.mL100g        : num  1.7 1.7 2.3 1.6 1.6 2 1.5 4 1 3.5 ...
    ##  $ CohumuloneLow.pct    : int  37 30 20 20 20 36 22 27 33 28 ...
    ##  $ CohumuloneHigh.pct   : int  45 34 24 21 26 41 26 31 35 35 ...
    ##  $ BPineneLow.pct       : num  NA 0.6 0.4 NA NA 0.6 NA NA NA 0.8 ...
    ##  $ BPineneHigh.pct      : num  NA 0.9 0.8 NA NA 0.9 NA NA NA 1 ...
    ##  $ MyrceneLow.pct       : int  39 45 45 39 20 45 50 30 35 55 ...
    ##  $ MyrceneHigh.pct      : int  48 55 50 41 25 55 75 45 37 60 ...
    ##  $ LinaloolLow.pct      : num  NA 0.4 0.5 NA NA 0.4 NA NA NA 0.4 ...
    ##  $ LinaloolHigh.pct     : num  NA 0.6 0.8 NA NA 0.7 NA NA NA 0.6 ...
    ##  $ CaryophylleneLow.pct : num  6 9 7 7.3 6 7 0 4 14.9 6 ...
    ##  $ CaryophylleneHigh.pct: num  8 12 10 7.5 9 11 2 6 15.1 8 ...
    ##  $ FarnseneLow.pct      : num  0 0.5 6 2 5 0.5 0.5 4 0.5 0.5 ...
    ##  $ FarnseneHigh.pct     : num  2 0.5 9 4 10 0.5 0.5 7 0.5 0.5 ...
    ##  $ HumuleneLow.pct      : num  23 15 19 20.9 20 10 0 13 39.9 8 ...
    ##  $ HumuleneHigh.pct     : num  26 22 24 21.1 25 18 3 19 30.1 11 ...
    ##  $ GerianolLow.pct      : num  0 0.4 0.1 0 0 0.1 0 0 0 0.7 ...
    ##  $ GerianolHigh.pct     : num  0 0.7 0.1 0 0 0.1 0 0 0 0.9 ...

You can see there are some NA values, but we will get to those in a
second.

Naming rows as the hop variety character strings:

    row.names(hops) <- hops$Variety

Removing records with too little data (10 or more NA values), and hop
blends, leaving 115 records:

    remove <- c("Falconer's Flight", "Falconer's Flight 7C's", "HBC 472", "Zythos")
    hops <- hops[!(row.names(hops) %in% remove),]
    hops <- hops[rowSums(is.na(hops)) < 10, ]

Changing factors from integer values, to numeric so they are treated as
continuous variables:

    hops$CohumuloneLow.pct <- as.numeric(hops$CohumuloneLow.pct)
    hops$CohumuloneHigh.pct <- as.numeric(hops$CohumuloneHigh.pct)
    hops$MyrceneLow.pct <- as.numeric(hops$MyrceneLow.pct)
    hops$MyrceneHigh.pct <- as.numeric(hops$MyrceneHigh.pct)

Creating new variables that average the high and low variables that will
be used as the single value for that hop variety:

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    hops <- mutate(hops, AA = (AALow.pct+AAHigh.pct)/2)
    hops <- mutate(hops, BA = (BALow.pct+BAHigh.pct)/2)
    hops <- mutate(hops, TO = (TOLow.mL100g+TOHigh.mL100g)/2)
    hops <- mutate(hops, Cohumulone = (CohumuloneLow.pct+CohumuloneHigh.pct)/2)
    hops <- mutate(hops, BPinene = (BPineneLow.pct+BPineneHigh.pct)/2)
    hops <- mutate(hops, Myrcene = (MyrceneLow.pct+MyrceneHigh.pct)/2)
    hops <- mutate(hops, Linalool = (LinaloolLow.pct+LinaloolHigh.pct)/2)
    hops <- mutate(hops, Caryophyllene = (CaryophylleneLow.pct+CaryophylleneHigh.pct)/2)
    hops <- mutate(hops, Farnsene = (FarnseneLow.pct+FarnseneHigh.pct)/2)
    hops <- mutate(hops, Humulene = (HumuleneLow.pct+HumuleneHigh.pct)/2)
    hops <- mutate(hops, Gerianol = (GerianolLow.pct+GerianolHigh.pct)/2)

Preserving row names, an unfortunate necessity with dplyr:

    row.names(hops) <- hops$Variety

Subsetting to remove variables no longer required:

    hops <- hops[c(-1,-(4:25))]

Imputing NA Values:
===================

Loading the Hmisc package, setting seed, imputing:

    library(Hmisc)

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     src, summarize

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

    set.seed(127)

    aregInfo <- aregImpute(~ AA + BA + TO + Cohumulone + BPinene + Myrcene + Linalool +
                                 Caryophyllene + Farnsene + Humulene + Gerianol,
                           data = hops, n.impute=10, nk=5, match = "kclosest")

    ## Iteration 1 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 6 unique values of x. knots set to 4 interior values.

    ## Iteration 2 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 7 unique values of x. knots set to 5 interior values.

    ## Iteration 3 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): could not obtain 5 interior knots with default algorithm.
    ##  Used alternate algorithm to obtain 4 knots

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots requested with 6 unique values of x.  knots set to 4 interior values.

    ## Iteration 4 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 5 unique values of x. knots set to 3 interior values.

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 7 unique values of x. knots set to 5 interior values.

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): could not obtain 5 interior knots with default algorithm.
    ##  Used alternate algorithm to obtain 3 knots

    ## Iteration 5 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 6 unique values of x. knots set to 4 interior values.

    ## Iteration 6 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 6 unique values of x. knots set to 4 interior values.

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 5 unique values of x. knots set to 3 interior values.

    ## Iteration 7 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 6 unique values of x. knots set to 4 interior values.

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): could not obtain 5 interior knots with default algorithm.
    ##  Used alternate algorithm to obtain 4 knots

    ## Iteration 8 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 6 unique values of x. knots set to 4 interior values.

    ## Iteration 9 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 5 unique values of x. knots set to 3 interior values.

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): could not obtain 5 interior knots with default algorithm.
    ##  Used alternate algorithm to obtain 3 knots

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): could not obtain 5 interior knots with default algorithm.
    ##  Used alternate algorithm to obtain 3 knots

    ## Iteration 10 
    Iteration 11 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots requested with 5 unique values of x.  knots set to 3 interior values.

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): could not obtain 5 interior knots with default algorithm.
    ##  Used alternate algorithm to obtain 3 knots

    ## Iteration 12 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): could not obtain 5 interior knots with default algorithm.
    ##  Used alternate algorithm to obtain 4 knots

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 6 unique values of x. knots set to 4 interior values.

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 7 unique values of x. knots set to 5 interior values.

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): could not obtain 5 interior knots with default algorithm.
    ##  Used alternate algorithm to obtain 4 knots

    ## Iteration 13 

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): 5 knots
    ## requested with 6 unique values of x. knots set to 4 interior values.

    ## Warning in rcspline.eval(z, knots = parms, nk = nk, inclx = TRUE): could not obtain 5 interior knots with default algorithm.
    ##  Used alternate algorithm to obtain 3 knots

Lets look at the R squared values:

    print(aregInfo)

    ## 
    ## Multiple Imputation using Bootstrap and PMM
    ## 
    ## aregImpute(formula = ~AA + BA + TO + Cohumulone + BPinene + Myrcene + 
    ##     Linalool + Caryophyllene + Farnsene + Humulene + Gerianol, 
    ##     data = hops, n.impute = 10, nk = 5, match = "kclosest")
    ## 
    ## n: 115   p: 11   Imputations: 10     nk: 5 
    ## 
    ## Number of NAs:
    ##            AA            BA            TO    Cohumulone       BPinene 
    ##             0             0             0             1            66 
    ##       Myrcene      Linalool Caryophyllene      Farnsene      Humulene 
    ##             1            55             2             3             1 
    ##      Gerianol 
    ##             0 
    ## 
    ##               type d.f.
    ## AA               s    4
    ## BA               s    4
    ## TO               s    4
    ## Cohumulone       s    4
    ## BPinene          s    4
    ## Myrcene          s    4
    ## Linalool         s    4
    ## Caryophyllene    s    4
    ## Farnsene         s    4
    ## Humulene         s    1
    ## Gerianol         s    2
    ## 
    ## Transformation of Target Variables Forced to be Linear
    ## 
    ## R-squares for Predicting Non-Missing Values for Each Variable
    ## Using Last Imputations of Predictors
    ##    Cohumulone       BPinene       Myrcene      Linalool Caryophyllene 
    ##         0.705         1.000         0.878         1.000         0.850 
    ##      Farnsene      Humulene 
    ##         0.722         0.877

So there is an issue with the perfect R squared values, which suggests
the values are overfit. For this analysis, this is ok and most likely
better than a random guess.

Lets fill in NA values to original data frame:

    imputed <- impute.transcan(aregInfo, data=hops, imputation=10, list.out=TRUE, pr=FALSE, check=FALSE)
    imputedHops <- as.data.frame(do.call(cbind,imputed))
    row.names(imputedHops) <- row.names(hops)

Ok, now to figure out optimal number of clusters based on either the
elbow, silhouette or gap statistic method:

    library(factoextra)

    ## Warning: package 'factoextra' was built under R version 3.4.4

    ## Welcome! Related Books: `Practical Guide To Cluster Analysis in R` at https://goo.gl/13EFCZ

    library(NbClust)

    scaledhops <- scale(imputedHops)

    fviz_nbclust(scaledhops, kmeans, method="wss") +
          geom_vline(xintercept = 4, linetype = 2)+
          labs(subtitle = "Elbow Method")

![](cluster_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    fviz_nbclust(scaledhops, kmeans, method = "silhouette")+
          labs(subtitle = "Silhouette Method")

![](cluster_files/figure-markdown_strict/unnamed-chunk-11-2.png)

    fviz_nbclust(scaledhops, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
          labs(subtitle = "Gap Statistic Method")

![](cluster_files/figure-markdown_strict/unnamed-chunk-11-3.png)

As the elbow and silhouette method would produce and unusable number of
clusters, we are going to go with the number suggested by the third
method. Nine seems like a good amount for selecting hop varieties.

Running k-means:
================

    library(factoextra)

    set.seed(1234)
    bob <- kmeans(scale(data.matrix(imputedHops)), centers = 9, nstart = 30)
    fviz_cluster(bob, data = imputedHops, main = "HomebrewBytes.com Hop Clusters")

![](cluster_files/figure-markdown_strict/unnamed-chunk-12-1.png)

Nifty visualization huh? Lets output the groups and distances:

Output:
=======

    kmeansresults <- bob$cluster
    # Writing the hop varieties along with the group they belong to:
    write.csv(kmeansresults, "./kmeansresults.csv")

    # Outputting the distances to csv
    sdist <- dist(scale(imputedHops))
    newmtx <- data.matrix(sdist)
    write.csv(newmtx, "./HopCluster/distances.csv")

Unfortunately, my programming skills are not up to ranking the hop names
against to the measured distance. To recreate this, sort the distances
csv by the 2nd column, copy the sorted row names (leftmost column with
hop names) into the values of the column you just sorted, and then copy
that entire column into a separate sheet. You should see the hop name at
the top, along with the same hop repeated. Do the same for all columns
and then transpose to have the same file as shown in the repo:
hop-results-clusters.xlsx

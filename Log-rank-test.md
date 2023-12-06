Log-rank Test
================
Zhezheng Jin
2023-11-30

``` r
library(survival)
head(rotterdam, 30)
```

    ##      pid year age meno  size grade nodes pgr  er hormon chemo rtime recur dtime
    ## 1393   1 1992  74    1  <=20     3     0  35 291      0     0  1799     0  1799
    ## 1416   2 1984  79    1 20-50     3     0  36 611      0     0  2828     0  2828
    ## 2962   3 1983  44    0  <=20     2     0 138   0      0     0  6012     0  6012
    ## 1455   4 1985  70    1 20-50     3     0   0  12      0     0  2624     0  2624
    ## 977    5 1983  75    1  <=20     3     0 260 409      0     0  4915     0  4915
    ## 617    6 1983  52    0  <=20     3     0 139 303      0     0  5888     0  5888
    ## 51     7 1993  40    0  <=20     2     0  13   4      0     0  2491     0  2491
    ## 473    8 1988  53    1  <=20     2     0   1   4      0     0  4150     0  4150
    ## 362    9 1988  60    1  <=20     2     0 627 151      0     0  3919     0  3919
    ## 2182  10 1988  52    0 20-50     3     5 316 976      0     0  3647     0  3647
    ## 1123  11 1989  66    1 20-50     3     0   0 283      0     0  2133     0  2133
    ## 375   12 1988  42    0  <=20     2     1 245  67      0     1  4146     0  4146
    ## 1303  13 1992  74    1 20-50     3     0 317 136      0     0  2124     0  2124
    ## 2565  14 1993  55    0   >50     3     9   0  21      1     0  1773     0  1773
    ## 1039  15 1989  57    1  <=20     3     0   0   0      0     0  2682     0  2682
    ## 917   16 1993  49    0  <=20     3     0 286  56      0     0  2591     0  2591
    ## 2079  17 1993  61    1 20-50     3     4   0 604      1     0  2734     0  2734
    ## 2246  18 1990  52    1 20-50     3     6   6   3      0     0  2379     0  2379
    ## 1822  19 1979  51    0 20-50     3     1  14   0      0     0  7027     0  7027
    ## 800   20 1993  62    1  <=20     3     0  22  84      0     0  2498     0  2498
    ## 1684  21 1993  57    1  <=20     3     1   4 271      1     0   851     0   851
    ## 171   22 1990  47    0  <=20     2     0 142 189      0     0  3716     0  3716
    ## 580   23 1992  61    1  <=20     3     0 354 105      0     0  2584     0  2584
    ## 15    24 1987  52    1  <=20     3     0 649 190      0     0  4665     0  4665
    ## 244   25 1993  52    1  <=20     3     0 147  91      0     0  2835     0  2835
    ## 1913  26 1986  63    1  <=20     3     2  24  30      0     0  3482     0  3482
    ## 1147  27 1990  61    1  <=20     3     0   0   0      0     0  3339     0  3339
    ## 62    28 1988  60    1  <=20     2     0 556 303      0     0  4090     0  4090
    ## 143   29 1993  32    0 20-50     2     1 665  94      0     1  2880     0  2880
    ## 252   30 1988  38    0  <=20     2     0 305  70      0     0  3997     0  3997
    ##      death
    ## 1393     0
    ## 1416     0
    ## 2962     0
    ## 1455     0
    ## 977      0
    ## 617      0
    ## 51       0
    ## 473      0
    ## 362      0
    ## 2182     0
    ## 1123     0
    ## 375      0
    ## 1303     0
    ## 2565     0
    ## 1039     0
    ## 917      0
    ## 2079     0
    ## 2246     0
    ## 1822     0
    ## 800      0
    ## 1684     0
    ## 171      0
    ## 580      0
    ## 15       0
    ## 244      0
    ## 1913     0
    ## 1147     0
    ## 62       0
    ## 143      0
    ## 252      0

## Log-rank test (death as event) comparing hormonal treatment

``` r
surv_obj1 <- Surv(time = rotterdam$dtime, event = rotterdam$death)

log_rank_test_hormon1 <- survdiff(surv_obj1 ~ hormon, data = rotterdam)

print(log_rank_test_hormon1)
```

    ## Call:
    ## survdiff(formula = surv_obj1 ~ hormon, data = rotterdam)
    ## 
    ##             N Observed Expected (O-E)^2/E (O-E)^2/V
    ## hormon=0 2643     1113     1162      2.04      23.7
    ## hormon=1  339      159      110     21.43      23.7
    ## 
    ##  Chisq= 23.7  on 1 degrees of freedom, p= 1e-06

Groups Compared: The test compared survival times between two groups:
hormon=0 and hormon=1.

Number of Subjects (N): There were 2,643 subjects in the hormon=0 group
and 339 subjects in the hormon=1 group.

The p-value of 1e-06 suggests that there is a statistically significant
difference in survival between the two hormone treatment groups. The
very low p-value (much less than 0.05) leads us to reject the null
hypothesis of no difference in survival between the two groups. The
result indicates that hormone treatment status (hormon) is strongly
associated with differences in survival outcomes in the dataset.
Specifically, the observed number of events in each group significantly
deviates from what would be expected if there were no difference in
survival between the groups.

## Log-rank test (recurrence as event) comparing hormonal treatment

``` r
surv_obj2 <- Surv(time = rotterdam$rtime, event = rotterdam$recur)

log_rank_test_hormon2 <- survdiff(surv_obj2 ~ hormon, data = rotterdam)

print(log_rank_test_hormon2)
```

    ## Call:
    ## survdiff(formula = surv_obj2 ~ hormon, data = rotterdam)
    ## 
    ##             N Observed Expected (O-E)^2/E (O-E)^2/V
    ## hormon=0 2643     1336     1371     0.911      9.48
    ## hormon=1  339      182      147     8.516      9.48
    ## 
    ##  Chisq= 9.5  on 1 degrees of freedom, p= 0.002

The p-value of 0.002 suggests that there is a statistically significant
difference in the recurrence rate between the two hormone treatment
groups. Given that the p-value is less than 0.05, we reject the null
hypothesis of no difference in recurrence rates between the two groups.
The result indicates that hormone treatment status (hormon) is
associated with a difference in recurrence rates in the dataset.
Specifically, the observed number of recurrences in each group deviates
from what would be expected if there were no difference in recurrence
rates between the groups.

## Log-rank test (combine both as event) comparing hormonal treatment

``` r
rfs  <- pmax(rotterdam$recur, rotterdam$death)
rfstime <- with(rotterdam, ifelse(recur==1, rtime, dtime))
surv_obj3 <- Surv(time = rfstime, event = rfs)
log_rank_test_hormon3 <- survdiff(surv_obj3 ~ hormon, data = rotterdam)

print(log_rank_test_hormon3)
```

    ## Call:
    ## survdiff(formula = surv_obj3 ~ hormon, data = rotterdam)
    ## 
    ##             N Observed Expected (O-E)^2/E (O-E)^2/V
    ## hormon=0 2643     1507     1551      1.25      13.4
    ## hormon=1  339      206      162     12.02      13.4
    ## 
    ##  Chisq= 13.4  on 1 degrees of freedom, p= 3e-04

The p-value of 0.0003 suggests that there is a statistically significant
difference in survival between the two hormone treatment groups. The
very low p-value (much less than 0.05) leads us to reject the null
hypothesis of no difference in survival between the two groups. The
result indicates that hormone treatment status (hormon) is strongly
associated with differences in survival outcomes in the dataset, when
considering both recurrence and death as the events of interest.
Specifically, the observed number of events in each group significantly
deviates from what would be expected if there were no difference in
survival between the groups.

## Stratified log-rank test (combine both as event) comparing hormonal treatment stratified by chemotherapy

``` r
stratified_log_rank_test <- survdiff(surv_obj3 ~ hormon + strata(chemo), data = rotterdam)

print(stratified_log_rank_test)
```

    ## Call:
    ## survdiff(formula = surv_obj3 ~ hormon + strata(chemo), data = rotterdam)
    ## 
    ##             N Observed Expected (O-E)^2/E (O-E)^2/V
    ## hormon=0 2643     1507     1553      1.33      14.5
    ## hormon=1  339      206      160     12.91      14.5
    ## 
    ##  Chisq= 14.5  on 1 degrees of freedom, p= 1e-04

Stratification: The test was stratified by the chemotherapy, meaning the
analysis was conducted separately within each chemotherapy status group,
but the overall test combines these analyses.

The p-value of 0.0001 suggests that there is a statistically significant
difference in survival between the two hormone treatment groups when
stratified by chemotherapy status. The very low p-value (much less than
0.05) leads us to reject the null hypothesis of no difference in
survival between the two groups within each stratum of chemotherapy
status. This result indicates that hormone treatment status (hormon) is
strongly associated with differences in the combined outcomes of
recurrence and death, and this association holds true even when
accounting for whether patients received chemotherapy or not.

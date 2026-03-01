# Survey Analysis with 2021 Canadian Census


``` r
library(duckdb)
```

    Loading required package: DBI

``` r
library(dbplyr)
library(srvyr)
```


    Attaching package: 'srvyr'

    The following object is masked from 'package:stats':

        filter

# Create database and load data

The code below creates a temporary duckdb database. It then directly
reads the csv into the database. This does NOT load into your R memory!
This is great for low RAM environments.

We then add a convenient variable pointing to the table in the database
(`c2021_ind_db`).

``` r
con <- dbConnect(duckdb())
nread <-duckdb_read_csv(
  conn=con, 
  name="c2021_ind", # Table name in database
  here::here("RawDat", "cen21_ind_98m0001x_part_rec21", "data_donnees_2021_ind_v2.csv") # location of csv
)

c2021_ind_db <- dplyr::tbl(con, I("c2021_ind"))
```

The only value returned is the number of rows that went into the table -
this can be a good check to compare with documentation. We can see that
980,868 rows are read using the variable `nread`.

Note that the object (`c2021_ind_db`) is not a tibble or dataframe but
points to the table in the database. However, the survey variables are
stored in the object - which includes the analytic weight, the replicate
weights (if applicable), and the stratum or cluster variables (if
applicable). Thus, the size of the object is NOT impacted by analytic
variables.

``` r
c2021_ind_db
```

    # Source:   SQL [?? x 144]
    # Database: DuckDB 1.4.4 [steph@Windows 10 x64:R 4.5.2/:memory:]
       PPSORT ABOID AGEGRP AGEIMM ATTSCH BFNMEMB BedRm CFInc CFInc_AT CFSTAT   CHDBN
        <int> <int>  <int>  <int>  <int>   <int> <int> <int>    <int>  <int>   <int>
     1      1     6     13      7      1       0     4    30       27      2 8.89 e7
     2      2     6     11      5      1       0     3    18       18      2 1.15 e4
     3      3     1     13     99      1       0     0     7        7      6 1.000e8
     4      4     6     16     99      1       0     4    15       15      2 1.000e8
     5      5     6     18     99      1       0     3    13       13      3 1.000e8
     6      6     2     16     99      1       0     4     1        1      7 1.000e8
     7      7     6     16     99      1       0     3    10       10      1 1.000e8
     8      8     6     16      7      1       0     4    30       27      1 1.000e8
     9      9     6     11     99      1       0     4    25       24      2 1.000e8
    10     10     6     12      6      1       0     4    22       22      2 1.000e8
    # ℹ more rows
    # ℹ 133 more variables: CIP2021 <int>, CIP2021_STEM_SUM <int>, CMA <int>,
    #   CONDO <int>, COVID_ERB <int>, COW <int>, CQPPB <int>, CapGn <int>,
    #   CfSize <int>, ChldC <int>, CitOth <int>, Citizen <int>, DIST <int>,
    #   DPGRSUM <int>, DTYPE <int>, EFDecile <int>, EFInc <int>, EFInc_AT <int>,
    #   EICBN <int>, ETHDER <int>, EfDIMBM_2018 <int>, EfSize <int>, EmpIn <int>,
    #   FOL <int>, FPTWK <int>, Gender <int>, GENSTAT <int>, GovtI <int>, …

``` r
object.size(c2021_ind_db) |> print(units="Kb")
```

    50.9 Kb

# Creating the survey design object

We create the survey design object from the database table. Again, this
does not load the data into R. Note, if you want to specify the degrees
of freedom, this is the best time to do it by using the option `degf` in
the function `as_survey_rep()` which will improve computation if
confidence intervals or testing are desired later.

``` r
cdes <- c2021_ind_db |>
  as_survey_rep(
    weights=WEIGHT,
    repweights=starts_with("WT"),
    type="other",
    scale = 1/35,
    mse=TRUE,
    rscales=1)

cdes
```

    Call: Called via srvyr
    Replicate weight design with 16 replicates and MSE variances.
    Sampling variables:
      - repweights: `WT1 + WT2 + WT3 + WT4 + WT5 + WT6 + WT7 + WT8 + WT9 + WT10 +
        WT11 + WT12 + WT13 + WT14 + WT15 + WT16` 
      - weights: WEIGHT 
    Data variables: 
      - PPSORT (int), ABOID (int), AGEGRP (int), AGEIMM (int), ATTSCH (int),
        BFNMEMB (int), BedRm (int), CFInc (int), CFInc_AT (int), CFSTAT (int),
        CHDBN (int), CIP2021 (int), CIP2021_STEM_SUM (int), CMA (int), CONDO (int),
        COVID_ERB (int), COW (int), CQPPB (int), CapGn (int), CfSize (int), ChldC
        (int), CitOth (int), Citizen (int), DIST (int), DPGRSUM (int), DTYPE (int),
        EFDecile (int), EFInc (int), EFInc_AT (int), EICBN (int), ETHDER (int),
        EfDIMBM_2018 (int), EfSize (int), EmpIn (int), FOL (int), FPTWK (int),
        Gender (int), GENSTAT (int), GovtI (int), GTRfs (int), HCORENEED_IND (int),
        HDGREE (int), HHInc (int), HHInc_AT (int), HHMRKINC (int), HHSIZE (int),
        HHTYPE (int), HLMOSTEN (int), HLMOSTFR (int), HLMOSTNO (int), HLREGEN
        (int), HLREGFR (int), HLREGNO (int), IMMCAT5 (int), IMMSTAT (int), IncTax
        (int), Invst (int), JOBPERM (int), KOL (int), LFACT (int), LICO_BT (int),
        LICO_AT (int), LIPROGTYPE (int), LI_ELIG_OML_U18 (int), LOCSTUD (int),
        LOC_ST_RES (int), LSTWRK (int), LWMOSTEN (int), LWMOSTFR (int), LWMOSTNO
        (int), LWREGEN (int), LWREGFR (int), LWREGNO (int), LoLIMA (int), LoLIMB
        (int), LoMBM_2018 (int), MODE (int), MTNEN (int), MTNFR (int), MTNNO (int),
        MarStH (int), Mob1 (int), Mob5 (int), MrkInc (int), NAICS (int), NOC21
        (int), NOL (int), NOS (int), OASGI (int), OtInc (int), PKID25 (int),
        PKID0_1 (int), PKID15_24 (int), PKID2_5 (int), PKID6_14 (int), PKIDS (int),
        POB (int), POBPAR1 (int), POBPAR2 (int), POWST (int), PR (int), PR1 (int),
        PR5 (int), PresMortG (int), PRIHM (int), PWDUR (int), PWLEAVE (int), PWOCC
        (int), PWPR (int), REGIND (int), Relig (int), REPAIR (int), ROOM (int),
        Retir (int), SHELCO (int), SSGRAD (int), Subsidy (int), SempI (int), Tenur
        (int), TotInc (int), TotInc_AT (int), VISMIN (int), Value (int), WKSWRK
        (int), WRKACT (int), Wages (int), YRIM (int), WEIGHT (dbl), WT1 (dbl), WT2
        (dbl), WT3 (dbl), WT4 (dbl), WT5 (dbl), WT6 (dbl), WT7 (dbl), WT8 (dbl),
        WT9 (dbl), WT10 (dbl), WT11 (dbl), WT12 (dbl), WT13 (dbl), WT14 (dbl), WT15
        (dbl), WT16 (dbl)

``` r
object.size(cdes) |> print(units="Kb")
```

    191648.1 Kb

# Examples

Demonstrating examples from the 2021 Census Public Use Microdata File
(PUMF) Individuals File Documentation and User guide. This text comes
directly from the documentation but we demonstrate the way to do the
calculations in R.

## Example 1/Example 5

The objective is to estimate the total number of women+ aged 25 and over
living in the Edmonton census metropolitan area (CMA) whose highest
level of schooling was a master’s degree or a doctorate.

First, we identify the records that satisfy the condition CMA = 835,
GENDER = 1, (AGEGRP ≥ 9 and AGEGRP ^= 88) and HDGREE = 12 or 13 on the
file. We accordingly obtain a total of 915 records that satisfy the
condition.

Secondly, we estimate the population total by summing up the WEIGHT of
the 915 records. The estimate of the population total is 33,865.

SE from docs: Thus, this method yields an estimate of the standard error
of 1,173.47

``` r
cdes |>
  filter(
    CMA==835,
    Gender==1, 
    between(AGEGRP, 9, 21),
    between(HDGREE, 12, 13)
    ) |>
  summarize(
    Est = survey_total(),
    NRec= unweighted(n())
  )
```

    # A tibble: 1 × 3
         Est Est_se  NRec
       <dbl>  <dbl> <int>
    1 33865.  1173.   915

## Example 2/Example 6

In this example, the denominator targets all individuals in a geographic
area. We want to estimate the proportion of immigrants among the
individuals living in the Montréal CMA.

First, we identify the records that satisfy the condition IMMSTAT = 2
and CMA = 462 on the file, as well as the records that satisfy the
condition CMA = 462.

Second, we calculate the two corresponding weighted totals.

Finally, the ratio of the two totals is the estimated proportion of
immigrants among the individuals living in the Montréal CMA.

The estimated proportion is 1,024,916 / 4,210,000 = 0.2434, which means
that just over 24% of the individuals in the Montréal CMA are
immigrants.

SE from docs: Thus, this method yields an estimate of the standard error
of 0.1014%.

``` r
cdes |>
  filter(CMA==462) |>
  group_by(IMMSTAT) |>
  summarize(
    tot=survey_total(),
    p=survey_mean()*100
  )
```

    Adding missing grouping variables: `IMMSTAT`

    # A tibble: 4 × 5
      IMMSTAT      tot tot_se       p    p_se
        <int>    <dbl>  <dbl>   <dbl>   <dbl>
    1       1 3023471. 12070. 71.8    0.103  
    2       2 1024916.  5365. 24.3    0.101  
    3       3  160908.  2538.  3.82   0.0660 
    4      88     704.   145.  0.0167 0.00343

## Example 3/Example 7

We want to estimate, out of all men+ aged 20 to 44 living in the
Vancouver CMA, the proportion whose de facto marital status is ‘divorced
(and not living common law).’ In this case, the numerator is the
weighted total of records satisfying the condition CMA = 933, GENDER =
2, 8 ≤ AGEGRP ≤ 12 and MARSTH = 5. The denominator is the weighted total
of records satisfying CMA = 933, GENDER = 2 and 8 ≤ AGEGRP ≤ 12. We
obtain 6,815 / 472,361 = 0.0144, which means approximately 1.4% of men+
aged 20 to 44 in Vancouver are divorced and not living common law

SE from docs: Thus, this method yields an estimate of the standard error
of 0.1414%.

``` r
cdes |>
  filter(
    CMA==933,
    Gender ==2,
    between(AGEGRP, 8, 12)
  ) |>
  group_by(MarStH) |>
   summarize(
    tot=survey_total(),
    p=survey_mean()*100
  )
```

    Adding missing grouping variables: `MarStH`

    # A tibble: 7 × 5
      MarStH      tot tot_se       p   p_se
       <int>    <dbl>  <dbl>   <dbl>  <dbl>
    1      1 236218.  3424.  50.0    0.525 
    2      2 166219.  2352.  35.2    0.379 
    3      3  57221.  1252.  12.1    0.279 
    4      4   5667.   588.   1.20   0.124 
    5      5   6815.   665.   1.44   0.142 
    6      6    148.    66.3  0.0314 0.0141
    7      8     74.1   50.6  0.0157 0.0108

## Example 4/Example 8

We want to estimate the average total income of women+ aged 15 years and
over living in Ontario who have an income (including negative values).
In the calculation of the numerator, WEIGHT is multiplied by the value
of the ‘total income’ variable for individuals with an income (where
TOTINC ^= 88,888,888, TOTINC ^= 99,999,999, TOTINC ^= 0) whose gender is
women+ (GENDER = 1) and who are aged 15 or over (AGEGRP ≥ 6, AGEGRP ^=
88) in the province of Ontario (PR = 35); the results are then totalled.

To estimate the average, the numerator (or estimated total income) is
divided by the sum of WEIGHT for individuals satisfying the same
conditions on TOTINC, GENDER, AGEGRP and PR. The result obtained is: \$
278,486,550,046/ 5,771,535= \$48,252, which means the average total
income of women+ aged 15 and over living in Ontario who have an income
is around \$48,252.

SE from docs: Thus, this method yields an estimate of the standard error
of 102.15.

``` r
cdes |>
  filter(
    Gender ==1,
    PR == 35,
    AGEGRP >= 6,
    AGEGRP !=88,
    !(TotInc %in% c(0, 88888888, 99999999))
  ) |>
  summarize(
    Avg=survey_mean(TotInc)
  )
```

    # A tibble: 1 × 2
         Avg Avg_se
       <dbl>  <dbl>
    1 48252.   102.

# References

Source: Statistics Canada, Census Public Use Microdata File (PUMF),
2021. Reproduced and distributed on an “as is” basis with the permission
of Statistics Canada.

Adapted from Statistics Canada, Census Public Use Microdata File (PUMF)
Individuals File Documentation and User guide, 2021. This does not
constitute an endorsement by Statistics Canada of this product.

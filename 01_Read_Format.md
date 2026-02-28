# Read and format 2021 Canadian Census Individual File


``` r
library(tidyr)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(readr)
library(stringr)
library(qs2)
```

    qs2 0.1.7

``` r
library(labelled)
```

# Process dictionary file to get data location

``` r
dct_in <- read_lines(here::here("RawDat", "cen21_ind_98m0001x_part_rec21", "English", "SAS, SPSS and STATA command files", "ipumf_2021_final_en.dct"))

dct <-
  tibble(
    inp=str_squish(dct_in[c(-1, -length(dct_in))])) |>
  separate_wider_delim(
    inp, delim=" ", 
    names=c("Start", "Type", "Variable", "Length", NA),
    too_many = "merge"
  ) |>
  mutate(
    Start=parse_number(Start),
    Length=parse_number(Length),
    End=Start+Length-1,
    Type=if_else(Type=="double", "d", "i")) |>
  select(-Length)

dct
```

    # A tibble: 144 × 4
       Start Type  Variable   End
       <dbl> <chr> <chr>    <dbl>
     1     1 i     ppsort       6
     2     7 i     aboid        7
     3     8 i     agegrp       9
     4    10 i     ageimm      11
     5    12 i     attsch      12
     6    13 i     bfnmemb     13
     7    14 i     bedrm       14
     8    15 i     cfinc       16
     9    17 i     cfinc_at    18
    10    19 i     cfstat      19
    # ℹ 134 more rows

# Process SAS formats for creating factors

``` r
sas_syn_in <- read_lines(here::here("RawDat", "cen21_ind_98m0001x_part_rec21", "English", "SAS, SPSS and STATA command files", "IPUMF_2021_final_prog_en_v2.sas"))
val_start <- which(str_detect(sas_syn_in, "value"))[1]
dat_start <- which(str_detect(sas_syn_in, "DATA"))[1]

fmts <- tibble(inp=sas_syn_in[seq(val_start, dat_start-1)]) |>
  mutate(
    inp=str_replace(inp, '        205 = "Halifax"', '\t\t205 = "Halifax"') ) |> # Manual fix
  separate_wider_delim(
    cols=inp, delim="\t", 
    names=c(NA, "Value", "Fmts_eq"),
    too_few="align_start") |>
  filter(Value != ";") |>
  mutate(
    Value=str_remove(Value, "value ") |> na_if("") |>
      str_to_lower() |> str_squish() |> str_remove("_$")) |>
  fill(Value) |>
  filter(!is.na(Fmts_eq)) |>
  separate_wider_delim(cols=Fmts_eq, delim="=",
                       names=c("lev", "lab")) |>
  mutate(
    lev=parse_number(lev),
    lab=str_squish(lab) |> str_remove_all('\\"')
  )

# Variables without formats
# All are expected to not have formats

setdiff(dct$Variable, unique(fmts$Value))
```

     [1] "ppsort"    "chdbn"     "covid_erb" "cqppb"     "capgn"     "chldc"    
     [7] "eicbn"     "empin"     "govti"     "gtrfs"     "inctax"    "invst"    
    [13] "mrkinc"    "oasgi"     "otinc"     "retir"     "shelco"    "sempi"    
    [19] "totinc"    "totinc_at" "value"     "wages"     "weight"    "wt1"      
    [25] "wt2"       "wt3"       "wt4"       "wt5"       "wt6"       "wt7"      
    [31] "wt8"       "wt9"       "wt10"      "wt11"      "wt12"      "wt13"     
    [37] "wt14"      "wt15"      "wt16"     

``` r
# Formats without variables (shouldn't happen)

setdiff(unique(fmts$Value), dct$Variable)
```

    character(0)

``` r
labs <- tibble(inp=sas_syn_in) |>
  filter(str_detect(inp, "Label")) |>
  mutate(inp=str_remove_all(inp, "\\tLabel ")) |>
  separate_wider_delim(cols=inp, delim="=",
                       names=c("Variable", "Description")) |>
  mutate(
    Variable=Variable |> str_squish() |> str_to_lower(),
    Description=Description |> str_remove_all('\\"') |> str_remove_all(";") |> str_replace_all("\x96", "-") |> str_squish(),
    Description=if_else(
      str_detect(Description, "Replicate"),
      str_c(Description, str_remove(Variable, "wt"), sep=" "),
      Description)
    
  )

labs
```

    # A tibble: 144 × 2
       Variable Description                                              
       <chr>    <chr>                                                    
     1 aboid    Indigenous: Indigenous identity - Detailed               
     2 agegrp   Age                                                      
     3 ageimm   Immigration: Age at Immigration                          
     4 attsch   Education: School attendance - Detailed                  
     5 bedrm    Bedrooms                                                 
     6 bfnmemb  Indigenous: Membership in a First Nation or Indian band  
     7 capgn    Income: Net capital gains or losses                      
     8 cfinc    Income: Total income of census family for all persons    
     9 cfinc_at Income: After-tax income of census family for all persons
    10 cfsize   Census family size, stored at the person level           
    # ℹ 134 more rows

``` r
tail(labs)
```

    # A tibble: 6 × 2
      Variable Description                     
      <chr>    <chr>                           
    1 wt12     Replicate PUMF weight 12        
    2 wt13     Replicate PUMF weight 13        
    3 wt14     Replicate PUMF weight 14        
    4 wt15     Replicate PUMF weight 15        
    5 wt16     Replicate PUMF weight 16        
    6 yrim     Immigration: Year of immigration

``` r
all(sort(unique(dct$Variable))==sort(labs$Variable))
```

    [1] TRUE

# Read in data and create factors

``` r
dat_in <- read_fwf(
  here::here("RawDat", "cen21_ind_98m0001x_part_rec21", "data_donnees_2021_ind.dat"),
  col_positions = fwf_positions(start=dct$Start, end=dct$End, col_names=dct$Variable),
  col_types = str_flatten(dct$Type_short))
```

    Warning: Unknown or uninitialised column: `Type_short`.

``` r
apply_format <- function(x){
  variable_name <- deparse(substitute(x))
  fmt_x <- fmts |>
    filter(Value==variable_name)
  
  if (!all(x %in% fmt_x$lev)){
    stop(glue::glue("In variable {variable_name}, there's an unexpected level"))
  }
  
  x |> factor(levels=fmt_x$lev, labels=fmt_x$lab)
  
}

dat <- dat_in |>
  mutate(across(all_of(unique(fmts$Value)), apply_format))

var_label(dat) <- as.list(labs$Description) |> rlang::set_names(labs$Variable)
```

# Summarize and save data

``` r
summarytools::dfSummary(dat, style="grid", graph.col=FALSE, plain.ascii=FALSE)
```

### Data Frame Summary

#### dat

**Dimensions:** 980868 x 144  
**Duplicates:** 0

<table style="width:97%;">
<colgroup>
<col style="width: 3%" />
<col style="width: 12%" />
<col style="width: 27%" />
<col style="width: 22%" />
<col style="width: 16%" />
<col style="width: 7%" />
<col style="width: 6%" />
</colgroup>
<thead>
<tr>
<th>No</th>
<th>Variable</th>
<th>Label</th>
<th>Stats / Values</th>
<th>Freqs (% of Valid)</th>
<th>Valid</th>
<th>Missing</th>
</tr>
</thead>
<tbody>
<tr>
<td>1</td>
<td>ppsort<br />
[numeric]</td>
<td>Unique record identifier</td>
<td>Mean (sd) : 490434.5 (283152.3)<br />
min &lt; med &lt; max:<br />
1 &lt; 490434.5 &lt; 980868<br />
IQR (CV) : 490433.5 (0.6)</td>
<td>980868 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>2</td>
<td>aboid<br />
[factor]</td>
<td>Indigenous: Indigenous identity - Detailed</td>
<td>1. First Nations (North Amer<br />
2. M�tis<br />
3. Inuk�(Inuit)<br />
4. Multiple Indigenous respo<br />
5. Indigenous responses not<br />
6. Non-Indigenous identity</td>
<td>28408 ( 2.9%)<br />
17049 ( 1.7%)<br />
1880 ( 0.2%)<br />
758 ( 0.1%)<br />
937 ( 0.1%)<br />
931836 (95.0%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>3</td>
<td>agegrp<br />
[factor]</td>
<td>Age</td>
<td>1. 0 to 4 years<br />
2. 5 to 6 years<br />
3. 7 to 9 years<br />
4. 10 to 11 years<br />
5. 12 to 14 years<br />
6. 15 to 17 years<br />
7. 18 to 19 years<br />
8. 20 to 24 years<br />
9. 25 to 29 years<br />
10. 30 to 34 years<br />
[ 12 others ]</td>
<td>49068 ( 5.0%)<br />
21891 ( 2.2%)<br />
33507 ( 3.4%)<br />
22921 ( 2.3%)<br />
34484 ( 3.5%)<br />
32290 ( 3.3%)<br />
20897 ( 2.1%)<br />
58485 ( 6.0%)<br />
64759 ( 6.6%)<br />
67392 ( 6.9%)<br />
575174 (58.6%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>4</td>
<td>ageimm<br />
[factor]</td>
<td>Immigration: Age at Immigration</td>
<td>1. 0 to 4 years<br />
2. 5 to 9 years<br />
3. 10 to 14 years<br />
4. 15 to 19 years<br />
5. 20 to 24 years<br />
6. 25 to 29 years<br />
7. 30 to 34 years<br />
8. 35 to 39 years<br />
9. 40 to 44 years<br />
10. 45 to 49 years<br />
[ 5 others ]</td>
<td>20215 ( 2.1%)<br />
20481 ( 2.1%)<br />
18290 ( 1.9%)<br />
17844 ( 1.8%)<br />
25162 ( 2.6%)<br />
35992 ( 3.7%)<br />
31131 ( 3.2%)<br />
21620 ( 2.2%)<br />
13480 ( 1.4%)<br />
8147 ( 0.8%)<br />
768506 (78.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>5</td>
<td>attsch<br />
[factor]</td>
<td>Education: School attendance - Detailed</td>
<td>1. Did not attend school<br />
2. Elementary or secondary s<br />
3. College, CEGEP, trade sch<br />
4. University<br />
5. Multiple responses<br />
6. Not applicable<br />
7. Not available</td>
<td>705357 (71.9%)<br />
37141 ( 3.8%)<br />
31897 ( 3.3%)<br />
40829 ( 4.2%)<br />
996 ( 0.1%)<br />
162040 (16.5%)<br />
2608 ( 0.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>6</td>
<td>bfnmemb<br />
[factor]</td>
<td>Indigenous: Membership in a First Nation or Indian band</td>
<td>1. Member of a First Nation<br />
2. Not a member of a First N</td>
<td>21339 ( 2.2%)<br />
959529 (97.8%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>7</td>
<td>bedrm<br />
[factor]</td>
<td>Bedrooms</td>
<td>1. No bedroom<br />
2. 1 bedroom<br />
3. 2 bedrooms<br />
4. 3 bedrooms<br />
5. 4 bedrooms<br />
6. 5 bedrooms or more<br />
7. Not available</td>
<td>4177 ( 0.4%)<br />
78111 ( 8.0%)<br />
198359 (20.2%)<br />
345688 (35.2%)<br />
239139 (24.4%)<br />
114544 (11.7%)<br />
850 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>8</td>
<td>cfinc<br />
[factor]</td>
<td>Income: Total income of census family for all persons</td>
<td>1. Under $2,000<br />
2. $2,000 to $4,999<br />
3. $5,000 to $6,999<br />
4. $7,000 to $9,999<br />
5. $10,000 to $11,999<br />
6. $12,000 to $14,999<br />
7. $15,000 to $16,999<br />
8. $17,000 to $19,999<br />
9. $20,000 to $24,999<br />
10. $25,000 to $29,999<br />
[ 24 others ]</td>
<td>11684 ( 1.2%)<br />
3188 ( 0.3%)<br />
2053 ( 0.2%)<br />
4023 ( 0.4%)<br />
3934 ( 0.4%)<br />
6306 ( 0.6%)<br />
6379 ( 0.7%)<br />
8941 ( 0.9%)<br />
30529 ( 3.1%)<br />
25346 ( 2.6%)<br />
878485 (89.6%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>9</td>
<td>cfinc_at<br />
[factor]</td>
<td>Income: After-tax income of census family for all persons</td>
<td>1. Under $2,000<br />
2. $2,000 to $4,999<br />
3. $5,000 to $6,999<br />
4. $7,000 to $9,999<br />
5. $10,000 to $11,999<br />
6. $12,000 to $14,999<br />
7. $15,000 to $16,999<br />
8. $17,000 to $19,999<br />
9. $20,000 to $24,999<br />
10. $25,000 to $29,999<br />
[ 23 others ]</td>
<td>12801 ( 1.3%)<br />
3253 ( 0.3%)<br />
2138 ( 0.2%)<br />
4153 ( 0.4%)<br />
3979 ( 0.4%)<br />
6514 ( 0.7%)<br />
6556 ( 0.7%)<br />
9569 ( 1.0%)<br />
32860 ( 3.4%)<br />
29075 ( 3.0%)<br />
869970 (88.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>10</td>
<td>cfstat<br />
[factor]</td>
<td>Household living arrangements of person, simple version</td>
<td>1. Married spouse or common-<br />
2. Married spouse or common-<br />
3. Parent in a one-parent fa<br />
4. Child of a couple<br />
5. Child of a parent in a on<br />
6. Person living alone<br />
7. Person not in a census fa<br />
8. Person not in a census fa</td>
<td>230970 (23.5%)<br />
232498 (23.7%)<br />
44600 ( 4.5%)<br />
216977 (22.1%)<br />
69695 ( 7.1%)<br />
118626 (12.1%)<br />
40948 ( 4.2%)<br />
26554 ( 2.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>11</td>
<td>chdbn<br />
[numeric]</td>
<td>Income: Child benefits</td>
<td>Mean (sd) : 89488003 (30555796)<br />
min &lt; med &lt; max:<br />
1 &lt; 1e+08 &lt; 1e+08<br />
IQR (CV) : 0 (0.3)</td>
<td>533 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>12</td>
<td>cip2021<br />
[factor]</td>
<td>Education: Major field of study, primary groupings (based on CIP
Canada 2021)</td>
<td>1. 01 Education<br />
2. 02 Visual and performing<br />
3. 03 Humanities<br />
4. 04 Social and behavioural<br />
5. 05 Business, management a<br />
6. 06 Physical and life scie<br />
7. 07 Mathematics, computer<br />
8. 08 Architecture, engineer<br />
9. 09 Agriculture, natural r<br />
10. 10 Health and related fie<br />
[ 5 others ]</td>
<td>29651 ( 3.0%)<br />
16019 ( 1.6%)<br />
24301 ( 2.5%)<br />
52800 ( 5.4%)<br />
99994 (10.2%)<br />
18804 ( 1.9%)<br />
20511 ( 2.1%)<br />
93763 ( 9.6%)<br />
9440 ( 1.0%)<br />
64632 ( 6.6%)<br />
550953 (56.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>13</td>
<td>cip2021_stem_sum<br />
[factor]</td>
<td>Education: Major field of study, STEM and BHASE (non-STEM) groupings
- Summary (based on CIP Canada 2021)</td>
<td>1. Science and science techn<br />
2. Engineering and engineeri<br />
3. Mathematics and computer<br />
4. Business and administrati<br />
5. Arts and humanities<br />
6. Social and behavioural sc<br />
7. Legal professions and stu<br />
8. Health care<br />
9. Education and teaching<br />
10. Trades, services, natural<br />
[ 3 others ]</td>
<td>23868 ( 2.4%)<br />
43528 ( 4.4%)<br />
20498 ( 2.1%)<br />
94302 ( 9.6%)<br />
39804 ( 4.1%)<br />
44576 ( 4.5%)<br />
7956 ( 0.8%)<br />
59249 ( 6.0%)<br />
29633 ( 3.0%)<br />
92682 ( 9.4%)<br />
524772 (53.5%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>14</td>
<td>cma<br />
[factor]</td>
<td>Census metropolitan area or census agglomeration of current
residence (2021)</td>
<td>1. Halifax<br />
2. Moncton � Saint John<br />
3. Qu�bec<br />
4. Montr�al<br />
5. Sherbrooke � Trois-Rivi�r<br />
6. Ottawa - Gatineau<br />
7. Oshawa<br />
8. Toronto<br />
9. Hamilton<br />
10. St. Catharines � Niagara<br />
[ 14 others ]</td>
<td>12392 ( 1.3%)<br />
7622 ( 0.8%)<br />
22104 ( 2.3%)<br />
113630 (11.6%)<br />
10051 ( 1.0%)<br />
39926 ( 4.1%)<br />
11209 ( 1.1%)<br />
165509 (16.9%)<br />
20984 ( 2.1%)<br />
11464 ( 1.2%)<br />
565977 (57.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>15</td>
<td>condo<br />
[factor]</td>
<td>Condominium status</td>
<td>1. Not condominium<br />
2. Condominium<br />
3. Not available</td>
<td>862651 (87.9%)<br />
114825 (11.7%)<br />
3392 ( 0.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>16</td>
<td>covid_erb<br />
[numeric]</td>
<td>Emergency and recovery benefits (COVID-19)</td>
<td>Mean (sd) : 77153244 (41944488)<br />
min &lt; med &lt; max:<br />
1 &lt; 1e+08 &lt; 1e+08<br />
IQR (CV) : 0 (0.5)</td>
<td>209 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>17</td>
<td>cow<br />
[factor]</td>
<td>Labour: Class of worker (derived)</td>
<td>1. Employee<br />
2. Unpaid family worker<br />
3. Self-employed, without pa<br />
4. Self-employed, with paid<br />
5. Self-employed, without pa<br />
6. Self-employed, with paid<br />
7. Not available<br />
8. Not applicable</td>
<td>474528 (48.4%)<br />
1879 ( 0.2%)<br />
14190 ( 1.4%)<br />
15183 ( 1.5%)<br />
42608 ( 4.3%)<br />
8356 ( 0.9%)<br />
272 ( 0.0%)<br />
423852 (43.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>18</td>
<td>cqppb<br />
[numeric]</td>
<td>Income: Canada Pension Plan (CPP) and Quebec Pension Plan (QPP)
benefits</td>
<td>Mean (sd) : 79359378 (40458013)<br />
min &lt; med &lt; max:<br />
1 &lt; 1e+08 &lt; 1e+08<br />
IQR (CV) : 0 (0.5)</td>
<td>352 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>19</td>
<td>capgn<br />
[numeric]</td>
<td>Income: Net capital gains or losses</td>
<td>Mean (sd) : 90492536 (29326668)<br />
min &lt; med &lt; max:<br />
-50000 &lt; 1e+08 &lt; 1e+08<br />
IQR (CV) : 0 (0.3)</td>
<td>359 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>20</td>
<td>cfsize<br />
[factor]</td>
<td>Census family size, stored at the person level</td>
<td>1. Person not in a census fa<br />
2. 2 persons<br />
3. 3 persons<br />
4. 4 persons<br />
5. 5 persons<br />
6. 6 persons<br />
7. 7 persons or more<br />
8. Not available</td>
<td>184126 (18.8%)<br />
285554 (29.1%)<br />
173781 (17.7%)<br />
211363 (21.5%)<br />
85305 ( 8.7%)<br />
26354 ( 2.7%)<br />
12830 ( 1.3%)<br />
1555 ( 0.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>21</td>
<td>chldc<br />
[numeric]</td>
<td>Income: Child care expenses paid</td>
<td>Mean (sd) : 96623914 (17867056)<br />
min &lt; med &lt; max:<br />
1 &lt; 1e+08 &lt; 1e+08<br />
IQR (CV) : 0 (0.2)</td>
<td>352 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>22</td>
<td>citoth<br />
[factor]</td>
<td>Citizenship: Other country of citizenship</td>
<td>1. United States<br />
2. Other Americas<br />
3. United Kingdom - British<br />
4. Other Europe<br />
5. Africa<br />
6. West Central Asia and the<br />
7. China, People’s Republic<br />
8. Other Eastern Asia<br />
9. Philippines<br />
10. Other Southeast Asia<br />
[ 6 others ]</td>
<td>10522 ( 1.1%)<br />
19233 ( 2.0%)<br />
10466 ( 1.1%)<br />
31078 ( 3.2%)<br />
20161 ( 2.1%)<br />
17908 ( 1.8%)<br />
14255 ( 1.5%)<br />
4689 ( 0.5%)<br />
10026 ( 1.0%)<br />
2551 ( 0.3%)<br />
839979 (85.6%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>23</td>
<td>citizen<br />
[factor]</td>
<td>Citizenship: Citizenship status and type - Summary</td>
<td>1. Canadian citizens by birt<br />
2. Canadian citizens by natu<br />
3. Not a Canadian citizen</td>
<td>729978 (74.4%)<br />
164940 (16.8%)<br />
85950 ( 8.8%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>24</td>
<td>dist<br />
[factor]</td>
<td>Commuting: Distance (straight-line) from home to work</td>
<td>1. Not applicable<br />
2. Less than 5 km<br />
3. 5 to 9.9 km<br />
4. 10 to 14.9 km<br />
5. 15 to 19.9 km<br />
6. 20 to 24.9 km<br />
7. 25 to 29.9 km<br />
8. Greater or equal to 30 km</td>
<td>688161 (70.2%)<br />
116270 (11.9%)<br />
60948 ( 6.2%)<br />
36892 ( 3.8%)<br />
23011 ( 2.3%)<br />
14711 ( 1.5%)<br />
9843 ( 1.0%)<br />
31032 ( 3.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>25</td>
<td>dpgrsum<br />
[factor]</td>
<td>Population group</td>
<td>1. White<br />
2. South Asian<br />
3. Chinese<br />
4. Black<br />
5. Filipino<br />
6. Arab<br />
7. Latin American<br />
8. Southeast Asian<br />
9. West Asian<br />
10. Korean<br />
[ 5 others ]</td>
<td>648291 (66.1%)<br />
64940 ( 6.6%)<br />
42494 ( 4.3%)<br />
35219 ( 3.6%)<br />
23803 ( 2.4%)<br />
17589 ( 1.8%)<br />
14434 ( 1.5%)<br />
8887 ( 0.9%)<br />
9033 ( 0.9%)<br />
5058 ( 0.5%)<br />
111120 (11.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>26</td>
<td>dtype<br />
[factor]</td>
<td>Structural type of dwelling</td>
<td>1. Single-detached house<br />
2. Apartment<br />
3. Other dwelling<br />
4. Not available</td>
<td>580460 (59.2%)<br />
262486 (26.8%)<br />
134530 (13.7%)<br />
3392 ( 0.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>27</td>
<td>efdecile<br />
[factor]</td>
<td>Income: National economic family after-tax income decile for all
persons</td>
<td>1. In bottom decile<br />
2. In second decile<br />
3. In third decile<br />
4. In fourth decile<br />
5. In fifth decile<br />
6. In sixth decile<br />
7. In seventh decile<br />
8. In eighth decile<br />
9. In ninth decile<br />
10. In top decile<br />
11. Not available</td>
<td>97628 (10.0%)<br />
97225 ( 9.9%)<br />
97753 (10.0%)<br />
97809 (10.0%)<br />
97798 (10.0%)<br />
97627 (10.0%)<br />
98152 (10.0%)<br />
98058 (10.0%)<br />
98033 (10.0%)<br />
97667 (10.0%)<br />
3118 ( 0.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>28</td>
<td>efinc<br />
[factor]</td>
<td>Income: Total income of economic family for all persons</td>
<td>1. Under $2,000<br />
2. $2,000 to $4,999<br />
3. $5,000 to $6,999<br />
4. $7,000 to $9,999<br />
5. $10,000 to $11,999<br />
6. $12,000 to $14,999<br />
7. $15,000 to $16,999<br />
8. $17,000 to $19,999<br />
9. $20,000 to $24,999<br />
10. $25,000 to $29,999<br />
[ 24 others ]</td>
<td>7498 ( 0.8%)<br />
2345 ( 0.2%)<br />
1590 ( 0.2%)<br />
3207 ( 0.3%)<br />
3226 ( 0.3%)<br />
5110 ( 0.5%)<br />
5209 ( 0.5%)<br />
7405 ( 0.8%)<br />
25075 ( 2.6%)<br />
21794 ( 2.2%)<br />
898409 (91.6%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>29</td>
<td>efinc_at<br />
[factor]</td>
<td>Income: After-tax income of economic family for all persons</td>
<td>1. Under $2,000<br />
2. $2,000 to $4,999<br />
3. $5,000 to $6,999<br />
4. $7,000 to $9,999<br />
5. $10,000 to $11,999<br />
6. $12,000 to $14,999<br />
7. $15,000 to $16,999<br />
8. $17,000 to $19,999<br />
9. $20,000 to $24,999<br />
10. $25,000 to $29,999<br />
[ 23 others ]</td>
<td>8601 ( 0.9%)<br />
2412 ( 0.2%)<br />
1662 ( 0.2%)<br />
3339 ( 0.3%)<br />
3269 ( 0.3%)<br />
5314 ( 0.5%)<br />
5369 ( 0.5%)<br />
7933 ( 0.8%)<br />
27127 ( 2.8%)<br />
25118 ( 2.6%)<br />
890724 (90.8%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>30</td>
<td>eicbn<br />
[numeric]</td>
<td>Income: Employment Insurance (EI) benefits</td>
<td>Mean (sd) : 91266366 (28229497)<br />
min &lt; med &lt; max:<br />
1 &lt; 1e+08 &lt; 1e+08<br />
IQR (CV) : 0 (0.3)</td>
<td>444 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>31</td>
<td>ethder<br />
[factor]</td>
<td>Ethnic or cultural origin: Derived single and selected multiple
ethnic or cultural origins</td>
<td>1. North American Indigenous<br />
2. Acadian<br />
3. Canadian<br />
4. French Canadian<br />
5. Qu�b�cois<br />
6. Other North American orig<br />
7. English<br />
8. Irish<br />
9. Scottish<br />
10. Other British Isles origi<br />
[ 50 others ]</td>
<td>24593 ( 2.5%)<br />
3408 ( 0.3%)<br />
112118 (11.4%)<br />
15842 ( 1.6%)<br />
19261 ( 2.0%)<br />
6504 ( 0.7%)<br />
30386 ( 3.1%)<br />
15922 ( 1.6%)<br />
14816 ( 1.5%)<br />
12007 ( 1.2%)<br />
726011 (74.0%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>32</td>
<td>efdimbm_2018<br />
[factor]</td>
<td>Income: Disposable income for 2018-base MBM of economic family for
all persons</td>
<td>1. Under $2,000<br />
2. $2,000 to $4,999<br />
3. $5,000 to $6,999<br />
4. $7,000 to $9,999<br />
5. $10,000 to $11,999<br />
6. $12,000 to $14,999<br />
7. $15,000 to $16,999<br />
8. $17,000 to $19,999<br />
9. $20,000 to $24,999<br />
10. $25,000 to $29,999<br />
[ 23 others ]</td>
<td>7208 ( 0.7%)<br />
2655 ( 0.3%)<br />
1988 ( 0.2%)<br />
3483 ( 0.4%)<br />
3067 ( 0.3%)<br />
5368 ( 0.5%)<br />
5129 ( 0.5%)<br />
8587 ( 0.9%)<br />
25337 ( 2.6%)<br />
28781 ( 2.9%)<br />
889265 (90.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>33</td>
<td>efsize<br />
[factor]</td>
<td>Economic family size, stored at the person level</td>
<td>1. Person not in an economic<br />
2. 2 persons<br />
3. 3 persons<br />
4. 4 persons<br />
5. 5 persons<br />
6. 6 persons<br />
7. 7 persons or more<br />
8. Not available</td>
<td>159496 (16.3%)<br />
264598 (27.0%)<br />
168702 (17.2%)<br />
210525 (21.5%)<br />
101439 (10.3%)<br />
43729 ( 4.5%)<br />
30824 ( 3.1%)<br />
1555 ( 0.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>34</td>
<td>empin<br />
[numeric]</td>
<td>Income: Employment income</td>
<td>Mean (sd) : 42129117 (49315367)<br />
min &lt; med &lt; max:<br />
-50000 &lt; 94000 &lt; 1e+08<br />
IQR (CV) : 99969999 (1.2)</td>
<td>257 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>35</td>
<td>fol<br />
[factor]</td>
<td>Language: First official language spoken</td>
<td>1. English<br />
2. French<br />
3. English and French<br />
4. Neither English nor Frenc</td>
<td>741011 (75.5%)<br />
208727 (21.3%)<br />
12955 ( 1.3%)<br />
18175 ( 1.9%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>36</td>
<td>fptwk<br />
[factor]</td>
<td>Labour: Full-time or part-time weeks worked in 2020</td>
<td>1. Worked mainly full-time w<br />
2. Worked mainly part-time w<br />
3. Not available<br />
4. Not applicable</td>
<td>406992 (41.5%)<br />
111561 (11.4%)<br />
0 ( 0.0%)<br />
462315 (47.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>37</td>
<td>gender<br />
[factor]</td>
<td>Gender of person (binary)</td>
<td>1. Woman+<br />
2. Man+</td>
<td>496738 (50.6%)<br />
484130 (49.4%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>38</td>
<td>genstat<br />
[factor]</td>
<td>Generation status: Detailed</td>
<td>1. First generation, respond<br />
2. Second generation, respon<br />
3. Second generation, respon<br />
4. Third generation or more,<br />
5. Not available</td>
<td>243517 (24.8%)<br />
100990 (10.3%)<br />
72542 ( 7.4%)<br />
547834 (55.9%)<br />
15985 ( 1.6%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>39</td>
<td>govti<br />
[numeric]</td>
<td>Income: Other income from government sources</td>
<td>Mean (sd) : 37387067 (48378362)<br />
min &lt; med &lt; max:<br />
1 &lt; 10200 &lt; 1e+08<br />
IQR (CV) : 99998699 (1.3)</td>
<td>704 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>40</td>
<td>gtrfs<br />
[numeric]</td>
<td>Income: Government transfers</td>
<td>Mean (sd) : 30229546 (45905506)<br />
min &lt; med &lt; max:<br />
1 &lt; 16900 &lt; 1e+08<br />
IQR (CV) : 99994799 (1.5)</td>
<td>542 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>41</td>
<td>hcoreneed_ind<br />
[factor]</td>
<td>Housing core need indicator</td>
<td>1. In core need<br />
2. Not in core need<br />
3. Not available</td>
<td>72172 ( 7.4%)<br />
872377 (88.9%)<br />
36319 ( 3.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>42</td>
<td>hdgree<br />
[factor]</td>
<td>Education: Highest certificate, diploma or degree</td>
<td>1. No certificate, diploma o<br />
2. High (secondary) school d<br />
3. Non-apprenticeship trades<br />
4. ·Apprenticeship certifica<br />
5. Program of 3 months to le<br />
6. Program of 1 to 2 years (<br />
7. Program of more than 2 ye<br />
8. University certificate or<br />
9. Bachelor’s degree<br />
10. University certificate or<br />
[ 5 others ]</td>
<td>131882 (13.4%)<br />
218657 (22.3%)<br />
39243 ( 4.0%)<br />
31949 ( 3.3%)<br />
24585 ( 2.5%)<br />
72844 ( 7.4%)<br />
56955 ( 5.8%)<br />
24359 ( 2.5%)<br />
143066 (14.6%)<br />
14090 ( 1.4%)<br />
223238 (22.8%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>43</td>
<td>hhinc<br />
[factor]</td>
<td>Income: Total income of household for all persons</td>
<td>1. Under $2,000<br />
2. $2,000 to $4,999<br />
3. $5,000 to $6,999<br />
4. $7,000 to $9,999<br />
5. $10,000 to $11,999<br />
6. $12,000 to $14,999<br />
7. $15,000 to $16,999<br />
8. $17,000 to $19,999<br />
9. $20,000 to $24,999<br />
10. $25,000 to $29,999<br />
[ 24 others ]</td>
<td>4985 ( 0.5%)<br />
1694 ( 0.2%)<br />
1112 ( 0.1%)<br />
2279 ( 0.2%)<br />
2288 ( 0.2%)<br />
3700 ( 0.4%)<br />
3907 ( 0.4%)<br />
5493 ( 0.6%)<br />
20441 ( 2.1%)<br />
17908 ( 1.8%)<br />
917061 (93.5%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>44</td>
<td>hhinc_at<br />
[factor]</td>
<td>Income: After-tax income of household</td>
<td>1. Under $2,000<br />
2. $2,000 to $4,999<br />
3. $5,000 to $6,999<br />
4. $7,000 to $9,999<br />
5. $10,000 to $11,999<br />
6. $12,000 to $14,999<br />
7. $15,000 to $16,999<br />
8. $17,000 to $19,999<br />
9. $20,000 to $24,999<br />
10. $25,000 to $29,999<br />
[ 23 others ]</td>
<td>6071 ( 0.6%)<br />
1762 ( 0.2%)<br />
1185 ( 0.1%)<br />
2392 ( 0.2%)<br />
2348 ( 0.2%)<br />
3888 ( 0.4%)<br />
4041 ( 0.4%)<br />
5855 ( 0.6%)<br />
21832 ( 2.2%)<br />
20419 ( 2.1%)<br />
911075 (92.9%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>45</td>
<td>hhmrkinc<br />
[factor]</td>
<td>Income: Market income of household</td>
<td>1. Under $2,000<br />
2. $2,000 to $4,999<br />
3. $5,000 to $6,999<br />
4. $7,000 to $9,999<br />
5. $10,000 to $11,999<br />
6. $12,000 to $14,999<br />
7. $15,000 to $16,999<br />
8. $17,000 to $19,999<br />
9. $20,000 to $24,999<br />
10. $25,000 to $29,999<br />
[ 24 others ]</td>
<td>60322 ( 6.1%)<br />
16316 ( 1.7%)<br />
10894 ( 1.1%)<br />
15241 ( 1.6%)<br />
10225 ( 1.0%)<br />
15531 ( 1.6%)<br />
10274 ( 1.0%)<br />
15214 ( 1.6%)<br />
25899 ( 2.6%)<br />
26456 ( 2.7%)<br />
774496 (79.0%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>46</td>
<td>hhsize<br />
[factor]</td>
<td>Household size</td>
<td>1. 1 person<br />
2. 2 persons<br />
3. 3 persons<br />
4. 4 persons<br />
5. 5 persons<br />
6. 6 persons<br />
7. 7 persons or more<br />
8. Not available</td>
<td>118744 (12.1%)<br />
276308 (28.2%)<br />
177806 (18.1%)<br />
216742 (22.1%)<br />
106601 (10.9%)<br />
47323 ( 4.8%)<br />
35789 ( 3.6%)<br />
1555 ( 0.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>47</td>
<td>hhtype<br />
[factor]</td>
<td>Household type</td>
<td>1. One-census-family househo<br />
2. One-census-family househo<br />
3. One-census-family househo<br />
4. One-census-family househo<br />
5. One-census-family househo<br />
6. One-census-family househo<br />
7. Multiple-census-family ho<br />
8. Non-census-family househo<br />
9. Non-census-family househo<br />
10. Not available</td>
<td>206882 (21.1%)<br />
397898 (40.6%)<br />
15360 ( 1.6%)<br />
38269 ( 3.9%)<br />
89158 ( 9.1%)<br />
21248 ( 2.2%)<br />
49355 ( 5.0%)<br />
118626 (12.1%)<br />
42425 ( 4.3%)<br />
1647 ( 0.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>48</td>
<td>hlmosten<br />
[factor]</td>
<td>Language: Language spoken most often at home - English
component</td>
<td>1. True - Respondent reporte<br />
2. False - Respondent did no<br />
3. Not available</td>
<td>665130 (67.8%)<br />
314670 (32.1%)<br />
1068 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>49</td>
<td>hlmostfr<br />
[factor]</td>
<td>Language: Language spoken most often at home - French component</td>
<td>1. True - Respondent reporte<br />
2. False - Respondent did no<br />
3. Not available</td>
<td>198400 (20.2%)<br />
781400 (79.7%)<br />
1068 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>50</td>
<td>hlmostno<br />
[factor]</td>
<td>Language: Language spoken most often at home - First write-in
component</td>
<td>1. No non-official language<br />
2. Indigenous languages<br />
3. Italian<br />
4. Spanish<br />
5. Portuguese<br />
6. German<br />
7. Russian<br />
8. Polish<br />
9. Slavic languages<br />
10. Other European languages<br />
[ 15 others ]</td>
<td>813565 (82.9%)<br />
2995 ( 0.3%)<br />
3050 ( 0.3%)<br />
10627 ( 1.1%)<br />
3697 ( 0.4%)<br />
1834 ( 0.2%)<br />
3818 ( 0.4%)<br />
2095 ( 0.2%)<br />
3465 ( 0.4%)<br />
2642 ( 0.3%)<br />
133080 (13.6%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>51</td>
<td>hlregen<br />
[factor]</td>
<td>Language: Other language(s) spoken regularly at home - English
component</td>
<td>1. True - Respondent reporte<br />
2. False - Respondent did no<br />
3. Not available</td>
<td>62201 ( 6.3%)<br />
917599 (93.5%)<br />
1068 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>52</td>
<td>hlregfr<br />
[factor]</td>
<td>Language: Other language(s) spoken regularly at home - French
component</td>
<td>1. True - Respondent reporte<br />
2. False - Respondent did no<br />
3. Not available</td>
<td>21206 ( 2.2%)<br />
958594 (97.7%)<br />
1068 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>53</td>
<td>hlregno<br />
[factor]</td>
<td>Language: Other language(s) spoken regularly at home - First
write-in component</td>
<td>1. False - Respondent did no<br />
2. True - Respondent reporte</td>
<td>902944 (92.1%)<br />
77924 ( 7.9%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>54</td>
<td>immcat5<br />
[factor]</td>
<td>Immigration: Admission category - Summary</td>
<td>1. Non-immigrants<br />
2. Non-permanent residents<br />
3. Immigrants who landed bef<br />
4. Economic immigrants·<br />
5. Immigrants sponsored by f<br />
6. Refugees<br />
7. Not available</td>
<td>729972 (74.4%)<br />
24320 ( 2.5%)<br />
41328 ( 4.2%)<br />
99063 (10.1%)<br />
57123 ( 5.8%)<br />
27689 ( 2.8%)<br />
1373 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>55</td>
<td>immstat<br />
[factor]</td>
<td>Immigration: Immigrant status</td>
<td>1. Non-immigrants<br />
2. Immigrants<br />
3. Non-permanent residents<br />
4. Not available</td>
<td>729972 (74.4%)<br />
225203 (23.0%)<br />
24320 ( 2.5%)<br />
1373 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>56</td>
<td>inctax<br />
[numeric]</td>
<td>Income: Income taxes</td>
<td>Mean (sd) : 40683258 (49084643)<br />
min &lt; med &lt; max:<br />
1 &lt; 19000 &lt; 1e+08<br />
IQR (CV) : 99994999 (1.2)</td>
<td>184 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>57</td>
<td>invst<br />
[numeric]</td>
<td>Income: Investment income</td>
<td>Mean (sd) : 74515233 (43573315)<br />
min &lt; med &lt; max:<br />
-50000 &lt; 1e+08 &lt; 1e+08<br />
IQR (CV) : 99901599 (0.6)</td>
<td>1417 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>58</td>
<td>jobperm<br />
[factor]</td>
<td>Labour: Job permanency</td>
<td>1. Employee - Permanent posi<br />
2. Not applicable<br />
3. Other employment - unpaid<br />
4. Employee - Fixed term (1<br />
5. Not available</td>
<td>382093 (39.0%)<br />
423852 (43.2%)<br />
82216 ( 8.4%)<br />
92435 ( 9.4%)<br />
272 ( 0.0%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>59</td>
<td>kol<br />
[factor]</td>
<td>Language: Knowledge of official languages</td>
<td>1. English only<br />
2. French only<br />
3. English and French<br />
4. Neither English nor Frenc<br />
5. Not available</td>
<td>677080 (69.0%)<br />
107916 (11.0%)<br />
176881 (18.0%)<br />
17923 ( 1.8%)<br />
1068 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>60</td>
<td>lfact<br />
[factor]</td>
<td>Labour: Labour force status - Detailed</td>
<td>1. Employed - Worked in refe<br />
2. Employed - Absent in refe<br />
3. Unemployed - Temporary la<br />
4. Unemployed - Temporary la<br />
5. Unemployed - Temporary la<br />
6. Unemployed - New job - Di<br />
7. Unemployed - New job - Lo<br />
8. Unemployed - New job - Lo<br />
9. Unemployed - Looked for f<br />
10. Unemployed - Looked for p<br />
[ 6 others ]</td>
<td>442970 (45.2%)<br />
24106 ( 2.5%)<br />
11087 ( 1.1%)<br />
8626 ( 0.9%)<br />
2470 ( 0.3%)<br />
3382 ( 0.3%)<br />
2800 ( 0.3%)<br />
1162 ( 0.1%)<br />
16382 ( 1.7%)<br />
8132 ( 0.8%)<br />
459751 (46.9%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>61</td>
<td>lico_bt<br />
[factor]</td>
<td>Income: Low-income status based on LICO-BT</td>
<td>1. Not applicable<br />
2. Not in low income<br />
3. In low income<br />
4. Not available</td>
<td>2919 ( 0.3%)<br />
899570 (91.7%)<br />
75261 ( 7.7%)<br />
3118 ( 0.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>62</td>
<td>lico_at<br />
[factor]</td>
<td>Income: Low-income status based on LICO-AT</td>
<td>1. Not applicable<br />
2. Not in low income<br />
3. In low income<br />
4. Not available</td>
<td>2919 ( 0.3%)<br />
924026 (94.2%)<br />
50805 ( 5.2%)<br />
3118 ( 0.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>63</td>
<td>liprogtype<br />
[factor]</td>
<td>Language: Type of French program attended in Canada by residents of
Canada outside Quebec</td>
<td>1. Not applicable<br />
2. Residents of Canada outsi<br />
3. Residents of Canada outsi<br />
4. Residents of Canada outsi<br />
5. Residents of Canada outsi</td>
<td>224250 (22.9%)<br />
32560 ( 3.3%)<br />
44653 ( 4.6%)<br />
3787 ( 0.4%)<br />
675618 (68.9%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>64</td>
<td>li_elig_oml_u18<br />
[factor]</td>
<td>Language: Eligibility for instruction in the minority official
language</td>
<td>1. Not applicable (Born befo<br />
2. Eligible child<br />
3. Child not eligible</td>
<td>782497 (79.8%)<br />
24108 ( 2.5%)<br />
174263 (17.8%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>65</td>
<td>locstud<br />
[factor]</td>
<td>Education: Location of study</td>
<td>1. Newfoundland and Labrador<br />
2. Prince Edward Island<br />
3. Nova Scotia<br />
4. New Brunswick<br />
5. Quebec<br />
6. Ontario<br />
7. Manitoba<br />
8. Saskatchewan<br />
9. Alberta<br />
10. British Columbia<br />
[ 8 others ]</td>
<td>6542 ( 0.7%)<br />
1632 ( 0.2%)<br />
12643 ( 1.3%)<br />
8687 ( 0.9%)<br />
101125 (10.3%)<br />
143918 (14.7%)<br />
12756 ( 1.3%)<br />
11348 ( 1.2%)<br />
37495 ( 3.8%)<br />
41206 ( 4.2%)<br />
603516 (61.5%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>66</td>
<td>loc_st_res<br />
[factor]</td>
<td>Education: Location of study compared with province or territory of
residence - Summary</td>
<td>1. Same as province or terri<br />
2. Different than province o<br />
3. Outside Canada<br />
4. No postsecondary certific<br />
5. Not applicable</td>
<td>331806 (33.8%)<br />
45960 ( 4.7%)<br />
90411 ( 9.2%)<br />
350651 (35.7%)<br />
162040 (16.5%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>67</td>
<td>lstwrk<br />
[factor]</td>
<td>Labour: When last worked for pay or in self-employment</td>
<td>1. Last worked in 2021<br />
2. Last worked in 2020<br />
3. Last worked before 2020<br />
4. Never worked<br />
5. Not applicable<br />
6. Not available</td>
<td>501950 (51.2%)<br />
54783 ( 5.6%)<br />
193525 (19.7%)<br />
67963 ( 6.9%)<br />
162040 (16.5%)<br />
607 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>68</td>
<td>lwmosten<br />
[factor]</td>
<td>Language: Language used most often at work - English component</td>
<td>1. Not applicable<br />
2. True - Respondent reporte<br />
3. False - Respondent did no<br />
4. Not available</td>
<td>423462 (43.2%)<br />
440798 (44.9%)<br />
115540 (11.8%)<br />
1068 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>69</td>
<td>lwmostfr<br />
[factor]</td>
<td>Language: Language used most often at work - French component</td>
<td>1. Not applicable<br />
2. True - Respondent reporte<br />
3. False - Respondent did no<br />
4. Not available</td>
<td>423462 (43.2%)<br />
116225 (11.8%)<br />
440113 (44.9%)<br />
1068 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>70</td>
<td>lwmostno<br />
[factor]</td>
<td>Language: Language used most often at work - First write-in
component</td>
<td>1. False - Respondent did no<br />
2. True - Respondent reporte<br />
3. Not applicable</td>
<td>545055 (55.6%)<br />
11961 ( 1.2%)<br />
423852 (43.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>71</td>
<td>lwregen<br />
[factor]</td>
<td>Language: Other language(s) used regularly at work - English
component</td>
<td>1. Not applicable<br />
2. True - Respondent reporte<br />
3. False - Respondent did no<br />
4. Not available</td>
<td>423462 (43.2%)<br />
25043 ( 2.6%)<br />
531295 (54.2%)<br />
1068 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>72</td>
<td>lwregfr<br />
[factor]</td>
<td>Language: Other language(s) used regularly at work - French
component</td>
<td>1. Not applicable<br />
2. True - Respondent reporte<br />
3. False - Respondent did no<br />
4. Not available</td>
<td>423462 (43.2%)<br />
16802 ( 1.7%)<br />
539536 (55.0%)<br />
1068 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>73</td>
<td>lwregno<br />
[factor]</td>
<td>Language: Other language(s) used regularly at work - First write-in
component</td>
<td>1. False - Respondent did no<br />
2. True - Respondent reporte<br />
3. Not applicable</td>
<td>545106 (55.6%)<br />
11910 ( 1.2%)<br />
423852 (43.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>74</td>
<td>lolima<br />
[factor]</td>
<td>Income: Low-income status based on LIM-AT</td>
<td>1. Not in low income<br />
2. In low income<br />
3. Not available</td>
<td>869848 (88.7%)<br />
107902 (11.0%)<br />
3118 ( 0.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>75</td>
<td>lolimb<br />
[factor]</td>
<td>Income: Low-income status based on LIM-BT</td>
<td>1. Not in low income<br />
2. In low income<br />
3. Not available</td>
<td>839935 (85.6%)<br />
137815 (14.1%)<br />
3118 ( 0.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>76</td>
<td>lombm_2018<br />
[factor]</td>
<td>Income: Poverty status based on 2018-base MBM</td>
<td>1. Not applicable<br />
2. Not in poverty<br />
3. In poverty<br />
4. Not available</td>
<td>2919 ( 0.3%)<br />
894945 (91.2%)<br />
79886 ( 8.1%)<br />
3118 ( 0.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>77</td>
<td>mode<br />
[factor]</td>
<td>Commuting: Main mode of commuting</td>
<td>1. Not applicable<br />
2. Car, truck or van - as a<br />
3. Car, truck or van - as a<br />
4. Public transit<br />
5. Walked<br />
6. Bicycle<br />
7. Motorcycle, scooter or mo<br />
8. Other method</td>
<td>628805 (64.1%)<br />
272720 (27.8%)<br />
22659 ( 2.3%)<br />
27073 ( 2.8%)<br />
18200 ( 1.9%)<br />
3728 ( 0.4%)<br />
580 ( 0.1%)<br />
7103 ( 0.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>78</td>
<td>mtnen<br />
[factor]</td>
<td>Language: Mother tongue - English component</td>
<td>1. True - Respondent reporte<br />
2. False - Respondent did no<br />
3. Not available</td>
<td>573052 (58.4%)<br />
406748 (41.5%)<br />
1068 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>79</td>
<td>mtnfr<br />
[factor]</td>
<td>Language: Mother tongue - French component</td>
<td>1. True - Respondent reporte<br />
2. False - Respondent did no<br />
3. Not available</td>
<td>203564 (20.8%)<br />
776236 (79.1%)<br />
1068 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>80</td>
<td>mtnno<br />
[factor]</td>
<td>Language: Mother Tongue - First write-in component</td>
<td>1. No non-official language<br />
2. Indigenous languages<br />
3. Arabic<br />
4. Mandarin<br />
5. Cantonese<br />
6. Chinese languages<br />
7. German<br />
8. Other Germanic languages<br />
9. Greek<br />
10. Urdu<br />
[ 23 others ]</td>
<td>735415 (75.0%)<br />
4817 ( 0.5%)<br />
16231 ( 1.7%)<br />
19208 ( 2.0%)<br />
16219 ( 1.7%)<br />
1771 ( 0.2%)<br />
6689 ( 0.7%)<br />
775 ( 0.1%)<br />
2563 ( 0.3%)<br />
7876 ( 0.8%)<br />
169304 (17.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>81</td>
<td>marsth<br />
[factor]</td>
<td>Marital status (de facto)</td>
<td>1. Never married (not living<br />
2. Married<br />
3. Living common law<br />
4. Separated (not living com<br />
5. Divorced (not living comm<br />
6. Widowed (not living commo<br />
7. Not available</td>
<td>398946 (40.7%)<br />
366826 (37.4%)<br />
105102 (10.7%)<br />
19114 ( 1.9%)<br />
50287 ( 5.1%)<br />
38946 ( 4.0%)<br />
1647 ( 0.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>82</td>
<td>mob1<br />
[factor]</td>
<td>Mobility 1: Mobility Status - Place of residence 1 year ago
(2020)</td>
<td>1. Non-movers<br />
2. Non-migrants<br />
3. Different CSD, same censu<br />
4. Different CD, same provin<br />
5. Interprovincial migrants<br />
6. External migrants<br />
7. Not available<br />
8. Not applicable</td>
<td>853344 (87.0%)<br />
69480 ( 7.1%)<br />
9883 ( 1.0%)<br />
22740 ( 2.3%)<br />
6251 ( 0.6%)<br />
7188 ( 0.7%)<br />
2830 ( 0.3%)<br />
9152 ( 0.9%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>83</td>
<td>mob5<br />
[factor]</td>
<td>Mobility 5: Mobility Status - Place of residence 5 years ago
(2016)</td>
<td>1. Non-movers<br />
2. Non-migrants<br />
3. Different CSD, same censu<br />
4. Different CD, same provin<br />
5. Interprovincial migrants<br />
6. External migrants<br />
7. Not applicable<br />
8. Not available</td>
<td>569326 (58.0%)<br />
156268 (15.9%)<br />
49166 ( 5.0%)<br />
86796 ( 8.8%)<br />
24715 ( 2.5%)<br />
45503 ( 4.6%)<br />
49094 ( 5.0%)<br />
0 ( 0.0%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>84</td>
<td>mrkinc<br />
[numeric]</td>
<td>Income: Market income</td>
<td>Mean (sd) : 28593656 (45111559)<br />
min &lt; med &lt; max:<br />
-50000 &lt; 59000 &lt; 1e+08<br />
IQR (CV) : 99978999 (1.6)</td>
<td>262 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>85</td>
<td>naics<br />
[factor]</td>
<td>Labour: Industry sectors (based on the North American Industry
Classification System [NAICS] Canada 2017 version 3.0)</td>
<td>1. 11 Agriculture, forestry,<br />
2. 21 Mining, quarrying, and<br />
3. 22 Utilities<br />
4. 23 Construction<br />
5. 31-33 Manufacturing<br />
6. 41 Wholesale trade<br />
7. 44-45 Retail trade<br />
8. 48-49 Transportation and<br />
9. 51 Information and cultur<br />
10. 52 Finance and insurance/<br />
[ 11 others ]</td>
<td>12834 ( 1.3%)<br />
6523 ( 0.7%)<br />
3614 ( 0.4%)<br />
42562 ( 4.3%)<br />
44234 ( 4.5%)<br />
16805 ( 1.7%)<br />
64481 ( 6.6%)<br />
28467 ( 2.9%)<br />
10797 ( 1.1%)<br />
23824 ( 2.4%)<br />
726727 (74.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>86</td>
<td>noc21<br />
[factor]</td>
<td>Labour: Occupation Major Group (based on the National Occupational
Classification [NOC] 2021 version 1.0)</td>
<td>1. Not applicable<br />
2. 00 Legislative and senior<br />
3. Middle management occupat<br />
4. 11 Professional occupatio<br />
5. 12 Administrative and fin<br />
6. 13 Administrative occupat<br />
7. 14 Administrative and fin<br />
8. 21 Professional occupatio<br />
9. 22 Technical occupations<br />
10. 31 Professional occupatio<br />
[ 18 others ]</td>
<td>423852 (43.2%)<br />
6593 ( 0.7%)<br />
56178 ( 5.7%)<br />
20947 ( 2.1%)<br />
13344 ( 1.4%)<br />
21502 ( 2.2%)<br />
25731 ( 2.6%)<br />
25486 ( 2.6%)<br />
14749 ( 1.5%)<br />
18347 ( 1.9%)<br />
354139 (36.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>87</td>
<td>nol<br />
[factor]</td>
<td>Language: Knowledge of non-official languages - First write-in
component</td>
<td>1. No non-official language<br />
2. Indigenous languages<br />
3. Arabic<br />
4. Chinese languages<br />
5. German<br />
6. Other Germanic languages<br />
7. Greek<br />
8. Other Indo-Iranian langua<br />
9. Italian<br />
10. Polish<br />
[ 17 others ]</td>
<td>691172 (70.5%)<br />
5882 ( 0.6%)<br />
18180 ( 1.9%)<br />
31808 ( 3.2%)<br />
7485 ( 0.8%)<br />
492 ( 0.1%)<br />
2949 ( 0.3%)<br />
24373 ( 2.5%)<br />
11458 ( 1.2%)<br />
4239 ( 0.4%)<br />
182830 (18.6%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>88</td>
<td>nos<br />
[factor]</td>
<td>Housing suitability</td>
<td>1. Suitable<br />
2. Not suitable</td>
<td>885530 (90.3%)<br />
95338 ( 9.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>89</td>
<td>oasgi<br />
[numeric]</td>
<td>Income: Old Age Security pension (OAS) and Guaranteed Income
Supplement (GIS)</td>
<td>Mean (sd) : 83618073 (37007542)<br />
min &lt; med &lt; max:<br />
1 &lt; 1e+08 &lt; 1e+08<br />
IQR (CV) : 0 (0.4)</td>
<td>289 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>90</td>
<td>otinc<br />
[numeric]</td>
<td>Income: Market income not included elsewhere</td>
<td>Mean (sd) : 85979799 (34716476)<br />
min &lt; med &lt; max:<br />
1 &lt; 1e+08 &lt; 1e+08<br />
IQR (CV) : 0 (0.4)</td>
<td>184 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>91</td>
<td>pkid25<br />
[factor]</td>
<td>Number of children in census family aged 25 and over</td>
<td>1. None<br />
2. One or more<br />
3. Not applicable<br />
4. Not available</td>
<td>685874 (69.9%)<br />
103501 (10.6%)<br />
184688 (18.8%)<br />
6805 ( 0.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>92</td>
<td>pkid0_1<br />
[factor]</td>
<td>Number of children in census family aged 0 or 1</td>
<td>1. None<br />
2. One or more<br />
3. Not applicable<br />
4. Not available</td>
<td>723857 (73.8%)<br />
65518 ( 6.7%)<br />
184688 (18.8%)<br />
6805 ( 0.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>93</td>
<td>pkid15_24<br />
[factor]</td>
<td>Number of children in census family aged 15 to 24</td>
<td>1. None<br />
2. One or more<br />
3. Not applicable<br />
4. Not available</td>
<td>567070 (57.8%)<br />
222305 (22.7%)<br />
184688 (18.8%)<br />
6805 ( 0.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>94</td>
<td>pkid2_5<br />
[factor]</td>
<td>Number of children in census family aged 2 to 5</td>
<td>1. None<br />
2. One or more<br />
3. Not applicable<br />
4. Not available</td>
<td>655234 (66.8%)<br />
134141 (13.7%)<br />
184688 (18.8%)<br />
6805 ( 0.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>95</td>
<td>pkid6_14<br />
[factor]</td>
<td>Number of children in census family aged 6 to 14</td>
<td>1. None<br />
2. One or more<br />
3. Not applicable<br />
4. Not available</td>
<td>526310 (53.7%)<br />
263065 (26.8%)<br />
184688 (18.8%)<br />
6805 ( 0.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>96</td>
<td>pkids<br />
[factor]</td>
<td>Indicator of whether any children are present in census family</td>
<td>1. None<br />
2. One or more<br />
3. Not applicable<br />
4. Not available</td>
<td>230987 (23.5%)<br />
558388 (56.9%)<br />
184688 (18.8%)<br />
6805 ( 0.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>97</td>
<td>pob<br />
[factor]</td>
<td>Place of birth of person</td>
<td>1. Canada<br />
2. United States<br />
3. Central America<br />
4. Jamaica<br />
5. Other Caribbean and Bermu<br />
6. South America<br />
7. United Kingdom<br />
8. Germany<br />
9. France<br />
10. Other Northern and Wester<br />
[ 23 others ]</td>
<td>721366 (73.5%)<br />
9263 ( 0.9%)<br />
5791 ( 0.6%)<br />
3657 ( 0.4%)<br />
7123 ( 0.7%)<br />
10781 ( 1.1%)<br />
13070 ( 1.3%)<br />
3366 ( 0.3%)<br />
4324 ( 0.4%)<br />
5046 ( 0.5%)<br />
197081 (20.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>98</td>
<td>pobpar1<br />
[factor]</td>
<td>Place of birth of parent 1</td>
<td>1. Canada<br />
2. Americas<br />
3. Europe<br />
4. Eastern Asia<br />
5. Southeast and Southern As<br />
6. Other countries and regio<br />
7. Not available</td>
<td>620070 (63.2%)<br />
45962 ( 4.7%)<br />
91834 ( 9.4%)<br />
46134 ( 4.7%)<br />
98083 (10.0%)<br />
62800 ( 6.4%)<br />
15985 ( 1.6%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>99</td>
<td>pobpar2<br />
[factor]</td>
<td>Place of birth of parent 2</td>
<td>1. Canada<br />
2. Americas<br />
3. Europe<br />
4. Eastern Asia<br />
5. Southeast and Southern As<br />
6. Other countries and regio<br />
7. Not available</td>
<td>559654 (57.1%)<br />
61813 ( 6.3%)<br />
127959 (13.0%)<br />
47757 ( 4.9%)<br />
101088 (10.3%)<br />
66612 ( 6.8%)<br />
15985 ( 1.6%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>100</td>
<td>powst<br />
[factor]</td>
<td>Commuting: Place of work status</td>
<td>1. Worked at home<br />
2. No fixed workplace<br />
3. Worked outside Canada<br />
4. Worked in census subdivis<br />
5. Worked in a different cen<br />
6. Worked in a different cen<br />
7. Not available<br />
8. Not applicable<br />
9. Worked in a different pro</td>
<td>113264 (11.5%)<br />
59106 ( 6.0%)<br />
1068 ( 0.1%)<br />
172893 (17.6%)<br />
63066 ( 6.4%)<br />
53439 ( 5.4%)<br />
2143 ( 0.2%)<br />
513791 (52.4%)<br />
2098 ( 0.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>101</td>
<td>pr<br />
[factor]</td>
<td>Province or territory of current residence (2021)</td>
<td>1. Newfoundland and Labrador<br />
2. Prince Edward Island<br />
3. Nova Scotia<br />
4. New Brunswick<br />
5. Quebec<br />
6. Ontario<br />
7. Manitoba<br />
8. Saskatchewan<br />
9. Alberta<br />
10. British Columbia<br />
11. Northern Canada</td>
<td>13552 ( 1.4%)<br />
4076 ( 0.4%)<br />
25789 ( 2.6%)<br />
20511 ( 2.1%)<br />
224250 (22.9%)<br />
378849 (38.6%)<br />
35311 ( 3.6%)<br />
29764 ( 3.0%)<br />
112878 (11.5%)<br />
132733 (13.5%)<br />
3155 ( 0.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>102</td>
<td>pr1<br />
[factor]</td>
<td>Mobility 1: Province or territory of residence 1 year ago
(2020)</td>
<td>1. Newfoundland and Labrador<br />
2. Prince Edward Island<br />
3. Nova Scotia<br />
4. New Brunswick<br />
5. Quebec<br />
6. Ontario<br />
7. Manitoba<br />
8. Saskatchewan<br />
9. Alberta<br />
10. British Columbia<br />
[ 3 others ]</td>
<td>13361 ( 1.4%)<br />
3948 ( 0.4%)<br />
24983 ( 2.5%)<br />
19983 ( 2.0%)<br />
220473 (22.5%)<br />
372433 (38.0%)<br />
34413 ( 3.5%)<br />
29234 ( 3.0%)<br />
110798 (11.3%)<br />
129047 (13.2%)<br />
22195 ( 2.3%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>103</td>
<td>pr5<br />
[factor]</td>
<td>Mobility 5: Province or territory of residence 5 years ago
(2016)</td>
<td>1. Newfoundland and Labrador<br />
2. Prince Edward Island<br />
3. Nova Scotia<br />
4. New Brunswick<br />
5. Quebec<br />
6. Ontario<br />
7. Manitoba<br />
8. Saskatchewan<br />
9. Alberta<br />
10. British Columbia<br />
[ 3 others ]</td>
<td>12966 ( 1.3%)<br />
3601 ( 0.4%)<br />
22977 ( 2.3%)<br />
18605 ( 1.9%)<br />
204312 (20.8%)<br />
341064 (34.8%)<br />
32036 ( 3.3%)<br />
27564 ( 2.8%)<br />
103110 (10.5%)<br />
117170 (11.9%)<br />
97463 ( 9.9%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>104</td>
<td>presmortg<br />
[factor]</td>
<td>Mortgage, presence of</td>
<td>1. With mortgage<br />
2. Without mortgage<br />
3. Not applicable<br />
4. Not available</td>
<td>470436 (48.0%)<br />
229204 (23.4%)<br />
279505 (28.5%)<br />
1723 ( 0.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>105</td>
<td>prihm<br />
[factor]</td>
<td>Primary household maintainer</td>
<td>1. Person is not primary mai<br />
2. Person is primary maintai<br />
3. Not applicable</td>
<td>414324 (42.2%)<br />
404504 (41.2%)<br />
162040 (16.5%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>106</td>
<td>pwdur<br />
[factor]</td>
<td>Commuting: Commuting duration</td>
<td>1. Less than 15 minutes<br />
2. Between 15 and 29 minutes<br />
3. Between 30 and 44 minutes<br />
4. Between 45 and 59 minutes<br />
5. 60 minutes and over<br />
6. Not applicable</td>
<td>112700 (11.5%)<br />
122894 (12.5%)<br />
67057 ( 6.8%)<br />
24708 ( 2.5%)<br />
24704 ( 2.5%)<br />
628805 (64.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>107</td>
<td>pwleave<br />
[factor]</td>
<td>Commuting: Time leaving for work</td>
<td>1. 5:00 a.m. to 5:59 a.m.<br />
2. 6:00 a.m. to 6:59 a.m.<br />
3. 7:00 a.m. to 7:59 a.m.<br />
4. 8:00 a.m. to 8:59 a.m.<br />
5. 9:00 a.m. to 3:59 p.m.<br />
6. 4:00 p.m. to 4:59 a.m.<br />
7. Not applicable</td>
<td>25426 ( 2.6%)<br />
61514 ( 6.3%)<br />
92835 ( 9.5%)<br />
73576 ( 7.5%)<br />
67195 ( 6.9%)<br />
31517 ( 3.2%)<br />
628805 (64.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>108</td>
<td>pwocc<br />
[factor]</td>
<td>Commuting: Commuting vehicle occupancy</td>
<td>1. Not applicable<br />
2. 1 worker<br />
3. 2 workers<br />
4. 3 or more workers</td>
<td>685489 (69.9%)<br />
269375 (27.5%)<br />
21458 ( 2.2%)<br />
4546 ( 0.5%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>109</td>
<td>pwpr<br />
[factor]</td>
<td>POW: Place of work province</td>
<td>1. Not applicable<br />
2. Newfoundland and Labrador<br />
3. Prince Edward Island<br />
4. Nova Scotia<br />
5. New Brunswick<br />
6. Quebec<br />
7. Ontario<br />
8. Manitoba<br />
9. Saskatchewan<br />
10. Alberta<br />
[ 3 others ]</td>
<td>573965 (58.5%)<br />
4624 ( 0.5%)<br />
1631 ( 0.2%)<br />
9675 ( 1.0%)<br />
8127 ( 0.8%)<br />
97684 (10.0%)<br />
153805 (15.7%)<br />
14699 ( 1.5%)<br />
12107 ( 1.2%)<br />
45871 ( 4.7%)<br />
58680 ( 6.0%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>110</td>
<td>regind<br />
[factor]</td>
<td>Indigenous: Registered or Treaty Indian status</td>
<td>1. Registered or Treaty Indi<br />
2. Not a Registered or Treat</td>
<td>22485 ( 2.3%)<br />
958383 (97.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>111</td>
<td>relig<br />
[factor]</td>
<td>Religion: Aggregated responses</td>
<td>1. Buddhist<br />
2. Christian, n.o.s.<br />
3. Anabaptist<br />
4. Anglican<br />
5. Baptist<br />
6. Catholic<br />
7. Christian Orthodox<br />
8. Jehovah’s Witness<br />
9. Latter Day Saints<br />
10. Lutheran<br />
[ 13 others ]</td>
<td>8609 ( 0.9%)<br />
72386 ( 7.4%)<br />
3389 ( 0.3%)<br />
30192 ( 3.1%)<br />
10583 ( 1.1%)<br />
290659 (29.6%)<br />
15971 ( 1.6%)<br />
2932 ( 0.3%)<br />
1797 ( 0.2%)<br />
7999 ( 0.8%)<br />
536351 (54.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>112</td>
<td>repair<br />
[factor]</td>
<td>Dwelling condition</td>
<td>1. Regular maintenance neede<br />
2. Major repairs needed<br />
3. Minor repairs are needed</td>
<td>669377 (68.2%)<br />
61328 ( 6.3%)<br />
250163 (25.5%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>113</td>
<td>room<br />
[factor]</td>
<td>Rooms</td>
<td>1. 1 room<br />
2. 2 rooms<br />
3. 3 rooms<br />
4. 4 rooms<br />
5. 5 rooms<br />
6. 6 rooms<br />
7. 7 rooms<br />
8. 8 rooms<br />
9. 9 rooms<br />
10. 10 rooms<br />
[ 2 others ]</td>
<td>3076 ( 0.3%)<br />
25810 ( 2.6%)<br />
72787 ( 7.4%)<br />
121299 (12.4%)<br />
145835 (14.9%)<br />
139895 (14.3%)<br />
131855 (13.4%)<br />
122159 (12.5%)<br />
80281 ( 8.2%)<br />
68425 ( 7.0%)<br />
69446 ( 7.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>114</td>
<td>retir<br />
[numeric]</td>
<td>Income: Private retirement income</td>
<td>Mean (sd) : 86678285 (33974350)<br />
min &lt; med &lt; max:<br />
1 &lt; 1e+08 &lt; 1e+08<br />
IQR (CV) : 0 (0.4)</td>
<td>1075 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>115</td>
<td>shelco<br />
[numeric]</td>
<td>Shelter cost</td>
<td>Mean (sd) : 1592 (1049.1)<br />
min &lt; med &lt; max:<br />
0 &lt; 1400 &lt; 5383<br />
IQR (CV) : 1354 (0.7)</td>
<td>75 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>116</td>
<td>ssgrad<br />
[factor]</td>
<td>Education: Secondary (high) school diploma or equivalency
certificate</td>
<td>1. No high school diploma or<br />
2. No high school diploma or<br />
3. No high school diploma or<br />
4. With high school diploma<br />
5. With high school diploma<br />
6. With high school diploma<br />
7. With high school diploma<br />
8. With high school diploma<br />
9. With high school diploma<br />
10. With high school diploma<br />
[ 4 others ]</td>
<td>131882 (13.4%)<br />
11214 ( 1.1%)<br />
4132 ( 0.4%)<br />
218657 (22.3%)<br />
59978 ( 6.1%)<br />
150252 (15.3%)<br />
24359 ( 2.5%)<br />
143066 (14.6%)<br />
14090 ( 1.4%)<br />
5238 ( 0.5%)<br />
218000 (22.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>117</td>
<td>subsidy<br />
[factor]</td>
<td>Subsidized housing</td>
<td>1. Subsidized<br />
2. Not subsidized<br />
3. Not applicable<br />
4. Not available</td>
<td>30700 ( 3.1%)<br />
248805 (25.4%)<br />
699640 (71.3%)<br />
1723 ( 0.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>118</td>
<td>sempi<br />
[numeric]</td>
<td>Income: Net self-employment income</td>
<td>Mean (sd) : 91098754 (28418324)<br />
min &lt; med &lt; max:<br />
-50000 &lt; 1e+08 &lt; 1e+08<br />
IQR (CV) : 0 (0.3)</td>
<td>249 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>119</td>
<td>tenur<br />
[factor]</td>
<td>Tenure</td>
<td>1. Owner<br />
2. Not available<br />
3. Renter; or Dwelling provi</td>
<td>699640 (71.3%)<br />
1723 ( 0.2%)<br />
279505 (28.5%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>120</td>
<td>totinc<br />
[numeric]</td>
<td>Income: Total income</td>
<td>Mean (sd) : 19846293 (39790254)<br />
min &lt; med &lt; max:<br />
-50000 &lt; 52000 &lt; 1e+08<br />
IQR (CV) : 93000 (2)</td>
<td>261 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>121</td>
<td>totinc_at<br />
[numeric]</td>
<td>Income: After-tax income</td>
<td>Mean (sd) : 19801029 (39765364)<br />
min &lt; med &lt; max:<br />
-50000 &lt; 46000 &lt; 1e+08<br />
IQR (CV) : 70000 (2)</td>
<td>245 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>122</td>
<td>vismin<br />
[factor]</td>
<td>Visible minority</td>
<td>1. Not a visible minority<br />
2. South Asian<br />
3. Chinese<br />
4. Black<br />
5. Filipino<br />
6. Arab<br />
7. Latin American<br />
8. Southeast Asian<br />
9. West Asian<br />
10. Korean<br />
[ 4 others ]</td>
<td>706307 (72.0%)<br />
66878 ( 6.8%)<br />
44805 ( 4.6%)<br />
39373 ( 4.0%)<br />
25270 ( 2.6%)<br />
18115 ( 1.8%)<br />
14459 ( 1.5%)<br />
9756 ( 1.0%)<br />
9062 ( 0.9%)<br />
5397 ( 0.6%)<br />
41446 ( 4.2%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>123</td>
<td>value<br />
[numeric]</td>
<td>Value (owner estimated)</td>
<td>Mean (sd) : 29137636 (44888776)<br />
min &lt; med &lt; max:<br />
1 &lt; 8e+05 &lt; 1e+08<br />
IQR (CV) : 99599999 (1.5)</td>
<td>282 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>124</td>
<td>wkswrk<br />
[factor]</td>
<td>Labour: Weeks worked during the reference year</td>
<td>1. None - worked in 2021 onl<br />
2. 1 to 9 weeks in 2020<br />
3. 10 to 19 weeks in 2020<br />
4. 20 to 29 weeks in 2020<br />
5. 30 to 39 weeks in 2020<br />
6. 40 to 48 weeks in 2020<br />
7. 49 to 52 weeks in 2020<br />
8. Not applicable<br />
9. Not available</td>
<td>38436 ( 3.9%)<br />
32936 ( 3.4%)<br />
43465 ( 4.4%)<br />
42411 ( 4.3%)<br />
35749 ( 3.6%)<br />
58455 ( 6.0%)<br />
305281 (31.1%)<br />
423528 (43.2%)<br />
607 ( 0.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>125</td>
<td>wrkact<br />
[factor]</td>
<td>Labour: Work activity during the reference year</td>
<td>1. Worked before 2020 or nev<br />
2. Worked in 2021 only<br />
3. Worked 1 to 13 weeks full<br />
4. Worked 1 to 13 weeks part<br />
5. Worked 14 to 26 weeks ful<br />
6. Worked 14 to 26 weeks par<br />
7. Worked 27 to 39 weeks ful<br />
8. Worked 27 to 39 weeks par<br />
9. Worked 40 to 48 weeks ful<br />
10. Worked 40 to 48 weeks par<br />
[ 4 others ]</td>
<td>261488 (26.7%)<br />
38436 ( 3.9%)<br />
28653 ( 2.9%)<br />
29097 ( 3.0%)<br />
30328 ( 3.1%)<br />
25008 ( 2.5%)<br />
27073 ( 2.8%)<br />
14402 ( 1.5%)<br />
44553 ( 4.5%)<br />
13902 ( 1.4%)<br />
467928 (47.7%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>126</td>
<td>wages<br />
[numeric]</td>
<td>Income: Wages, salaries and commissions</td>
<td>Mean (sd) : 46927321 (49846435)<br />
min &lt; med &lt; max:<br />
1 &lt; 130000 &lt; 1e+08<br />
IQR (CV) : 99962999 (1.1)</td>
<td>207 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>127</td>
<td>yrim<br />
[factor]</td>
<td>Immigration: Year of immigration</td>
<td>1. Before 1955<br />
2. 1955 to 1959<br />
3. 1960 to 1964<br />
4. 1965 to 1969<br />
5. 1970 to 1974<br />
6. 1975 to 1979<br />
7. 1980 to 1984<br />
8. 1985 to 1989<br />
9. 1990 to 1994<br />
10. 1995<br />
[ 27 others ]</td>
<td>3386 ( 0.3%)<br />
4891 ( 0.5%)<br />
3842 ( 0.4%)<br />
9460 ( 1.0%)<br />
10257 ( 1.0%)<br />
9399 ( 1.0%)<br />
8944 ( 0.9%)<br />
12262 ( 1.3%)<br />
20728 ( 2.1%)<br />
3825 ( 0.4%)<br />
893874 (91.1%)</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>128</td>
<td>weight<br />
[numeric]</td>
<td>Individuals weighting factor</td>
<td>Mean (sd) : 37 (0)<br />
min &lt; med &lt; max:<br />
36.9 &lt; 37 &lt; 37.1<br />
IQR (CV) : 0 (0)</td>
<td>13 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>129</td>
<td>wt1<br />
[numeric]</td>
<td>Replicate PUMF weight 1</td>
<td>Mean (sd) : 37 (54.8)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>130</td>
<td>wt2<br />
[numeric]</td>
<td>Replicate PUMF weight 2</td>
<td>Mean (sd) : 37 (54.7)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>131</td>
<td>wt3<br />
[numeric]</td>
<td>Replicate PUMF weight 3</td>
<td>Mean (sd) : 37.2 (55.1)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>132</td>
<td>wt4<br />
[numeric]</td>
<td>Replicate PUMF weight 4</td>
<td>Mean (sd) : 36.9 (54.6)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>133</td>
<td>wt5<br />
[numeric]</td>
<td>Replicate PUMF weight 5</td>
<td>Mean (sd) : 37 (54.8)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>134</td>
<td>wt6<br />
[numeric]</td>
<td>Replicate PUMF weight 6</td>
<td>Mean (sd) : 37.1 (54.8)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>135</td>
<td>wt7<br />
[numeric]</td>
<td>Replicate PUMF weight 7</td>
<td>Mean (sd) : 37.1 (54.9)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>136</td>
<td>wt8<br />
[numeric]</td>
<td>Replicate PUMF weight 8</td>
<td>Mean (sd) : 37.1 (54.8)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>137</td>
<td>wt9<br />
[numeric]</td>
<td>Replicate PUMF weight 9</td>
<td>Mean (sd) : 37.1 (54.8)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>138</td>
<td>wt10<br />
[numeric]</td>
<td>Replicate PUMF weight 10</td>
<td>Mean (sd) : 37 (54.7)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>139</td>
<td>wt11<br />
[numeric]</td>
<td>Replicate PUMF weight 11</td>
<td>Mean (sd) : 37 (54.8)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>140</td>
<td>wt12<br />
[numeric]</td>
<td>Replicate PUMF weight 12</td>
<td>Mean (sd) : 37 (54.7)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>141</td>
<td>wt13<br />
[numeric]</td>
<td>Replicate PUMF weight 13</td>
<td>Mean (sd) : 37.1 (54.9)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>142</td>
<td>wt14<br />
[numeric]</td>
<td>Replicate PUMF weight 14</td>
<td>Mean (sd) : 37 (54.7)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>143</td>
<td>wt15<br />
[numeric]</td>
<td>Replicate PUMF weight 15</td>
<td>Mean (sd) : 36.9 (54.6)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
<tr>
<td>144</td>
<td>wt16<br />
[numeric]</td>
<td>Replicate PUMF weight 16</td>
<td>Mean (sd) : 37 (54.8)<br />
min &lt; med &lt; max:<br />
22.8 &lt; 22.9 &lt; 249.4<br />
IQR (CV) : 0 (1.5)</td>
<td>25 distinct values</td>
<td>980868<br />
(100.0%)</td>
<td>0<br />
(0.0%)</td>
</tr>
</tbody>
</table>

``` r
qd_save(dat, here::here("Microdat", "data_fmt_2021_ind.qs2"))
```

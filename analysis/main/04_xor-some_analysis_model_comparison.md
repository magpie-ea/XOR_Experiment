Xor-Some Preregistered Study: Analyses
================
Polina Tsvilodub
8/31/2021

Read data created in preprocessing script:

``` r
d_critical_zScored_wide <- read_csv("./../../data/main/results_prereg_tidy_final_zScored_wide.csv")
d_critical_zScored <- read_csv("./../../data/main/results_prereg_tidy_final_zScored_long.csv")

d_critical_zScored_wide_xor_prior_order <- read_csv("./../../data/main/results_prereg_tidy_final_zScored_wide_xor_priors-order.csv")

d_critical_zScored_wide_xor_prior_type <- read_csv("./../../data/main/results_prereg_tidy_final_zScored_wide_xor_priors-type.csv")

d_critical_zScored_wide_xor <- d_critical_zScored_wide %>% filter(main_type == "xor")
```

## Comparison of prior and relevance effects

Since we observed potential colinearity effects in the main
preregistered analysis for the trigger “or”, we further explore the
effects of this colinearity on the strength of the individual effects in
the regression models.

To do so, we compute individual models omitting one of the factors on
the “or” data, and compute the Bayes Factor (BF) of the model omitting
one of the colinear predictors, relative to the full model.

``` r
# Full model 
# set priors
priors <- set_prior("student_t(1, 0, 0.25)", class = "b")

model_SI <- brm(target ~ prior*competence*relevance +
                   (1 + prior + competence + relevance || submission_id) +
                   (1 | title),
                 data = d_critical_zScored_wide_xor,
                 prior = priors,
                 sample_prior = T,
                 control = list(adapt_delta = 0.95),
                 cores = 4,
                 iter = 20000,
                 save_all_pars = TRUE
                )
```

``` r
summary(model_SI)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * competence * relevance + (1 + prior + competence + relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide_xor (Number of observations: 824) 
    ## Samples: 4 chains, each with iter = 20000; warmup = 10000; thin = 1;
    ##          total post-warmup samples = 40000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 206) 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)      0.05      0.04     0.00     0.14 1.00    13344    15213
    ## sd(prior)          0.24      0.07     0.07     0.37 1.00     5676     4446
    ## sd(competence)     0.12      0.07     0.01     0.27 1.00     6271    11864
    ## sd(relevance)      0.07      0.05     0.00     0.19 1.00     9078    14639
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.26      0.06     0.15     0.38 1.00    12919    18927
    ## 
    ## Population-Level Effects: 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                     -0.17      0.06    -0.28    -0.06 1.00    14885
    ## prior                         -0.06      0.05    -0.16     0.04 1.00    23820
    ## competence                     0.15      0.05     0.06     0.24 1.00    28077
    ## relevance                      0.08      0.04     0.00     0.16 1.00    28582
    ## prior:competence              -0.03      0.04    -0.11     0.06 1.00    37378
    ## prior:relevance                0.00      0.04    -0.08     0.08 1.00    33995
    ## competence:relevance           0.07      0.04    -0.00     0.14 1.00    39436
    ## prior:competence:relevance     0.01      0.04    -0.07     0.09 1.00    37439
    ##                            Tail_ESS
    ## Intercept                     21509
    ## prior                         28930
    ## competence                    29597
    ## relevance                     30578
    ## prior:competence              33033
    ## prior:relevance               31332
    ## competence:relevance          31410
    ## prior:competence:relevance    32813
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.86      0.03     0.80     0.91 1.00    12608    21324
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# model with prior effect only
model_prior_comp <- brm(target ~ prior*competence +
                   (1 + prior + competence || submission_id) +
                   (1 | title),
                 data = d_critical_zScored_wide_xor,
                 prior = priors,
                 sample_prior = T,
                 control = list(adapt_delta = 0.97),
                 cores = 4,
                 iter = 20000,
                 save_all_pars = TRUE
                 )
```

``` r
summary(model_prior_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * competence + (1 + prior + competence || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide_xor (Number of observations: 824) 
    ## Samples: 4 chains, each with iter = 20000; warmup = 10000; thin = 1;
    ##          total post-warmup samples = 40000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 206) 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)      0.05      0.04     0.00     0.14 1.00    19003    19886
    ## sd(prior)          0.24      0.08     0.06     0.37 1.00     7193     7254
    ## sd(competence)     0.12      0.07     0.01     0.27 1.00     8292    16088
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.24      0.06     0.14     0.36 1.00    13276    20195
    ## 
    ## Population-Level Effects: 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept           -0.17      0.06    -0.28    -0.07 1.00    27845    27664
    ## prior               -0.07      0.05    -0.17     0.03 1.00    32953    29639
    ## competence           0.15      0.04     0.06     0.23 1.00    42682    32197
    ## prior:competence    -0.03      0.04    -0.11     0.05 1.00    59059    32277
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.86      0.03     0.81     0.92 1.00    17047    24931
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# model with relevance effect only
model_rel_comp <- brm(target ~ relevance*competence +
                   (1 + relevance + competence || submission_id) +
                   (1 | title),
                 data = d_critical_zScored_wide_xor,
                 prior = priors,
                 sample_prior = T,
                 control = list(adapt_delta = 0.97),
                 cores = 4,
                 iter = 20000,
                 save_all_pars = TRUE
                 )
```

``` r
summary(model_rel_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ relevance * competence + (1 + relevance + competence || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide_xor (Number of observations: 824) 
    ## Samples: 4 chains, each with iter = 20000; warmup = 10000; thin = 1;
    ##          total post-warmup samples = 40000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 206) 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)      0.05      0.04     0.00     0.14 1.00    14900    17515
    ## sd(relevance)      0.07      0.05     0.00     0.19 1.00    10521    16021
    ## sd(competence)     0.13      0.07     0.01     0.27 1.00     5883    11059
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.29      0.06     0.19     0.41 1.00    14037    22722
    ## 
    ## Population-Level Effects: 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept               -0.18      0.06    -0.30    -0.06 1.00    13872
    ## relevance                0.09      0.04     0.00     0.17 1.00    33081
    ## competence               0.13      0.04     0.05     0.22 1.00    29805
    ## relevance:competence     0.07      0.04     0.00     0.15 1.00    40108
    ##                      Tail_ESS
    ## Intercept               22010
    ## relevance               31460
    ## competence              29979
    ## relevance:competence    31209
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.88      0.02     0.83     0.93 1.00    22114    24796
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# model WITHOUT comptenece effect, for reasons of symmetry
model_rel_pri <- brm(target ~ relevance*prior +
                   (1 + relevance + prior || submission_id) +
                   (1 | title),
                 data = d_critical_zScored_wide_xor,
                 prior = priors,
                 sample_prior = T,
                 control = list(adapt_delta = 0.97),
                 cores = 4,
                 iter = 20000,
                 save_all_pars = TRUE
                 )
```

``` r
summary(model_rel_pri)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ relevance * prior + (1 + relevance + prior || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide_xor (Number of observations: 824) 
    ## Samples: 4 chains, each with iter = 20000; warmup = 10000; thin = 1;
    ##          total post-warmup samples = 40000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 206) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.05      0.04     0.00     0.15 1.00    15665    18292
    ## sd(relevance)     0.07      0.05     0.00     0.18 1.00    11683    17647
    ## sd(prior)         0.24      0.08     0.06     0.37 1.00     7560     7735
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.31      0.06     0.21     0.44 1.00    14304    24362
    ## 
    ## Population-Level Effects: 
    ##                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept          -0.16      0.07    -0.29    -0.03 1.00    16922    23013
    ## relevance           0.11      0.04     0.03     0.19 1.00    36156    31012
    ## prior              -0.04      0.05    -0.14     0.06 1.00    34518    31060
    ## relevance:prior     0.00      0.04    -0.08     0.08 1.00    47379    32502
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.87      0.03     0.82     0.92 1.00    18372    25102
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# computes the BFs 
# in favor of the prior only model over full model for XOR
bf_pri_comp_full <- bayes_factor(model_prior_comp, model_SI, silent = TRUE)
# in favor of the relevance only model over the full model for XOR
bf_rel_comp_full <- bayes_factor(model_rel_comp, model_SI, silent = TRUE)
# in favor of the relevance model over the prior model
bf_rel_comp_pri_comp <- bayes_factor(model_rel_comp, model_prior_comp, silent = TRUE)
# and in vice versa in favor of the prior model over the relevance model
bf_pri_comp_rel_comp <- bayes_factor(model_prior_comp, model_rel_comp, silent = TRUE)

# for symmetry, in favor of model without competence over full model
bf_rel_pri_full <- bayes_factor(model_rel_pri, model_SI, silent = TRUE)
bf_rel_pri_rel_comp <- bayes_factor(model_rel_pri, model_rel_comp, silent = TRUE)
bf_rel_pri_pri_comp <- bayes_factor(model_rel_pri, model_prior_comp, silent = TRUE)
```

``` r
# try to make table by extracting numbers here

bf_results <- tribble(
  ~"Model in favor", ~"Model against", ~"BF",
  "Prior*Comp", "Pri*Comp*Rel", round(bf_pri_comp_full[1]$bf, 1),
  "Rel*Comp", "Pri*Comp*Rel", round(bf_rel_comp_full[1]$bf, 1),
  "Rel*Comp", "Pri*Comp", round(bf_rel_comp_pri_comp[1]$bf, 1),
  "Prior*Comp", "Rel*Comp", round(bf_pri_comp_rel_comp[1]$bf, 1),
  "Rel*Pri", "Pri*Comp*Rel", round(bf_rel_pri_full[1]$bf, 1),
  "Rel*Pri", "Rel*Comp", round(bf_rel_pri_rel_comp[1]$bf, 1),
  "Rel*Pri", "Pri*Comp", round(bf_rel_pri_pri_comp[1]$bf, 1)
)

write_csv(bf_results, "../../data/main/bf_main_comparisons.csv")
```

In order to investigate the correlation between relevance and competence
we observed for “or”, we also compute a model without the competence
effect. Since we also observed correlation between the remaining two
factors prior and relevance, perhaps the best way to disentangle the
effects is to fit single-predictor models, and compate them to an
intercept only model.

``` r
# intercept only model
priors_int <- set_prior("student_t(1, 0, 0.25)", class = "Intercept")
model_int <- brm(target ~ 1 +
                   (1 | submission_id) +
                   (1 | title),
                 data = d_critical_zScored_wide_xor,
                 prior = priors_int,
                 sample_prior = T,
                 control = list(adapt_delta = 0.97),
                 cores = 4,
                 iter = 20000,
                 save_all_pars = TRUE
                 )
# single predictor models
model_rel <- brm(target ~ relevance +
                   (1 + relevance || submission_id) +
                   (1 | title),
                 data = d_critical_zScored_wide_xor,
                 prior = priors,
                 sample_prior = T,
                 control = list(adapt_delta = 0.97),
                 cores = 4,
                 iter = 20000,
                 save_all_pars = TRUE
                 )

model_comp <- brm(target ~ competence +
                   (1 + competence || submission_id) +
                   (1 | title),
                 data = d_critical_zScored_wide_xor,
                 prior = priors,
                 sample_prior = T,
                 control = list(adapt_delta = 0.97),
                 cores = 4,
                 iter = 20000,
                 save_all_pars = TRUE
                 )

model_pri <- brm(target ~ prior +
                   (1 + prior || submission_id) +
                   (1 | title),
                 data = d_critical_zScored_wide_xor,
                 prior = priors,
                 sample_prior = T,
                 control = list(adapt_delta = 0.97),
                 cores = 4,
                 iter = 20000,
                 save_all_pars = TRUE
                 )
```

``` r
summary(model_int)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ 1 + (1 | submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide_xor (Number of observations: 824) 
    ## Samples: 4 chains, each with iter = 20000; warmup = 10000; thin = 1;
    ##          total post-warmup samples = 40000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 206) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.05      0.04     0.00     0.14 1.00    20062    20538
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.32      0.06     0.23     0.45 1.00    16743    24094
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -0.17      0.06    -0.30    -0.05 1.00    22158    28190
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.90      0.02     0.85     0.94 1.00    83850    28092
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
summary(model_rel)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ relevance + (1 + relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide_xor (Number of observations: 824) 
    ## Samples: 4 chains, each with iter = 20000; warmup = 10000; thin = 1;
    ##          total post-warmup samples = 40000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 206) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.05      0.04     0.00     0.14 1.00    14283    17139
    ## sd(relevance)     0.07      0.05     0.00     0.18 1.00    11292    16021
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.33      0.06     0.23     0.46 1.00    14471    24902
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -0.17      0.07    -0.30    -0.04 1.00    12471    19922
    ## relevance     0.11      0.04     0.03     0.19 1.00    36305    32408
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.89      0.02     0.85     0.94 1.00    47779    29909
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
summary(model_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ competence + (1 + competence || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide_xor (Number of observations: 824) 
    ## Samples: 4 chains, each with iter = 20000; warmup = 10000; thin = 1;
    ##          total post-warmup samples = 40000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 206) 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)      0.05      0.04     0.00     0.14 1.00    19904    20524
    ## sd(competence)     0.12      0.07     0.01     0.27 1.00     7386    14026
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.28      0.05     0.19     0.40 1.00    16893    26533
    ## 
    ## Population-Level Effects: 
    ##            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     -0.19      0.06    -0.31    -0.07 1.00    21887    27482
    ## competence     0.13      0.04     0.05     0.22 1.00    49686    34080
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.89      0.02     0.84     0.94 1.00    37686    29028
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
summary(model_pri)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior + (1 + prior || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide_xor (Number of observations: 824) 
    ## Samples: 4 chains, each with iter = 20000; warmup = 10000; thin = 1;
    ##          total post-warmup samples = 40000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 206) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.05      0.04     0.00     0.14 1.00    14410    18410
    ## sd(prior)         0.23      0.08     0.05     0.36 1.00     5973     5232
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.30      0.06     0.20     0.43 1.00    14362    23063
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -0.18      0.06    -0.30    -0.05 1.00    14425    22393
    ## prior        -0.05      0.05    -0.14     0.05 1.00    30719    29953
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.87      0.03     0.83     0.92 1.00    17597    23514
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Below, the BFs in favor of the single predictors over the intercept-only
model are computed. If these are large (let’s say \>10), they indicate
that the respective factor is important for explaining the data.

``` r
bf_pri_int <- bayes_factor(model_pri, model_int, silent = TRUE)
bf_comp_int <- bayes_factor(model_comp, model_int, silent = TRUE)
bf_rel_int <- bayes_factor(model_rel, model_int, silent = TRUE)
```

Below, the BFs in favor of the two-predictor models over the respective
single-predictor models are computed. If these are large, they would
indicate that 1) the added model complexity is justified by a better fit
to the data and 2) that the second factor is “necessary” to explain the
data.

``` r
bf_pri_comp_pri <- bayes_factor(model_prior_comp, model_pri, silent = TRUE)
bf_pri_comp_comp <- bayes_factor(model_prior_comp, model_comp, silent = TRUE)

bf_rel_pri_pri <- bayes_factor(model_rel_pri, model_pri, silent = TRUE)
bf_rel_pri_rel <- bayes_factor(model_rel_pri, model_rel, silent = TRUE)

bf_rel_comp_rel <- bayes_factor(model_rel_comp, model_rel, silent = TRUE)
bf_rel_comp_comp <- bayes_factor(model_rel_comp, model_comp, silent = TRUE)
```

``` r
bf_results_explore <- tribble(
  ~"Model in favor", ~"Model against", ~"BF",
  "Prior", "Int", round(bf_pri_int[1]$bf, 1),
  "Comp", "Int", round(bf_comp_int[1]$bf, 1),
  "Rel", "Int", round(bf_rel_int[1]$bf, 1),
  "Prior*Comp", "Prior", round(bf_pri_comp_pri[1]$bf, 1),
  "Prior*Comp", "Comp", round(bf_pri_comp_comp[1]$bf, 1),
  "Rel*Pri", "Prior", round(bf_rel_pri_pri[1]$bf, 1),
  "Rel*Pri", "Rel", round(bf_rel_pri_rel[1]$bf, 1),
  "Rel*Comp", "Rel", round(bf_rel_comp_rel[1]$bf, 1),
  "Rel*Comp", "Comp", round(bf_rel_comp_comp[1]$bf, 1)
)

write_csv(bf_results_explore, "../../data/main/bf_expl_comparisons.csv")

bf_results %>% 
  rbind(., bf_results_explore) %>%
  write_csv(., "../../data/main/bf_full_comparisons.csv")
```

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
priors <- set_prior("student_t(1, 0, 2)", class = "b")

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
    ## sd(Intercept)      0.05      0.04     0.00     0.14 1.00    13987    17256
    ## sd(prior)          0.24      0.07     0.07     0.37 1.00     6301     5278
    ## sd(competence)     0.12      0.07     0.01     0.27 1.00     5581    12007
    ## sd(relevance)      0.07      0.05     0.00     0.19 1.00     9838    15848
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.26      0.06     0.15     0.38 1.00    12232    19237
    ## 
    ## Population-Level Effects: 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                     -0.17      0.06    -0.28    -0.06 1.00    16722
    ## prior                         -0.06      0.05    -0.17     0.04 1.00    25310
    ## competence                     0.16      0.05     0.07     0.24 1.00    30211
    ## relevance                      0.08      0.04     0.00     0.17 1.00    28957
    ## prior:competence              -0.03      0.05    -0.12     0.06 1.00    39174
    ## prior:relevance               -0.00      0.04    -0.09     0.08 1.00    36049
    ## competence:relevance           0.07      0.04    -0.00     0.15 1.00    39000
    ## prior:competence:relevance     0.01      0.04    -0.08     0.09 1.00    40485
    ##                            Tail_ESS
    ## Intercept                     24001
    ## prior                         30760
    ## competence                    29698
    ## relevance                     32261
    ## prior:competence              32432
    ## prior:relevance               30433
    ## competence:relevance          31239
    ## prior:competence:relevance    31669
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.86      0.03     0.80     0.91 1.00    11931    21293
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
    ## sd(Intercept)      0.05      0.04     0.00     0.14 1.00    13608    17183
    ## sd(prior)          0.24      0.08     0.05     0.37 1.00     5211     4385
    ## sd(competence)     0.12      0.07     0.01     0.27 1.00     6123    12340
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.24      0.06     0.14     0.36 1.00    12189    17385
    ## 
    ## Population-Level Effects: 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept           -0.17      0.05    -0.28    -0.07 1.00    17639    22967
    ## prior               -0.08      0.05    -0.17     0.03 1.00    24832    30666
    ## competence           0.16      0.04     0.07     0.24 1.00    29260    30545
    ## prior:competence    -0.03      0.04    -0.12     0.06 1.00    42101    31290
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.86      0.03     0.81     0.92 1.00    12342    22071
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
    ## sd(Intercept)      0.05      0.04     0.00     0.14 1.00    18296    19066
    ## sd(relevance)      0.07      0.05     0.00     0.19 1.00    13683    19201
    ## sd(competence)     0.13      0.07     0.01     0.27 1.00     7933    13893
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.29      0.06     0.19     0.41 1.00    15228    24818
    ## 
    ## Population-Level Effects: 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept               -0.18      0.06    -0.30    -0.06 1.00    21487
    ## relevance                0.09      0.04     0.01     0.17 1.00    44958
    ## competence               0.14      0.05     0.05     0.23 1.00    42314
    ## relevance:competence     0.08      0.04     0.00     0.15 1.00    57675
    ##                      Tail_ESS
    ## Intercept               25818
    ## relevance               32210
    ## competence              31985
    ## relevance:competence    32835
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.88      0.02     0.83     0.93 1.00    28674    27937
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
    ## sd(Intercept)     0.05      0.04     0.00     0.15 1.00    11211    14000
    ## sd(relevance)     0.07      0.05     0.00     0.19 1.00     9204    14488
    ## sd(prior)         0.24      0.08     0.05     0.37 1.00     4669     3293
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.32      0.06     0.21     0.44 1.00    12139    20837
    ## 
    ## Population-Level Effects: 
    ##                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept          -0.16      0.07    -0.29    -0.03 1.00    10296    17737
    ## relevance           0.11      0.04     0.03     0.19 1.00    25828    28032
    ## prior              -0.04      0.05    -0.14     0.06 1.00    26032    29232
    ## relevance:prior    -0.00      0.04    -0.08     0.08 1.00    35128    32420
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.87      0.03     0.82     0.92 1.00    13596    23180
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# computes the BFs 
# in favor of the prior only model over full model for XOR
bayes_factor(model_prior_comp, model_SI, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_prior_comp over model_SI: 4475726.52554

``` r
# in favor of the relevance only model over the full model for XOR
bayes_factor(model_rel_comp, model_SI, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_rel_comp over model_SI: 2705154.95323

``` r
# in favor of the relevance model over the prior model
bayes_factor(model_rel_comp, model_prior_comp, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_rel_comp over model_prior_comp: 0.63502

``` r
# and in vice versa in favor of the prior model over the relevance model
bayes_factor(model_prior_comp, model_rel_comp, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_prior_comp over model_rel_comp: 1.46496

``` r
# for symmetry, in favor of model without competence over full model
bayes_factor(model_rel_pri, model_SI, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_rel_pri over model_SI: 115981.50876

``` r
bayes_factor(model_rel_pri, model_rel_comp, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_rel_pri over model_rel_comp: 0.04326

``` r
bayes_factor(model_rel_pri, model_prior_comp, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_rel_pri over model_prior_comp: 0.02298

In order to investigate the correlation between relevance and competence
we observed for “or”, we also compute a model without the competence
effect. Since we also observed correlation between the remaining two
factors prior and relevance, perhaps the best way to disentangle the
effects is to fit single-predictor models, and compate them to an
intercept only model.

``` r
# intercept only model
priors_int <- set_prior("student_t(3, 0.1, 2.5)", class = "Intercept")
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
    ## sd(Intercept)     0.05      0.04     0.00     0.14 1.00    18758    20989
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.32      0.06     0.23     0.45 1.00    16343    25586
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -0.19      0.07    -0.32    -0.06 1.00    16642    24593
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.90      0.02     0.85     0.94 1.00    76054    29251
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
    ## sd(Intercept)     0.05      0.04     0.00     0.14 1.00    18003    20390
    ## sd(relevance)     0.07      0.05     0.00     0.18 1.00    13615    18596
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.33      0.06     0.23     0.46 1.00    15243    25478
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -0.17      0.07    -0.30    -0.03 1.00    15313    22855
    ## relevance     0.11      0.04     0.03     0.19 1.00    41616    33328
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.89      0.02     0.85     0.94 1.00    60241    28210
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
    ## sd(Intercept)      0.05      0.04     0.00     0.14 1.00    14875    17203
    ## sd(competence)     0.13      0.07     0.01     0.27 1.00     6208    11853
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.28      0.05     0.19     0.40 1.00    15323    24311
    ## 
    ## Population-Level Effects: 
    ##            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     -0.19      0.06    -0.31    -0.07 1.00    13883    21822
    ## competence     0.14      0.04     0.05     0.22 1.00    31507    30186
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.89      0.02     0.84     0.93 1.00    26609    26434
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
    ## sd(Intercept)     0.05      0.04     0.00     0.14 1.00    14412    17314
    ## sd(prior)         0.23      0.08     0.05     0.37 1.00     6019     5861
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.30      0.06     0.20     0.42 1.00    13331    23254
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -0.18      0.06    -0.30    -0.05 1.00    14412    22374
    ## prior        -0.05      0.05    -0.15     0.05 1.00    27719    29712
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.87      0.03     0.83     0.92 1.00    15950    24168
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Below, the BFs in favor of the single predictors over the intercept-only
model are computed. If these are large (let’s say \>10), they indicate
that the respective factor is important for explaining the data.

``` r
bayes_factor(model_pri, model_int, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_pri over model_int: 0.02291

``` r
bayes_factor(model_comp, model_int, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_comp over model_int: 0.16188

``` r
bayes_factor(model_rel, model_int, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_rel over model_int: 0.01806

Below, the BFs in favor of the two-predictor models over the respective
single-predictor models are computed. If these are large, they would
indicate that 1) the added model complexity is justified by a better fit
to the data and 2) that the second factor is “necessary” to explain the
data.

``` r
bayes_factor(model_prior_comp, model_pri, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_prior_comp over model_pri: 0.01319

``` r
bayes_factor(model_prior_comp, model_comp, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_prior_comp over model_comp: 0.00181

``` r
bayes_factor(model_rel_pri, model_pri, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_rel_pri over model_pri: 0.00034

``` r
bayes_factor(model_rel_pri, model_rel, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_rel_pri over model_rel: 0.00038

``` r
bayes_factor(model_rel_comp, model_rel, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_rel_comp over model_rel: 0.01014

``` r
bayes_factor(model_rel_comp, model_comp, silent = TRUE)
```

    ## Estimated Bayes factor in favor of model_rel_comp over model_comp: 0.00114

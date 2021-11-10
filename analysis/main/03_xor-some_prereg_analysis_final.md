Xor-Some Preregistered Study: Analyses
================
Polina Tsvilodub
8/31/2021

Read data created in preprocessing script:

``` r
d_critical_zScored_wide <- read_csv("./../../data/main/results_prereg_tidy_final_zScored_wide.csv")
d_critical_zScored <- read_csv("./../../data/main/results_prereg_tidy_final_zScored_long.csv")
d_critical_zScored_wide_xor_priors <- read_csv("./../../data/main/results_prereg_tidy_final_zScored_wide_xor_priors.csv")
```

## Main analysis

Next, fit the maximal model *across* experiments (i.e., *across SI
trigger types*). For convergence reasons, correlation of random effects
has to be set to 0.

``` r
# Full model 
d_critical_zScored_wide <- d_critical_zScored_wide %>% mutate(
  main_type = as.factor(main_type)
)
# some = 0, xor = 1
contrasts(d_critical_zScored_wide$main_type) 

# set priors
priors <- set_prior("student_t(1, 0, 2)", class = "b")

model_SI <- brm(target ~ prior*competence*relevance* main_type +
                   (1 + prior + competence + relevance + main_type || submission_id) +
                   (1 | title),
                 data = d_critical_zScored_wide,
                 prior = priors,
                 sample_prior = T,
                 control = list(adapt_delta = 0.95),
                 cores = 4,
                 iter = 3000)
```

``` r
summary(model_SI)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * competence * relevance * main_type + (1 + prior + competence + relevance + main_type || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide (Number of observations: 1600) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 200) 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)        0.02      0.01     0.00     0.05 1.00     4580     2453
    ## sd(prior)            0.20      0.04     0.13     0.27 1.00     1803     2059
    ## sd(competence)       0.10      0.05     0.01     0.19 1.01      828     1366
    ## sd(relevance)        0.06      0.04     0.00     0.15 1.00     1187     2365
    ## sd(main_typexor)     0.05      0.04     0.00     0.14 1.00     2095     2085
    ## 
    ## ~title (Number of levels: 64) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.25      0.04     0.18     0.33 1.00     2192     3434
    ## 
    ## Population-Level Effects: 
    ##                                         Estimate Est.Error l-95% CI u-95% CI
    ## Intercept                                   0.12      0.06     0.01     0.23
    ## prior                                      -0.17      0.04    -0.24    -0.10
    ## competence                                  0.22      0.04     0.13     0.30
    ## relevance                                   0.04      0.04    -0.05     0.12
    ## main_typexor                               -0.29      0.08    -0.44    -0.13
    ## prior:competence                            0.06      0.03     0.00     0.12
    ## prior:relevance                            -0.02      0.03    -0.08     0.04
    ## competence:relevance                        0.05      0.04    -0.03     0.12
    ## prior:main_typexor                          0.11      0.06    -0.00     0.21
    ## competence:main_typexor                    -0.07      0.06    -0.19     0.04
    ## relevance:main_typexor                      0.04      0.06    -0.07     0.16
    ## prior:competence:relevance                 -0.01      0.03    -0.06     0.05
    ## prior:competence:main_typexor              -0.09      0.05    -0.19     0.02
    ## prior:relevance:main_typexor                0.02      0.05    -0.08     0.12
    ## competence:relevance:main_typexor           0.02      0.05    -0.08     0.13
    ## prior:competence:relevance:main_typexor     0.01      0.05    -0.09     0.11
    ##                                         Rhat Bulk_ESS Tail_ESS
    ## Intercept                               1.00     2388     3470
    ## prior                                   1.00     4389     4635
    ## competence                              1.00     3484     4200
    ## relevance                               1.00     3245     3859
    ## main_typexor                            1.00     2555     3750
    ## prior:competence                        1.00     4653     4656
    ## prior:relevance                         1.00     4229     4442
    ## competence:relevance                    1.00     3396     4267
    ## prior:main_typexor                      1.00     4277     4219
    ## competence:main_typexor                 1.00     3842     4771
    ## relevance:main_typexor                  1.00     3333     4149
    ## prior:competence:relevance              1.00     4166     3897
    ## prior:competence:main_typexor           1.00     4626     4703
    ## prior:relevance:main_typexor            1.00     4869     4396
    ## competence:relevance:main_typexor       1.00     3891     4137
    ## prior:competence:relevance:main_typexor 1.00     4389     4494
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.83      0.02     0.80     0.87 1.00     3086     4328
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Next, extract posterior contrasts of interest: effects of each predictor
{prior, competence and relevance} for each trigger.

``` r
model_SI %>% spread_draws(b_Intercept, b_prior, b_competence, b_relevance,
                          b_main_typexor, `b_prior:competence`,
                          `b_prior:relevance`, `b_competence:relevance`,
                          `b_prior:main_typexor`, `b_competence:main_typexor`,
                          `b_relevance:main_typexor`, `b_prior:competence:relevance`, 
                          `b_prior:competence:main_typexor`, `b_prior:relevance:main_typexor`,
                          `b_competence:relevance:main_typexor`, 
                          `b_prior:competence:relevance:main_typexor`) %>% 
  mutate(
    prior_xor = b_prior + `b_prior:main_typexor`,
    prior_some = b_prior,
    competence_xor = b_competence + `b_competence:main_typexor`,
    competence_some = b_competence,
    relevance_xor =  b_relevance + `b_relevance:main_typexor`,
    relevance_some = b_relevance
  ) -> model_SI_posteriors

# check P that the effects are positive / negative / no effect present
posterior_hypotheses <- model_SI_posteriors %>% 
  select(prior_xor, prior_some, 
         competence_xor, competence_some,
         relevance_xor, relevance_some) %>%
  gather(key, val) %>%
  group_by(key) %>% mutate(positive = mean(val > 0.05),
                           negative = mean(val < -0.05),
                           no = mean(val %>% between(-0.05, 0.05))) %>%
  summarise(positive_eff = mean(positive),
            negative_eff = mean(negative),
            no_eff = mean(no))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
posterior_hypotheses
```

    ## # A tibble: 6 x 4
    ##   key             positive_eff negative_eff no_eff
    ##   <chr>                  <dbl>        <dbl>  <dbl>
    ## 1 competence_some       1          0        0     
    ## 2 competence_xor        0.985      0        0.0147
    ## 3 prior_some            0          1.00     0.0005
    ## 4 prior_xor             0.0105     0.614    0.375 
    ## 5 relevance_some        0.403      0.0192   0.578 
    ## 6 relevance_xor         0.782      0.000833 0.217

#### Perform BF analysis

We want to address a conjunctive hypothesis, one triplet for each
trigger word, namely:

1.  the slope of ‘competence’ is positive
2.  the slope of ‘prior’ is negative
3.  the slope of ‘relevance’ is positive

We judge there to be strong evidence for positivity of a slope variable
\(\beta_X\), if the posterior probability \(P(\beta_X > \delta \mid D)\)
is at least \(.95\), for \(\delta = 0.05\) the parameter that defines
our ‘region of practical equivalence’. A posterior odds ratio of at
least \(\frac{.95}{.05} = 19\) corresponds to a Bayes factor of at least
19 when prior odds are 1.

The binary test of conformity with the theoretical predictions therefore
is:

``` r
test_conjunction_of_all_hypotheses <-  function(posterior_hypotheses) {
  posterior_hypotheses %>% 
    mutate(hypothesis_true = case_when(
      key == 'competence_some' ~ positive_eff > 0.95,
      key == 'competence_xor'  ~ positive_eff > 0.95,
      key == 'prior_some' ~ negative_eff > 0.95,
      key == 'prior_xor'  ~ negative_eff > 0.95, 
      key == 'relevance_some' ~ positive_eff > 0.95,
      key == 'relevance_xor'  ~ positive_eff > 0.95
    )) %>% 
    pull(hypothesis_true) %>% all()
}
# applied to the pilot data
test_conjunction_of_all_hypotheses(posterior_hypotheses)
```

    ## [1] FALSE

### Plot posterior distributions

``` r
model_SI_posteriors %>% select(prior_xor, prior_some,
                               competence_xor, competence_some,
                               relevance_xor, relevance_some) %>%
  pivot_longer(cols = everything(), names_to = "effect", values_to = "value") -> model_SI_posteriors_long


model_SI_posteriors_long %>%
  ggplot(.) + # 
  stat_halfeye(aes(y = effect, x = value, 
                   fill = stat(case_when(x %>% between(-0.05, 0.05) ~ "no effect", x < -0.05 ~ "negative effect", x > 0.05 ~ "positive effect"))
                   ), 
               alpha = .7) +
  geom_vline(xintercept = c(-.05, .05), linetype = "dashed") +
  scale_fill_manual(name = "Direction of effect", values = c("coral", "yellow3", "mediumseagreen")) +
  ggtitle("Posterior effect distributions. Points indicate posterior means\nThick lines indicate 66% CrIs, thin lines 95% CrIs.")
```

![](03_xor-some_prereg_analysis_final_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Exploratory analyses

Model with binary predictors:

``` r
d_critical_zScored %>% 
  select(submission_id, title, main_type, block_extended, response_centered, prior_class, class_condition) %>% 
  unique() %>% 
  pivot_wider(
    names_from = class_condition, 
    values_from = prior_class
  )  %>%
  mutate(relevance = factor(relevance, levels = c(1, 0)),
         competence = factor(competence, levels = c(1, 0)),
         prior = factor(prior, levels = c(1, 0)),
         main_type = as.factor(main_type)) %>% 
  filter(block_extended == "target") -> d_critical_wide_cat_zScored
# sum code predictors
# low prior: -1, high prior:: 1 
contrasts(d_critical_wide_cat_zScored$prior) <- contr.sum(2)
# low comp : -1, high comp : 1
contrasts(d_critical_wide_cat_zScored$competence) <- contr.sum(2)
# low rel: -1, high rel: 1
contrasts(d_critical_wide_cat_zScored$relevance) <- contr.sum(2)
# leave main type dummy coded
contrasts(d_critical_wide_cat_zScored$main_type) 

# xor, maximal model with interactions and maximal REs
model_xor_cat_zScored <- brm(
  response_centered ~ prior*competence*relevance*main_type + 
    (1 + prior + competence + relevance + main_type || submission_id) +
    (1 | title),
  data = d_critical_wide_cat_zScored,
  prior = priors,
  sample_prior = T,
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_cat_zScored)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: response_centered ~ prior * competence * relevance * main_type + (1 + prior + competence + relevance + main_type || submission_id) + (1 | title) 
    ##    Data: d_critical_wide_cat_zScored (Number of observations: 1600) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 200) 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)        0.02      0.01     0.00     0.05 1.00     4771     2851
    ## sd(prior1)           0.20      0.04     0.13     0.27 1.00     1851     2277
    ## sd(competence1)      0.12      0.05     0.02     0.20 1.00     1284     1773
    ## sd(relevance1)       0.17      0.04     0.08     0.24 1.00     1520     1445
    ## sd(main_typexor)     0.05      0.04     0.00     0.14 1.00     2567     3123
    ## 
    ## ~title (Number of levels: 64) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.17      0.04     0.10     0.25 1.00     1684     2503
    ## 
    ## Population-Level Effects: 
    ##                                            Estimate Est.Error l-95% CI u-95% CI
    ## Intercept                                      0.18      0.04     0.10     0.27
    ## prior1                                        -0.18      0.05    -0.27    -0.09
    ## competence1                                    0.30      0.04     0.21     0.39
    ## relevance1                                     0.05      0.05    -0.03     0.14
    ## main_typexor                                  -0.37      0.06    -0.49    -0.24
    ## prior1:competence1                             0.09      0.04     0.00     0.17
    ## prior1:relevance1                              0.01      0.04    -0.08     0.09
    ## competence1:relevance1                         0.01      0.04    -0.08     0.09
    ## prior1:main_typexor                            0.03      0.06    -0.10     0.15
    ## competence1:main_typexor                      -0.17      0.06    -0.29    -0.05
    ## relevance1:main_typexor                       -0.12      0.06    -0.24     0.01
    ## prior1:competence1:relevance1                  0.02      0.04    -0.07     0.10
    ## prior1:competence1:main_typexor               -0.09      0.06    -0.21     0.04
    ## prior1:relevance1:main_typexor                 0.06      0.06    -0.06     0.18
    ## competence1:relevance1:main_typexor            0.09      0.06    -0.03     0.21
    ## prior1:competence1:relevance1:main_typexor    -0.14      0.06    -0.26    -0.02
    ##                                            Rhat Bulk_ESS Tail_ESS
    ## Intercept                                  1.00     5215     4713
    ## prior1                                     1.00     4339     3694
    ## competence1                                1.00     3755     4079
    ## relevance1                                 1.00     4655     4538
    ## main_typexor                               1.00     4806     4339
    ## prior1:competence1                         1.00     4446     3980
    ## prior1:relevance1                          1.00     4628     3877
    ## competence1:relevance1                     1.00     4621     4212
    ## prior1:main_typexor                        1.00     4518     4348
    ## competence1:main_typexor                   1.00     4082     4042
    ## relevance1:main_typexor                    1.00     4657     3821
    ## prior1:competence1:relevance1              1.00     4652     4345
    ## prior1:competence1:main_typexor            1.00     4364     3840
    ## prior1:relevance1:main_typexor             1.00     4772     4170
    ## competence1:relevance1:main_typexor        1.00     4209     3860
    ## prior1:competence1:relevance1:main_typexor 1.00     4454     3999
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.82      0.02     0.79     0.86 1.00     1979     3419
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Now extract posterior contrasts of interest from categorical model:
effects of each predictor {prior, competence and relevance} for each
trigger.

``` r
model_xor_cat_zScored %>% spread_draws(b_Intercept, b_prior1, b_competence1, b_relevance1,
                          b_main_typexor, `b_prior1:competence1`,
                          `b_prior1:relevance1`, `b_competence1:relevance1`,
                          `b_prior1:main_typexor`, `b_competence1:main_typexor`,
                          `b_relevance1:main_typexor`, `b_prior1:competence1:relevance1`, 
                          `b_prior1:competence1:main_typexor`, `b_prior1:relevance1:main_typexor`,
                          `b_competence1:relevance1:main_typexor`, 
                          `b_prior1:competence1:relevance1:main_typexor`) %>% 
  mutate(
    prior_xor = 2 * b_prior1 + 2* `b_prior1:main_typexor`,
    prior_some = 2 * b_prior1,
    competence_xor = 2 * b_competence1 + 2* `b_competence1:main_typexor`,
    competence_some = 2 * b_competence1,
    relevance_xor =  2 * b_relevance1 + 2* `b_relevance1:main_typexor`,
    relevance_some = 2* b_relevance1
  ) -> model_xor_cat_zScored_posteriors

# check P that the effects are positive / negative / no effect present
posterior_hypotheses_cat <- model_xor_cat_zScored_posteriors %>% 
  select(prior_xor, prior_some, 
         competence_xor, competence_some,
         relevance_xor, relevance_some) %>%
  gather(key, val) %>%
  group_by(key) %>% mutate(positive = mean(val > 0.05),
                           negative = mean(val < -0.05),
                           no = mean(val %>% between(-0.05, 0.05))) %>%
  summarise(positive_eff = mean(positive),
            negative_eff = mean(negative),
            no_eff = mean(no))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
posterior_hypotheses_cat
```

    ## # A tibble: 6 x 4
    ##   key             positive_eff negative_eff  no_eff
    ##   <chr>                  <dbl>        <dbl>   <dbl>
    ## 1 competence_some     1            0        0      
    ## 2 competence_xor      0.992        0.000667 0.00683
    ## 3 prior_some          0            1.00     0.0005 
    ## 4 prior_xor           0.000167     0.997    0.003  
    ## 5 relevance_some      0.734        0.0402   0.225  
    ## 6 relevance_xor       0.0277       0.795    0.178

``` r
test_conjunction_of_all_hypotheses(posterior_hypotheses_cat)
```

    ## [1] FALSE

``` r
model_xor_cat_zScored_posteriors %>% select(prior_xor, prior_some,
                               competence_xor, competence_some,
                               relevance_xor, relevance_some) %>%
  pivot_longer(cols = everything(), names_to = "effect", values_to = "value") -> model_xor_cat_posteriors_long


model_xor_cat_posteriors_long %>%
  ggplot(.) + # 
  stat_halfeye(aes(y = effect, x = value, 
                   fill = stat(case_when(x %>% between(-0.05, 0.05) ~ "no effect", x < -0.05 ~ "negative effect", x > 0.05 ~ "positive effect"))
                   ), 
               alpha = .7) +
  geom_vline(xintercept = c(-.05, .05), linetype = "dashed") +
  scale_fill_manual(name = "Direction of effect", values = c("coral", "yellow3", "mediumseagreen")) +
  ggtitle("Posterior effect distributions. Points indicate posterior means\nThick lines indicate 66% CrIs, thin lines 95% CrIs.")
```

![](03_xor-some_prereg_analysis_final_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Model with aggregated by-story ratings:

``` r
d_critical_zScored_wide_byItem <- d_critical_zScored_wide %>% ungroup() %>%
  group_by(title, main_type) %>%
  summarize(mean_response = mean(target),
            mean_prior = mean(prior),
            mean_relevance = mean(relevance),
            mean_competence = mean(competence))


model_means <- brm(mean_response ~ mean_prior * mean_relevance * mean_competence * main_type,
                       data = d_critical_zScored_wide_byItem,
                       prior = priors,
                       sample_prior = T,
                       family = "gaussian",
                       cores = 4,
                       iter = 3000
                       )
```

``` r
summary(model_means)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: mean_response ~ mean_prior * mean_relevance * mean_competence * main_type 
    ##    Data: d_critical_zScored_wide_byItem (Number of observations: 64) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Population-Level Effects: 
    ##                                                        Estimate Est.Error
    ## Intercept                                                  0.06      0.06
    ## mean_prior                                                -0.31      0.10
    ## mean_relevance                                             0.08      0.10
    ## mean_competence                                            0.43      0.09
    ## main_typexor                                              -0.21      0.08
    ## mean_prior:mean_relevance                                  0.05      0.13
    ## mean_prior:mean_competence                                 0.15      0.13
    ## mean_relevance:mean_competence                            -0.03      0.13
    ## mean_prior:main_typexor                                   -0.06      0.15
    ## mean_relevance:main_typexor                               -0.17      0.13
    ## mean_competence:main_typexor                              -0.08      0.12
    ## mean_prior:mean_relevance:mean_competence                 -0.03      0.17
    ## mean_prior:mean_relevance:main_typexor                    -0.04      0.20
    ## mean_prior:mean_competence:main_typexor                   -0.23      0.20
    ## mean_relevance:mean_competence:main_typexor                0.20      0.18
    ## mean_prior:mean_relevance:mean_competence:main_typexor    -0.07      0.31
    ##                                                        l-95% CI u-95% CI Rhat
    ## Intercept                                                 -0.06     0.18 1.00
    ## mean_prior                                                -0.49    -0.12 1.00
    ## mean_relevance                                            -0.13     0.29 1.00
    ## mean_competence                                            0.25     0.60 1.00
    ## main_typexor                                              -0.37    -0.04 1.00
    ## mean_prior:mean_relevance                                 -0.21     0.31 1.00
    ## mean_prior:mean_competence                                -0.11     0.40 1.00
    ## mean_relevance:mean_competence                            -0.28     0.23 1.00
    ## mean_prior:main_typexor                                   -0.35     0.23 1.00
    ## mean_relevance:main_typexor                               -0.42     0.08 1.00
    ## mean_competence:main_typexor                              -0.31     0.15 1.00
    ## mean_prior:mean_relevance:mean_competence                 -0.37     0.32 1.00
    ## mean_prior:mean_relevance:main_typexor                    -0.42     0.33 1.00
    ## mean_prior:mean_competence:main_typexor                   -0.63     0.17 1.00
    ## mean_relevance:mean_competence:main_typexor               -0.15     0.55 1.00
    ## mean_prior:mean_relevance:mean_competence:main_typexor    -0.69     0.53 1.00
    ##                                                        Bulk_ESS Tail_ESS
    ## Intercept                                                  5644     4226
    ## mean_prior                                                 4732     4584
    ## mean_relevance                                             3188     3815
    ## mean_competence                                            3398     4316
    ## main_typexor                                               6966     4576
    ## mean_prior:mean_relevance                                  3439     4327
    ## mean_prior:mean_competence                                 3396     4264
    ## mean_relevance:mean_competence                             3326     3896
    ## mean_prior:main_typexor                                    5195     4378
    ## mean_relevance:main_typexor                                3573     3889
    ## mean_competence:main_typexor                               4199     4794
    ## mean_prior:mean_relevance:mean_competence                  3771     4061
    ## mean_prior:mean_relevance:main_typexor                     4441     4607
    ## mean_prior:mean_competence:main_typexor                    4482     4449
    ## mean_relevance:mean_competence:main_typexor                4243     4190
    ## mean_prior:mean_relevance:mean_competence:main_typexor     5104     4816
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.27      0.03     0.22     0.33 1.00     4732     4276
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Extract posterior contrasts of interest from by-story mean based model:
effects of each predictor {prior, competence and relevance} for each
trigger.

``` r
model_means %>% spread_draws(b_Intercept, b_mean_prior, b_mean_competence, b_mean_relevance,
                          b_main_typexor, `b_mean_prior:mean_competence`,
                          `b_mean_prior:mean_relevance`, `b_mean_relevance:mean_competence`,
                          `b_mean_prior:main_typexor`, `b_mean_competence:main_typexor`,
                          `b_mean_relevance:main_typexor`,
                          `b_mean_prior:mean_relevance:mean_competence`, 
                          `b_mean_prior:mean_competence:main_typexor`,
                          `b_mean_prior:mean_relevance:main_typexor`,
                          `b_mean_relevance:mean_competence:main_typexor`, 
                          `b_mean_prior:mean_relevance:mean_competence:main_typexor`) %>% 
  mutate(
    prior_xor = b_mean_prior + `b_mean_prior:main_typexor`,
    prior_some = b_mean_prior,
    competence_xor = b_mean_competence + `b_mean_competence:main_typexor`,
    competence_some = b_mean_competence,
    relevance_xor =  b_mean_relevance + `b_mean_relevance:main_typexor`,
    relevance_some = b_mean_relevance
  ) -> model_means_posteriors

# check P that the effects are positive / negative / no effect present
posterior_hypotheses_means <- model_means_posteriors %>% 
  select(prior_xor, prior_some, 
         competence_xor, competence_some,
         relevance_xor, relevance_some) %>%
  gather(key, val) %>%
  group_by(key) %>% mutate(positive = mean(val > 0.05),
                           negative = mean(val < -0.05),
                           no = mean(val %>% between(-0.05, 0.05))) %>%
  summarise(positive_eff = mean(positive),
            negative_eff = mean(negative),
            no_eff = mean(no))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
posterior_hypotheses_means
```

    ## # A tibble: 6 x 4
    ##   key             positive_eff negative_eff   no_eff
    ##   <chr>                  <dbl>        <dbl>    <dbl>
    ## 1 competence_some     1               0     0       
    ## 2 competence_xor      1.00            0     0.000167
    ## 3 prior_some          0.0005          0.996 0.004   
    ## 4 prior_xor           0.000167        0.997 0.00317 
    ## 5 relevance_some      0.607           0.104 0.290   
    ## 6 relevance_xor       0.0303          0.701 0.269

``` r
test_conjunction_of_all_hypotheses(posterior_hypotheses_means)
```

    ## [1] FALSE

``` r
model_means_posteriors %>% select(prior_xor, prior_some,
                               competence_xor, competence_some,
                               relevance_xor, relevance_some) %>%
  pivot_longer(cols = everything(), names_to = "effect", values_to = "value") -> model_mean_posteriors_long


model_mean_posteriors_long %>%
  ggplot(.) + # 
  stat_halfeye(aes(y = effect, x = value, 
                   fill = stat(case_when(x %>% between(-0.05, 0.05) ~ "no effect", x < -0.05 ~ "negative effect", x > 0.05 ~ "positive effect"))
                   ), 
               alpha = .7) +
  geom_vline(xintercept = c(-.05, .05), linetype = "dashed") +
  scale_fill_manual(name = "Direction of effect", values = c("coral", "yellow3", "mediumseagreen")) +
  ggtitle("Posterior effect distributions. Points indicate posterior means\nThick lines indicate 66% CrIs, thin lines 95% CrIs.")
```

![](03_xor-some_prereg_analysis_final_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

A non-preregistered, but nevertheless possibly interesting exploratory
analysis including the two individual prior ratings of the conditionals
in the xor condition as predictors (as opposed to the mean of the two
used in the preceding models):

``` r
# set priors
priors <- set_prior("student_t(1, 0, 2)", class = "b")

model_xor_priors <- brm(target ~ prior_0*prior_1*competence*relevance +
                   (1 + prior_0 + prior_1 + competence + relevance || submission_id) +
                   (1 | title),
                 data = d_critical_zScored_wide_xor_priors,
                 prior = priors,
                 sample_prior = T,
                 control = list(adapt_delta = 0.95),
                 cores = 4,
                 iter = 3000)
```

``` r
summary(model_xor_priors)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior_0 * prior_1 * competence * relevance + (1 + prior_0 + prior_1 + competence + relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide_xor_priors (Number of observations: 559) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 192) 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)      0.07      0.05     0.00     0.20 1.00     2313     2572
    ## sd(prior_0)        0.16      0.09     0.01     0.34 1.00      958     2010
    ## sd(prior_1)        0.18      0.10     0.01     0.36 1.00      783     1631
    ## sd(competence)     0.11      0.08     0.00     0.29 1.00     1567     2586
    ## sd(relevance)      0.09      0.06     0.00     0.23 1.00     1889     2844
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.23      0.08     0.07     0.39 1.00     1238     1392
    ## 
    ## Population-Level Effects: 
    ##                                      Estimate Est.Error l-95% CI u-95% CI Rhat
    ## Intercept                               -0.17      0.07    -0.31    -0.03 1.00
    ## prior_0                                 -0.08      0.09    -0.25     0.09 1.00
    ## prior_1                                  0.04      0.09    -0.14     0.21 1.00
    ## competence                               0.18      0.07     0.04     0.30 1.00
    ## relevance                                0.09      0.06    -0.04     0.22 1.00
    ## prior_0:prior_1                         -0.03      0.06    -0.14     0.08 1.00
    ## prior_0:competence                      -0.08      0.10    -0.28     0.11 1.00
    ## prior_1:competence                       0.04      0.10    -0.15     0.24 1.00
    ## prior_0:relevance                        0.08      0.09    -0.10     0.25 1.00
    ## prior_1:relevance                       -0.06      0.09    -0.23     0.11 1.00
    ## competence:relevance                     0.05      0.07    -0.08     0.19 1.00
    ## prior_0:prior_1:competence              -0.04      0.06    -0.16     0.07 1.00
    ## prior_0:prior_1:relevance                0.02      0.06    -0.09     0.13 1.00
    ## prior_0:competence:relevance             0.08      0.10    -0.12     0.28 1.00
    ## prior_1:competence:relevance            -0.07      0.10    -0.27     0.14 1.00
    ## prior_0:prior_1:competence:relevance     0.01      0.06    -0.10     0.12 1.00
    ##                                      Bulk_ESS Tail_ESS
    ## Intercept                                4874     4514
    ## prior_0                                  5923     4703
    ## prior_1                                  4325     4305
    ## competence                               5657     5236
    ## relevance                                6045     4687
    ## prior_0:prior_1                          7852     4631
    ## prior_0:competence                       5071     4316
    ## prior_1:competence                       4995     3791
    ## prior_0:relevance                        6074     4916
    ## prior_1:relevance                        5929     4559
    ## competence:relevance                     5428     4945
    ## prior_0:prior_1:competence               6298     4952
    ## prior_0:prior_1:relevance                5262     4931
    ## prior_0:competence:relevance             5156     4541
    ## prior_1:competence:relevance             4981     4967
    ## prior_0:prior_1:competence:relevance     5135     4698
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.88      0.04     0.82     0.96 1.00     1961     3318
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Check the intervals of intervals, computing them separately using only
one prior estimate, to see if we get different results:

``` r
model_xor_priors %>% spread_draws(b_Intercept, b_prior_0, b_prior_1, b_competence, b_relevance) %>% 
  mutate(
    prior0_xor = b_prior_0,
    prior1_xor = b_prior_1,
    competence_xor = b_competence,
    relevance_xor =  b_relevance
  ) -> model_xor_priors_posteriors

# check P that the effects are positive / negative / no effect present
posterior_hypotheses_xor_priors <- model_xor_priors_posteriors %>% 
  select(prior0_xor, prior1_xor, 
         competence_xor, relevance_xor) %>%
  gather(key, val) %>%
  group_by(key) %>% mutate(positive = mean(val > 0.05),
                           negative = mean(val < -0.05),
                           no = mean(val %>% between(-0.05, 0.05))) %>%
  summarise(positive_eff = mean(positive),
            negative_eff = mean(negative),
            no_eff = mean(no))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
posterior_hypotheses_xor_priors
```

    ## # A tibble: 4 x 4
    ##   key            positive_eff negative_eff no_eff
    ##   <chr>                 <dbl>        <dbl>  <dbl>
    ## 1 competence_xor       0.967      0.000333 0.0325
    ## 2 prior0_xor           0.0757     0.614    0.311 
    ## 3 prior1_xor           0.433      0.158    0.410 
    ## 4 relevance_xor        0.732      0.0152   0.253

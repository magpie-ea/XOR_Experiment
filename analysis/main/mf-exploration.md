Exploring influence of REs on models fits
================
Michael Franke
12/05/2022

# Data

We use the data with the “new” z-scoring for the prior data for ‘or’.

``` r
d_critical_clean <- read_csv("../../data/main/results_prereg_tidy_noDuplicates_zScored_meansFirst_wide.csv") %>% 
  mutate(main_type = factor(main_type))
head(d_critical_clean)
```

    ## # A tibble: 6 × 7
    ##   submission_id title               main_type relevance competence  prior target
    ##           <dbl> <chr>               <fct>         <dbl>      <dbl>  <dbl>  <dbl>
    ## 1          2620 Grandma             xor          0.0789    -1.02    0.818      0
    ## 2          2620 Champagne reception some        -1.16      -1.02   -0.778      0
    ## 3          2620 Final exam          some         0.991     -0.807  -0.778      0
    ## 4          2620 Alissa's paint      xor         -1.35       0.0567  1.78       0
    ## 5          2620 Harvard admission … some         0.991     -0.613  -0.778      0
    ## 6          2620 Mrs. Gibbs' worry   xor          0.336      1.14   -0.299      0

We also want additional information about the vignettes, i.e., their
intuitive pre-classifications.

``` r
vignettes <- read_csv("./../../data/main/results_73_xor-some-Prolific-main_N275_anonym.csv") %>% 
  select(title, main_type, relevance, competence, prior) %>% 
  unique() %>% 
  filter(! is.na(main_type)) %>% 
  rename(relCat = relevance,
         compCat = competence,
         priCat = prior)
head(vignettes)
```

    ## # A tibble: 6 × 5
    ##   title                   main_type relCat compCat priCat
    ##   <chr>                   <chr>      <dbl>   <dbl>  <dbl>
    ## 1 Grandma                 xor            0       1      1
    ## 2 Champagne reception     some           0       0      0
    ## 3 Final exam              some           1       0      0
    ## 4 Alissa's paint          xor            0       0      1
    ## 5 Harvard admission exams some           1       0      1
    ## 6 Mrs. Gibbs' worry       xor            1       1      1

# Functions for fitting models

We are interested in comparing the following models, specified here in
terms of `brms` formulas:

``` r
## full model with REs for subjects and items
f_full <- formula(
  target ~ prior * competence * relevance * main_type +
    (1 + prior + competence + relevance + main_type || submission_id) +
    (1 | title)
)

## model with RE for 'title' only (by-item random intercepts)
f_RETitle <- formula(
  target ~ prior * competence * relevance * main_type +
    (1 | title)
)

## RE for 'submject_id' only (by-subj random intercepts)
f_RESubj <- formula(
  target ~ prior * competence * relevance * main_type +
    (1 + prior + competence + relevance + main_type || submission_id)
)

## model without REs; fixed effects only 
f_noREs <- formula(
  target ~ prior * competence * relevance * main_type
)
```

Here’s a function for running the models:

``` r
run_model <- function(form, data){

  ## some = 0, xor = 1
  contrasts(data$main_type) 

  ## set priors
  priors <- set_prior("student_t(1, 0, 2)", class = "b")

  ## run and return model
  brm(
    formula      = form,
    data         = data,
    prior        = priors,
    sample_prior = T,
    control      = list(adapt_delta = 0.95),
    cores        = 4,
    iter         = 3000
  )
  
} 
```

And here is a function for returning the relevant result summary from a
fitted model:

``` r
## function to get prediction for a model fit
get_predictions <- function(model_SI) {
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
              no_eff = mean(no),
              negative_eff = mean(negative))
  posterior_hypotheses
}
```

# Fitting and inspecting the relevant models

Model fits:

``` r
model_SI         <- run_model(f_full,d_critical_clean) 
model_SI_RETitle <- run_model(f_RETitle,d_critical_clean)
model_SI_RESubj  <- run_model(f_RESubj,d_critical_clean)
model_SI_noREs   <- run_model(f_noREs,d_critical_clean)
```

We can now inspect the results for each model, and find that **including
by-item random intercepts determines whether we see a main effect of
prior for the ‘or’ items, irrespective of whether we include or omit the
by-subject random effects**. We may conclude from this that it is really
the by-item random effects that “explain away” the main effect of prior
on ‘or’ items.

``` r
## We compare the results for three model which differ only in the random effects
## they take into account. We care about a potential main effect of 'prior' in the
## 'or' condition.

## (i)  full REs (subj. & item)  -> no effect 
get_predictions(model_SI)
```

    ## # A tibble: 6 × 4
    ##   key             positive_eff   no_eff negative_eff
    ##   <chr>                  <dbl>    <dbl>        <dbl>
    ## 1 competence_some      1       0            0       
    ## 2 competence_xor       0.995   0.00517      0       
    ## 3 prior_some           0       0.000333     1.00    
    ## 4 prior_xor            0.00467 0.337        0.658   
    ## 5 relevance_some       0.404   0.584        0.0125  
    ## 6 relevance_xor        0.765   0.234        0.000667

``` r
## (ii) no subj., just item REs  -> no effect 
get_predictions(model_SI_RETitle)
```

    ## # A tibble: 6 × 4
    ##   key             positive_eff no_eff negative_eff
    ##   <chr>                  <dbl>  <dbl>        <dbl>
    ## 1 competence_some       1      0           0      
    ## 2 competence_xor        0.989  0.0113      0      
    ## 3 prior_some            0      0           1      
    ## 4 prior_xor             0.0025 0.328       0.670  
    ## 5 relevance_some        0.517  0.475       0.00833
    ## 6 relevance_xor         0.812  0.188       0.0005

``` r
## (iii) just subj., no item REs -> strong effect 
get_predictions(model_SI_RESubj)
```

    ## # A tibble: 6 × 4
    ##   key             positive_eff  no_eff negative_eff
    ##   <chr>                  <dbl>   <dbl>        <dbl>
    ## 1 competence_some        1     0            0      
    ## 2 competence_xor         1     0            0      
    ## 3 prior_some             0     0            1      
    ## 4 prior_xor              0     0.00833      0.992  
    ## 5 relevance_some         0.497 0.499        0.00383
    ## 6 relevance_xor          0.268 0.723        0.00883

``` r
## (iv) no REs                   -> strong effect 
get_predictions(model_SI_noREs)
```

    ## # A tibble: 6 × 4
    ##   key             positive_eff  no_eff negative_eff
    ##   <chr>                  <dbl>   <dbl>        <dbl>
    ## 1 competence_some        1     0             0     
    ## 2 competence_xor         1     0             0     
    ## 3 prior_some             0     0             1     
    ## 4 prior_xor              0     0.00483       0.995 
    ## 5 relevance_some         0.555 0.444         0.0005
    ## 6 relevance_xor          0.335 0.659         0.0055

# Probing which vignettes are most influential in producing/hiding the effect

Which vignettes does the model estimate large random by-item effect for?
Here’s a function that prints the top 6 most extreme estimates for the
random effects in question.

``` r
extract_REs <- function(model_fit) {
  randEff <- brms::ranef(model_fit)$title %>% as.data.frame() %>% 
    as_tibble(rownames = "title")
  vignettes  %>% 
    full_join(randEff, by= 'title')
}
```

Applying this function to both model with by-item random effects, we see
that these top six do not differ much between whether we include the
by-subject random effects or not.

``` r
## full model
extract_REs(model_SI) %>% arrange(Estimate.Intercept)
```

    ## # A tibble: 64 × 9
    ##    title       main_type relCat compCat priCat Estimate.Interc… Est.Error.Inter…
    ##    <chr>       <chr>      <dbl>   <dbl>  <dbl>            <dbl>            <dbl>
    ##  1 Gourmet de… some           0       0      1           -0.583            0.146
    ##  2 Dinosaurs   some           0       0      1           -0.455            0.145
    ##  3 Carl's par… xor            1       0      0           -0.362            0.149
    ##  4 Greg's mov… xor            0       0      1           -0.355            0.168
    ##  5 Semester a… xor            1       1      1           -0.350            0.150
    ##  6 College     xor            1       0      0           -0.345            0.144
    ##  7 Violinist   some           1       0      1           -0.333            0.151
    ##  8 Crime       xor            1       0      1           -0.279            0.143
    ##  9 Harvard ad… some           1       0      1           -0.258            0.147
    ## 10 Swiss watc… some           1       0      1           -0.252            0.151
    ## # … with 54 more rows, and 2 more variables: Q2.5.Intercept <dbl>,
    ## #   Q97.5.Intercept <dbl>

``` r
extract_REs(model_SI) %>% arrange(-Estimate.Intercept)
```

    ## # A tibble: 64 × 9
    ##    title       main_type relCat compCat priCat Estimate.Interc… Est.Error.Inter…
    ##    <chr>       <chr>      <dbl>   <dbl>  <dbl>            <dbl>            <dbl>
    ##  1 Harold's p… xor            0       0      0            0.366            0.141
    ##  2 Jimmy's tr… xor            0       0      0            0.346            0.145
    ##  3 Brad Pitt   xor            0       0      0            0.241            0.163
    ##  4 Unexpected… xor            0       1      0            0.232            0.136
    ##  5 Puzzles     some           0       1      0            0.215            0.149
    ##  6 Rotten tom… some           1       0      0            0.214            0.136
    ##  7 Grandma     xor            0       1      1            0.184            0.150
    ##  8 Nuts        some           1       0      0            0.171            0.145
    ##  9 M&M's in t… some           0       1      1            0.168            0.149
    ## 10 Mrs. Todd'… some           0       1      0            0.161            0.143
    ## # … with 54 more rows, and 2 more variables: Q2.5.Intercept <dbl>,
    ## #   Q97.5.Intercept <dbl>

``` r
## model without by-subject REs
extract_REs(model_SI_RETitle) %>% arrange(Estimate.Intercept)
```

    ## # A tibble: 64 × 9
    ##    title       main_type relCat compCat priCat Estimate.Interc… Est.Error.Inter…
    ##    <chr>       <chr>      <dbl>   <dbl>  <dbl>            <dbl>            <dbl>
    ##  1 Gourmet de… some           0       0      1           -0.573            0.150
    ##  2 Dinosaurs   some           0       0      1           -0.472            0.147
    ##  3 Greg's mov… xor            0       0      1           -0.370            0.168
    ##  4 Carl's par… xor            1       0      0           -0.369            0.154
    ##  5 College     xor            1       0      0           -0.352            0.152
    ##  6 Semester a… xor            1       1      1           -0.345            0.152
    ##  7 Violinist   some           1       0      1           -0.342            0.156
    ##  8 Crime       xor            1       0      1           -0.301            0.144
    ##  9 Harvard ad… some           1       0      1           -0.285            0.148
    ## 10 Swiss watc… some           1       0      1           -0.278            0.152
    ## # … with 54 more rows, and 2 more variables: Q2.5.Intercept <dbl>,
    ## #   Q97.5.Intercept <dbl>

``` r
extract_REs(model_SI_RETitle) %>% arrange(-Estimate.Intercept)
```

    ## # A tibble: 64 × 9
    ##    title       main_type relCat compCat priCat Estimate.Interc… Est.Error.Inter…
    ##    <chr>       <chr>      <dbl>   <dbl>  <dbl>            <dbl>            <dbl>
    ##  1 Jimmy's tr… xor            0       0      0            0.370            0.147
    ##  2 Harold's p… xor            0       0      0            0.364            0.142
    ##  3 Puzzles     some           0       1      0            0.238            0.153
    ##  4 Unexpected… xor            0       1      0            0.236            0.137
    ##  5 Brad Pitt   xor            0       0      0            0.225            0.164
    ##  6 Rotten tom… some           1       0      0            0.202            0.139
    ##  7 Grandma     xor            0       1      1            0.187            0.154
    ##  8 Teacher pr… some           1       1      0            0.184            0.149
    ##  9 Nuts        some           1       0      0            0.173            0.149
    ## 10 Gift unwra… some           0       1      1            0.163            0.145
    ## # … with 54 more rows, and 2 more variables: Q2.5.Intercept <dbl>,
    ## #   Q97.5.Intercept <dbl>

To further find out how the addition of by-item REs affects the model
fit, we can try fittin a model *with* by-item REs, but omitting
vignettes which receive rather high/low estimates for their RE. Here we
gather the titles of the top-*n* highest or lowest ranked vignettes for
each trigger word.

``` r
get_extreme_vignettes <- function(model = model_SI, n=5, trigger_type = 'some', high = T) {
  extract_REs(model) %>% arrange(Estimate.Intercept * ifelse(high, 1, -1)) %>% 
    filter(main_type == trigger_type) %>% 
    pull(title) %>% .[1:n]
}

hi_some <- get_extreme_vignettes(trigger_type = 'some', high = T)
lo_some <- get_extreme_vignettes(trigger_type = 'some', high = F)
hi_or   <- get_extreme_vignettes(trigger_type = 'xor', high = T)
lo_or   <- get_extreme_vignettes(trigger_type = 'xor', high = F)
```

So, let’s fit some model on reduced data sets, excluding “extreme
vignettes”.

``` r
predictions_for_reduced_data <- function(excluded_vignettes) {
  run_model(
    f_full, 
    d_critical_clean %>% filter(! title %in% excluded_vignettes)
  ) %>% get_predictions()  
}

predictions_for_reduced_data(hi_some)
predictions_for_reduced_data(lo_some)
predictions_for_reduced_data(hi_or)
predictions_for_reduced_data(lo_or)
```

It seems that the vignettes for ‘some’ which are estimated to have
particularly high random intercepts are most conducive for whether the
main effect shows or not.

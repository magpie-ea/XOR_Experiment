XOR-Some Prolific Pilot 2
================
Polina Tsvilodub
4/2/2021

In this second pilot on Prolific we gather a more substantial amount of
pilot data for the xor-some study on Prolific. The structure of the
experiment was as follows: Participants read instructions, completed
three example trials, and then completed 8 main blocks consisting of 4
xor and 4 some items. Each main block had the following structure:
Participants read the background story, answered one comprehension
question, then answered competence / relevance / prior questions in
randomized order; then they read another 3 comprehension questions,
after which the critical utterance was added below the background story.
They answered the inference strength question, and then competence /
relevance questions in randomized order again.

N=118 participants were recruited for this pilot and compensated 2
pounds/participant. 8 items (4 some + 4 xor) were sampled at random for
each participant such that each participant saw one item in each
condition (relevance X competence X prior = 8 unique conditions).

## Checks & Exclusions

Checking if there are any comments indicating technical issues which is
not the case:

``` r
d %>% distinct(comments) %>% View()
```

Check native languages. Participants not indicating English as (one of)
their native language(s) are be excluded. We also compute some
participant demographics.

``` r
d %>% distinct(languages)
```

    ## # A tibble: 17 x 1
    ##    languages          
    ##    <chr>              
    ##  1 English            
    ##  2 engish             
    ##  3 english            
    ##  4 English & Yoruba   
    ##  5 Polish/English     
    ##  6 <NA>               
    ##  7 English, Arabic    
    ##  8 Englid             
    ##  9 ENGLISH            
    ## 10 English; Portuguese
    ## 11 british            
    ## 12 Englks             
    ## 13 Spanish            
    ## 14 English. Afrikaans 
    ## 15 English, Malay     
    ## 16 english, punjabi   
    ## 17 English, French

``` r
cat("Number of partiipants before excluding non-natives: ", d %>% distinct(submission_id) %>% count() %>% pull() ) 
```

    ## Number of partiipants before excluding non-natives:  118

``` r
# exclude non-natives if necessary
d_native <- d %>% 
 # filter(("en" | "En" | "bri") %in% languages)
  filter(grepl("[(en)(br)]", languages, ignore.case = T))

cat("Number of partiipants after excluding non-natives: ", d_native %>% distinct(submission_id) %>% count() %>% pull() )
```

    ## Number of partiipants after excluding non-natives:  116

``` r
cat("Mean age: ", d_native %>% pull(age) %>% mean(., na.rm = T) )
```

    ## Mean age:  35.25

``` r
d_native %>% count(gender) %>% mutate(n = n/88)
```

    ## # A tibble: 2 x 2
    ##   gender     n
    ##   <chr>  <dbl>
    ## 1 female    75
    ## 2 male      41

Next, we check whether all the conditions were used correctly.

``` r
# check xor/some vs. trial type
d_native %>% count(main_type, condition) 
```

    ## # A tibble: 5 x 3
    ##   main_type condition     n
    ##   <chr>     <chr>     <int>
    ## 1 some      critical   2784
    ## 2 some      test       1856
    ## 3 xor       critical   3248
    ## 4 xor       test       1856
    ## 5 <NA>      example     464

``` r
# check xor/some vs. experimental condition
d_native %>% count(main_type, exp_condition)
```

    ## # A tibble: 17 x 3
    ##    main_type exp_condition     n
    ##    <chr>     <chr>         <int>
    ##  1 some      hhh             490
    ##  2 some      hhl             550
    ##  3 some      hlh             620
    ##  4 some      hll             620
    ##  5 some      lhh             510
    ##  6 some      lhl             570
    ##  7 some      llh             620
    ##  8 some      lll             660
    ##  9 xor       hhh             737
    ## 10 xor       hhl             671
    ## 11 xor       hlh             594
    ## 12 xor       hll             594
    ## 13 xor       lhh             715
    ## 14 xor       lhl             649
    ## 15 xor       llh             594
    ## 16 xor       lll             550
    ## 17 <NA>      <NA>            464

``` r
# count items used
d_native %>% count(title)
```

    ## # A tibble: 65 x 2
    ##    title                                                                       n
    ##    <chr>                                                                   <int>
    ##  1 "<font size=\"4\" color= \"#00BFFF\"> EXAMPLE </font> <br/> Joe's shop…   464
    ##  2 "Alex's racket"                                                           231
    ##  3 "Alissa's paint"                                                          132
    ##  4 "Attendance"                                                              150
    ##  5 "Bill's order"                                                            220
    ##  6 "Brad Pitt"                                                                66
    ##  7 "Brad's clothes"                                                          154
    ##  8 "Carl's party"                                                            198
    ##  9 "Champagne reception"                                                     160
    ## 10 "Chloe's holiday"                                                         143
    ## # … with 55 more rows

Check the time participants spent overall on the experiment before
cleaning the data:

``` r
overall_timeSpent <- d_native %>% mutate(timeSpent = round(timeSpent, 2)) %>% distinct(timeSpent) 

#  summarize(timeCounts = count(timeSpent) / d_native %>% )
ggplot(data = overall_timeSpent, aes(y=timeSpent, alpha = 0.7)) +
  geom_boxplot() +
  ggtitle("Overall time participants took in mins")
```

![](xor-some-prolific-pilot2_files/figure-gfm/responseTime-1.png)<!-- -->

``` r
d_main <- d_native %>% select(-age, -botresponse, -comments, -education, -endTime, 
                              -gender, -languages, -optionLeft, -optionRight, -startDate,
                              -startTime, -timeSpent) %>%
  filter(trial_name != "example")
d_exmpl <- d_native %>% select(-age, -botresponse, -comments, -education, -endTime, 
                              -gender, -languages, -optionLeft, -optionRight, -startDate,
                              -startTime, -timeSpent) %>%
  filter(trial_name == "example")
d_critical <- d_main %>% filter(condition == "critical")
```

Plot responses on comprehension questions by type before applying
exclusion criteria:

    ## Warning: `as_data_frame()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(strap)`

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Next, we exclude participants based on their ratings in the main trials:
Participants who gave all responses within the range of 10 and
participants who failed more than 0.2 of the comprehension questions are
excluded from analysis. Participants who failed all example trials are
excluded, as well. The bot check trial is not considered for exclusions.

``` r
# get participants failing example trials
d_exmpl <- d_exmpl %>% group_by(submission_id) %>% 
  mutate(example_condition = ifelse(grepl("as certainly true", question), "true", 
                                    ifelse(grepl("as certainly false", question), "false",
                                           "uncertain")),
         passed_example_trial = case_when(example_condition == "true" ~ response >= 80,
                                    example_condition == "false" ~ response <= 20,
                                    example_condition == "uncertain" ~ response >= 30),
       #  check if all trials passed
         passed_example = case_when(sum(passed_example_trial) == 0 ~ FALSE,
                                    TRUE ~ TRUE)
         ) %>% filter(passed_example == F)

cat("Subjects failing the example trials: ", d_exmpl %>% distinct(submission_id) %>% pull() %>% length())
```

    ## Subjects failing the example trials:  0

``` r
# apply exclusion criteria to main trials
# check range of responses per participant
d_main_fail <- d_main %>% group_by(submission_id) %>%
  mutate(passed_main = case_when(max(response) - min(response) <= 10 ~ FALSE,
                                 TRUE ~ TRUE)
         ) %>% filter(passed_main == F)
cat("Subjects providing the same ratings throughout the trials: ", d_main_fail %>% distinct(submission_id) %>% pull() %>% length())
```

    ## Subjects providing the same ratings throughout the trials:  0

``` r
# get participants failing comprehension questions
d_test <- d_test %>%
  group_by(submission_id) %>%
  mutate(passed_filler_trial = case_when(test_condition == "true" ~ response >= 70,
                                   test_condition == "false" ~ response <= 40,
                                   test_condition == "uncertain" ~ response %in% (0:90)),
         mean_comprehension = mean(passed_filler_trial),
         passed_filler = mean_comprehension >= 0.8
         ) %>%
  filter(passed_filler == F)

cat("Subjects failing the comprehension trials: ", d_test %>% distinct(submission_id) %>% pull() %>% length())
```

    ## Subjects failing the comprehension trials:  17

``` r
# put it all together
d_full_clean <- anti_join(d_main, d_main_fail, by = "submission_id")
d_full_clean <- anti_join(d_full_clean, d_exmpl, d_test, by = "submission_id")
d_full_clean <- anti_join(d_full_clean, d_test, by = "submission_id")

cat("Nr. of participants left after cleaning: ", d_full_clean %>% distinct(submission_id) %>% pull() %>% length())
```

    ## Nr. of participants left after cleaning:  99

``` r
# get overall mean ratings / subject
d_full_clean %>% group_by(submission_id) %>% summarise(mean_rating = mean(response)) %>% arrange(mean_rating)
```

    ## # A tibble: 99 x 2
    ##    submission_id mean_rating
    ##            <dbl>       <dbl>
    ##  1          1960        30.0
    ##  2          1956        36.7
    ##  3          1920        39.3
    ##  4          1864        39.7
    ##  5          1950        40.4
    ##  6          1896        42.6
    ##  7          1901        43.2
    ##  8          1878        44.1
    ##  9          1900        44.8
    ## 10          1935        45.0
    ## # … with 89 more rows

Create more extensive condition labels, including information about
whether a rating was produced with or without the utterance given.

``` r
# extending 'conditions' labels to include whether the utterance was present or not
d_critical_long <- d_critical_long %>% 
  mutate(block_extended = ifelse(
    !w_utterance, 
    block, 
    ifelse(block %in% c("some", "xor"), "target", str_c(block, "_wUtt", ""))
  ))
```

Average nr of responses per question per vignette:

``` r
d_critical_long %>% 
  filter(class_condition == block | block == "xor" | block == "some") %>%
  select(-class_condition, -prior_class) %>%
  unique() %>% count(title, block_extended, main_type) %>%
  mutate(n = ifelse(main_type == "xor" & block_extended == "prior", n/2, n)) %>%
  summarize(mean_observations = mean(n))
```

    ## # A tibble: 1 x 1
    ##   mean_observations
    ##               <dbl>
    ## 1              12.4

Compute the average deviation from expected responses for each vignette,
individually for each dimension (rel / comp / pri):

``` r
d_critical_long %>% 
  filter(block != "xor" & block != "some") %>%
  filter(block == class_condition) %>%
  mutate(expected_rating = prior_class * 100,
         response_deviation = abs(response - expected_rating)) %>%
  group_by(title, block_extended) %>%
  summarize(mean_deviation = mean(response_deviation)) %>%
  arrange(desc(mean_deviation), .by_group = T) %>%
  # get the largest deviations - over 50 
  filter(mean_deviation >= 50) -> d_critical_deviations

#d_critical_deviations %>% write_csv("../data/pilots/pilot2_byItem_rating_deviations.csv")

print(d_critical_deviations)
```

    ## # A tibble: 74 x 3
    ## # Groups:   title [44]
    ##    title          block_extended  mean_deviation
    ##    <chr>          <chr>                    <dbl>
    ##  1 Alex's racket  prior                     61.8
    ##  2 Alex's racket  relevance                 57.9
    ##  3 Alex's racket  relevance_wUtt            51.7
    ##  4 Alissa's paint competence                65.6
    ##  5 Attendance     prior                     82.9
    ##  6 Bill's order   prior                     52.7
    ##  7 Brad Pitt      competence_wUtt           56.3
    ##  8 Brad's clothes competence_wUtt           69.1
    ##  9 Brad's clothes relevance                 66.8
    ## 10 Brad's clothes relevance_wUtt            64.5
    ## # … with 64 more rows

``` r
# count which dimension is the most problematic - the prior
d_critical_deviations %>% group_by(block_extended) %>% count()
```

    ## # A tibble: 5 x 2
    ## # Groups:   block_extended [5]
    ##   block_extended      n
    ##   <chr>           <int>
    ## 1 competence          8
    ## 2 competence_wUtt    19
    ## 3 prior              22
    ## 4 relevance          13
    ## 5 relevance_wUtt     12

``` r
# check which items have the most deviations over 50 
d_critical_deviations %>% group_by(title) %>% count() %>% arrange(desc(.$n))
```

    ## # A tibble: 44 x 2
    ## # Groups:   title [44]
    ##    title                           n
    ##    <chr>                       <int>
    ##  1 Greg's movie                    5
    ##  2 Brad's clothes                  4
    ##  3 Shelter                         4
    ##  4 Alex's racket                   3
    ##  5 Health inspector                3
    ##  6 Joshua's delay                  3
    ##  7 Suzy's fruits                   3
    ##  8 Crime                           2
    ##  9 Dave's kids                     2
    ## 10 Emails from a broken laptop     2
    ## # … with 34 more rows

## Plots

Plot main rel / comp / prior questions by main condition and by prior
classification of the item (x-axis), separated into with / without
critical utterance (shape, color). The prior questions were only used
without the utterance.

``` r
d_critical_long %>% 
  filter(block != "xor" & block != "some") %>%
  filter(block == class_condition) %>%
  group_by(main_type, class_condition, w_utterance, prior_class) %>% 
  summarize(mean_response = mean(response)) -> d_critical_summary

d_critical_long %>% 
  filter(block != "xor" & block != "some") %>%
  filter(block == class_condition) %>%
  ggplot(., aes(x = as.factor(prior_class), y = response, shape = w_utterance, color = w_utterance)) +
  geom_point(size = 2, alpha = 0.6, position = position_jitter(width = 0.1)) +
  geom_point(data = d_critical_summary, aes(x = as.factor(prior_class), y = mean_response, shape = w_utterance), 
             color = "red", size = 3) +
  xlab("Anticipated categorization of the items (low / high)") +
  facet_wrap(main_type~class_condition) # get ratings from the respective trials only 
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# check whether the ratings in the two prior questions are the same in xor 
d_critical_long %>% 
  filter(block != "xor" & block != "some") %>%
  filter(block == class_condition) %>%
  filter(block == "prior", main_type == "xor") %>%
  mutate(priorQ_nr = rep(c(1,2), 396)) -> d_xor_priors
d_xor_priors %>% group_by(priorQ_nr, prior_class) %>%
  summarise(mean = mean(response)) -> d_xor_priors_summary
d_xor_priors %>%
  ggplot(., aes(x = as.factor(priorQ_nr), y = response )) +
  geom_point(size = 2, alpha = 0.6, position = position_jitter(width = 0.1)) +
  geom_point(data = d_xor_priors_summary, aes(x = as.factor(priorQ_nr), y = mean), color = "red", size = 3) +
  xlab("First vs Second prior question for high vs low prior conditions") +
  facet_wrap(~as.factor(prior_class)) # get ratings from the respective trials only 
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Plot inference ratings as a function of *anticipated* rating of the
explanatory factor, similar to the paper:

``` r
d_critical_long %>% 
  filter(block == "xor" | block == "some") %>%
  #group_by(block, class_condition, prior_class) %>%
  #summarize(mean_rating = mean(response)) %>%
  ggplot(., aes(x = prior_class, y = response)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method="lm") +
  facet_wrap(block~class_condition) +
  xlab("Anticipated categorization of the items") +
  ggtitle("Inference strength ratings by-predictor")
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# make a versatile wide representation of the critical data
d_critical_wide <- d_critical_long %>% 
  select(submission_id, title, main_type, block_extended, response) %>% 
  unique() %>% 
  pivot_wider(
    names_from = block_extended, 
    values_from = response, 
    values_fn = mean # getting means for double prior measurement in "xor"
  ) 
```

Plot inference strength ratings against raw predictor ratings for each
participant / each item:

``` r
d_critical_wide %>%
  ggplot(., aes(x = relevance, y = target)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm") +
  facet_wrap(~main_type, ncol = 1) -> p.rel

d_critical_wide %>%
  ggplot(., aes(x = competence, y = target)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm") +
  ylab("") +
  facet_wrap(~main_type, ncol = 1) -> p.comp

d_critical_wide %>%
  ggplot(., aes(x = prior, y = target)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm") +
  ylab("") +
  facet_wrap(~main_type, ncol = 1) -> p.pri

gridExtra::grid.arrange(p.rel, p.comp, p.pri, ncol = 3) 
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Plot mean ratings (across all participants) for each vignette (with its
respective condition indicated in the facet title, in the order
relevance/competence/prior), in each condition. For relevance and
competence, the color indicates whether it was presented without the
utterance or with.

``` r
bar.width = 0.8
d_critical_long %>% 
  mutate(title = paste(title, exp_condition, sep = "_")) %>%
  filter(block == class_condition | block == "xor" | block == "some") %>%
  group_by(block_extended, title, w_utterance, main_type) %>%
  summarize(mean_rating = mean(response)) %>% 
  ggplot(., aes(x = block_extended, y = mean_rating, fill = w_utterance)) +
  geom_col(alpha = 0.7, width = bar.width, position = position_dodge(width = bar.width)) +
  #geom_point(size = 2, alpha = 0.5, position = position_jitter(width = 0.1)) +
  facet_wrap(main_type~title, ncol = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("By-item by-question mean ratings")
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# correlation plot for "some"
GGally::ggpairs(
  filter(d_critical_wide, main_type == "some") %>%  
    select(prior:ncol(d_critical_wide))
)
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# correlation plot for "or"
GGally::ggpairs(
  filter(d_critical_wide, main_type == "xor") %>%  
    select(prior:ncol(d_critical_wide))
  )
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

Check by-participant behavior to see if they show versatile response
strategies for the different questions for xor vs. some. Participants
show a sufficient range of responses across conditions.

``` r
d_critical_long %>%
  filter(block == class_condition | block == "xor" | block == "some") %>%
  ggplot(., aes(x = block, y = response, color = block)) +
  geom_point(alpha = 0.7, size = 2) + 
  facet_wrap(main_type~submission_id, ncol = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("By-participant ratings all xor vs. some trials")
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## Explore z-scoring

Visual exploration of z-scoring each measure (rel / comp / pri / target
/ comp-wUtterance / rel-wUtterance) per participant, collapsing some and
or together.

``` r
d_critical_zScored <- d_critical_long %>% group_by(submission_id, block_extended) %>%
  mutate(block_mean = mean(response),
         block_sd = sd(response),
         response_centered = (response - block_mean)/block_sd)

d_critical_zScored %>% 
  filter(block != "xor" & block != "some") %>%
  filter(block == class_condition) %>%
  group_by(main_type, class_condition, w_utterance, prior_class) %>% 
  summarize(mean_response = mean(response_centered)) -> d_critical_zScore_summary

d_critical_zScored %>% 
  filter(block != "xor" & block != "some") %>%
  filter(block == class_condition) %>%
  ggplot(., aes(x = as.factor(prior_class), y = response_centered, shape = w_utterance, color = w_utterance)) +
  geom_point(size = 2, alpha = 0.6, position = position_jitter(width = 0.1)) +
  geom_point(data = d_critical_zScore_summary, aes(x = as.factor(prior_class), y = mean_response, shape = w_utterance), 
             color = "red", size = 3) +
  xlab("Anticipated categorization of the items (low / high)") +
  facet_wrap(main_type~class_condition) + # get ratings from the respective trials only 
  ggtitle("Centered responses to by-predictor questions")
```

    ## Warning: Removed 8 rows containing missing values (geom_point).

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Plot z-scored predictor ratings against z-scored target ratings

``` r
d_critical_zScored_wide <- d_critical_zScored %>% 
  select(submission_id, title, main_type, block_extended, response_centered) %>% 
  unique() %>% 
  pivot_wider(
    names_from = block_extended, 
    values_from = response_centered, 
    values_fn = mean # getting means for double prior measurement in "xor"
  ) 
  
d_critical_zScored_wide %>%
  ggplot(., aes(x = relevance, y = target)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm") +
  facet_wrap(~main_type, ncol = 1) -> p.rel.z

d_critical_zScored_wide %>%
  ggplot(., aes(x = competence, y = target)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm") +
  ylab("") +
  facet_wrap(~main_type, ncol = 1) -> p.comp.z

d_critical_zScored_wide %>%
  ggplot(., aes(x = prior, y = target)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm") +
  ylab("") +
  facet_wrap(~main_type, ncol = 1) -> p.pri.z

gridExtra::grid.arrange(p.rel.z, p.comp.z, p.pri.z, ncol = 3) 
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Stats

Maximal models on raw ratings:

``` r
# xor, maximal model with interactions and maximal REs
model_xor <- brm(
  target ~ prior*competence*relevance + 
    (1 + prior + competence + relevance || submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

    ## Warning: There were 6000 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
    ## http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded

    ## Warning: Examine the pairs() plot to diagnose sampling problems

    ## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#bulk-ess

    ## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#tail-ess

``` r
summary(model_xor)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * competence * relevance + (1 + prior + competence + relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "xor") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     17.06      2.96    11.03    22.68 1.00      770     1624
    ## sd(prior)          0.09      0.07     0.00     0.25 1.00      941     1822
    ## sd(competence)     0.14      0.06     0.01     0.26 1.01      438      569
    ## sd(relevance)      0.09      0.06     0.01     0.21 1.02      480     1572
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    10.36      6.62     4.51    29.14 1.03       88       26
    ## 
    ## Population-Level Effects: 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                     61.40     12.87    31.66    82.27 1.03      111
    ## prior                          0.00      0.23    -0.46     0.45 1.00     1963
    ## competence                     0.06      0.15    -0.24     0.35 1.00     1869
    ## relevance                      0.17      0.18    -0.20     0.53 1.00     2035
    ## prior:competence               0.00      0.00    -0.01     0.01 1.00     2085
    ## prior:relevance               -0.00      0.00    -0.01     0.00 1.00     2286
    ## competence:relevance          -0.00      0.00    -0.01     0.00 1.00     1845
    ## prior:competence:relevance     0.00      0.00    -0.00     0.00 1.00     2193
    ##                            Tail_ESS
    ## Intercept                        29
    ## prior                          3363
    ## competence                     3473
    ## relevance                      3612
    ## prior:competence               3350
    ## prior:relevance                3845
    ## competence:relevance           3407
    ## prior:competence:relevance     3555
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    24.69      1.21    22.46    27.18 1.00     2070     3988
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# some, maximal model with interactions and maximal REs
model_some <- brm(
  target ~ prior*competence*relevance + 
    (1 + prior + competence + relevance || submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

    ## Warning: There were 6000 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
    ## http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded

    ## Warning: Examine the pairs() plot to diagnose sampling problems

    ## Warning: The largest R-hat is 1.05, indicating chains have not mixed.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#r-hat

    ## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#bulk-ess

    ## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#tail-ess

``` r
summary(model_some)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * competence * relevance + (1 + prior + competence + relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "some") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     10.21      1.84     6.41    13.73 1.01      882     1301
    ## sd(prior)          0.07      0.05     0.00     0.16 1.04       69      947
    ## sd(competence)     0.03      0.02     0.00     0.08 1.00      724      896
    ## sd(relevance)      0.04      0.03     0.00     0.11 1.01      559     1492
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     9.06      3.95     5.12    14.14 1.01      250       71
    ## 
    ## Population-Level Effects: 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                     76.98      7.37    62.71    89.30 1.01      381
    ## prior                         -0.16      0.21    -0.57     0.24 1.00     2274
    ## competence                     0.13      0.10    -0.07     0.31 1.00     2240
    ## relevance                     -0.07      0.08    -0.23     0.08 1.00     2507
    ## prior:competence               0.00      0.00    -0.00     0.01 1.00     2190
    ## prior:relevance                0.00      0.00    -0.00     0.01 1.00     2294
    ## competence:relevance           0.00      0.00    -0.00     0.00 1.00     2699
    ## prior:competence:relevance    -0.00      0.00    -0.00     0.00 1.00     2280
    ##                            Tail_ESS
    ## Intercept                       134
    ## prior                          3704
    ## competence                     3594
    ## relevance                      3550
    ## prior:competence               3794
    ## prior:relevance                3636
    ## competence:relevance           3266
    ## prior:competence:relevance     3805
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    19.96      0.91    18.30    21.86 1.00     2547     3594
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

#### Stats on z-scored data

``` r
# xor, maximal model with interactions and maximal REs on z-scored data
model_xor_zScored <- brm(
  target ~ prior*competence*relevance + 
    (1 + prior + competence + relevance || submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_zScored)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * competence * relevance + (1 + prior + competence + relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "x (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)      0.09      0.06     0.00     0.22 1.00     2650     3196
    ## sd(prior)          0.15      0.09     0.01     0.34 1.00     1425     2711
    ## sd(competence)     0.24      0.10     0.03     0.42 1.00     1016     1677
    ## sd(relevance)      0.12      0.08     0.01     0.30 1.00     1657     3037
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.36      0.08     0.20     0.54 1.00     2107     3417
    ## 
    ## Population-Level Effects: 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                     -0.21      0.08    -0.37    -0.05 1.00     4110
    ## prior                         -0.13      0.07    -0.26     0.01 1.00     6800
    ## competence                     0.07      0.07    -0.06     0.21 1.00     6320
    ## relevance                     -0.02      0.06    -0.14     0.11 1.00     7359
    ## prior:competence               0.12      0.07    -0.02     0.26 1.00     9843
    ## prior:relevance                0.04      0.07    -0.10     0.17 1.00     8691
    ## competence:relevance          -0.03      0.06    -0.15     0.09 1.00     6594
    ## prior:competence:relevance    -0.00      0.07    -0.14     0.14 1.00     8183
    ##                            Tail_ESS
    ## Intercept                      3947
    ## prior                          5082
    ## competence                     5466
    ## relevance                      4985
    ## prior:competence               5268
    ## prior:relevance                4764
    ## competence:relevance           4488
    ## prior:competence:relevance     4834
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.85      0.04     0.77     0.93 1.00     2216     3392
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# some, maximal model with interactions and maximal REs
model_some_zScored <- brm(
  target ~ prior*competence*relevance + 
    (1 + prior + competence + relevance || submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_zScored)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * competence * relevance + (1 + prior + competence + relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "s (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)      0.10      0.07     0.00     0.24 1.00     1951     2976
    ## sd(prior)          0.10      0.06     0.00     0.23 1.00     1868     2940
    ## sd(competence)     0.17      0.09     0.01     0.33 1.00     1263     1997
    ## sd(relevance)      0.07      0.05     0.00     0.19 1.00     2665     2551
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.36      0.08     0.23     0.53 1.00     2337     3491
    ## 
    ## Population-Level Effects: 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                      0.22      0.08     0.06     0.39 1.00     4115
    ## prior                         -0.04      0.05    -0.14     0.06 1.00     7328
    ## competence                     0.13      0.06     0.00     0.25 1.00     5623
    ## relevance                     -0.05      0.06    -0.17     0.06 1.00     7552
    ## prior:competence              -0.08      0.04    -0.17     0.00 1.00     8462
    ## prior:relevance               -0.01      0.05    -0.10     0.08 1.00     8425
    ## competence:relevance           0.02      0.06    -0.09     0.14 1.00     7169
    ## prior:competence:relevance     0.00      0.04    -0.08     0.09 1.00     8566
    ##                            Tail_ESS
    ## Intercept                      4435
    ## prior                          5022
    ## competence                     4918
    ## relevance                      5324
    ## prior:competence               4910
    ## prior:relevance                4566
    ## competence:relevance           4685
    ## prior:competence:relevance     4709
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.82      0.04     0.75     0.89 1.00     2824     3578
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

#### Stats with dichotomized predictors

Maximal model on raw inference ratings, using anticipated binary
categorization of items as predictors

``` r
d_critical_long %>% 
  select(submission_id, title, main_type, block_extended, response, prior_class, class_condition) %>% 
  unique() %>% 
  pivot_wider(
    names_from = class_condition, 
    values_from = prior_class
  )  %>%
  mutate(relevance = as.factor(relevance),
         competence = as.factor(competence),
         prior = as.factor(prior)) %>% 
  filter(block_extended == "target") -> d_critical_wide_cat
# sum code predictors
# low prior: 1, high prior:: -1 
contrasts(d_critical_wide_cat$prior) <- contr.sum(2)
# low comp : 1, high comp : -1
contrasts(d_critical_wide_cat$competence) <- contr.sum(2)
# low rel: 1, high rel: -1
contrasts(d_critical_wide_cat$relevance) <- contr.sum(2)

# xor, maximal model with interactions and maximal REs
model_xor_cat <- brm(
  response ~ prior*competence*relevance + 
    (1 + prior + competence + relevance || submission_id) +
    (1 | title),
  data = d_critical_wide_cat %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_cat)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: response ~ prior * competence * relevance + (1 + prior + competence + relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_wide_cat %>% filter(main_type == "xor") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)      20.70      2.08    16.93    25.09 1.00     1840     3283
    ## sd(prior1)          5.46      2.62     0.47    10.24 1.00      954     1726
    ## sd(competence1)     7.24      2.75     1.13    11.98 1.00     1038     1553
    ## sd(relevance1)      2.90      2.00     0.12     7.37 1.00     1715     2815
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     7.88      2.78     2.06    13.24 1.00     1341     1313
    ## 
    ## Population-Level Effects: 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## Intercept                        67.91      2.77    62.43    73.40 1.00
    ## prior1                            2.02      2.02    -1.95     6.05 1.00
    ## competence1                      -3.39      2.11    -7.51     0.77 1.00
    ## relevance1                        0.36      2.03    -3.84     4.27 1.00
    ## prior1:competence1                0.04      1.99    -3.93     4.01 1.00
    ## prior1:relevance1                 2.88      2.00    -1.15     6.75 1.00
    ## competence1:relevance1            4.04      2.03     0.00     8.00 1.00
    ## prior1:competence1:relevance1     2.74      2.01    -1.27     6.70 1.00
    ##                               Bulk_ESS Tail_ESS
    ## Intercept                         3038     3768
    ## prior1                            5982     3905
    ## competence1                       6022     4671
    ## relevance1                        5518     4104
    ## prior1:competence1                5408     4443
    ## prior1:relevance1                 5764     4786
    ## competence1:relevance1            6304     4901
    ## prior1:competence1:relevance1     5517     3952
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    22.76      1.50    19.85    25.76 1.00     1016     2266
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
model_some_cat <- brm(
  response ~ prior*competence*relevance + 
    (1 + prior + competence + relevance || submission_id) +
    (1 | title),
  data = d_critical_wide_cat %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_cat)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: response ~ prior * competence * relevance + (1 + prior + competence + relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_wide_cat %>% filter(main_type == "some" (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)      10.86      1.61     7.77    14.00 1.00     1741     3171
    ## sd(prior1)          1.77      1.28     0.08     4.67 1.00     2172     3508
    ## sd(competence1)     6.34      2.11     1.43    10.08 1.00     1092     1178
    ## sd(relevance1)      3.59      2.02     0.21     7.51 1.00     1086     2465
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     7.39      2.14     3.40    11.85 1.00     1594     1995
    ## 
    ## Population-Level Effects: 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## Intercept                        82.35      2.01    78.28    86.31 1.00
    ## prior1                            2.03      1.76    -1.49     5.45 1.00
    ## competence1                      -7.24      1.84   -10.90    -3.58 1.00
    ## relevance1                       -1.19      1.76    -4.67     2.16 1.00
    ## prior1:competence1                1.72      1.73    -1.66     5.17 1.00
    ## prior1:relevance1                -3.07      1.69    -6.43     0.30 1.00
    ## competence1:relevance1            1.23      1.71    -2.20     4.51 1.00
    ## prior1:competence1:relevance1    -1.26      1.72    -4.58     2.23 1.00
    ##                               Bulk_ESS Tail_ESS
    ## Intercept                         3768     3853
    ## prior1                            4350     4045
    ## competence1                       5170     3949
    ## relevance1                        4601     3812
    ## prior1:competence1                4435     4247
    ## prior1:relevance1                 4961     4304
    ## competence1:relevance1            4726     4173
    ## prior1:competence1:relevance1     4785     4195
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    18.36      1.12    16.26    20.63 1.01     1080     2347
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Maximal model on z-scored inference ratings, using anticipated binary
categorization of items as predictors

``` r
d_critical_zScored %>% 
  select(submission_id, title, main_type, block_extended, response_centered, prior_class, class_condition) %>% 
  unique() %>% 
  pivot_wider(
    names_from = class_condition, 
    values_from = prior_class
  )  %>%
  mutate(relevance = as.factor(relevance),
         competence = as.factor(competence),
         prior = as.factor(prior)) %>% 
  filter(block_extended == "target") -> d_critical_wide_cat_zScored
# sum code predictors
# low prior: 1, high prior:: -1 
contrasts(d_critical_wide_cat_zScored$prior) <- contr.sum(2)
# low comp : 1, high comp : -1
contrasts(d_critical_wide_cat_zScored$competence) <- contr.sum(2)
# low rel: 1, high rel: -1
contrasts(d_critical_wide_cat_zScored$relevance) <- contr.sum(2)

# xor, maximal model with interactions and maximal REs
model_xor_cat_zScored <- brm(
  response_centered ~ prior*competence*relevance + 
    (1 + prior + competence + relevance || submission_id) +
    (1 | title),
  data = d_critical_wide_cat_zScored %>% filter(main_type == "xor"),
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
    ## Formula: response_centered ~ prior * competence * relevance + (1 + prior + competence + relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_wide_cat_zScored %>% filter(main_type = (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)       0.10      0.07     0.00     0.25 1.00     1618     2515
    ## sd(prior1)          0.18      0.09     0.01     0.35 1.00     1223     2308
    ## sd(competence1)     0.23      0.09     0.02     0.39 1.00     1131     1519
    ## sd(relevance1)      0.11      0.07     0.01     0.26 1.00     1879     3280
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.33      0.09     0.18     0.53 1.00     1766     3324
    ## 
    ## Population-Level Effects: 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## Intercept                        -0.23      0.08    -0.38    -0.08 1.00
    ## prior1                            0.10      0.08    -0.06     0.26 1.00
    ## competence1                      -0.19      0.08    -0.34    -0.02 1.00
    ## relevance1                       -0.03      0.08    -0.18     0.12 1.00
    ## prior1:competence1                0.01      0.08    -0.15     0.15 1.00
    ## prior1:relevance1                 0.10      0.08    -0.05     0.25 1.00
    ## competence1:relevance1            0.15      0.08    -0.00     0.30 1.00
    ## prior1:competence1:relevance1     0.05      0.07    -0.10     0.19 1.00
    ##                               Bulk_ESS Tail_ESS
    ## Intercept                         5276     4897
    ## prior1                            5539     4552
    ## competence1                       4955     4212
    ## relevance1                        5293     4587
    ## prior1:competence1                4735     4113
    ## prior1:relevance1                 5116     4577
    ## competence1:relevance1            4914     4735
    ## prior1:competence1:relevance1     5145     4483
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.83      0.04     0.74     0.91 1.00     1388     2700
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
model_some_cat_zScored <- brm(
  response_centered ~ prior*competence*relevance + 
    (1 + prior + competence + relevance || submission_id) +
    (1 | title),
  data = d_critical_wide_cat_zScored %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_cat_zScored)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: response_centered ~ prior * competence * relevance + (1 + prior + competence + relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_wide_cat_zScored %>% filter(main_type = (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)       0.10      0.07     0.01     0.25 1.00     1550     2954
    ## sd(prior1)          0.07      0.05     0.00     0.18 1.00     2431     3063
    ## sd(competence1)     0.16      0.09     0.01     0.33 1.00     1142     1488
    ## sd(relevance1)      0.13      0.08     0.01     0.29 1.00     1147     1979
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.27      0.09     0.10     0.45 1.00     1506     1816
    ## 
    ## Population-Level Effects: 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## Intercept                         0.22      0.07     0.09     0.35 1.00
    ## prior1                            0.09      0.07    -0.05     0.22 1.00
    ## competence1                      -0.33      0.07    -0.47    -0.19 1.00
    ## relevance1                       -0.03      0.07    -0.17     0.10 1.00
    ## prior1:competence1                0.05      0.07    -0.08     0.18 1.00
    ## prior1:relevance1                -0.12      0.07    -0.26     0.01 1.00
    ## competence1:relevance1            0.09      0.07    -0.05     0.22 1.00
    ## prior1:competence1:relevance1    -0.05      0.07    -0.18     0.09 1.00
    ##                               Bulk_ESS Tail_ESS
    ## Intercept                         5212     4709
    ## prior1                            4356     4335
    ## competence1                       4380     4345
    ## relevance1                        4783     4046
    ## prior1:competence1                4814     4456
    ## prior1:relevance1                 4347     4186
    ## competence1:relevance1            4978     4307
    ## prior1:competence1:relevance1     4779     4496
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.82      0.04     0.74     0.89 1.00     2542     3566
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

In summary, the different models provide somewhat different results,
specially when using raw ratings vs. binary predictors. Since
interpreting the categorical models in relation to the plots might be
not as straight-forward, I am not entirely sure whether the qualitative
results match in all models. Using centered data seems to provide a
little tighter estimates in the models; not sure where the slight
qualitative differences come from.

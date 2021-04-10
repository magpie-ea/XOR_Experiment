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
         response_deviation = abs(response - expected_rating),
         block = ifelse(block == "xor" | block == "some", "target", block)
         ) %>%
  group_by(title, block) %>%
  summarize(mean_deviation = mean(response_deviation)) %>%
  arrange(desc(mean_deviation), .by_group = T) %>%
  # get the largest deviations - over 50 
  filter(mean_deviation >= 50) -> d_critical_deviations

#d_critical_deviations %>% write_csv("../data/pilots/pilot2_byItem_rating_deviations.csv")

print(d_critical_deviations)
```

    ## # A tibble: 47 x 3
    ## # Groups:   title [37]
    ##    title          block      mean_deviation
    ##    <chr>          <chr>               <dbl>
    ##  1 Alex's racket  prior                61.8
    ##  2 Alex's racket  relevance            54.8
    ##  3 Alissa's paint competence           54.2
    ##  4 Attendance     prior                82.9
    ##  5 Bill's order   prior                52.7
    ##  6 Brad Pitt      competence           52.3
    ##  7 Brad's clothes relevance            65.6
    ##  8 Brad's clothes prior                58.6
    ##  9 College        competence           51.0
    ## 10 Crime          prior                51.4
    ## # … with 37 more rows

``` r
# count which dimension is the most problematic - the prior
d_critical_deviations %>% group_by(block) %>% count()
```

    ## # A tibble: 3 x 2
    ## # Groups:   block [3]
    ##   block          n
    ##   <chr>      <int>
    ## 1 competence    13
    ## 2 prior         22
    ## 3 relevance     12

``` r
# check which items have the most deviations over 50 
d_critical_deviations %>% group_by(title) %>% count() %>% arrange(desc(.$n))
```

    ## # A tibble: 37 x 2
    ## # Groups:   title [37]
    ##    title                n
    ##    <chr>            <int>
    ##  1 Greg's movie         3
    ##  2 Shelter              3
    ##  3 Alex's racket        2
    ##  4 Brad's clothes       2
    ##  5 Gourmet desserts     2
    ##  6 Health inspector     2
    ##  7 Suzy's fruits        2
    ##  8 The game             2
    ##  9 Alissa's paint       1
    ## 10 Attendance           1
    ## # … with 27 more rows

Compute the standard deviation for each question for each item as an
additional measure of spread in the participants’ responses within-item:

``` r
d_critical_long %>% 
  filter(block != "xor" & block != "some") %>%
  filter(block == class_condition) %>%
  mutate(
         block = ifelse(block == "xor" | block == "some", "target", block)
         ) %>%
  group_by(title, block) %>%
  summarize(item_sd = sd(response)) %>%
  arrange(desc(item_sd), .by_group = T) %>%
  # arbitrary set to 25
  filter(item_sd >= 25) -> d_critical_sd
d_critical_sd
```

    ## # A tibble: 90 x 3
    ## # Groups:   title [59]
    ##    title               block      item_sd
    ##    <chr>               <chr>        <dbl>
    ##  1 Alex's racket       competence    26.6
    ##  2 Alissa's paint      competence    26.9
    ##  3 Attendance          prior         25.0
    ##  4 Bill's order        relevance     25.6
    ##  5 Brad Pitt           competence    25.5
    ##  6 Brad's clothes      competence    34.4
    ##  7 Brad's clothes      relevance     28.0
    ##  8 Carl's party        relevance     33.2
    ##  9 Carl's party        competence    28.8
    ## 10 Champagne reception relevance     28.1
    ## # … with 80 more rows

``` r
#d_critical_sd %>% write_csv("../data/pilots/pilot2_byItem_rating_SDs.csv")
```

Visually check the spread of by-item ratings among subjects for each
question:

``` r
d_critical_long %>% 
  mutate(title = paste(title, exp_condition, sep = "_"),
         block = ifelse(block == "xor" | block == "some", "target", block)) %>%
  filter(block == class_condition | block == "target") %>%
  ggplot(., aes(x = block, y = response, color = block)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.1)) +
  ylab("Raw responses to respective questions") +
  facet_wrap(main_type~title, ncol = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("By-item by-question raw ratings")
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
For the following items, participants seem to *disagree* in their
judgments for all of the predictors: Emails from a broken laptop, Gift
unwrapping, Martha’s cookies, Suzie’s fruits, Brad’s clothes, Crime,
maybe more..

Check by-vignette by-predictor empirical means, to see if the range is
somewhat covered:

``` r
d_critical_long %>% 
  mutate(title = paste(title, exp_condition, sep = "_"),
         block = ifelse(block == "xor" | block == "some", "target", block)) %>%
  filter(block == class_condition | block == "target") %>%
  group_by(title, block) %>%
  summarise(mean_response = mean(response)) %>%
  arrange(mean_response)
```

    ## # A tibble: 256 x 3
    ## # Groups:   title [64]
    ##    title                     block mean_response
    ##    <chr>                     <chr>         <dbl>
    ##  1 Final exam_hll            prior          10.6
    ##  2 Teacher_hhl               prior          10.8
    ##  3 Champagne reception_lll   prior          12  
    ##  4 Mrs Todd's math exams_lhl prior          13.1
    ##  5 Teacher problems_hhl      prior          14.3
    ##  6 Underage drinking_lhl     prior          14.3
    ##  7 Harold's pet_lll          prior          15.0
    ##  8 Shelter_llh               prior          15.4
    ##  9 Tail-donkey blindfold_hhl prior          16.2
    ## 10 Stand-up comedy show_lll  prior          17  
    ## # … with 246 more rows

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
  ylab("Responses to respective predictor question") +
  xlab("Anticipated categorization of the items (low / high)") +
  facet_wrap(main_type~class_condition) # get ratings from the respective trials only 
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

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
  ylab("Responses to prior questions") +
  xlab("First vs Second prior question for high vs low prior conditions") +
  facet_wrap(~as.factor(prior_class)) # get ratings from the respective trials only 
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Plot inference ratings as a function of *anticipated* rating of the
explanatory factor, similar to the paper:

``` r
d_critical_long %>% 
  filter(block == "xor" | block == "some") %>%
  ggplot(., aes(x = prior_class, y = response)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method="lm") +
  ylab("Inference strength ratings") +
  facet_wrap(block~class_condition) +
  xlab("Anticipated categorization of the items") +
  ggtitle("Inference strength ratings by-predictor")
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

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

Plot inference strength ratings against raw predictor ratings from each
participant across items:

``` r
d_critical_wide %>%
  ggplot(., aes(x = relevance, y = target)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm") +
  ylab("Inference strength ratings") +
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

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

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
  ylab("Mean responses to respective questions") +
  xlab("Question type") +
  facet_wrap(main_type~title, ncol = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("By-item by-question mean ratings")
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Plot by-item trends between predictor ratings and target inference
strength ratings (relevance / competence ratings with utterance are
omitted):

``` r
# make a versatile wide representation of the critical data
d_critical_predictorsXtarget <- d_critical_wide %>% 
  pivot_longer(
    cols = c(relevance, prior, competence, relevance_wUtt, competence_wUtt),
    names_to = "predictors",
    values_to = "predictor_response"
  ) %>%
  filter(predictors != "relevance_wUtt", predictors != "competence_wUtt") 

d_critical_predictorsXtarget %>%
  ggplot(., aes(x = predictor_response, y = target)) +
  geom_point() +
  facet_wrap(title~predictors, ncol= 6) +
  xlab("Response to predictor questions") +
  ylab("Inference strength rating") +
  ggtitle("By-item Inference Strength Ratings vs. Predictor Ratings (left to right)")
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Plot inference strength ratings against predictor ratings, by-item,
using by-item mean ratings across participants. Error bars indicate the
standard deviation of the mean:

``` r
d_critical_predictorsXtarget_summary <- d_critical_predictorsXtarget %>% 
  group_by(title, predictors, main_type) %>%
  summarise(mean_target = mean(target),
            mean_predictor = mean(predictor_response), 
            sd_target = sd(target),
            sd_predictor = sd(predictor_response))  


d_critical_predictorsXtarget_summary %>%
  ggplot(., aes(x = mean_predictor, y = mean_target)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_target - sd_target, ymax = mean_target + sd_target), alpha = 0.7) +
  geom_errorbarh(aes(xmin = mean_predictor - sd_predictor, xmax = mean_predictor + sd_predictor), alpha = 0.7) +
  geom_text(aes(label=title), size = 2.5, nudge_y = 4) +
  geom_smooth(method = "lm") +
  ylab("Mean inference strength rating") +
  xlab("Mean predictor ratings") + 
  facet_wrap(main_type~predictors)
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
# correlation plot for "some"
GGally::ggpairs(
  filter(d_critical_wide, main_type == "some") %>%  
    select(prior, competence, relevance, target, competence_wUtt, relevance_wUtt) 
)
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
# correlation plot for "or"
GGally::ggpairs(
  filter(d_critical_wide, main_type == "xor") %>%  
    select(prior, competence, relevance, target, competence_wUtt, relevance_wUtt)
  )
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

## Explore z-scoring

Visual exploration of z-scoring each measure (rel / comp / pri / target
/ comp-wUtterance / rel-wUtterance) per participant, collapsing some and
or together.

``` r
d_critical_zScored <- d_critical_long %>% group_by(submission_id, block_extended) %>%
  mutate(block_mean = mean(response),
         block_sd = sd(response),
         response_centered = (response - block_mean)/block_sd,
         # catch the cases where sd is 0 
         response_centered = ifelse(is.na(response_centered), 0, response_centered))

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
  ylab("Responses to respective predictor questions") +
  xlab("Anticipated categorization of the items (low / high)") +
  facet_wrap(main_type~class_condition) + # get ratings from the respective trials only 
  ggtitle("Centered responses to by-predictor questions")
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

Plot z-scored predictor ratings against z-scored target inference
ratings

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
  ylab("Inference strength ratings") +
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

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

Correlation plots of z-scored data:

``` r
# correlation plot for "some"
GGally::ggpairs(
  filter(d_critical_zScored_wide, main_type == "some") %>% ungroup() %>%  
    select(prior, competence, relevance, target, competence_wUtt, relevance_wUtt)
)
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
# correlation plot for "or"
GGally::ggpairs(
  filter(d_critical_zScored_wide, main_type == "xor") %>%  ungroup() %>%
    select(prior, competence, relevance, target, competence_wUtt, relevance_wUtt)
  )
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->

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
    ## sd(Intercept)     17.06      2.90    10.92    22.44 1.00      816     1122
    ## sd(prior)          0.10      0.07     0.00     0.25 1.01      920     1771
    ## sd(competence)     0.14      0.06     0.01     0.25 1.01      494      772
    ## sd(relevance)      0.09      0.06     0.00     0.22 1.00      664     1074
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     9.20      2.61     4.13    14.43 1.00     1221     1837
    ## 
    ## Population-Level Effects: 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                     63.24      9.75    44.06    82.39 1.00     1926
    ## prior                         -0.01      0.24    -0.47     0.45 1.00     2142
    ## competence                     0.06      0.15    -0.22     0.36 1.00     1945
    ## relevance                      0.17      0.18    -0.18     0.53 1.00     2181
    ## prior:competence               0.00      0.00    -0.01     0.01 1.00     2195
    ## prior:relevance               -0.00      0.00    -0.01     0.00 1.00     2206
    ## competence:relevance          -0.00      0.00    -0.01     0.00 1.00     2115
    ## prior:competence:relevance     0.00      0.00    -0.00     0.00 1.00     2226
    ##                            Tail_ESS
    ## Intercept                      3020
    ## prior                          3514
    ## competence                     3269
    ## relevance                      2816
    ## prior:competence               3479
    ## prior:relevance                3162
    ## competence:relevance           3432
    ## prior:competence:relevance     3578
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    24.74      1.22    22.48    27.19 1.00     1974     4150
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Exploring plots of posterior samples for pairwise parameters shows that
there is a visible correlation between prior & competence, and a
somewhat weaker correlation between competence & relevance. The
interaction estimates also show a correlation with the main effects (as
a rough overall judgment).

``` r
#shinystan::launch_shinystan(model_xor)
```

Plot correlations of main effects

``` r
bayesplot::mcmc_pairs(model_xor, pars = c("b_prior", "b_competence", "b_relevance"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

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
    ## sd(Intercept)     10.23      1.87     6.31    13.69 1.00     1167     1248
    ## sd(prior)          0.06      0.04     0.00     0.15 1.01     1020     1521
    ## sd(competence)     0.03      0.02     0.00     0.08 1.00     1393     1624
    ## sd(relevance)      0.04      0.03     0.00     0.11 1.00      718     1242
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     8.58      1.91     5.09    12.66 1.00     1893     3337
    ## 
    ## Population-Level Effects: 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                     77.71      5.90    66.06    89.18 1.00     1973
    ## prior                         -0.16      0.20    -0.55     0.24 1.00     1890
    ## competence                     0.13      0.09    -0.06     0.31 1.00     2320
    ## relevance                     -0.07      0.08    -0.23     0.08 1.00     2254
    ## prior:competence               0.00      0.00    -0.00     0.01 1.00     1894
    ## prior:relevance                0.00      0.00    -0.00     0.01 1.00     2034
    ## competence:relevance           0.00      0.00    -0.00     0.00 1.00     2485
    ## prior:competence:relevance    -0.00      0.00    -0.00     0.00 1.00     2072
    ##                            Tail_ESS
    ## Intercept                      3308
    ## prior                          3049
    ## competence                     3546
    ## relevance                      3848
    ## prior:competence               3199
    ## prior:relevance                3314
    ## competence:relevance           3687
    ## prior:competence:relevance     3243
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    19.99      0.91    18.33    21.86 1.00     2474     2990
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

For “some”, the correlation of prior and relevance is higher than for
prior and competence; there is also a visible correlation of competence
and relevance.

``` r
#shinystan::launch_shinystan(model_some)
```

``` r
bayesplot::mcmc_pairs(model_some, pars = c("b_prior", "b_competence", "b_relevance"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

#### Smaller models with raw data to check for collinearity

Models on raw data with only one predictor:

``` r
# xor
model_xor_prior <- brm(
  target ~ prior + 
    (1 + prior | submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  control = list(adapt_delta = 0.99),
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_prior)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior + (1 + prior | submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "xor") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)           20.53      2.79    15.31    26.48 1.00     2874
    ## sd(prior)                0.09      0.07     0.00     0.26 1.00      732
    ## cor(Intercept,prior)    -0.11      0.51    -0.93     0.92 1.00     4470
    ##                      Tail_ESS
    ## sd(Intercept)            3842
    ## sd(prior)                 915
    ## cor(Intercept,prior)     3573
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     9.69      2.44     5.22    14.86 1.00     2216     3026
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    69.18      4.41    60.57    77.77 1.00     3354     4329
    ## prior        -0.03      0.08    -0.17     0.12 1.00     4390     4980
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    25.16      1.10    23.08    27.44 1.00     4429     4629
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# xor
model_xor_comp <- brm(
  target ~ competence + 
    (1 + competence | submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  control = list(adapt_delta = 0.99),
  cores = 4,
  iter = 3000
)
```

    ## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#bulk-ess

``` r
summary(model_xor_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ competence + (1 + competence | submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "xor") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                20.90      4.09    13.10    29.25 1.00     1228
    ## sd(competence)                0.20      0.09     0.02     0.37 1.01      333
    ## cor(Intercept,competence)    -0.23      0.40    -0.74     0.78 1.01      633
    ##                           Tail_ESS
    ## sd(Intercept)                 3141
    ## sd(competence)                1142
    ## cor(Intercept,competence)     1325
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     9.59      2.38     5.25    14.77 1.00     2253     2854
    ## 
    ## Population-Level Effects: 
    ##            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     63.92      4.53    55.18    72.79 1.00     4626     4474
    ## competence     0.07      0.06    -0.05     0.19 1.00     5025     4205
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    24.39      1.25    22.02    26.97 1.00     1130     3038
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# xor
model_xor_rel <- brm(
  target ~ relevance + 
    (1 + relevance | submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  control = list(adapt_delta = 0.99),
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_rel)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ relevance + (1 + relevance | submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "xor") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               19.68      2.89    14.06    25.55 1.00     2801
    ## sd(relevance)                0.08      0.06     0.00     0.22 1.01      573
    ## cor(Intercept,relevance)     0.05      0.52    -0.89     0.94 1.00     3214
    ##                          Tail_ESS
    ## sd(Intercept)                3722
    ## sd(relevance)                 985
    ## cor(Intercept,relevance)     3545
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    10.00      2.28     5.93    14.84 1.00     2227     2983
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    66.93      4.03    59.08    74.93 1.00     3208     4036
    ## relevance     0.02      0.06    -0.09     0.13 1.00     4920     3997
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    25.08      1.12    22.95    27.33 1.00     3501     4405
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# some
model_some_rel <- brm(
  target ~ relevance + 
    (1 + relevance | submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "some"),
  family = "gaussian",
  control = list(adapt_delta = 0.99),
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_rel)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ relevance + (1 + relevance | submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "some") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               11.71      2.20     7.66    16.37 1.00     2358
    ## sd(relevance)                0.04      0.03     0.00     0.11 1.00     1251
    ## cor(Intercept,relevance)    -0.16      0.55    -0.95     0.91 1.00     4278
    ##                          Tail_ESS
    ## sd(Intercept)                3800
    ## sd(relevance)                1523
    ## cor(Intercept,relevance)     3455
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    11.07      1.88     7.73    15.09 1.00     1992     3691
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    82.22      3.79    74.79    89.62 1.00     2988     4425
    ## relevance    -0.00      0.04    -0.09     0.09 1.00     4581     5035
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    19.79      0.87    18.21    21.59 1.00     3724     4170
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# some
model_some_prior <- brm(
  target ~ prior + 
    (1 + prior | submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "some"),
  family = "gaussian",
  control = list(adapt_delta = 0.99),
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_prior)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior + (1 + prior | submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "some") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)           11.91      1.93     8.25    15.89 1.00     2204
    ## sd(prior)                0.09      0.07     0.00     0.24 1.00      870
    ## cor(Intercept,prior)    -0.33      0.49    -0.96     0.83 1.00     3788
    ##                      Tail_ESS
    ## sd(Intercept)            3361
    ## sd(prior)                1978
    ## cor(Intercept,prior)     3009
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    10.97      1.93     7.64    15.27 1.00     2165     3760
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    82.31      2.93    76.41    87.89 1.00     2388     3708
    ## prior        -0.01      0.05    -0.11     0.09 1.00     5153     5404
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    19.80      0.88    18.20    21.64 1.00     3697     4383
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# some
model_some_comp <- brm(
  target ~ competence + 
    (1 + competence | submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "some"),
  family = "gaussian",
  control = list(adapt_delta = 0.92),
  cores = 4,
  iter = 3000
)
```

    ## Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.92 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ## Warning: Examine the pairs() plot to diagnose sampling problems

``` r
summary(model_some_comp)
```

    ## Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta
    ## above 0.92 may help. See http://mc-stan.org/misc/warnings.html#divergent-
    ## transitions-after-warmup

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ competence + (1 + competence | submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "some") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                15.31      2.85    10.01    21.14 1.00     1606
    ## sd(competence)                0.09      0.04     0.01     0.18 1.00     1360
    ## cor(Intercept,competence)    -0.80      0.27    -0.99    -0.02 1.00     2276
    ##                           Tail_ESS
    ## sd(Intercept)                 2397
    ## sd(competence)                1156
    ## cor(Intercept,competence)     1803
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     8.77      1.82     5.54    12.61 1.00     2384     3281
    ## 
    ## Population-Level Effects: 
    ##            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     74.05      3.26    67.65    80.42 1.00     3110     4306
    ## competence     0.14      0.04     0.06     0.22 1.00     4098     4347
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    19.45      0.90    17.78    21.32 1.00     2664     3656
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

With two predictors:

``` r
# xor
model_xor_pri_comp <- brm(
  target ~ prior*competence + 
    (1 + prior*competence || submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_pri_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * competence + (1 + prior * competence || submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "xor") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)           18.14      2.68    12.98    23.41 1.00     1473
    ## sd(prior)                0.09      0.06     0.00     0.23 1.00     1190
    ## sd(competence)           0.14      0.06     0.02     0.25 1.00      660
    ## sd(prior:competence)     0.00      0.00     0.00     0.00 1.00     1582
    ##                      Tail_ESS
    ## sd(Intercept)            2362
    ## sd(prior)                1804
    ## sd(competence)            966
    ## sd(prior:competence)     2812
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     9.18      2.52     4.21    14.29 1.00     1931     2485
    ## 
    ## Population-Level Effects: 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept           69.83      7.01    56.06    83.90 1.00     3737     3818
    ## prior               -0.15      0.15    -0.43     0.13 1.00     3610     3765
    ## competence          -0.01      0.10    -0.21     0.18 1.00     4310     3983
    ## prior:competence     0.00      0.00    -0.00     0.01 1.00     3792     4118
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    24.67      1.15    22.51    27.04 1.00     2344     3974
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_xor_pri_comp, pars = c("b_prior", "b_competence"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
# xor
model_xor_pri_rel <- brm(
  target ~ prior*relevance + 
    (1 + prior*relevance || submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  control = list(adapt_delta = 0.92),
  cores = 4,
  iter = 3000
)
```

    ## Warning: There were 2 divergent transitions after warmup. Increasing adapt_delta above 0.92 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ## Warning: Examine the pairs() plot to diagnose sampling problems

``` r
summary(model_xor_pri_rel)
```

    ## Warning: There were 2 divergent transitions after warmup. Increasing adapt_delta
    ## above 0.92 may help. See http://mc-stan.org/misc/warnings.html#divergent-
    ## transitions-after-warmup

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * relevance + (1 + prior * relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "xor") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)          18.73      2.41    13.96    23.44 1.00     1669     2702
    ## sd(prior)               0.09      0.06     0.00     0.23 1.00      921     1902
    ## sd(relevance)           0.09      0.06     0.00     0.21 1.00      939     1936
    ## sd(prior:relevance)     0.00      0.00     0.00     0.00 1.00     1349     2283
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     9.69      2.53     4.94    14.91 1.00     1509     1078
    ## 
    ## Population-Level Effects: 
    ##                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept          66.17      6.02    54.44    78.05 1.00     3735     4116
    ## prior               0.03      0.12    -0.21     0.26 1.00     4352     4530
    ## relevance           0.06      0.09    -0.12     0.23 1.00     4549     4748
    ## prior:relevance    -0.00      0.00    -0.00     0.00 1.00     4786     5026
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    24.98      1.17    22.79    27.38 1.00     3275     4209
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_xor_pri_rel, pars = c("b_prior", "b_relevance"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r
# xor
model_xor_rel_comp <- brm(
  target ~ relevance*competence + 
    (1 + relevance*competence || submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_rel_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ relevance * competence + (1 + relevance * competence || submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "xor") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               17.43      2.76    11.80    22.80 1.00      966
    ## sd(relevance)                0.09      0.06     0.00     0.21 1.00      718
    ## sd(competence)               0.14      0.06     0.02     0.25 1.01      456
    ## sd(relevance:competence)     0.00      0.00     0.00     0.00 1.00     1338
    ##                          Tail_ESS
    ## sd(Intercept)                1509
    ## sd(relevance)                1771
    ## sd(competence)                787
    ## sd(relevance:competence)     2080
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     9.66      2.34     5.38    14.47 1.00     2017     2748
    ## 
    ## Population-Level Effects: 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept               63.26      6.12    51.35    75.05 1.00     2981
    ## relevance                0.01      0.10    -0.19     0.21 1.00     3146
    ## competence               0.07      0.09    -0.11     0.25 1.00     3645
    ## relevance:competence    -0.00      0.00    -0.00     0.00 1.00     3390
    ##                      Tail_ESS
    ## Intercept                3997
    ## relevance                4204
    ## competence               3971
    ## relevance:competence     3799
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    24.58      1.19    22.28    26.96 1.00     2101     3016
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_xor_rel_comp, pars = c("b_competence", "b_relevance"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

``` r
# some
model_some_pri_comp <- brm(
  target ~ prior*competence + 
    (1 + prior*competence || submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_pri_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * competence + (1 + prior * competence || submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "some") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)           10.62      1.68     7.30    13.96 1.00     2223
    ## sd(prior)                0.06      0.04     0.00     0.15 1.00     1837
    ## sd(competence)           0.03      0.02     0.00     0.08 1.00     2182
    ## sd(prior:competence)     0.00      0.00     0.00     0.00 1.00     2254
    ##                      Tail_ESS
    ## sd(Intercept)            3357
    ## sd(prior)                3313
    ## sd(competence)           2951
    ## sd(prior:competence)     3145
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     8.51      1.89     5.10    12.52 1.00     2163     3123
    ## 
    ## Population-Level Effects: 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept           73.30      3.68    66.11    80.52 1.00     5165     4496
    ## prior                0.04      0.09    -0.14     0.21 1.00     6389     4794
    ## competence           0.18      0.05     0.08     0.28 1.00     5333     4304
    ## prior:competence    -0.00      0.00    -0.00     0.00 1.00     6838     4520
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    19.90      0.89    18.26    21.72 1.00     3528     4477
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_some_pri_comp, pars = c("b_prior", "b_competence"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

``` r
# some
model_some_pri_rel <- brm(
  target ~ prior*relevance + 
    (1 + prior*relevance || submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_pri_rel)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * relevance + (1 + prior * relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "some") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)          10.61      1.79     6.89    14.07 1.00     1258     1333
    ## sd(prior)               0.06      0.04     0.00     0.16 1.00     1842     3113
    ## sd(relevance)           0.04      0.03     0.00     0.11 1.00     1347     1978
    ## sd(prior:relevance)     0.00      0.00     0.00     0.00 1.00     1688     2957
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)    11.12      1.98     7.76    15.65 1.00     2024     3344
    ## 
    ## Population-Level Effects: 
    ##                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept          82.73      4.47    74.17    91.59 1.00     4817     5046
    ## prior              -0.02      0.10    -0.21     0.17 1.00     5984     4412
    ## relevance          -0.01      0.05    -0.11     0.10 1.00     5333     4781
    ## prior:relevance     0.00      0.00    -0.00     0.00 1.00     6443     4944
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    19.79      0.88    18.14    21.61 1.00     3488     4177
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_some_pri_rel, pars = c("b_prior", "b_relevance"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

``` r
# some
model_some_rel_comp <- brm(
  target ~ relevance*competence + 
    (1 + relevance*competence || submission_id) +
    (1 | title),
  data = d_critical_wide %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_rel_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ relevance * competence + (1 + relevance * competence || submission_id) + (1 | title) 
    ##    Data: d_critical_wide %>% filter(main_type == "some") (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               10.53      1.87     6.75    13.97 1.01     1276
    ## sd(relevance)                0.04      0.03     0.00     0.11 1.00     1029
    ## sd(competence)               0.03      0.02     0.00     0.08 1.00     2017
    ## sd(relevance:competence)     0.00      0.00     0.00     0.00 1.00     2459
    ##                          Tail_ESS
    ## sd(Intercept)                 845
    ## sd(relevance)                1617
    ## sd(competence)               3151
    ## sd(relevance:competence)     2401
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     8.77      1.84     5.45    12.70 1.00     2297     3771
    ## 
    ## Population-Level Effects: 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept               76.18      5.12    66.40    86.35 1.00     3710
    ## relevance               -0.03      0.07    -0.16     0.11 1.00     3963
    ## competence               0.12      0.07    -0.03     0.26 1.00     3256
    ## relevance:competence     0.00      0.00    -0.00     0.00 1.00     3339
    ##                      Tail_ESS
    ## Intercept                4252
    ## relevance                4818
    ## competence               4168
    ## relevance:competence     4287
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    19.91      0.91    18.25    21.84 1.00     2935     2996
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_some_rel_comp, pars = c("b_competence", "b_relevance"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

#### Stats on z-scored data

The same stats (full, one predictor, to predictors) on z-scored data:

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
    ## sd(Intercept)      0.09      0.06     0.00     0.22 1.00     2261     3351
    ## sd(prior)          0.15      0.09     0.01     0.34 1.01     1279     2634
    ## sd(competence)     0.25      0.10     0.03     0.42 1.00     1225     1412
    ## sd(relevance)      0.12      0.08     0.00     0.30 1.00     1629     3044
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.36      0.08     0.21     0.54 1.00     1926     3196
    ## 
    ## Population-Level Effects: 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                     -0.21      0.08    -0.36    -0.05 1.00     4126
    ## prior                         -0.12      0.07    -0.25     0.01 1.00     6636
    ## competence                     0.07      0.07    -0.07     0.21 1.00     5930
    ## relevance                     -0.02      0.06    -0.14     0.11 1.00     7437
    ## prior:competence               0.12      0.07    -0.02     0.26 1.00     7519
    ## prior:relevance                0.04      0.07    -0.10     0.17 1.00     8348
    ## competence:relevance          -0.03      0.06    -0.14     0.09 1.00     7154
    ## prior:competence:relevance    -0.00      0.07    -0.14     0.13 1.00     7690
    ##                            Tail_ESS
    ## Intercept                      4378
    ## prior                          4324
    ## competence                     4955
    ## relevance                      4582
    ## prior:competence               4785
    ## prior:relevance                4777
    ## competence:relevance           4936
    ## prior:competence:relevance     4738
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.84      0.04     0.77     0.93 1.00     1849     3354
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_xor_zScored, pars = c("b_prior", "b_relevance", "b_competence"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

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
    ## sd(Intercept)      0.10      0.07     0.01     0.24 1.00     1815     2938
    ## sd(prior)          0.10      0.07     0.00     0.24 1.00     1526     2552
    ## sd(competence)     0.17      0.08     0.01     0.33 1.00     1005     1626
    ## sd(relevance)      0.07      0.05     0.00     0.19 1.00     2582     2801
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.36      0.08     0.22     0.53 1.00     2504     3137
    ## 
    ## Population-Level Effects: 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                      0.22      0.08     0.05     0.38 1.00     3755
    ## prior                         -0.04      0.05    -0.14     0.06 1.00     6707
    ## competence                     0.13      0.06     0.01     0.25 1.00     5675
    ## relevance                     -0.05      0.06    -0.17     0.07 1.00     6708
    ## prior:competence              -0.08      0.04    -0.17     0.00 1.00     7598
    ## prior:relevance               -0.01      0.04    -0.10     0.08 1.00     7656
    ## competence:relevance           0.02      0.06    -0.09     0.13 1.00     7351
    ## prior:competence:relevance     0.00      0.04    -0.08     0.08 1.00     7196
    ##                            Tail_ESS
    ## Intercept                      4402
    ## prior                          4907
    ## competence                     4593
    ## relevance                      4420
    ## prior:competence               4372
    ## prior:relevance                4820
    ## competence:relevance           4859
    ## prior:competence:relevance     4519
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.82      0.04     0.75     0.89 1.00     2889     3607
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_some_zScored, pars = c("b_prior", "b_relevance", "b_competence"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

#### Smaller models with z-scored data

``` r
# xor
model_xor_zScored_prior <- brm(
  target ~ prior + 
    (1 + prior | submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_zScored_prior)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior + (1 + prior | submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "x (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)            0.08      0.06     0.00     0.22 1.00     2686
    ## sd(prior)                0.15      0.09     0.01     0.34 1.00     1376
    ## cor(Intercept,prior)    -0.06      0.57    -0.95     0.94 1.00     1936
    ##                      Tail_ESS
    ## sd(Intercept)            3252
    ## sd(prior)                2706
    ## cor(Intercept,prior)     3282
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.38      0.08     0.24     0.57 1.00     2082     3216
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -0.20      0.09    -0.38    -0.04 1.00     2830     2971
    ## prior        -0.11      0.07    -0.24     0.02 1.00     6235     4272
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.88      0.04     0.81     0.95 1.00     3778     3814
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# xor
model_xor_zScored_comp <- brm(
  target ~ competence + 
    (1 + competence | submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_zScored_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ competence + (1 + competence | submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "x (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                 0.09      0.06     0.00     0.23 1.00     2392
    ## sd(competence)                0.25      0.10     0.04     0.43 1.00     1379
    ## cor(Intercept,competence)    -0.23      0.53    -0.96     0.89 1.01      754
    ##                           Tail_ESS
    ## sd(Intercept)                 2819
    ## sd(competence)                1911
    ## cor(Intercept,competence)     1384
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.40      0.08     0.26     0.57 1.00     1986     3701
    ## 
    ## Population-Level Effects: 
    ##            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     -0.23      0.09    -0.41    -0.06 1.00     3258     3732
    ## competence     0.07      0.07    -0.06     0.21 1.00     5394     4212
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.86      0.04     0.79     0.93 1.00     3168     4166
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# xor
model_xor_zScored_rel <- brm(
  target ~ relevance + 
    (1 + relevance | submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_zScored_rel)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ relevance + (1 + relevance | submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "x (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                0.08      0.06     0.00     0.22 1.00     2425
    ## sd(relevance)                0.14      0.09     0.01     0.32 1.00     1508
    ## cor(Intercept,relevance)     0.09      0.57    -0.94     0.96 1.00     2065
    ##                          Tail_ESS
    ## sd(Intercept)                2566
    ## sd(relevance)                2496
    ## cor(Intercept,relevance)     2823
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.41      0.08     0.27     0.59 1.00     2197     3498
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -0.22      0.09    -0.40    -0.05 1.00     2593     3616
    ## relevance     0.01      0.06    -0.11     0.13 1.00     5325     3977
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.88      0.03     0.82     0.95 1.00     4767     3862
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# some
model_some_zScored_rel <- brm(
  target ~ relevance + 
    (1 + relevance | submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_zScored_rel)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ relevance + (1 + relevance | submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "s (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                0.09      0.06     0.00     0.23 1.00     1241
    ## sd(relevance)                0.06      0.05     0.00     0.17 1.00     2618
    ## cor(Intercept,relevance)     0.00      0.57    -0.95     0.94 1.00     3634
    ##                          Tail_ESS
    ## sd(Intercept)                2056
    ## sd(relevance)                2587
    ## cor(Intercept,relevance)     3604
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.46      0.08     0.33     0.64 1.00     1951     3588
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.22      0.10     0.03     0.40 1.00     1534     2821
    ## relevance    -0.04      0.06    -0.15     0.08 1.00     3562     3969
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.84      0.03     0.78     0.91 1.00     4901     3955
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# some
model_some_zScored_prior <- brm(
  target ~ prior + 
    (1 + prior | submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_zScored_prior)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior + (1 + prior | submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "s (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)            0.10      0.07     0.00     0.26 1.01     1691
    ## sd(prior)                0.11      0.07     0.01     0.26 1.00     1468
    ## cor(Intercept,prior)     0.27      0.55    -0.87     0.97 1.00     2116
    ##                      Tail_ESS
    ## sd(Intercept)            2463
    ## sd(prior)                1973
    ## cor(Intercept,prior)     3151
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.46      0.08     0.33     0.64 1.00     2002     3369
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.21      0.10     0.02     0.40 1.00     1898     3013
    ## prior        -0.01      0.05    -0.10     0.08 1.00     6556     3974
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.84      0.03     0.77     0.90 1.00     4694     4413
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# some
model_some_zScored_comp <- brm(
  target ~ competence + 
    (1 + competence | submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_zScored_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ competence + (1 + competence | submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "s (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                 0.16      0.07     0.01     0.30 1.00     1296
    ## sd(competence)                0.21      0.07     0.04     0.34 1.00     1340
    ## cor(Intercept,competence)    -0.63      0.37    -0.99     0.49 1.00     1170
    ##                           Tail_ESS
    ## sd(Intercept)                 2047
    ## sd(competence)                1679
    ## cor(Intercept,competence)     1190
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.38      0.08     0.25     0.55 1.00     2166     3574
    ## 
    ## Population-Level Effects: 
    ##            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept      0.21      0.08     0.05     0.37 1.00     3282     3898
    ## competence     0.15      0.06     0.04     0.27 1.00     5072     4461
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.81      0.04     0.74     0.88 1.00     2213     3477
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

With two predictors:

``` r
# xor
model_xor_zScored_pri_comp <- brm(
  target ~ prior*competence + 
    (1 + prior*competence || submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_zScored_pri_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * competence + (1 + prior * competence || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "x (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)            0.08      0.06     0.00     0.22 1.00     2493
    ## sd(prior)                0.13      0.09     0.01     0.31 1.00     1683
    ## sd(competence)           0.23      0.10     0.03     0.41 1.00     1308
    ## sd(prior:competence)     0.18      0.11     0.01     0.42 1.00     1396
    ##                      Tail_ESS
    ## sd(Intercept)            2972
    ## sd(prior)                2741
    ## sd(competence)           1976
    ## sd(prior:competence)     2621
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.35      0.08     0.20     0.52 1.00     1925     3000
    ## 
    ## Population-Level Effects: 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept           -0.21      0.08    -0.37    -0.05 1.00     3880     4141
    ## prior               -0.13      0.07    -0.26    -0.00 1.00     6137     4522
    ## competence           0.08      0.07    -0.05     0.22 1.00     6128     4728
    ## prior:competence     0.13      0.07    -0.02     0.26 1.00     8595     4822
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.84      0.04     0.77     0.93 1.00     2572     3713
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_xor_zScored_pri_comp, pars = c("b_prior", "b_competence"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-80-1.png)<!-- -->

``` r
# xor
model_xor_zScored_pri_rel <- brm(
  target ~ prior*relevance + 
    (1 + prior*relevance || submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_zScored_pri_rel)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * relevance + (1 + prior * relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "x (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)           0.08      0.06     0.00     0.22 1.00     2218     2576
    ## sd(prior)               0.16      0.09     0.01     0.35 1.00     1388     2643
    ## sd(relevance)           0.14      0.09     0.01     0.33 1.00     1355     2598
    ## sd(prior:relevance)     0.15      0.10     0.01     0.35 1.00     1728     2602
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.38      0.08     0.24     0.56 1.00     2262     3339
    ## 
    ## Population-Level Effects: 
    ##                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept          -0.20      0.09    -0.37    -0.03 1.00     3128     3900
    ## prior              -0.10      0.07    -0.24     0.04 1.00     5544     5071
    ## relevance           0.00      0.07    -0.13     0.13 1.00     6731     4842
    ## prior:relevance     0.05      0.07    -0.09     0.19 1.00     7875     4915
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.86      0.04     0.79     0.94 1.00     3242     3290
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_xor_zScored_pri_rel, pars = c("b_prior", "b_relevance"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-83-1.png)<!-- -->

``` r
# xor
model_xor_zScored_rel_comp <- brm(
  target ~ relevance*competence + 
    (1 + relevance*competence || submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_zScored_rel_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ relevance * competence + (1 + relevance * competence || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "x (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                0.08      0.06     0.00     0.22 1.00     2097
    ## sd(relevance)                0.13      0.08     0.01     0.31 1.00     1370
    ## sd(competence)               0.25      0.10     0.04     0.43 1.00     1172
    ## sd(relevance:competence)     0.10      0.07     0.00     0.28 1.00     1932
    ##                          Tail_ESS
    ## sd(Intercept)                2640
    ## sd(relevance)                2520
    ## sd(competence)               1897
    ## sd(relevance:competence)     3101
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.40      0.08     0.25     0.57 1.00     2243     3821
    ## 
    ## Population-Level Effects: 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept               -0.23      0.09    -0.40    -0.06 1.00     2948
    ## relevance               -0.01      0.07    -0.13     0.13 1.00     7016
    ## competence               0.06      0.07    -0.08     0.20 1.00     5816
    ## relevance:competence    -0.03      0.06    -0.16     0.09 1.00     5725
    ##                      Tail_ESS
    ## Intercept                3975
    ## relevance                5041
    ## competence               4265
    ## relevance:competence     4144
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.85      0.04     0.78     0.93 1.00     2580     3641
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_xor_zScored_rel_comp, pars = c("b_relevance", "b_competence"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-86-1.png)<!-- -->

``` r
# some
model_some_zScored_pri_comp <- brm(
  target ~ prior*competence + 
    (1 + prior*competence || submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_zScored_pri_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * competence + (1 + prior * competence || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "s (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)            0.10      0.06     0.00     0.24 1.00     1845
    ## sd(prior)                0.09      0.06     0.00     0.22 1.00     1937
    ## sd(competence)           0.15      0.08     0.01     0.31 1.00     1282
    ## sd(prior:competence)     0.12      0.07     0.01     0.26 1.00     1157
    ##                      Tail_ESS
    ## sd(Intercept)            2408
    ## sd(prior)                3369
    ## sd(competence)           2070
    ## sd(prior:competence)     1871
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.37      0.07     0.23     0.52 1.00     2380     3655
    ## 
    ## Population-Level Effects: 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept            0.20      0.08     0.05     0.36 1.00     3638     4013
    ## prior               -0.04      0.05    -0.13     0.05 1.00     7444     4637
    ## competence           0.13      0.06     0.02     0.25 1.00     6048     5009
    ## prior:competence    -0.08      0.04    -0.16     0.01 1.00     8430     4907
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.81      0.04     0.74     0.88 1.00     3197     3677
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_some_zScored_pri_comp, pars = c("b_prior", "b_competence"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-89-1.png)<!-- -->

``` r
# some
model_some_zScored_pri_rel <- brm(
  target ~ prior*relevance + 
    (1 + prior*relevance || submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_zScored_pri_rel)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ prior * relevance + (1 + prior * relevance || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "s (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)           0.09      0.07     0.00     0.24 1.00     2128     2777
    ## sd(prior)               0.10      0.07     0.00     0.25 1.00     1575     3000
    ## sd(relevance)           0.06      0.05     0.00     0.18 1.00     3221     3121
    ## sd(prior:relevance)     0.10      0.06     0.01     0.22 1.00     1937     2667
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.46      0.08     0.33     0.63 1.00     2753     4167
    ## 
    ## Population-Level Effects: 
    ##                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept           0.21      0.10     0.02     0.40 1.00     2869     3519
    ## prior              -0.02      0.05    -0.12     0.08 1.00     8244     5145
    ## relevance          -0.04      0.06    -0.16     0.09 1.00     7007     4790
    ## prior:relevance    -0.00      0.05    -0.09     0.09 1.00     7423     4799
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.83      0.04     0.76     0.90 1.00     4997     4309
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_some_zScored_pri_rel, pars = c("b_prior", "b_relevance"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-92-1.png)<!-- -->

``` r
# some
model_some_zScored_rel_comp <- brm(
  target ~ relevance*competence + 
    (1 + relevance*competence || submission_id) +
    (1 | title),
  data = d_critical_zScored_wide %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_zScored_rel_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: target ~ relevance * competence + (1 + relevance * competence || submission_id) + (1 | title) 
    ##    Data: d_critical_zScored_wide %>% filter(main_type == "s (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                0.10      0.07     0.00     0.24 1.00     2029
    ## sd(relevance)                0.07      0.05     0.00     0.18 1.00     3022
    ## sd(competence)               0.15      0.08     0.01     0.32 1.00     1212
    ## sd(relevance:competence)     0.11      0.07     0.01     0.27 1.00     1556
    ##                          Tail_ESS
    ## sd(Intercept)                2890
    ## sd(relevance)                2987
    ## sd(competence)               2185
    ## sd(relevance:competence)     2777
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.37      0.08     0.24     0.54 1.00     2448     3355
    ## 
    ## Population-Level Effects: 
    ##                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                0.22      0.08     0.06     0.38 1.00     3505
    ## relevance               -0.04      0.05    -0.14     0.07 1.00     7379
    ## competence               0.15      0.06     0.03     0.27 1.00     5213
    ## relevance:competence     0.02      0.05    -0.09     0.12 1.00     7234
    ##                      Tail_ESS
    ## Intercept                3583
    ## relevance                4724
    ## competence               4577
    ## relevance:competence     4381
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.82      0.04     0.75     0.89 1.00     2847     3873
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
bayesplot::mcmc_pairs(model_some_zScored_rel_comp, pars = c("b_relevance", "b_competence"))
```

![](xor-some-prolific-pilot2_files/figure-gfm/unnamed-chunk-95-1.png)<!-- -->

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
  mutate(relevance = factor(relevance, levels = c(1, 0)),
         competence = factor(competence, levels = c(1, 0)),
         prior = factor(prior, levels = c(1, 0))) %>% 
  filter(block_extended == "target") -> d_critical_wide_cat
# sum code predictors
# low prior: -1, high prior:: 1 
contrasts(d_critical_wide_cat$prior) <- contr.sum(2)
# low comp : -1, high comp : 1
contrasts(d_critical_wide_cat$competence) <- contr.sum(2)
# low rel: -1, high rel: 1
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
    ## sd(Intercept)      20.77      2.09    16.98    25.12 1.00     2105     3584
    ## sd(prior1)          5.50      2.60     0.52    10.20 1.00      908     1655
    ## sd(competence1)     7.39      2.72     1.29    12.08 1.01      796      871
    ## sd(relevance1)      2.90      1.96     0.14     7.33 1.00     1757     2848
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     8.15      2.65     3.12    13.63 1.00     1755     2244
    ## 
    ## Population-Level Effects: 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## Intercept                        67.86      2.81    62.37    73.40 1.00
    ## prior1                           -2.07      2.04    -6.16     1.81 1.00
    ## competence1                       3.37      2.12    -0.88     7.42 1.00
    ## relevance1                       -0.33      2.05    -4.32     3.76 1.00
    ## prior1:competence1                0.12      2.06    -3.87     4.23 1.00
    ## prior1:relevance1                 2.92      2.06    -1.07     6.96 1.00
    ## competence1:relevance1            4.05      2.06     0.15     8.17 1.00
    ## prior1:competence1:relevance1    -2.69      1.97    -6.63     1.27 1.00
    ##                               Bulk_ESS Tail_ESS
    ## Intercept                         3031     3720
    ## prior1                            4965     4653
    ## competence1                       4654     4369
    ## relevance1                        5166     4035
    ## prior1:competence1                5065     4122
    ## prior1:relevance1                 4764     4394
    ## competence1:relevance1            5044     4156
    ## prior1:competence1:relevance1     4702     4396
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    22.69      1.50    19.92    25.75 1.00      980     2285
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

We test the following hypotheses for the categorical predictors models
(identical for both or and some): H1: For higher prior of the stronger
alternative being true, the inference strength is lower H2: For higher
speaker comptence, the inference strength is higher H3: For higher
listener relevance, the inference strength is higher

``` r
# xor
# test H1
hypothesis(model_xor_cat, "prior1 < 0")
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
    ## 1 (prior1) < 0    -2.07      2.04    -5.44     1.17       5.56      0.85     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# test H2
hypothesis(model_xor_cat, "competence1 > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (competence1) > 0     3.37      2.12    -0.16     6.79      16.49      0.94
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# test H3
hypothesis(model_xor_cat, "relevance1 > 0")
```

    ## Hypothesis Tests for class b:
    ##         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (relevance1) > 0    -0.33      2.05    -3.69     3.07       0.74      0.43
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

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

    ## Warning: There were 4 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ## Warning: Examine the pairs() plot to diagnose sampling problems

``` r
summary(model_some_cat)
```

    ## Warning: There were 4 divergent transitions after warmup. Increasing adapt_delta
    ## above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-
    ## transitions-after-warmup

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
    ## sd(Intercept)      10.86      1.57     7.84    14.00 1.00     1890     2916
    ## sd(prior1)          1.81      1.31     0.06     4.92 1.00     1976     2899
    ## sd(competence1)     6.35      2.10     1.37     9.99 1.00     1070     1021
    ## sd(relevance1)      3.62      2.00     0.23     7.54 1.00      923     2057
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     7.46      2.22     3.06    11.95 1.00     1198      969
    ## 
    ## Population-Level Effects: 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## Intercept                        82.36      1.95    78.54    86.20 1.00
    ## prior1                           -2.04      1.77    -5.53     1.52 1.00
    ## competence1                       7.21      1.78     3.59    10.67 1.00
    ## relevance1                        1.22      1.80    -2.23     4.91 1.00
    ## prior1:competence1                1.69      1.72    -1.80     5.14 1.00
    ## prior1:relevance1                -3.07      1.72    -6.48     0.30 1.00
    ## competence1:relevance1            1.21      1.73    -2.22     4.66 1.00
    ## prior1:competence1:relevance1     1.26      1.75    -2.35     4.66 1.00
    ##                               Bulk_ESS Tail_ESS
    ## Intercept                         3383     4057
    ## prior1                            3292     3897
    ## competence1                       4221     3716
    ## relevance1                        3693     3184
    ## prior1:competence1                3928     4038
    ## prior1:relevance1                 4334     4263
    ## competence1:relevance1            3354     3395
    ## prior1:competence1:relevance1     3801     3363
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma    18.36      1.07    16.33    20.44 1.00     1203     2329
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# some
# test H1
hypothesis(model_some_cat, "prior1 < 0")
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
    ## 1 (prior1) < 0    -2.04      1.77    -4.92     0.89       7.44      0.88     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# test H2
hypothesis(model_some_cat, "competence1 > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (competence1) > 0     7.21      1.78     4.19    10.11       2999         1
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# test H3
hypothesis(model_some_cat, "relevance1 > 0")
```

    ## Hypothesis Tests for class b:
    ##         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (relevance1) > 0     1.22       1.8    -1.63      4.2       3.07      0.75
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

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
  mutate(relevance = factor(relevance, levels = c(1, 0)),
         competence = factor(competence, levels = c(1, 0)),
         prior = factor(prior, levels = c(1, 0))) %>% 
  filter(block_extended == "target") -> d_critical_wide_cat_zScored
# sum code predictors
# low prior: -1, high prior:: 1 
contrasts(d_critical_wide_cat_zScored$prior) <- contr.sum(2)
# low comp : -1, high comp : 1
contrasts(d_critical_wide_cat_zScored$competence) <- contr.sum(2)
# low rel: -1, high rel: 1
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
    ## sd(Intercept)       0.09      0.07     0.00     0.24 1.00     1700     3035
    ## sd(prior1)          0.19      0.09     0.02     0.35 1.00     1072     1641
    ## sd(competence1)     0.23      0.09     0.03     0.39 1.00      897     1320
    ## sd(relevance1)      0.11      0.07     0.00     0.26 1.00     1484     2383
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.33      0.09     0.17     0.53 1.00     1882     2960
    ## 
    ## Population-Level Effects: 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## Intercept                        -0.23      0.07    -0.37    -0.08 1.00
    ## prior1                           -0.10      0.08    -0.25     0.05 1.00
    ## competence1                       0.19      0.08     0.02     0.35 1.00
    ## relevance1                        0.03      0.08    -0.12     0.19 1.00
    ## prior1:competence1                0.01      0.08    -0.15     0.16 1.00
    ## prior1:relevance1                 0.10      0.07    -0.05     0.24 1.00
    ## competence1:relevance1            0.15      0.08    -0.00     0.30 1.00
    ## prior1:competence1:relevance1    -0.05      0.08    -0.20     0.11 1.00
    ##                               Bulk_ESS Tail_ESS
    ## Intercept                         3958     4048
    ## prior1                            4129     4583
    ## competence1                       3932     3599
    ## relevance1                        3961     4084
    ## prior1:competence1                3989     4383
    ## prior1:relevance1                 3654     4289
    ## competence1:relevance1            4437     4215
    ## prior1:competence1:relevance1     3662     3650
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.82      0.04     0.74     0.91 1.00     1354     2851
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# xor
# test H1
hypothesis(model_xor_cat_zScored, "prior1 < 0")
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
    ## 1 (prior1) < 0     -0.1      0.08    -0.23     0.02      10.54      0.91     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# test H2
hypothesis(model_xor_cat_zScored, "competence1 > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (competence1) > 0     0.19      0.08     0.06     0.32      85.96      0.99
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# test H3
hypothesis(model_xor_cat_zScored, "relevance1 > 0")
```

    ## Hypothesis Tests for class b:
    ##         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (relevance1) > 0     0.03      0.08    -0.09     0.16       1.95      0.66
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

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
    ## sd(Intercept)       0.10      0.07     0.00     0.24 1.00     2005     2803
    ## sd(prior1)          0.07      0.05     0.00     0.18 1.00     2945     3355
    ## sd(competence1)     0.16      0.09     0.01     0.33 1.01     1279     2191
    ## sd(relevance1)      0.14      0.08     0.01     0.30 1.00     1372     2137
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.27      0.08     0.11     0.45 1.00     1672     1440
    ## 
    ## Population-Level Effects: 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## Intercept                         0.22      0.07     0.09     0.35 1.00
    ## prior1                           -0.09      0.07    -0.21     0.04 1.00
    ## competence1                       0.33      0.07     0.19     0.46 1.00
    ## relevance1                        0.03      0.07    -0.10     0.17 1.00
    ## prior1:competence1                0.05      0.07    -0.08     0.19 1.00
    ## prior1:relevance1                -0.12      0.07    -0.25     0.01 1.00
    ## competence1:relevance1            0.09      0.07    -0.05     0.22 1.00
    ## prior1:competence1:relevance1     0.05      0.07    -0.08     0.18 1.00
    ##                               Bulk_ESS Tail_ESS
    ## Intercept                         5241     4016
    ## prior1                            5436     4725
    ## competence1                       5422     4176
    ## relevance1                        5205     4450
    ## prior1:competence1                5606     4823
    ## prior1:relevance1                 5415     4121
    ## competence1:relevance1            5637     4320
    ## prior1:competence1:relevance1     5385     4284
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.82      0.04     0.74     0.89 1.00     2423     3234
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# some
# test H1
hypothesis(model_some_cat_zScored, "prior1 < 0")
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
    ## 1 (prior1) < 0    -0.09      0.07    -0.19     0.02       9.64      0.91     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# test H2
hypothesis(model_some_cat_zScored, "competence1 > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (competence1) > 0     0.33      0.07     0.21     0.44        Inf         1
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# test H3
hypothesis(model_some_cat_zScored, "relevance1 > 0")
```

    ## Hypothesis Tests for class b:
    ##         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (relevance1) > 0     0.03      0.07    -0.08     0.14       2.06      0.67
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

#### Single predictor z-scored models with categorical predictors

For xor:

``` r
model_xor_cat_zScored_pri <- brm(
  response_centered ~ prior + 
    (1 + prior | submission_id) +
    (1 | title),
  data = d_critical_wide_cat_zScored %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_cat_zScored_pri)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: response_centered ~ prior + (1 + prior | submission_id) + (1 | title) 
    ##    Data: d_critical_wide_cat_zScored %>% filter(main_type = (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)             0.08      0.06     0.00     0.22 1.00     2050
    ## sd(prior1)                0.18      0.09     0.01     0.35 1.00     1047
    ## cor(Intercept,prior1)     0.13      0.55    -0.90     0.96 1.00     1257
    ##                       Tail_ESS
    ## sd(Intercept)             2114
    ## sd(prior1)                1922
    ## cor(Intercept,prior1)     1973
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.41      0.08     0.28     0.59 1.00     2390     3631
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -0.22      0.09    -0.40    -0.05 1.00     2893     3319
    ## prior1       -0.11      0.09    -0.29     0.07 1.00     3120     4148
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.87      0.04     0.80     0.94 1.00     2804     3590
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
model_xor_cat_zScored_comp <- brm(
  response_centered ~ competence + 
    (1 + competence | submission_id) +
    (1 | title),
  data = d_critical_wide_cat_zScored %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_cat_zScored_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: response_centered ~ competence + (1 + competence | submission_id) + (1 | title) 
    ##    Data: d_critical_wide_cat_zScored %>% filter(main_type = (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                  0.09      0.06     0.00     0.23 1.00     2070
    ## sd(competence1)                0.25      0.09     0.04     0.41 1.00     1330
    ## cor(Intercept,competence1)    -0.22      0.53    -0.97     0.89 1.00      598
    ##                            Tail_ESS
    ## sd(Intercept)                  2772
    ## sd(competence1)                1726
    ## cor(Intercept,competence1)     1468
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.35      0.08     0.20     0.53 1.00     1923     3385
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept      -0.23      0.08    -0.39    -0.09 1.00     4348     4163
    ## competence1     0.19      0.08     0.03     0.36 1.00     4494     4028
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.86      0.04     0.79     0.93 1.00     3216     4115
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
model_xor_cat_zScored_rel <- brm(
  response_centered ~ relevance + 
    (1 + relevance | submission_id) +
    (1 | title),
  data = d_critical_wide_cat_zScored %>% filter(main_type == "xor"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_xor_cat_zScored_rel)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: response_centered ~ relevance + (1 + relevance | submission_id) + (1 | title) 
    ##    Data: d_critical_wide_cat_zScored %>% filter(main_type = (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                 0.08      0.06     0.00     0.22 1.00     2774
    ## sd(relevance1)                0.11      0.07     0.01     0.27 1.00     1805
    ## cor(Intercept,relevance1)    -0.00      0.58    -0.95     0.95 1.00     3270
    ##                           Tail_ESS
    ## sd(Intercept)                 3490
    ## sd(relevance1)                3275
    ## cor(Intercept,relevance1)     4012
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.42      0.08     0.28     0.60 1.00     2170     3083
    ## 
    ## Population-Level Effects: 
    ##            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     -0.22      0.09    -0.40    -0.05 1.00     2842     3748
    ## relevance1     0.05      0.09    -0.13     0.24 1.00     2940     3078
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.88      0.03     0.82     0.95 1.00     5321     4191
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

For some:

``` r
model_some_cat_zScored_pri <- brm(
  response_centered ~ prior + 
    (1 + prior | submission_id) +
    (1 | title),
  data = d_critical_wide_cat_zScored %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

    ## Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ## Warning: Examine the pairs() plot to diagnose sampling problems

``` r
summary(model_some_cat_zScored_pri)
```

    ## Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta
    ## above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-
    ## transitions-after-warmup

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: response_centered ~ prior + (1 + prior | submission_id) + (1 | title) 
    ##    Data: d_critical_wide_cat_zScored %>% filter(main_type = (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)             0.09      0.07     0.00     0.24 1.00     1784
    ## sd(prior1)                0.07      0.05     0.00     0.18 1.00     2743
    ## cor(Intercept,prior1)     0.08      0.58    -0.94     0.96 1.00     4592
    ##                       Tail_ESS
    ## sd(Intercept)             2628
    ## sd(prior1)                2849
    ## cor(Intercept,prior1)     3982
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.46      0.08     0.32     0.64 1.00     1807     3598
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.21      0.09     0.03     0.38 1.00     2685     3740
    ## prior1       -0.08      0.09    -0.26     0.11 1.00     2239     3431
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.84      0.03     0.78     0.91 1.00     5663     3610
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
model_some_cat_zScored_comp <- brm(
  response_centered ~ competence + 
    (1 + competence | submission_id) +
    (1 | title),
  data = d_critical_wide_cat_zScored %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

``` r
summary(model_some_cat_zScored_comp)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: response_centered ~ competence + (1 + competence | submission_id) + (1 | title) 
    ##    Data: d_critical_wide_cat_zScored %>% filter(main_type = (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                  0.17      0.07     0.02     0.31 1.00      908
    ## sd(competence1)                0.21      0.08     0.04     0.35 1.00      931
    ## cor(Intercept,competence1)    -0.71      0.35    -0.99     0.42 1.01      947
    ##                            Tail_ESS
    ## sd(Intercept)                  1295
    ## sd(competence1)                 956
    ## cor(Intercept,competence1)     1145
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.30      0.07     0.18     0.46 1.00     2076     3165
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       0.22      0.07     0.07     0.36 1.00     3667     4367
    ## competence1     0.33      0.07     0.18     0.47 1.00     3514     3835
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.80      0.04     0.73     0.88 1.00     1378     2854
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
model_some_cat_zScored_rel <- brm(
  response_centered ~ relevance + 
    (1 + relevance | submission_id) +
    (1 | title),
  data = d_critical_wide_cat_zScored %>% filter(main_type == "some"),
  family = "gaussian",
  cores = 4,
  iter = 3000
)
```

    ## Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ## Warning: Examine the pairs() plot to diagnose sampling problems

``` r
summary(model_some_cat_zScored_rel)
```

    ## Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta
    ## above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-
    ## transitions-after-warmup

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: response_centered ~ relevance + (1 + relevance | submission_id) + (1 | title) 
    ##    Data: d_critical_wide_cat_zScored %>% filter(main_type = (Number of observations: 396) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 99) 
    ##                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                 0.09      0.06     0.00     0.24 1.00     2227
    ## sd(relevance1)                0.12      0.08     0.01     0.29 1.00     1549
    ## cor(Intercept,relevance1)     0.12      0.55    -0.93     0.96 1.00     3290
    ##                           Tail_ESS
    ## sd(Intercept)                 3016
    ## sd(relevance1)                3210
    ## cor(Intercept,relevance1)     3941
    ## 
    ## ~title (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.47      0.08     0.33     0.66 1.00     2328     3594
    ## 
    ## Population-Level Effects: 
    ##            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept      0.21      0.10     0.02     0.40 1.00     3990     3688
    ## relevance1     0.04      0.10    -0.15     0.23 1.00     4128     4325
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.83      0.03     0.77     0.90 1.00     5237     4637
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

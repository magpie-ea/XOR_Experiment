# XOR Experiment Exclusive 'Or': what matters?


## Goal
Our goal is to get insights into the factors that determine the strength/availability of exclusive readings of 'or'. Textbook accounts (Gricean, neo-Gricean or grammatical) see a role for relevance, and Gricean accounts would also predict that presumptions about the speaker's competence matter. Another possibility is that exclusive readings arise from exhaustifying each individual disjunct. Integrative or holistic approaches (that could be built on top of Gricean, grammaticalist or exhaustification-based accounts) would also acknowledge a role of prior expectations.

Much theoretical work tacitly assumes that exclusive readings of 'or' are scalar implicatures, arising from comparison of 'or' with 'and' and that exclusive readings are readily available. We want to put this received wisdom to the test. Our working hypothesis, to be tested here, is that various contextual factors matter to the availability of exclusive readings.

Previous experimental work on exclusive 'or' has almost entirely (check this) focused on recording truth-judgements of disjunctive sentences with critical conditions comprised of situations in which a corresponding conjunctive sentence would be true. This is an arguably unnaturally artificial test of disjunctive sentences, as they usually suggest some form of speaker uncertainty. We would therefore like to present short contexts for utterances of disjunction so as to manipulate different factors that different theoretical positions would emphasize as more or less relevant:

1. **relevance** : how likely is it that the listener is interested in whether both disjuncts are true
2. **competence** : how likely is it that the speaker knows whether both disjuncts are true
3. **prior** : how likely is it *a priori* (from the point of view of the interpreter/observer of a dialogue) that given that one disjunct is true, the other is also true
4. **exhaustivity** : how strongly would an utterance of just one disjunct convey that the other disjunct is not true

## Experiment initialisation

The information about the vignettes is in `js/vignettes_or.js` and `js/vignettes_some.js`.

Each vignette is an object that contains all the info needed to generate the experiment (background story, statements, control questions, utterances and so on).

There are 16 vignettes overall, two for every type.

The code that generates the experiment is in `js/exp.js`. What it does:
1) Randomly picks 4 vignettes of different type form `js/vignettes_or.js` and 4 from `vignettes_some.js`. The 8 chosen vignettes are all of different type.
2) Randomly picks 4 control questions (out of 6) for each vignette.
3) the order of the first 3 blocks is decided at random (relevance, competence, prior prob.). xor is always last.
4) the vignettes in each block are shuffled so that in every block they are shown in different order.

Every time the page is loaded, the experiment is initialised.
The generated data for each experiment can be seen by typing `rcp.exp.data` in the browser console.

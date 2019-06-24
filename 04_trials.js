// In this file you can specify the trial data for your experiment
// info important to create tirals

// vignettes is an object which contains all necessary information for the trials
let vignettes = [
  {
    "name": "Mrs Gibbs' worry",
    "author": "micha",
    "thoughts": "",
    "type": "hhh",
    "relevance": "1",
    "competence": "1",
    "prior": "1",
    "background": "Mrs Gibbs is worried about her husband's health. Her friend Cindy, who is a waitress at a local restaurant, served Mrs Gibbs' husband yesterday.",
    "utterance_or": "Cindy says to Mrs Gibbs: 'Yesterday your husband had a steak or a beer.",
    "utterance_disj1": "Cindy says to Mrs Gibbs: 'Yesterday your husband had a steak.",
    "utterance_disj2": "Cindy says to Mrs Gibbs: 'Yesterday your husband had a beer.'",
    "question_rel": "It is important for Mrs Gibbs to know whether Mr Gibbs had both a steak and a beer.",
    "question_comp": "Cindy knows whether Mr Gibbs had both a steak and a beer.",
    "question_pri1": "If Mr Gibbs had a steak, it is likely that he also had a beer.",
    "question_pri2": "If Mr Gibbs had a beer, it is likely that he also had a steak.",
    "question_xor": "From what Cindy said we may conclude that Mr Gibbs did not have both a steak and a beer.",
    "question_xorI": "Cindy wants to convey that Mr Gibbs did not have both a steak and a beer.",
    "question_exh1": "From what Cindy said we may conclude that Mr Gibbs did not have a beer as well.",
    "question_exh2": "From what Cindy said we may conclude that Mr Gibbs did not have a steak as well.",
    "test_true1": "Mrs Gibbs did not accompany her husband when Cindy served him.",
    "test_true2": "Cindy was at the restaurant yesterday.",
    "test_false1": "Mrs Gibbs is worried about Cindy's health.",
    "test_false2": "Cindy works as shop assistant.",
    "test_uncertain1": "Cindy is secretly in love with Mrs Gibbs' husband.",
    "test_uncertain2": "Mr Gibbs often eats at the local restaurant.",
    "test_all": "Mrs Gibbs did not accompany her husband when Cindy served him."
  },

  {
    "name": "Mrs Sanders' son",
    "author": "micha",
    "thoughts": "Bob: The verb 'reassured' suggests a non-conjunctive interpretation.",
    "type": "hhl",
    "relevance": "1",
    "competence": "1",
    "prior": "0",
    "background": "During summer camp, every student was allowed at most one main dish for lunch. But some students queued up twice. Mrs Sanders was with her son Ted during the whole lunch break yesterday. A teacher approaches Mrs Sanders to find out how her son behaved during lunch.",
    "utterance_or": "Mrs Sanders says to the teacher: 'Ted had pizza or pasta yesterday.'",
    "utterance_disj1": "Mrs Sanders says to the teacher: 'Ted had pizza yesterday.'",
    "utterance_disj2": "Mrs Sanders says to the teacher: 'Ted had pasta yesterday.'",
    "question_rel": "It is important for the teacher to know whether Ted had both pizza and pasta.",
    "question_comp": "Mrs Sanders knows whether Ted had both pizza and pasta.",
    "question_pri1": "If Ted had pizza, it is likely that he also had pasta.",
    "question_pri2": "If Ted had pasta, it is likely that he also had pizza.",
    "question_xor": "From what Mrs Sanders said we may conclude that Ted did not have both pizza and pasta.",
    "question_xorI": "Mrs Sanders wanted to convey that Ted did not have both pizza and pasta.",
    "question_exh1": "From what Mrs Sanders said we may conclude that Ted did not have pasta as well.",
    "question_exh2": "From what Mrs Sanders said we may conclude that Ted did not have pizza as well.",
    "test_true1": "The teacher is trying to find out who queued up twice.",
    "test_true2": "Ted was at the summer camp.",
    "test_false1": "Ted is Mrs Sanders' husband.",
    "test_false2": "Mrs Sanders is Ted's teacher.",
    "test_uncertain1": "Mrs Sanders had pasta for lunch yesterday.",
    "test_uncertain2": "Mrs Sanders and the teacher are friends from college."
  },

  {
    "name": "Brad's clothes",
    "author": "micha",
    "thoughts": "Bob: Is the conjunction relevant? -> Micha: I thought yes, because just one piece of new clothing would not make him look much less shabby.",
    "type": "hlh",
    "relevance": "1",
    "competence": "0",
    "prior": "1",
    "background": "Brad is notorious for his shabby clothes. His friends Gina and Mandy make frequent jokes about it, much to Brad’s distress. Gina heard that he finally went shopping the other day.",
    "utterance_or": "Gina tells Mandy: 'Brad bought a sweater or a pair of jeans.'",
    "utterance_disj1": "Gina tells Mandy: 'Brad bought a sweater.'",
    "utterance_disj2": "Gina tells Mandy: 'Brad bought a pair of jeans.'",
    "question_rel": "It is important for Mandy to know whether Brad bought both a sweater and a pair of jeans.",
    "question_comp": "Gina knows whether Brad bought both a sweater and a pair of jeans.",
    "question_pri1": "If Brad bought a sweater, it is likely that he also bought a pair of jeans.",
    "question_pri2": "If Brad bought a pair of jeans, it is likely that he also bought a sweater.",
    "question_xor": "From what Gina said we may conclude that Brad did not buy both a sweater and a pair of jeans.",
    "question_xorI": "Gina wants to convey that Brad did not buy both a sweater and a pair of jeans.",
    "question_exh1": "From what Gina said we may conclude that Brad did not buy pair of jeans as well.",
    "question_exh2": "From what Gina said we may conclude that Brad did not buy a sweater as well.",
    "test_true1": "Brad is not happy about the fact that his friends make jokes about his shabby clothes.",
    "test_true2": "Brad likes wearing shabby clothes.",
    "test_false1": "Gina and Mandy accompanied Brad on his shopping trip.",
    "test_false2": "Gina likes the way Brad dresses.",
    "test_uncertain1": "Mandy feels attracted to Brad.",
    "test_uncertain2": "It was Mandy who told Gina that Brad went shopping."
  },

  {
    "name": "Carl's party",
    "author": "micha",
    "thoughts": "Bob: Is the conjunction relevant? -> Micha: my intuition is that Carl's wife would care to know, even if dancing with only one is already bad.",
    "type": "hll",
    "relevance": "1",
    "competence": "0",
    "prior": "0",
    "background": "Many people came to Carl’s garden party yesterday, including his ex-wives Sue and Mary. It was terribly crowded everywhere. John helped out in the kitchen where it was much more quiet during most of the evening.",
    "utterance_or": "Afterwards John told Carl’s wife: 'Carl danced passionately with Sue or Mary.'",
    "utterance_disj1": "Afterwards John told Carl’s wife: 'Carl danced passionately with Sue.'",
    "utterance_disj2": "Afterwards John told Carl’s wife: 'Carl danced passionately with Mary.'",
    "question_rel": "It is important for Carl's wife to know whether Carl danced passionately with both Sue and Mary.",
    "question_comp": "John knows whether Carl danced passionately with both Sue and Mary.",
    "question_pri1": "If Carl danced passionately with Sue, it is likely that he also danced passionately with Mary.",
    "question_pri2": "If Carl danced passionately with Mary, it is likely that he also danced passionately with Sue.",
    "question_xor": "From what John said we may conclude that Carl did not dance passionately with both Sue and Mary.",
    "question_xorI": "John wanted to convey that Carl did not dance passionately with both Sue and Mary.",
    "question_exh1": "From what John said we may conclude that Carl did not dance passionately with Mary as well.",
    "question_exh2": "From what John said we may conclude that Carl did not dance passionately with Sue as well.",
    "test_true1": "John did not spend much time outside.",
    "test_true2": "Carl was married at least twice.",
    "test_false1": "John and Carl, the hosts, are a gay couple.",
    "test_false2": "John enjoys crowded gatherings.",
    "test_uncertain1": "Carl's wife liked the music very much.",
    "test_uncertain2": "John and Sue are a couple."
  },

  {
    "name": "Bill's order",
    "author": "micha",
    "thoughts": "",
    "type": "lhh",
    "relevance": "0",
    "competence": "1",
    "prior": "1",
    "background": "Jake and Bill went jogging together in the afternoon. Afterwards they were very hungry and went to a restaurant.",
    "utterance_or": "Jake later told Bill’s wife: 'Bill ordered a starter or a side dish with his main course.'",
    "utterance_disj1": "Jake later told Bill’s wife: 'Bill ordered a starter with his main course.'",
    "utterance_disj2": "Jake later told Bill’s wife: 'Bill ordered a side dish with his main course.'",
    "question_rel": "It is important for Bill's wife to know whether Bill ordered both a starter and a side dish.",
    "question_comp": "Jake knows whether Bill ordered both a starter and a side dish.",
    "question_pri1": "If Bill ordered a starter, it is likely that he also ordered a side dish.",
    "question_pri2": "If Bill ordered a side dish, it is likely that he also ordered a starter.",
    "question_xor": "From what Jake said we may conclude that Bill did not order both a starter and a side dish.",
    "question_xorI": "Jake wanted to convey that Bill did not order both a starter and a side dish.",
    "question_exh1": "From what Jake said we may conclude that Bill did not order a side dish as well.",
    "question_exh2": "From what Jake said we may conclude that Bill did not order a starter as well.",
    "test_true1": "Jake and Bill spent a lot of time together that day.",
    "test_true2": "Jake and Bill had dinner at the restaurant.",
    "test_false1": "Jake and Bill went to the restaurant before they went jogging.",
    "test_false2": "Jake and Bill played computer games in the afternoon.",
    "test_uncertain1": "Jake ordered a steak.",
    "test_uncertain2": "Bill is vegetarian."
  },

  {
    "name": "Tony's clubbing",
    "author": "micha",
    "thoughts": "Bob: Not sure whether 'test_false' is really false or rather uncertain.",
    "type": "lhl",
    "relevance": "0",
    "competence": "1",
    "prior": "0",
    "background": "Tony loves to dance to electronic music, preferably Techno. The simpler and more monotonic, the better. He contemplates going to club Macabre tonight, but he has no idea what music they play. He phones his friend Rob who occasionally works at club Macabre as a bartender.",
    "utterance_or": "Rob tells Tony: 'They play Jazz or Metal tonight.'",
    "utterance_disj1": "Rob tells Tony: 'They play Jazz tonight.'",
    "utterance_disj2": "Rob tells Tony: 'They play Metal tonight.'",
    "question_rel": "It is important for Tony to know whether they play both Jazz and Metal tonight.",
    "question_comp": "Rob knows whether they play both Jazz and Metal at the club tonight.",
    "question_pri1": "If they play Jazz at the club tonight, it is likely that they also play Metal.",
    "question_pri2": "If they play Metal at the club tonight, it is likely that they also play Jazz.",
    "question_xor": "From what Rob said we may conclude that they do not play both Jazz and Metal tonight.",
    "question_xorI": "Rob wanted to convey that they do not play both Jazz and Metal tonight.",
    "question_exh1": "From what Rob said we may conclude that they do not play Metal as well.",
    "question_exh2": "From what Rob said we may conclude that they do not play Jazz as well.",
    "test_true1": "Techno is quite different from Jazz and from Metal.",
    "test_true2": "Tony feels like having a night out tonight.",
    "test_false1": "Tony has to work at club Macabre tonight.",
    "test_false2": "Rob works as a DJ at the club.",
    "test_uncertain1": "Rob is a big fan of Techno music as well.",
    "test_uncertain2": "Tony and Rob used to play hockey together."
  },

  {
    "name": "Joshua's delay",
    "author": "micha",
    "thoughts": "Bob: Is it likely to both have lunch and do homework? -> Micha: added information that the parents are strict, hope that pushes the likelihood, but yes, not very strong item. Bob: Maybe add 'both' to question_rel and question_comp",
    "type": "llh",
    "relevance": "0",
    "competence": "0",
    "prior": "1",
    "background": "Kevin, Cory and Joshua are 7-year-olds from the same neighborhood. They are friends but argue a lot. Yesterday Joshua, who has very strict parents, was later than usual to arrive at their hang-out. Kevin thinks that Joshua doesn’t like to play with them anymore.",
    "utterance_or": "Cory says to Kevin: 'Joshua had to eat lunch or do his homework.'",
    "utterance_disj1": "Cory says to Kevin: 'Joshua had to eat lunch.'",
    "utterance_disj2": "Cory says to Kevin: 'Joshua had to do his homework.'",
    "question_rel": "It is important for Kevin to know whether Joshua had to both eat lunch and do his homework.",
    "question_comp": "Cory knows whether Joshua had to both eat lunch and do his homework.",
    "question_pri1": "If Joshua had to do his homework, it is likely that he also had to eat lunch.",
    "question_pri2": "If Joshua had to do eat lunch, it is likely that he also had to do his homework.",
    "question_xor": "From what Cory said we may conclude that Joshua did not have to both eat lunch and do his homework.",
    "question_xorI": "Cory wanted to convey that Joshua did not have to both eat lunch and do his homework.",
    "question_exh1": "From what Cory said we may conclude that Joshua did not have to do his homework as well.",
    "question_exh2": "From what Cory said we may conclude that Joshua did not have to eat lunch as well.",
    "test_true1": "Kevin and Cory expected Joshua to arrive earlier.",
    "test_true2": "Joshua's parents didn't let him go and play earlier.",
    "test_false1": "Kevin, Cory and Joshua are brothers.",
    "test_false2": "Joshua lives far away from Kevin and Cory and that is why he was late.",
    "test_uncertain1": "Kevin, Cory and Joshua do not like hanging out with girls.",
    "test_uncertain2": "Kevin, Cory and Joshua usually play hide and seek."
  },

  {
    "name": "Jimmy's trip",
    "author": "micha",
    "thoughts": "Bob: Is it unlikely to visit both the Statue of Liberty and the Empire State Building?",
    "type": "lll",
    "relevance": "0",
    "competence": "0",
    "prior": "0",
    "background": "Jimmy skipped school to go on a day-trip to New York City last week. Having to be back in the afternoon, he barely had one hour to spend in the city. Jimmy's younger brother, who is very talkative even if he doesn't know what he is talking about, told the neighbors about the trip afterwards.",
    "utterance_or": "Jimmy's younger brother told the neighbors: 'Jimmy visited the Statue of Liberty or the Empire State Building.'",
    "utterance_disj1": "Jimmy's younger brother told the neighbors: 'Jimmy visited the Statue of Liberty.'",
    "utterance_disj2": "Jimmy's younger brother told the neighbors: 'Jimmy visited the Empire State Building.'",
    "question_rel": "It is important for the neighbors to know whether Jimmy visited both the Statue of Liberty and the Empire State Building.",
    "question_comp": "Jimmy's brother knows whether Jimmy visited both the Statue of Liberty and the Empire State Building.",
    "question_pri1": "If Jimmy visited the Statue of Liberty, it is likely that he also visited the Empire State Building.",
    "question_pri2": "If Jimmy visited the Empire State Building, it is likely that he also visited the Statue of Liberty.",
    "question_xor": "From what the brother said we may conclude that Jimmy did not visit both the Statue of Liberty and the Empire State Building.",
    "question_xorI": "Jimmy's brother wanted to convey that Jimmy did not visit both the Statue of Liberty and the Empire State Building.",
    "question_exh1": "From what the brother said we may conclude that Jimmy did not visit the Empire State Building as well.",
    "question_exh2": "From what the brother said we may conclude that Jimmy did not visit the Statue of Liberty as well.",
    "test_true1": "Jimmy had to rush to be back home and did not have much time to spend in the city.",
    "test_true2": "Jimmy's brother knew that he skipped school.",
    "test_false1": "Jimmy has a younger sister.",
    "test_false2": "Jimmy went on a school trip to New York City.",
    "test_uncertain1": "Jimmy had told his friends about his plans to visit New York City before.",
    "test_uncertain2": "The neighbours sometimes look after Jimmy's brother."
  },

  {
    "name": "Danny's racket",
    "author": "bob",
    "thoughts": "",
    "type": "hhh",
    "relevance": "1",
    "competence": "1",
    "prior": "1",
    "background": "Danny and Alex reserved a squash court but Alex still has to buy a racket and a pair of shoes. Danny is talking to Alex's girlfriend Jill who just went to the sports store with him.",
    "utterance_or": "Jill says to Danny: 'Alex bought a racket or a pair of shoes.'",
    "utterance_disj1": "Jill says to Danny: 'Alex bought a racket.'",
    "utterance_disj2": "Jill says to Danny: 'Alex bought a pair of shoes.'",
    "question_rel": "It is important for Danny to know whether Alex bought both a racket and a pair of shoes.",
    "question_comp": "Alex's girlfriend knows whether Alex bought both a racket and a pair of shoes.",
    "question_pri1": "If Alex bought a racket, it is likely that he also bought a pair of shoes.",
    "question_pri2": "If Alex bought a pair of shoes, it is likely that he also bought a racket.",
    "question_xor": "From what Alex's girlfriend said we may conclude that Alex did not buy both a racket and a pair of shoes.",
    "question_xorI": "Alex's girlfriend wanted to convey that Alex did not buy both a racket and a pair of shoes.",
    "question_exh1": "From what Alex's girlfriend said we may conclude that Alex did not buy a pair of shoes as well.",
    "question_exh2": "From what Alex's girlfriend said we may conclude that Alex did not buy a racket as well.",
    "test_true1": "Alex needs to buy some equipment before he can play squash with Danny.",
    "test_true2": "Alex went to the sports store.",
    "test_false1": "Danny and Alex went to the sports store together.",
    "test_false2": "Jill is Danny's girlfriend.",
    "test_uncertain1": "Danny and Alex have known each other since high school.",
    "test_uncertain2": "Jill also plays squash.",
  },

  {
    "name": "Pete's accident",
    "author": "bob",
    "thoughts": "",
    "type": "hhl",
    "relevance": "1",
    "competence": "1",
    "prior": "0",
    "background": "Pete was playing on the schoolyard during lunch break when he fell off the slide. His teacher drove him to the hospital immediately and called Pete's mother from there.",
    "utterance_or": "The teacher told Pete's mother: 'Pete broke his arm or his leg.'",
    "utterance_disj1": "The teacher told Pete's mother: 'Pete broke his arm.'",
    "utterance_disj2": "The teacher told Pete's mother: 'Pete broke his leg.'",
    "question_rel": "It is important for Pete's mother to know whether Pete broke both his arm and his leg.",
    "question_comp": "The teacher knows whether Pete broke both his arm and his leg.",
    "question_pri1": "If Pete broke his arm, it is likely that he also broke his leg.",
    "question_pri2": "If Pete broke his leg, it is likely that he also broke his arm.",
    "question_xor": "From what Pete's teacher said we may conclude that Pete did not break both his arm and his leg.",
    "question_xorI": "Pete's teacher wanted to convey that Pete did not break both his arm and his leg.",
    "question_exh1": "From what Pete's teacher said we may conclude that Pete did not break his leg as well.",
    "question_exh2": "From what Pete's teacher said we may conclude that Pete did not break his arm as well.",
    "test_true1": "Pete's mother was not there when he fell off the slide.",
    "test_true2": "Pete's teacher was worried about him.",
    "test_false1": "Pete was injured while playing in the garden.",
    "test_false2": "Pete was taken back home after he fell off the slide.",
    "test_uncertain1": "Pete's teacher saw him fall off the slide.",
    "test_uncertain2": "The slide was old and shaky."
  },

  {
    "name": "Gigi's exams",
    "author": "bob",
    "thoughts": "",
    "type": "hlh",
    "relevance": "1",
    "competence": "0",
    "prior": "1",
    "background": "Recently, Gigi took exams in Physics and Chemistry. She studied day and night and even paid for private tutoring. Carrie just heard some rumors from her fellow students.",
    "utterance_or": "Carrie tells Gigi's best friend: 'Gigi passed her Physics exam or her Chemistry exam.'",
    "utterance_disj1": "Carrie tells Gigi's best friend: 'Gigi passed her Physics exam.'",
    "utterance_disj2": "Carrie tells Gigi's best friend: 'Gigi passed her Chemistry exam.'",
    "question_rel": "It is important for Gigi's best friend to know whether Gigi passed both her Physics exam and her Chemistry exam.",
    "question_comp": "Carrie knows whether Gigi passed both her Physics exam and her Chemistry exam.",
    "question_pri1": "If Gigi passed her Physics exam, it is likely that she also passed her Chemistry exam.",
    "question_pri2": "If Gigi passed her Chemistry exam, it is likely that she also passed her Physics exam.",
    "question_xor": "From what Carrie said we may conclude that Gigi did not pass both her Physics exam and her Chemistry exam.",
    "question_xorI": "Carrie wanted to convey that Gigi did not pass both her Physics exam and her Chemistry exam.",
    "question_exh1": "From what Carrie said we may conclude that Gigi did not pass her Chemistry exam as well.",
    "question_exh2": "From what Carrie said we may conclude that Gigi did not pass her Physics exam as well.",
    "test_true1": "Gigi studied hard to pass her exams.",
    "test_true2": "Gigi and Carrie are students.",
    "test_false1": "Gigi hired a private tutor to help her with her Biology and Chemistry exams.",
    "test_false2": "Gigi got help from Carrie with the exams.",
    "test_uncertain1": "Gigi aspires to a career in science.",
    "test_uncertain2": "Gigi passed both exams."
  },

  {
    "name": "Leo's wife",
    "author": "bob",
    "thoughts": "",
    "type": "hll",
    "relevance": "1",
    "competence": "0",
    "prior": "0",
    "background": "Leo is sponsoring a golf tournament. In particular, he provides a luxurious car for any hole-in-one. When Leo arrives at the tournament, his friend Hans comes up to him to tell about the events so far.",
    "utterance_or": "Hans tells Leo: 'I heard someone just hit a hole-in-one on the first or second hole.'",
    "utterance_disj1": "Hans tells Leo: 'I heard someone just hit a hole-in-one on the first hole'",
    "utterance_disj2": "Hans tells Leo: 'I heard someone just hit a hole-in-one on the second hole.''",
    "question_rel": "It is important for Leo to know whether someone made a hole-in-one on both the first and the second hole.",
    "question_comp": "Hans knows whether someone just made a hole-in-one on both the first and the second hole.",
    "question_pri1": "If someone made a hole-in-one on the first hole, it is likely that she also made one on the second hole.",
    "question_pri2": "If someone made a hole-in-one on the second hole, it is likely that she also made one on the first hole.",
    "question_xor": "From what Hans said we may conclude that the player in question didn't make a hole-in-one on both the first and the second hole.",
    "question_xorI": "Hans wanted to convey that the player in question didn't make a hole-in-one on both the first and the second hole.",
    "question_exh1": "From what Hans said we may conclude that the player in question didn't make a hole-in-one on the second hole as well.",
    "question_exh2": "From what Hans said we may conclude that the player in question didn't make a hole-in-one on the first hole as well",
    "test_true1": "Leo is wealthy.",
    "test_true2": "Hans knows what happened in the tournament.",
    "test_false1": "Leo is sponsoring a tennis tournament.",
    "test_false2": "Leo was at the tournament from the very beginning.",
    "test_uncertain1": "Leo is an avid golf player.",
    "test_uncertain2": "Hans took part in the tournament."
  },

  {
    "name": "Chloe's holiday",
    "author": "bob",
    "thoughts": "Micha added the friend's names to the background story, because otherwise the prior questions are weird, when presented without the utterance. -- Susan bringing lotion and Amelia doing so are likely independent events; that makes the prior question a bit odd. Maybe phrase it unconditionally in case of purported independence.",
    "type": "lhh",
    "relevance": "0",
    "competence": "1",
    "prior": "1",
    "background": "Chloe is telling her father about her holidays with her two friends Susan and Amelia. Chloe forgot to bring suntan lotion.",
    "utterance_or": "Chloe tells her father: 'Susan or Amelia brought some extra suntan lotion I could use.'",
    "utterance_disj1": "Chloe tells her father: 'Susan brought some extra suntan lotion I could use.'",
    "utterance_disj2": "Chloe tells her father: 'Amelia brought some extra suntan lotion I could use.'",
    "question_rel": "It is important for Chloe's father to know whether both Susan and Amelia brought some extra suntan lotion.",
    "question_comp": "Chloe knows whether both Susan and Amelia brought some extra suntan lotion.",
    "question_pri1": "If Susan brought some extra suntan lotion, it is likely that Amelia brought some as well.",
    "question_pri2": "If Amelia brought some extra suntan lotion, it is likely that Susan brought some as well.",
    "question_xor": "From what Chloe said we may conclude that Susan and Amelia did not both bring some extra suntan lotion.",
    "question_xorI": "Chloe wanted to convey that Susan and Amelia did not both bring some extra suntan lotion.",
    "question_exh1": "From what Chloe said we may conclude that Amelia did not bring some extra suntan lotion as well.",
    "question_exh2": "From what Chloe said we may conclude that Susan did not bring some extra suntan lotion as well.",
    "test_true1": "Chloe recently got back from holidays.",
    "test_true2": "Amelia and Susan know each other.",
    "test_false1": "Chloe went on holiday with a big group of her friends.",
    "test_false2": "Chloe's father went on holiday with her.",
    "test_uncertain1": "Chloe forgot about bringing some beach towels, too.",
    "test_uncertain2": "Amelia spent the whole holiday indoors."
  },

  {
    "name": "Lily's husband",
    "author": "bob",
    "thoughts": "",
    "type": "lhl",
    "relevance": "0",
    "competence": "1",
    "prior": "0",
    "background": "Lily is worried that her husband Eric is still drinking despite the doctor's orders. She phones up Eric's drinking buddy Bill who went to a bar with him last night.",
    "utterance_or": "Bill tells Lily: 'Your husband drank whisky or vodka.'",
    "utterance_disj1": "Bill tells Lily: 'Your husband drank whisky.'",
    "utterance_disj2": "Bill tells Lily: 'Your husband drank vodka.'",
    "question_rel": "It is important for Lily to know whether her husband drank both whisky and vodka.",
    "question_comp": "Bill knows whether Eric drank both whisky and vodka.",
    "question_pri1": "If Eric drank whisky, it is likely that he drank vodka as well.",
    "question_pri2": "If Eric drank vodka, it is likely that he drank whisky as well.",
    "question_xor": "From what Bill said we may conclude that Eric did not drink both whisky and vodka.",
    "question_xorI": "Bill wanted to convey that Eric did not drink both whisky and vodka.",
    "question_exh1": "From what Bill said we may conclude that Eric did not drink vodka as well.",
    "question_exh2": "From what Bill said we may conclude that Eric did not drink whisky as well.",
    "test_true1": "Eric's doctor told him to stop drinking alcohol.",
    "test_true2": "Bill drinks alcohol.",
    "test_false1": "Eric and Lily went to a bar together last night.",
    "test_false2": "Bill phoned Lily to tell him whether Eric had alcohol last night.",
    "test_uncertain1": "Eric has a problem with his liver for which he has to stop drinking alcohol.",
    "test_uncertain2": "Eric got back home rather late."
  },

  {
    "name": "Gerald's graduation",
    "author": "bob",
    "thoughts": "",
    "type": "llh",
    "relevance": "0",
    "competence": "0",
    "prior": "1",
    "background": "Nico and Rick are talking about their mutual friend Gerald. Gerald has been studying for ages but Nico heard he recently graduated.",
    "utterance_or": "Nico says to Rick: 'His mother or his father attended the ceremony.'",
    "utterance_disj1": "Nico says to Rick: 'His mother attended the ceremony.'",
    "utterance_disj2": "Nico says to Rick: 'His father attended the ceremony.'",
    "question_rel": "It is important for Rick to know whether both Gerald's mother and his father attended the ceremony.",
    "question_comp": "Nico knows whether both Gerald's mother and his father attended the ceremony.",
    "question_pri1": "If Gerald's mother attended the ceremony, it is likely that his father attended as well.",
    "question_pri2": "If Gerald's father attended the ceremony, it is likely that his mother attended as well.",
    "question_xor": "From what Nico said we may conclude that Gerald's mother and his father did not both attend the ceremony.",
    "question_xorI": "Nico wanted to convey that Gerald's mother and his father did not both attend the ceremony.",
    "question_exh1": "From what Nico said we may conclude that Gerald's father did not attend the ceremony as well.",
    "question_exh2": "From what Nico said we may conclude that Gerald's mother did not attend the ceremony as well.",
    "test_true1": "Gerald is friends with both Nico and Rick.",
    "test_true2": "Gerald took longer than expected to graduate.",
    "test_false1": "Nico finally graduated after studying for a long time.",
    "test_false2": "Gerald still hasn't finish his studies.",
    "test_uncertain1": "Gerald took almost ten years to graduate.",
    "test_uncertain2": "Rick and Gerald both studied philosophy."
  },

  {
    "name": "Harold's pet",
    "author": "bob",
    "thoughts": "",
    "type": "lll",
    "relevance": "0",
    "competence": "0",
    "prior": "0",
    "background": "Richard and Tom are talking about Harold. Harold has been wanting a pet for a long time but his girlfriend wouldn't let him. Richard heard she finally gave in.",
    "utterance_or": "Richard says to Tom: 'Harold bought a cat or a dog.'",
    "utterance_disj1": "Richard says to Tom: 'Harold bought a cat.'",
    "utterance_disj2": "Richard says to Tom: 'Harold bought a dog.'",
    "question_rel": "It is important for Tom to know whether Harold bought both a cat and a dog.",
    "question_comp": "Richard knows whether Harold bought both a cat and a dog.",
    "question_pri1": "If Harold bought a cat, it is likely that he bought a dog as well.",
    "question_pri2": "If Harold bought a dog, it is likely that he bought a cat as well.",
    "question_xor": "From what Richard said we may conclude that Harold did not buy both a cat and a dog.",
    "question_xorI": "Richard wanted to convey that Harold did not buy both a cat and a dog.",
    "question_exh1": "From what Richard said we may conclude that Harold did not buy a dog as well.",
    "question_exh2": "From what Richard said we may conclude that Harold did not buy a cat as well.",
    "test_true1": "Harold's girlfriend didn't want a pet in her house before.",
    "test_true2": "Harold got a pet.",
    "test_false1": "Harold is single.",
    "test_false2": "Harold hates animals.",
    "test_uncertain1": "Harold's girlfriend gave in because Harold kept nagging her about wanting a pet.",
    "test_uncertain2": "Richard and Tom haven't met Harold's girlfriend in person."
  }
];

// create 4 trail blocks
let comp_block;
let pri_block;
let rel_block;
let xor_block;
let example;

// add additional attribute to each item, which includes all 6 test questions,
vignettes = assign_questions(vignettes);

//chose randomly 8 stories, for each participant new
let chosen_stories = _.sampleSize(vignettes, 9);

let eight_stories = chosen_stories.slice(0, 8);
//let example_trial = chosen_stories.slice(8,9);

console.log(eight_stories);


// create random order of 8 stories, different for each block
comp_block = _.cloneDeep(eight_stories);
rel_block = _.cloneDeep(eight_stories);
pri_block = _.cloneDeep(eight_stories);
xor_block = _.cloneDeep(eight_stories);


const questions = ["Joe went shopping without his wife Sue.", "Sue was very happy about the flowers.", "Joe and Sue are divorced.", "Joe and Sue have no children."];
const explanations = ["The background story implies this statement, so we rate it as certainly true.", "The background story makes this very likely, but we cannot be sure.", "The background story suggests the contrary, but we cannot be sure.", "The background story implies the contrary, so we rate this as certainly false."];


const trials_practice = [{
    title: 'Joe´s shopping',
    QUD: '<font size="4" color= "#00BFFF"> EXAMPLE </font>  <br><br> <font size="6"> Joe´s shopping </font>  <br /> ' + "<br /> Joe went shopping yesterday, while his wife Sue was at home with the kids. He bought flowers for his wife on the way home.",
    optionLeft: "certainly false",
    optionRight: "certainly true",
    condition: "example",
    question: `<br> ------------------------------- <br/>` + questions[0] + ' <br /> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/> <br/> <i> <font color="8B0000">' + explanations[0] + '</i> </font>',
},
  {
    title: 'Joe´s shopping',
    QUD: '<font size="4" color= "#00BFFF"> EXAMPLE </font>  <br><br> <font size="6"> Joe´s shopping </font>  <br /> ' + "<br /> Joe went shopping yesterday, while his wife Sue was at home with the kids. He bought flowers for his wife on the way home.",
    optionLeft: "certainly false",
    optionRight: "certainly true",
    condition: "example",
    question: `<br> ------------------------------- <br/>` + questions[1] + ' <br /> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/> <br/> <i> <font color="8B0000">' + explanations[1] + '</i></font>'
},
  {
    title: 'Joe´s shopping',
    QUD: '<font size="4" color= "#00BFFF"> EXAMPLE </font>  <br><br> <font size="6"> Joe´s shopping </font>  <br /> ' + "<br /> Joe went shopping yesterday, while his wife Sue was at home with the kids. He bought flowers for his wife on the way home.",
    optionLeft: "certainly false",
    optionRight: "certainly true",
    condition: "example",
    question: `<br> ------------------------------- <br/>` + questions[2] + '<br /> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/> <br/> <i> <font color="8B0000">' + explanations[2] + '</i></font>'
},
  {
    title: 'Joe´s shopping',
    QUD: '<font size="4" color= "#00BFFF"> EXAMPLE </font>  <br><br> <font size="6"> Joe´s shopping </font>  <br /> ' + "<br /> Joe went shopping yesterday, while his wife Sue was at home with the kids. He bought flowers for his wife on the way home.",
    optionLeft: "certainly false",
    optionRight: "certainly true",
    condition: "example",
    question: `<br> ------------------------------- <br/>` + questions[3] + ' <br /> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/> <br/> <i> <font color="8B0000">' + explanations[3] + '</i></font>'
}];


// call function to create each block with the shuffled 8 eight_stories and the correct string
// flatten the array, as it is encapusulated
comp_block = _.flatten(_.map(_.shuffle(eight_stories), function (x) {
  return (create_block(x, "comp"));
}));
pri_block = _.flatten(_.map(_.shuffle(eight_stories), function (x) {
  return (create_block(x, "pri"));
}));
rel_block = _.flatten(_.map(_.shuffle(eight_stories), function (x) {
  return (create_block(x, 'rel'));
}));
xor_block = _.flatten(_.map(_.shuffle(eight_stories), function (x) {
  return (create_block(x, 'xor'));
}));

// In this file you can instantiate your views
// We here first instantiate wrapping views, then the trial views


/** Wrapping views below

* Obligatory properties

    * trials: int - the number of trials this view will appear
    * name: string

*Optional properties
    * buttonText: string - the text on the button (default: 'next')
    * text: string - the text to be displayed in this view
    * title: string - the title of this view

    * More about the properties and functions of the wrapping views - https://babe-project.github.io/babe-docs/01_designing_experiments/01_template_views/#wrapping-views

*/

// Every experiment should start with an intro view. Here you can welcome your participants and tell them what the experiment is about
const intro = babeViews.view_generator("intro",{
    trials: 1,
    name: 'intro',
    // If you use JavaScripts Template String `I am a Template String`, you can use HTML <></> and javascript ${} inside
    text:  `Welcome to an XPrag Experiment
            <br />
            <br />
            Thank you for your participation in our study!
            Your anonymous data makes an important contribution to our understanding of human learning and memory.
            <br />
            <br />
            Legal information:
            By answering the following questions, you are participating in a study
            being performed by scientists from the University of Osnabrueck.
            <br />
            <br />
            You must be at least 18 years old to participate.
            <br />
            <br />
            Your participation in this research is voluntary.
            You may decline to answer any or all of the following questions.
            You may decline further participation, at any time, without adverse consequences.
            <br />
            <br />
            Your anonymity is assured; the researchers who have requested your
            participation will not receive any personal information about you.`,
   buttonText: 'Begin the experiment'
});

// For most tasks, you need instructions views
const instructions = babeViews.view_generator("instructions",{
    trials: 1,
    name: 'instrucions',
    title: 'General Instructions',
    text:  `In the following, you will be presented with 8 short stories.
            Please read them very carefully, even if they appear to be repeated and you think that you remember them well enough.
            We ask you to rate 2 or 3 statements about each short story.
            Please indicate, using an adjustable slider bar, how likely you think a statement
            is true based on the story you read.
            <br />
            <br />
            The experiment consists of 4 blocks with pauses between.
            Within each block you are asked to rate different kinds of statements.
            <br />
            <br />
            The background story is in a grey box.
            The statements to be rated are preceded by ?
            <br />
            <br />
            An example is provided next.`,
    buttonText: 'next'
});


// In the post test questionnaire you can ask your participants addtional questions
const post_test = babeViews.view_generator("post_test",{
    trials: 1,
    name: 'post_test',
    title: 'Additional information',
    text: 'Answering the following questions is optional, but your answers will help us analyze our results.'

    // You can change much of what appears here, e.g., to present it in a different language, as follows:
    // buttonText: 'Weiter',
    // age_question: 'Alter',
    // gender_question: 'Geschlecht',
    // gender_male: 'männlich',
    // gender_female: 'weiblich',
    // gender_other: 'divers',
    // edu_question: 'Höchster Bildungsabschluss',
    // edu_graduated_high_school: 'Abitur',
    // edu_graduated_college: 'Hochschulabschluss',
    // edu_higher_degree: 'Universitärer Abschluss',
    // languages_question: 'Muttersprache',
    // languages_more: '(in der Regel die Sprache, die Sie als Kind zu Hause gesprochen haben)',
    // comments_question: 'Weitere Kommentare'
});

// The 'thanks' view is crucial; never delete it; it submits the results!
const thanks = babeViews.view_generator("thanks", {
    trials: 1,
    name: 'thanks',
    title: 'Thank you for taking part in this experiment!',
    prolificConfirmText: 'Press the button'
});


//my own objects

const comp_trial = babeViews.view_generator("slider_rating",{
  trials: comp_block.length, //1,
  name: 'comp_question',
  trial_type:'comp_slider',
  data: _.shuffle(comp_block),
  //hook: {
  //      after_response_enabled: show2ndquestion
  //    }

})

const pri_trial = babeViews.view_generator("slider_rating", {
    trials:pri_block.length, //1,
    //title: pri1_block.title,
    name: 'prior_probability_question',
    trial_type: 'prior_slider',
    data: _.shuffle(pri_block)
});

const rel_trial = babeViews.view_generator("slider_rating",{
  trials: rel_block.length, //2,
  name: 'rel_question',
  trial_type:'rel_slider',
  data: _.shuffle(rel_block),
  // hook: {
  //      after_response_enabled: show2ndquestion
  //    }

})

const xor_trial = babeViews.view_generator("slider_rating", {
    trials: xor_block.length,
    //title: xor_block.title,
    name: 'xor_question',
    trial_type: 'xor_slider',
    data: _.shuffle(xor_block)
});






/** trial (babe's Trial Type Views) below

* Obligatory properties

    - trials: int - the number of trials this view will appear
    - name: string - the name of the view type as it shall be known to _babe (e.g. for use with a progress bar)
            and the name of the trial as you want it to appear in the submitted data
    - data: array - an array of trial objects

* Optional properties

    - pause: number (in ms) - blank screen before the fixation point or stimulus show
    - fix_duration: number (in ms) - blank screen with fixation point in the middle
    - stim_duration: number (in ms) - for how long to have the stimulus on the screen
      More about trial life cycle - https://babe-project.github.io/babe-docs/01_designing_experiments/04_lifecycles_hooks/

    - hook: object - option to hook and add custom functions to the view
      More about hooks - https://babe-project.github.io/babe-docs/01_designing_experiments/04_lifecycles_hooks/

* All about the properties of trial views
* https://babe-project.github.io/babe-docs/01_designing_experiments/01_template_views/#trial-views
*/



// There are many more templates available:
// forced_choice, slider_rating, dropdown_choice, testbox_input, rating_scale, image_selection, sentence_choice,
// key_press, self_paced_reading and self_paced_reading_rating_scale

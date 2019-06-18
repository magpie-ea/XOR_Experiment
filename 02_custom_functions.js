// Here, you can define all custom functions, you want to use and initialize some variables

// add additional attribute to obects vignettes containing all 6 test questions
function add_complete_control_question (vign){
    let test;
    let pri_question;
  for (var i = 0; i < vign.length; i++) {
      vign[i].allQ = _.shuffle([vign[i].test_true1, vign[i].test_true2, vign[i].test_false1, vign[i].test_false2, vign[i].test_uncertain1, vign[i].test_uncertain2]);
      pri_question = _.shuffle([vign[i].question_pri1, vign[i].question_pri2]);
      vign[i].critical_question = [vign[i].question_comp, vign[i].question_rel, vign[i].question_xor, pri_question[0], pri_question[1]];
  }
    return (vign);
}

// takes in six test questions for each story and randomly chooses which one to ask in which block
function choose_control_question (eight_stories){
  console.log(eight_stories);
  //let four_questions;  /*_.sampleSize(vign[i].allQ, 4);*/
  for (var i = 0; i < eight_stories.length; i++) {
    var four_questions = _.sampleSize(eight_stories[i].allQ, 4);

  console.log(four_questions);

    comp_block[i].question = `<br> ------------------------------- <br/>` + four_questions[0] + `<br> ------------------------------- <br/> <font size="2"">
    How likely do you think it is that the statement is true, given the
    information in the background story?</font> <br/>`;
    pri_block[i].question = `<br> ------------------------------- <br/>`+ four_questions[1]+ `<br> ------------------------------- <br/> <font size="2"">
    How likely do you think it is that the statement is true, given the
    information in the background story?</font> <br/>`;
    rel_block[i].question = `<br> ------------------------------- <br/>` + four_questions[2]+ `<br> ------------------------------- <br/> <font size="2"">
    How likely do you think it is that the statement is true, given the
    information in the background story?</font> <br/>`;
    xor_block[i].question = `<br> ------------------------------- <br/>` + four_questions[3]+ `<br> ------------------------------- <br/> <font size="2"">
    How likely do you think it is that the statement is true, given the
    information in the background story?</font> <br/>`;

  }

  return (eight_stories);
}
<<<<<<< HEAD
//console.log(comp_block[0].question);
//console.log(pri_block[0].question);
=======

const block_mapper = {"comp" : 0,
                      "rel" : 1,
                      "xor" : 2,
                      "pri" : 3 };
>>>>>>> 4fa8331799d744bc732c890d6791bd48b3d364c5

// takes in vignettes and makes it usable for new format
function create_block_template(b, blockString) {

<<<<<<< HEAD
          //var b = a;
          b.title = b.name;
          console.log(b.title);
          b.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
          b.optionLeft = "certainly false";
          b.optionRight  = "certainly true";
          b.question = blockString == "comp" ? question_comp
          /*b.question =  /choose_test_question(); ------------------------------- <br/> <font size="2">
          How likely do you think it is that the statement is true, given the
          information in the background story?</font> <br/>
          ------------------------------- <br/>`
          + _.sample(b.allQ);*/
          console.log(b);
          return(b);
=======
    let utterance = blockString == "xor" ? '<br>' + b.utterance_or : '';

    let t1 = {};
    t1.title = b.name;
    t1.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
    t1.optionLeft = "certainly false";
    t1.optionRight  = "certainly true";
    t1.condition = "test";
    t1.block = blockString;
    t1.question =  `<br> ------------------------------- <br/>` +     b.allQ[block_mapper[blockString]] + `<br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;

    let t2 = {};
    t2.title = b.name;
    t2.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background + utterance;
    t2.optionLeft = "certainly false";
    t2.optionRight  = "certainly true";
    t2.condition = "critical";
    t2.block = blockString;
    t2.question =  `<br> ------------------------------- <br/>` +  b.critical_question[block_mapper[blockString]] + `<br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;

    // let t3 = {};

    // if (blockString == "pri") {
    //     t3.title = b.name;
    //     t3.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
    //     t3.optionLeft = "certainly false";
    //     t3.optionRight  = "certainly true";
    //     t2.condition = "critical";
    //     t2.block = blockString;
    //     t3.question =  `<br> ------------------------------- <br/>` +     b.critical_question[block_mapper[blockString]+1] + `<br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
    // }

    // if (blockString == "pri") {
    //     return([t1,t2, t3]);
    // } else {
    //     return([t1,t2]);
    // }
    return([t1,t2]);
>>>>>>> 4fa8331799d744bc732c890d6791bd48b3d364c5
}




show2ndquestion = function (data, next){
  console.log("show2ndquestion");
  babeViews.view_generator("slider_rating",{
    trials: 1,
    name: 'rel_question',
    trial_type:'rel_slider',
    data: _.shuffle(rel2_block),

  })
}


//var chosen_test_q = story_chosen[i].allQ[Math.floor(Math.random()*story_chosen[i].allQ.length)];




// in the following approach, I tried to have the different  question for everywhere
// block as a separate b.question, but as I have a random test question, which chan
// be the same in every block, maybe I only need different hooks, but not different functions
// in order to create each block
// // create competence block
// const create_comp_block(a) {
//           var b = a;
//           b.title = b.name;
//           b.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
//           b.optionLeft = "certainly false";
//           b.optionRight  = "certainly true";
//           b.question =   `------------------------------- <br/> <font size="2">
//           How likely do you think it is that the statement is true, given the
//           information in the background story?</font> <br/>
//           ------------------------------- <br/>`
//           + _.sample(b.allQ);
//           /*b.question_comp;*/
//           return(b);
// }
//
// // creating new prior 1 block
// function create_prior1_block(a) {
//       var b = a;
//       b.title = b.name;
//       b.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
//       b.optionLeft = "certainly false";
//       b.optionRight  = "certainly true";
//       b.question = `------------------------------- <br/> <font size="2">
//       How likely do you think it is that the statement is true, given the
//       information in the background story?</font> <br/>
//       ------------------------------- <br/>`
//       +_.sample(b.allQ);
//       return(b);
// }
//
// // creating prior two block
// function create_prior2_block(a) {
//          var b = a;
//          b.title = b.name;
//          b.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
//          b.optionLeft = "certainly false";
//          b.optionRight  = "certainly true";
//          b.question = `------------------------------- <br/> <font size="2">
//          How likely do you think it is that the statement is true, given the
//          information in the background story?</font> <br/> ------------------------------- <br/>`
//          +_.sample(b.allQ);
//          return(b);
// }
//
// // create relevance block
// function create_rel_block(a) {
//          var b = a;
//          b.title = b.name;
//          b.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
//          b.optionLeft = "certainly false";
//          b.optionRight  = "certainly true";
//          b.question = `------------------------------- <br/> <font size="2">
//          How likely do you think it is that the statement is true, given the
//          information in the background story?</font> <br/> ------------------------------- <br/>`
//          + _.sample(b.allQ);
//          return(b);
// }
//
//
//
// //creating new xor block-based trial info
// function create_xor_block(a) {
//       var b = a;
//       b.title = b.name;
//       b.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
//       b.optionLeft = "certainly false";
//       b.optionRight  = "certainly true";
//       b.question = `------------------------------- <br/> <font size="2">
//       How likely do you think it is that the statement is true, given the
//       information in the background story?</font> <br/> ------------------------------- <br/>`
//       +_.sample(b.allQ);
//       return(b);
// }
//

// function to create hook to show second question in same view
// only for competence, b.question_comp is the question needed here;

//show2ndquestion = function(data,next){


//}




/* Variables
*
*
*/
const coin = _.sample(["head", "tail"]); // You can determine global (random) parameters here
// Declare your variables here



/* Helper functions
*
*
*/


/* For generating random participant IDs */
    // https://stackoverflow.com/questions/1349404/generate-random-string-characters-in-javascript
// dec2hex :: Integer -> String
const dec2hex = function(dec) {
    return ("0" + dec.toString(16)).substr(-2);
};
// generateId :: Integer -> String
const generateID = function(len) {
    let arr = new Uint8Array((len || 40) /2);
    window.crypto.getRandomValues(arr);
    return Array.from(arr, this.dec2hex).join("");
};
// Declare your helper functions here



/* Hooks
*
*
*/

// Error feedback if participants exceeds the time for responding
const time_limit = function(data, next) {
    if (typeof window.timeout === 'undefined'){
        window.timeout = [];
    }
    // Add timeouts to the timeoutarray
    // Reminds the participant to respond after 5 seconds
    window.timeout.push(setTimeout(function(){
          $('#reminder').text('Please answer more quickly!');
    }, 5000));
    next();
};

// compares the chosen answer to the value of `option1`
check_response = function(data, next) {
    $('input[name=answer]').on('change', function(e) {
        if (e.target.value === data.correct) {
            alert('Your answer is correct! Yey!');
        } else {
            alert('Sorry, this answer is incorrect :( The correct answer was ' + data.correct);
        }
        next();
    })
}


// Declare your hooks here


/* Generators for custom view templates, answer container elements and enable response functions
*
*
*/

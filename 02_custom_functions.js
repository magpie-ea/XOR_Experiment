// Here, you can define all custom functions, you want to use and initialize some variables

// function to handle what value goes into attribute question at what time
// add additional attribute allQ to obects vignettes containing all 6 test questions
// add additional attribute critical_question, with critical question for all 4 blocks
function assign_questions (vign){
    let pri_question;
  for (var i = 0; i < vign.length; i++) {
      vign[i].allQ = _.shuffle([vign[i].test_true1, vign[i].test_true2, vign[i].test_false1, vign[i].test_false2, vign[i].test_uncertain1, vign[i].test_uncertain2]);
      pri_question = _.shuffle([vign[i].question_pri1, vign[i].question_pri2]);
      vign[i].critical_question = [vign[i].question_comp, vign[i].question_rel, vign[i].question_xor, pri_question[0], pri_question[1]];
  }
    return (vign);
}

// construct help to map what position in array corresponds to what block
const block_mapper = {"comp" : 0,
                      "rel" : 1,
                      "xor" : 2,
                      "pri" : 3 };


// takes in the eight stories and makes the info usable for new format
// t1 and t2 are used in every block, t3 is only used for prior block
// utterance is only used for xor block
function create_block(b, blockString) {

    let utterance = blockString == "xor" ? '<br /> ' + b.utterance_or : '';

    let t1 = {};
    t1.title = b.name;
    t1.QUD = '<font size="6">'+ t1.title + '</font>  <br /> ' + b.background;
    t1.optionLeft = "certainly false";
    t1.optionRight  = "certainly true";
    t1.condition = "test";
    t1.block = blockString;
    t1.question =  `<br> ------------------------------- <br/>` +  b.allQ[block_mapper[blockString]] + `<br> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
    switch (b) {
      case b.allQ[block_mapper[blockString]] == b.test_true1:
        t1.test_question = 'test_true1';
        break;
      case b.allQ[block_mapper[blockString]] == b.test_true2:
        t1.test_question = 'test_true2';
        break;
      case b.allQ[block_mapper[blockString]] == b.test_false1:
        t1.test_question = 'test_false1';
        break;
      case b.allQ[block_mapper[blockString]] == b.test_false2:
        t1.test_question = 'test_false2';
        break;
      case b.allQ[block_mapper[blockString]] == b.test_uncertain1:
        t1.test_question = 'test_uncertain1';
        break;
      case b.allQ[block_mapper[blockString]] == b.test_uncertain2:
        t1.test_question = 'test_true2';
        break;
    }
    // switch (b, blockString) {
    //   case 0:
    //     t1.test_question = 'test_true1';
    //     break;
    //   case 1:
    //     t1.test_question = 'test_true2';
    //     break;
    //   case 2:
    //     t1.test_question = 'test_false1';
    //     break;
    //   case 3:
    //     t1.test_question = 'test_false2';
    //     break;
    //   case 4:
    //     t1.test_question = 'test_uncertain1';
    //     break;
    //   case 5:
    //     t1.test_question = 'test_true2';
    // }
//console.log(t1.test_question);

    let t2 = {};
    t2.title = b.name;
    t2.QUD = '<font size="6">'+ t2.title + '</font>  <br /> ' + b.background + '<br />  <b>' + utterance;
    t2.optionLeft = "certainly false";
    t2.optionRight  = "certainly true";
    t2.condition = "critical";
    t2.block = blockString;
    t2.question =  `<br> ------------------------------- <br/>` +  b.critical_question[block_mapper[blockString]] + `<br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
    t2.critical_question = b.critical_question[block_mapper[blockString]];

     let t3 = {};
     if (blockString == "pri") {
        t3.title = b.name;
        t3.QUD = '<font size="6">'+ t3.title + '</font>  <br /> ' + b.background;
        t3.optionLeft = "certainly false";
        t3.optionRight  = "certainly true";
        t2.condition = "critical";
        t2.block = blockString;
        t3.question =  `<br> ------------------------------- <br/>` + b.critical_question[block_mapper[blockString]+1] + `<br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
    }


    if (blockString == "pri") {
        if (b.critical_question[block_mapper[blockString]] == b.question_pri1 ){
          t2.prior_used = 'question_pri1';
          t3.prior_used = 'question_pri2';
        } else {
          t2.prior_used = 'question_pri2';
          t3.prior_used = 'question_pri1';
        }

          return([t1,t2,t3]);
    } else {
      return([t1,t2]);
    }
}




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

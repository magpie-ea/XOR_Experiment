// Here, you can define all custom functions, you want to use and initialize some variables

// function to handle what value goes into attribute question at what time
// add additional attribute allQ to obects vignettes containing all 6 test questions
// add additional attribute critical_question, with critical question for all 4 blocks
function assign_questions(vign) {
  let pri_question;
  for (var i = 0; i < vign.length; i++) {
    vign[i].allQ = _.shuffle([vign[i].test_true1, vign[i].test_true2, vign[i].test_true, vign[i].test_false1, vign[i].test_false2, vign[i].test_false, vign[i].test_uncertain1, vign[i].test_uncertain2, vign[i].test_uncertain]);
    pri_question = _.shuffle([vign[i].question_pri1, vign[i].question_pri2]);
    vign[i].critical_question = [vign[i].question_comp, vign[i].question_rel, vign[i].question_xor, pri_question[0], pri_question[1]];
  }
  return (vign);
}

// construct help to map what position in array corresponds to what block
const block_mapper = {
  "comp": 0,
  "rel": 1,
  "xor": 2,
  "pri": 3
};

// takes in the eight stories and makes the info usable for new format
// t1 and t2 are used in every block, t3 is only used for prior block
// utterance is only used for xor block

// the new format for the pilot from March 2021 is to have all three IV questions in one block, then fillers,
// and then a second block for the same item containing the xor question, as well as repeated Comp & Rel elicitation
function create_block(b) { // blockString

  const blockString = ["rel", "comp", "pri", "xor"];

  // get the critical sentence only for inference condition
  const utterance_mapper = {
    "rel": '',
    "comp": '',
    "pri": '',
    "xor": b.utterance_or
  };

  // let utterance = blockString == "xor" ? '<br /> ' + b.utterance_or : '';

  // first comprehension question block
  let t1 = {};
  t1.title = b.name;
  t1.QUD = '<font size="6">' + t1.title + '</font>  <br /> <br />' + b.background + '<br />  <b>' + utterance_mapper[blockString[0]];
  t1.optionLeft = "certainly false";
  t1.optionRight = "certainly true";
  t1.condition = "test";
  t1.block = 'test_question1';
  t1.question = `<br> ------------------------------- <br/> <b>` + b.allQ[block_mapper[blockString[0]]] + `</b><br> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;


  switch (true) {

  case (b.allQ[block_mapper[blockString[0]]] == b.test_true1):
    t1.test_question = 'test_true1';
    break;
  case (b.allQ[block_mapper[blockString[0]]] == b.test_true2):
    t1.test_question = 'test_true2';
    break;
  case (b.allQ[block_mapper[blockString[0]]] == b.test_true):
      t1.test_question = 'test_true';
      break;
  case (b.allQ[block_mapper[blockString[0]]] == b.test_false1):
    t1.test_question = 'test_false1';
    break;
  case (b.allQ[block_mapper[blockString[0]]] == b.test_false2):
    t1.test_question = 'test_false2';
    break;
  case (b.allQ[block_mapper[blockString[0]]] == b.test_false):
      t1.test_question = 'test_false';
      break;
  case (b.allQ[block_mapper[blockString[0]]] == b.test_uncertain1):
    t1.test_question = 'test_uncertain1';
    break;
  case (b.allQ[block_mapper[blockString[0]]] == b.test_uncertain2):
    t1.test_question = 'test_uncertain2';
    break;
  case (b.allQ[block_mapper[blockString[0]]] == b.test_uncertain):
      t1.test_question = 'test_uncertain';
      break;
  }
  // console.log("t1")
  // console.log(t1)

// relevance IV question block
  let t2 = {};
  t2.title = b.name;
  t2.QUD = '<font size="6">' + t2.title + '</font>  <br /> <br /> ' + b.background + '<br />  <b>' + utterance_mapper[blockString[0]];
  t2.optionLeft = "certainly false";
  t2.optionRight = "certainly true";
  t2.condition = "critical";
  t2.block = blockString[0];
  t2.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[0]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
  t2.test_question = "no";

// competence IV question block
  let t3 = {};
  t3.title = b.name;
  t3.QUD = '<font size="6">' + t3.title + '</font>  <br /> <br /> ' + b.background + '<br />  <b>' + utterance_mapper[blockString[1]];
  t3.optionLeft = "certainly false";
  t3.optionRight = "certainly true";
  t3.condition = "critical";
  t3.block = blockString[1];
  t3.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[1]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
  t3.test_question = "no";

// first prior IV question block
  let t4 = {};
  // if (blockString == "pri") {
    t4.title = b.name;
    t4.QUD = '<font size="6">' + t4.title + '</font>  <br /> <br />' + b.background;
    t4.optionLeft = "certainly false";
    t4.optionRight = "certainly true";
    t4.condition = "critical";
    t4.test_question = "no";
    t4.block = blockString[2];
    t4.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[2]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
  // }
  // Second prior question
  let t5 = {};
  t5.title = b.name;
  t5.QUD = '<font size="6">' + t5.title + '</font>  <br /> <br />' + b.background;
  t5.optionLeft = "certainly false";
  t5.optionRight = "certainly true";
  t5.condition = "critical";
  t5.test_question = "no";
  t5.block = blockString[2];
  t5.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[2]] + 1] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;

  // more test questions
  let t6 = {};
  t6.title = b.name;
  t6.QUD = '<font size="6">' + t6.title + '</font>  <br /> <br />' + b.background + '<br />  <b>' + utterance_mapper[blockString[0]];
  t6.optionLeft = "certainly false";
  t6.optionRight = "certainly true";
  t6.condition = "test";
  t6.block = 'test_question2';
  t6.question = `<br> ------------------------------- <br/> <b>` + b.allQ[0] + `</b><br> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;


  switch (true) {

  case (b.allQ[0] == b.test_true1):
    t6.test_question = 'test_true1';
    break;
  case (b.allQ[0] == b.test_true2):
    t6.test_question = 'test_true2';
    break;
  case (b.allQ[0] == b.test_true):
    t6.test_question = 'test_true';
    break;
  case (b.allQ[0] == b.test_false1):
    t6.test_question = 'test_false1';
    break;
  case (b.allQ[0] == b.test_false2):
    t6.test_question = 'test_false2';
    break;
  case (b.allQ[0] == b.test_false):
    t6.test_question = 'test_false';
    break;
  case (b.allQ[0] == b.test_uncertain1):
    t6.test_question = 'test_uncertain1';
    break;
  case (b.allQ[0] == b.test_uncertain2):
    t6.test_question = 'test_uncertain2';
    break;
  case (b.allQ[0] == b.test_uncertain):
    t6.test_question = 'test_uncertain';
    break;
  }
  // second filler question
  let t7 = {};
  t7.title = b.name;
  t7.QUD = '<font size="6">' + t7.title + '</font>  <br /> <br />' + b.background + '<br />  <b>' + utterance_mapper[blockString[0]];
  t7.optionLeft = "certainly false";
  t7.optionRight = "certainly true";
  t7.condition = "test";
  t7.block = 'test_question3';
  t7.question = `<br> ------------------------------- <br/> <b>` + b.allQ[2] + `</b><br> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;


  switch (true) {

  case (b.allQ[2] == b.test_true1):
    t7.test_question = 'test_true1';
    break;
  case (b.allQ[2] == b.test_true2):
    t7.test_question = 'test_true2';
    break;
  case (b.allQ[2] == b.test_true):
    t7.test_question = 'test_true';
    break;
  case (b.allQ[2] == b.test_false1):
    t7.test_question = 'test_false1';
    break;
  case (b.allQ[2] == b.test_false2):
    t7.test_question = 'test_false2';
    break;
  case (b.allQ[2] == b.test_false):
    t7.test_question = 'test_false';
    break;
  case (b.allQ[2] == b.test_uncertain1):
    t7.test_question = 'test_uncertain1';
    break;
  case (b.allQ[2] == b.test_uncertain2):
    t7.test_question = 'test_uncertain2';
    break;
  case (b.allQ[2] == b.test_uncertain):
    t7.test_question = 'test_uncertain';
    break;
  }

// third filler question
let t8 = {};
t8.title = b.name;
t8.QUD = '<font size="6">' + t8.title + '</font>  <br /> <br />' + b.background + '<br />  <b>' + utterance_mapper[blockString[0]];
t8.optionLeft = "certainly false";
t8.optionRight = "certainly true";
t8.condition = "test";
t8.block = 'test_question4';
t8.question = `<br> ------------------------------- <br/> <b>` + b.allQ[3] + `</b><br> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;


switch (true) {

case (b.allQ[3] == b.test_true1):
  t8.test_question = 'test_true1';
  break;
case (b.allQ[3] == b.test_true2):
  t8.test_question = 'test_true2';
  break;
case (b.allQ[3] == b.test_true):
  t8.test_question = 'test_true';
  break;
case (b.allQ[3] == b.test_false1):
  t8.test_question = 'test_false1';
  break;
case (b.allQ[3] == b.test_false2):
  t8.test_question = 'test_false2';
  break;
case (b.allQ[3] == b.test_false):
  t8.test_question = 'test_false';
  break;
case (b.allQ[3] == b.test_uncertain1):
  t8.test_question = 'test_uncertain1';
  break;
case (b.allQ[3] == b.test_uncertain2):
  t8.test_question = 'test_uncertain2';
  break;
case (b.allQ[3] == b.test_uncertain):
  t8.test_question = 'test_uncertain';
  break;
}
  // maybe an info block

  // xor question
  let t9 = {};
  t9.title = b.name;
  t9.QUD = '<font size="6">' + t9.title + '</font>  <br /> <br /> ' + '<font color="#00BFFF">Please note the additional sentence of this trial. Please answer the following questions by taking into account the additional inforamtion provided in this sentence.</font>' + '<br /> <br />' + b.background + '<br /> <br/> <b>' + utterance_mapper[blockString[3]];
  t9.optionLeft = "certainly false";
  t9.optionRight = "certainly true";
  t9.condition = "critical";
  t9.block = blockString[3];
  t9.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[3]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
  t9.test_question = "no";

  // second round of rel and comp
  // rel
  let t10 = {};
  t10.title = b.name;
  t10.QUD = '<font size="6">' + t10.title + '</font>  <br /> <br /> ' + b.background + '<br /> <br /> <b>' + utterance_mapper[blockString[3]];
  t10.optionLeft = "certainly false";
  t10.optionRight = "certainly true";
  t10.condition = "critical";
  t10.block = blockString[0];
  t10.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[0]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
  t10.test_question = "no";

  //comp
  let t11 = {};
  t11.title = b.name;
  t11.QUD = '<font size="6">' + t11.title + '</font>  <br /> <br /> ' + b.background + '<br /> <br />  <b>' + utterance_mapper[blockString[3]];
  t11.optionLeft = "certainly false";
  t11.optionRight = "certainly true";
  t11.condition = "critical";
  t11.block = blockString[1];
  t11.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[1]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
  t11.test_question = "no";


  return (_.flattenDeep([t1, _.shuffle([t2, t3, _.shuffle([t4, t5])]), t6, t7, t8, t9, _.shuffle([t10, t11]) ]));


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
const dec2hex = function (dec) {
  return ("0" + dec.toString(16))
    .substr(-2);
};
// generateId :: Integer -> String
const generateID = function (len) {
  let arr = new Uint8Array((len || 40) / 2);
  window.crypto.getRandomValues(arr);
  return Array.from(arr, this.dec2hex)
    .join("");
};
// Declare your helper functions here

/* Hooks
 *
 *
 */

// Error feedback if participants exceeds the time for responding
const time_limit = function (data, next) {
  if (typeof window.timeout === 'undefined') {
    window.timeout = [];
  }
  // Add timeouts to the timeoutarray
  // Reminds the participant to respond after 5 seconds
  window.timeout.push(setTimeout(function () {
    $('#reminder')
      .text('Please answer more quickly!');
  }, 5000));
  next();
};

// compares the chosen answer to the value of `option1`
check_response = function (data, next) {
  $('input[name=answer]')
    .on('change', function (e) {
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

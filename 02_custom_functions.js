// Here, you can define all custom functions, you want to use and initialize some variables

// function to handle what value goes into attribute question at what time
// add additional attribute allQ to obects vignettes containing all 6 test questions
// add additional attribute critical_question, with critical question for all 4 blocks
function assign_questions(vign) {
  let pri_question;
  for (var i = 0; i < vign.length; i++) {
    vign[i].allQ = _.shuffle([vign[i].test_true1, vign[i].test_true2, vign[i].test_false1, vign[i].test_false2, vign[i].test_uncertain1, vign[i].test_uncertain2]);
    pri_question = _.shuffle([vign[i].question_pri1, vign[i].question_pri2]);
    vign[i].critical_question = [vign[i].question_comp, vign[i].question_rel, vign[i].question_xor, pri_question[0], pri_question[1]];
  }
  return (vign);
}

// add critical questions for some and use only the 6 comprehension questions that are available for some
function assign_questions_some(vign) {
  for (var i = 0; i < vign.length; i++) {
    vign[i].allQ = _.shuffle([vign[i].test_true1, vign[i].test_true2, vign[i].test_false1, vign[i].test_false2, vign[i].test_uncertain1, vign[i].test_uncertain2]);
    vign[i].critical_question = [vign[i].question_comp, vign[i].question_rel, vign[i].question_imp, vign[i].question_priC];
  }
  return (vign);
}

// construct recording of the used test question
function test_Q_used(block, item, type, q_nr) {
  // if (type == "xor") {
    switch(true) {
      case (block.allQ[q_nr] == block.test_true1):
        item.test_question = 'test_true1';
        break;
      case (block.allQ[q_nr] == block.test_true2):
        item.test_question = 'test_true2';
        break;
      // case (block.allQ[q_nr] == block.test_true):
      //     item.test_question = 'test_true';
      //     break;
      case (block.allQ[q_nr] == block.test_false1):
        item.test_question = 'test_false1';
        break;
      case (block.allQ[q_nr] == block.test_false2):
        item.test_question = 'test_false2';
        break;
      // case (block.allQ[q_nr] == block.test_false):
      //     item.test_question = 'test_false';
      //     break;
      case (block.allQ[q_nr] == block.test_uncertain1):
        item.test_question = 'test_uncertain1';
        break;
      case (block.allQ[q_nr] == block.test_uncertain2):
        item.test_question = 'test_uncertain2';
        break;
      // case (block.allQ[q_nr] == block.test_uncertain):
      //     item.test_question = 'test_uncertain';
      //     break;
    }
    // for some, use only 6 comprehension questions
  // } else {
  //   switch(true) {
  //     case (block.allQ[q_nr] == block.test_true1):
  //       item.test_question = 'test_true1';
  //       break;
  //     case (block.allQ[q_nr] == block.test_true2):
  //         item.test_question = 'test_true2';
  //         break;
  //     case (block.allQ[q_nr] == block.test_false1):
  //       item.test_question = 'test_false1';
  //       break;
  //     case (block.allQ[q_nr] == block.test_false2):
  //         item.test_question = 'test_false2';
  //         break;
  //     case (block.allQ[q_nr] == block.test_uncertain1):
  //       item.test_question = 'test_uncertain1';
  //       break;
  //     case (block.allQ[q_nr] == block.test_uncertain2):
  //         item.test_question = 'test_uncertain2';
  //         break;
  //   }
  // }
  return (item);
}

// construct help to map what position in array corresponds to what block
const block_mapper = {
  "comp": 0,
  "rel": 1,
  "xor": 2,
  "pri": 3
};

// takes in the eight stories and makes the info usable for new format

// the new format for the pilot from March 2021 is to have all three IV questions in one block, then fillers,
// and then a second block for the same item containing the xor question, as well as repeated Comp & Rel elicitation
// the first three blocks appear in random order, the second round of rel and comp is also randomized

function create_block(b, type) { // blockString

  const blockString = ["rel", "comp", "pri", "xor"];

  // get the critical sentence only for inference condition
  const utterance_mapper = {
    "rel": '',
    "comp": '',
    "pri": '',
    "xor": b.utterance_or,
    "some": b.utterance_imp
  };

  // let utterance = blockString == "xor" ? '<br /> ' + b.utterance_or : '';

  // first comprehension question block
  let t1 = {};
  t1.exp_condition = b.type;
  t1.relevance = b.relevance;
  t1.competence = b.competence;
  t1.prior = b.prior;
  t1.ID = b.ID;
  t1.main_type = type;
  t1.title = b.name;
  t1.QUD =  b.background;
  t1.critical_question = ``;
  t1.optionLeft = '<p style="font-family: sans-serif">certainly false</p>';
  t1.optionRight = '<p style="font-family: sans-serif">certainly true</p>';
  t1.condition = "test";
  t1.block = 'test_question1';
  t1.prompt = '<b>' + b.allQ[block_mapper[blockString[0]]] + '</b>';
  t1.question = '<font size="3">' + "How likely is it that the statement in the blue box is true given the story?" + '</font>';

  test_Q_used(b, t1, type, 1);

// relevance question block
  let t2 = {};
  t2.exp_condition = b.type;
  t2.relevance = b.relevance;
  t2.competence = b.competence;
  t2.prior = b.prior;
  t2.ID = b.ID;
  t2.main_type = type;
  t2.title = b.name;
  t2.QUD = b.background;
  t2.critical_question = ``;
  t2.optionLeft = '<p style="font-family: sans-serif">certainly false</p>';
  t2.optionRight = '<p style="font-family: sans-serif">certainly true</p>';
  t2.condition = "critical";
  t2.block = blockString[0];
  t2.prompt = '<b>' + b.critical_question[block_mapper[blockString[0]]] + '</b>';
  t2.test_question = "no";
  t2.question = '<font size="3">' + "How likely is it that the statement in the blue box is true given the story?" + '</font>';

// competence question block
  let t3 = {};
  t3.exp_condition = b.type;
  t3.relevance = b.relevance;
  t3.competence = b.competence;
  t3.prior = b.prior;
  t3.ID = b.ID;
  t3.main_type = type;
  t3.title = b.name;
  t3.QUD =  b.background;
  t3.critical_question = ``;
  t3.optionLeft = '<p style="font-family: sans-serif">certainly false</p>';
  t3.optionRight = '<p style="font-family: sans-serif">certainly true</p>';
  t3.condition = "critical";
  t3.block = blockString[1];
  t3.prompt = `<b>` + b.critical_question[block_mapper[blockString[1]]] + `</b>`;
  t3.test_question = "no";
  t3.question = '<font size="3">' + "How likely is it that the statement in the blue box is true given the story?" + '</font>';

// first prior question block
  let t4 = {};
  t4.exp_condition = b.type;
  t4.relevance = b.relevance;
  t4.competence = b.competence;
  t4.prior = b.prior;
  t4.ID = b.ID;
  t4.main_type = type;
  t4.title = b.name;
  t4.QUD = b.background;
  t4.critical_question = ``;
  t4.optionLeft = '<p style="font-family: sans-serif">certainly false</p>';
  t4.optionRight = '<p style="font-family: sans-serif">certainly true</p>';
  t4.condition = "critical";
  t4.test_question = "no";
  t4.block = blockString[2];
  t4.prompt = `<b>` + b.critical_question[block_mapper[blockString[2]]] + `</b>`;
  t4.question = '<font size="3">' + "How likely is it that the statement in the blue box is true given the story?" + '</font>';

  // Second prior question block (only used for xor)
  let t5 = {};
  t5.exp_condition = b.type;
  t5.relevance = b.relevance;
  t5.competence = b.competence;
  t5.prior = b.prior;
  t5.ID = b.ID;
  t5.main_type = type;
  t5.title = b.name;
  t5.QUD =  b.background;
  t5.critical_question = ``;
  t5.optionLeft = '<p style="font-family: sans-serif">certainly false</p>';
  t5.optionRight = '<p style="font-family: sans-serif">certainly true</p>';
  t5.condition = "critical";
  t5.test_question = "no";
  t5.block = blockString[2];
  t5.prompt = `<b>` + b.critical_question[block_mapper[blockString[2]] + 1] + `</b>`;
  t5.question = '<font size="3">' + "How likely is it that the statement in the blue box is true given the story?" + '</font>';

  // more test questions used as fillers before the inference question
  let t6 = {};
  t6.exp_condition = b.type;
  t6.relevance = b.relevance;
  t6.competence = b.competence;
  t6.prior = b.prior;
  t6.ID = b.ID;
  t6.main_type = type;
  t6.title = b.name;
  t6.QUD =  b.background;
  t6.critical_question = ``;
  t6.optionLeft = '<p style="font-family: sans-serif">certainly false</p>';
  t6.optionRight = '<p style="font-family: sans-serif">certainly true</p>';
  t6.condition = "test";
  t6.block = 'test_question2';
  t6.prompt = `<b>` + b.allQ[0] + `</b>`;
  t6.question = '<font size="3">' + "How likely is it that the statement in the blue box is true given the story?" + '</font>';

  test_Q_used(b, t6, type, 0);

  // second filler question
  let t7 = {};
  t7.exp_condition = b.type;
  t7.relevance = b.relevance;
  t7.competence = b.competence;
  t7.prior = b.prior;
  t7.ID = b.ID;
  t7.main_type = type;
  t7.title = b.name;
  t7.QUD =  b.background;
  t7.critical_question = ``;
  t7.optionLeft = '<p style="font-family: sans-serif">certainly false</p>';
  t7.optionRight = '<p style="font-family: sans-serif">certainly true</p>';
  t7.condition = "test";
  t7.block = 'test_question3';
  t7.prompt = `<b>` + b.allQ[2] + `</b>`;
  t7.question = '<font size="3">' + "How likely is it that the statement in the blue box is true given the story?" + '</font>';

  test_Q_used(b, t7, type, 2);


// third filler question
  let t8 = {};
  t8.exp_condition = b.type;
  t8.relevance = b.relevance;
  t8.competence = b.competence;
  t8.prior = b.prior;
  t8.ID = b.ID;
  t8.main_type = type;
  t8.title = b.name;
  t8.QUD =  b.background;
  t8.critical_question = ``;
  t8.optionLeft = '<p style="font-family: sans-serif">certainly false</p>';
  t8.optionRight = '<p style="font-family: sans-serif">certainly true</p>';
  t8.condition = "test";
  t8.block = 'test_question4';
  t8.prompt = `<b>` + b.allQ[3] + `</b>`;
  t8.question = '<font size="3">' + "How likely is it that the statement in the blue box is true given the story?" + '</font>';

  test_Q_used(b, t8, type, 3);

  // maybe an info block

  // inference question
  // an information statement in blue appears above the context drawing subjects' attention to the new sentence
  let t9 = {};
  t9.exp_condition = b.type;
  t9.relevance = b.relevance;
  t9.competence = b.competence;
  t9.prior = b.prior;
  t9.ID = b.ID;
  t9.main_type = type;
  t9.title = b.name;
  t9.QUD =  b.background;
  // use correct utterance
  switch(true) {
    case (type == "some"):
      t9.critical_question = '<b>' + utterance_mapper["some"] + '</b>';
      break;
    default:
      t9.critical_question = '<b>' + utterance_mapper[blockString[3]] + '</b>';
  }
  t9.optionLeft = '<p style="font-family: sans-serif">certainly false</p>';
  t9.optionRight = '<p style="font-family: sans-serif">certainly true</p>';
  t9.condition = "critical";
  t9.block = type;
  t9.prompt = `<b>` + b.critical_question[block_mapper[blockString[3]]] + `</b>`;
  t9.test_question = "no";
  t9.question = '<font size="3">' + "How likely is it that the statement in the blue box is true given the story?" + '</font>';

  // second round of rel and comp
  // rel
  let t10 = {};
  t10.exp_condition = b.type;
  t10.relevance = b.relevance;
  t10.competence = b.competence;
  t10.prior = b.prior;
  t10.ID = b.ID;
  t10.main_type = type;
  t10.title = b.name;
  t10.QUD = b.background;
  switch(true) {
    case (type == "some"):
      t10.critical_question = '<b>' + utterance_mapper["some"] + '</b>';
      break;
    default:
      t10.critical_question = '<b>' + utterance_mapper[blockString[3]] + '</b>';
  }
  t10.optionLeft = '<p style="font-family: sans-serif">certainly false</p>';
  t10.optionRight = '<p style="font-family: sans-serif">certainly true</p>';
  t10.condition = "critical";
  t10.block = blockString[0];
  t10.prompt = `<b>` + b.critical_question[block_mapper[blockString[0]]] + `</b>`;
  t10.test_question = "no";
  t10.question = '<font size="3">' + "How likely is it that the statement in the blue box is true given the story?" + '</font>';

  //comp
  let t11 = {};
  t11.exp_condition = b.type;
  t11.relevance = b.relevance;
  t11.competence = b.competence;
  t11.prior = b.prior;
  t11.ID = b.ID;
  t11.main_type = type;
  t11.title = b.name;
  t11.QUD = b.background;

  switch(true) {
    case (type == "some"):
      t11.critical_question = '<b>' + utterance_mapper["some"] + '</b>';
      break;
    default:
      t11.critical_question = '<b>' + utterance_mapper[blockString[3]] + '</b>';
  }
  t11.optionLeft = '<p style="font-family: sans-serif">certainly false</p>';
  t11.optionRight = '<p style="font-family: sans-serif">certainly true</p>';
  t11.condition = "critical";
  t11.block = blockString[1];
  t11.prompt = `<b>` + b.critical_question[block_mapper[blockString[1]]] + `</b>`;
  t11.test_question = "no";
  t11.question = '<font size="3">' + "How likely is it that the statement in the blue box is true given the story?" + '</font>';

  // return both prior question blocks for xor, and make only one prior question for some
  if (type == "xor") {
    return (_.flattenDeep([t1, _.shuffle([t2, t3, _.shuffle([t4, t5])]), t6, t7, t8, t9, _.shuffle([t10, t11]) ]));
  } else {
    return (_.flattenDeep([t1, _.shuffle([t2, t3, t4]), t6, t7, t8, t9, _.shuffle([t10, t11]) ]));

  }


}



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



// Declare your hooks here


/* Generators for custom view templates, answer container elements and enable response functions
 *
 *
 */

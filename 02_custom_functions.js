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

// add critical questions for some and use only the 6 comprehension questions that are available for some
function assign_questions_some(vign) {
  for (var i = 0; i < vign.length; i++) {
    vign[i].allQ = _.shuffle([vign[i].test_true1, vign[i].test_true, vign[i].test_false1, vign[i].test_false, vign[i].test_uncertain1, vign[i].test_uncertain]);
    vign[i].critical_question = [vign[i].question_comp, vign[i].question_rel, vign[i].question_imp, vign[i].question_priC];
  }
  return (vign);
}

// construct recording of the used test question
function test_Q_used(block, item, type, q_nr) {
  if (type == "xor") {
    switch(true) {
      case (block.allQ[q_nr] == block.test_true1):
        item.test_question = 'test_true1';
        break;
      case (block.allQ[q_nr] == block.test_true2):
        item.test_question = 'test_true2';
        break;
      case (block.allQ[q_nr] == block.test_true):
          item.test_question = 'test_true';
          break;
      case (block.allQ[q_nr] == block.test_false1):
        item.test_question = 'test_false1';
        break;
      case (block.allQ[q_nr] == block.test_false2):
        item.test_question = 'test_false2';
        break;
      case (block.allQ[q_nr] == block.test_false):
          item.test_question = 'test_false';
          break;
      case (block.allQ[q_nr] == block.test_uncertain1):
        item.test_question = 'test_uncertain1';
        break;
      case (block.allQ[q_nr] == block.test_uncertain2):
        item.test_question = 'test_uncertain2';
        break;
      case (block.allQ[q_nr] == block.test_uncertain):
          item.test_question = 'test_uncertain';
          break;
    }
    // for some, use only 6 comprehension questions
  } else {
    switch(true) {
      case (block.allQ[q_nr] == block.test_true1):
        item.test_question = 'test_true1';
        break;
      case (block.allQ[q_nr] == block.test_true):
          item.test_question = 'test_true';
          break;
      case (block.allQ[q_nr] == block.test_false1):
        item.test_question = 'test_false1';
        break;
      case (block.allQ[q_nr] == block.test_false):
          item.test_question = 'test_false';
          break;
      case (block.allQ[q_nr] == block.test_uncertain1):
        item.test_question = 'test_uncertain1';
        break;
      case (block.allQ[q_nr] == block.test_uncertain):
          item.test_question = 'test_uncertain';
          break;
    }
  }
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
  t1.condition = b.type;
  t1.relevance = b.relevance;
  t1.competence = b.competence;
  t1.prior = b.prior;
  t1.ID = b.ID;
  t1.main_type = type;
  t1.title = b.name;
  t1.QUD = '<font size="6">' + t1.title + '</font>  <br /> <br />' + b.background + '<br />  <b>' + utterance_mapper[blockString[0]];
  t1.optionLeft = "certainly false";
  t1.optionRight = "certainly true";
  t1.condition = "test";
  t1.block = 'test_question1';
  t1.question = `<br> ------------------------------- <br/> <b>` + b.allQ[block_mapper[blockString[0]]] + `</b><br> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;


  test_Q_used(b, t1, type, 1);

// relevance question block
  let t2 = {};
  t2.condition = b.type;
  t2.relevance = b.relevance;
  t2.competence = b.competence;
  t2.prior = b.prior;
  t2.ID = b.ID;
  t2.title = b.name;
  t2.QUD = '<font size="6">' + t2.title + '</font>  <br /> <br /> ' + b.background + '<br />  <b>' + utterance_mapper[blockString[0]];
  t2.optionLeft = "certainly false";
  t2.optionRight = "certainly true";
  t2.condition = "critical";
  t2.block = blockString[0];
  t2.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[0]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
  t2.test_question = "no";

// competence question block
  let t3 = {};
  t3.condition = b.type;
  t3.relevance = b.relevance;
  t3.competence = b.competence;
  t3.prior = b.prior;
  t3.ID = b.ID;
  t3.title = b.name;
  t3.QUD = '<font size="6">' + t3.title + '</font>  <br /> <br /> ' + b.background + '<br />  <b>' + utterance_mapper[blockString[1]];
  t3.optionLeft = "certainly false";
  t3.optionRight = "certainly true";
  t3.condition = "critical";
  t3.block = blockString[1];
  t3.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[1]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
  t3.test_question = "no";

// first prior question block
  let t4 = {};
  t4.condition = b.type;
  t4.relevance = b.relevance;
  t4.competence = b.competence;
  t4.prior = b.prior;
  t4.ID = b.ID;
  t4.title = b.name;
  t4.QUD = '<font size="6">' + t4.title + '</font>  <br /> <br />' + b.background+ '<br />  <b>' + utterance_mapper[blockString[1]];
  t4.optionLeft = "certainly false";
  t4.optionRight = "certainly true";
  t4.condition = "critical";
  t4.test_question = "no";
  t4.block = blockString[2];
  t4.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[2]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;

  // Second prior question block (only used for xor)
  let t5 = {};
  t5.condition = b.type;
  t5.relevance = b.relevance;
  t5.competence = b.competence;
  t5.prior = b.prior;
  t5.ID = b.ID;
  t5.title = b.name;
  t5.QUD = '<font size="6">' + t5.title + '</font>  <br /> <br />' + b.background+ '<br />  <b>' + utterance_mapper[blockString[1]];
  t5.optionLeft = "certainly false";
  t5.optionRight = "certainly true";
  t5.condition = "critical";
  t5.test_question = "no";
  t5.block = blockString[2];
  t5.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[2]] + 1] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;

  // more test questions used as fillers before the inference question
  let t6 = {};
  t6.condition = b.type;
  t6.relevance = b.relevance;
  t6.competence = b.competence;
  t6.prior = b.prior;
  t6.ID = b.ID;
  t6.title = b.name;
  t6.QUD = '<font size="6">' + t6.title + '</font>  <br /> <br />' + b.background + '<br />  <b>' + utterance_mapper[blockString[0]];
  t6.optionLeft = "certainly false";
  t6.optionRight = "certainly true";
  t6.condition = "test";
  t6.block = 'test_question2';
  t6.question = `<br> ------------------------------- <br/> <b>` + b.allQ[0] + `</b><br> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;

  test_Q_used(b, t6, type, 0);

  // second filler question
  let t7 = {};
  t7.condition = b.type;
  t7.relevance = b.relevance;
  t7.competence = b.competence;
  t7.prior = b.prior;
  t7.ID = b.ID;
  t7.title = b.name;
  t7.QUD = '<font size="6">' + t7.title + '</font>  <br /> <br />' + b.background + '<br />  <b>' + utterance_mapper[blockString[0]];
  t7.optionLeft = "certainly false";
  t7.optionRight = "certainly true";
  t7.condition = "test";
  t7.block = 'test_question3';
  t7.question = `<br> ------------------------------- <br/> <b>` + b.allQ[2] + `</b><br> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;

  test_Q_used(b, t7, type, 2);


// third filler question
  let t8 = {};
  t8.condition = b.type;
  t8.relevance = b.relevance;
  t8.competence = b.competence;
  t8.prior = b.prior;
  t8.ID = b.ID;
  t8.title = b.name;
  t8.QUD = '<font size="6">' + t8.title + '</font>  <br /> <br />' + b.background + '<br />  <b>' + utterance_mapper[blockString[0]];
  t8.optionLeft = "certainly false";
  t8.optionRight = "certainly true";
  t8.condition = "test";
  t8.block = 'test_question4';
  t8.question = `<br> ------------------------------- <br/> <b>` + b.allQ[3] + `</b><br> ------------------------------- <br/> <font size="2"> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;

  test_Q_used(b, t8, type, 3);

  // maybe an info block

  // inference question
  // an information statement in blue appears above the context drawing subjects' attention to the new sentence
  let t9 = {};
  t9.condition = b.type;
  t9.relevance = b.relevance;
  t9.competence = b.competence;
  t9.prior = b.prior;
  t9.ID = b.ID;
  t9.title = b.name;
  // use correct utterance 
  switch(true) {
    case (type == "some"):
      t9.QUD = '<font size="6">' + t9.title + '</font>  <br /> <br /> ' + '<font color="#00BFFF">Please note the additional sentence of this trial. Please answer the following questions by taking into account the additional inforamtion provided in this sentence.</font>' + '<br /> <br />' + b.background + '<br /> <br/> <b>' + utterance_mapper["some"];
      break;
    default:
      t9.QUD = '<font size="6">' + t9.title + '</font>  <br /> <br /> ' + '<font color="#00BFFF">Please note the additional sentence of this trial. Please answer the following questions by taking into account the additional inforamtion provided in this sentence.</font>' + '<br /> <br />' + b.background + '<br /> <br/> <b>' + utterance_mapper[blockString[3]];
  }
  t9.optionLeft = "certainly false";
  t9.optionRight = "certainly true";
  t9.condition = "critical";
  t9.block = type;
  t9.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[3]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
  t9.test_question = "no";

  // second round of rel and comp
  // rel
  let t10 = {};
  t10.condition = b.type;
  t10.relevance = b.relevance;
  t10.competence = b.competence;
  t10.prior = b.prior;
  t10.ID = b.ID;
  t10.title = b.name;
  switch (true) {
    case (type == "some"):
      t10.QUD = '<font size="6">' + t10.title + '</font>  <br /> <br /> ' + b.background + '<br /> <br /> <b>' + utterance_mapper["some"];
      break;
    default:
      t10.QUD = '<font size="6">' + t10.title + '</font>  <br /> <br /> ' + b.background + '<br /> <br /> <b>' + utterance_mapper[blockString[3]];
  }
  t10.optionLeft = "certainly false";
  t10.optionRight = "certainly true";
  t10.condition = "critical";
  t10.block = blockString[0];
  t10.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[0]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
  t10.test_question = "no";

  //comp
  let t11 = {};
  t11.condition = b.type;
  t11.relevance = b.relevance;
  t11.competence = b.competence;
  t11.prior = b.prior;
  t11.ID = b.ID;
  t11.title = b.name;
  switch(true) {
    case (type == "some"):
        t11.QUD = '<font size="6">' + t11.title + '</font>  <br /> <br /> ' + b.background + '<br /> <br />  <b>' + utterance_mapper["some"];
        break;
    default:
        t11.QUD = '<font size="6">' + t11.title + '</font>  <br /> <br /> ' + b.background + '<br /> <br />  <b>' + utterance_mapper[blockString[3]];
  }
  t11.optionLeft = "certainly false";
  t11.optionRight = "certainly true";
  t11.condition = "critical";
  t11.block = blockString[1];
  t11.question = `<br> ------------------------------- <br/><b>` + b.critical_question[block_mapper[blockString[1]]] + `</b><br> ------------------------------- <br/> <font size="2""> How likely do you think it is that the statement is true, given the information in the background story?</font> <br/>`;
  t11.test_question = "no";


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

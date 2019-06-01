// Here, you can define all custom functions, you want to use and initialize some variables

// add additional attribute to obects vignettes containing all 6 test questions
function add_complete_test_question (vign){
  console.log("function wird aufgerufen");
  let test;
  for (var i = 0; i < vign.length; i++) {
    //console.log(vign[i]);
    vign[i].allQ = [vign[i].test_true1, vign[i].test_true2, vign[i].test_false1, vign[i].test_false2, vign[i].test_uncertain1, vign[i].test_uncertain2];
    //let all_test_questions = {vign[i].test_true1, vign[i].test_true2, vign[i].test_false1, vign[i].test_false2, vign[i].test_uncertain1, vign[i].test_uncertain2}
      // console.log(all_test_questions);
    console.log(vign[i].allQ);
    //var chosen_test_q = _.sample(vign.allQ);
    //var story_chosen.name = _.sample(vign.name);
    //console.log("this"+story_chosen.name)
    //var chosen_test_q = vign[i].allQ[Math.floor(Math.random()*vign[i].allQ.length)];

  }
  //var chosen_test_q = story_chosen[i].allQ[Math.floor(Math.random()*story_chosen[i].allQ.length)];
//  console.log(chosen_test_q);
    return (vign);

}




// create competence block
function create_comp_block(a) {
          var b = a;
          b.title = b.name;
          b.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
          b.optionLeft = "certainly false";
          b.optionRight  = "certainly true";
          b.question =   `------------------------------- <br/> <font size="2">
          How likely do you think it is that the statement is true, given the
          information in the background story?</font> <br/>
          ------------------------------- <br/>`
          + _.sample(b.allQ)
          /*b.question_comp;*/
          return(b);
}

// creating new prior 1 block
function create_prior1_block(a) {
      var b = a;
      b.title = b.name;
      b.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
      b.optionLeft = "certainly false";
      b.optionRight  = "certainly true";
      b.question = `------------------------------- <br/> <font size="2">
      How likely do you think it is that the statement is true, given the
      information in the background story?</font> <br/>
      ------------------------------- <br/>`
      +b.question_pri1;
      b.test_true1 = b.test_true1;
      return(b);
}

// creating prior two block
function create_prior2_block(a) {
         var b = a;
         b.title = b.name;
         b.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
         b.optionLeft = "certainly false";
         b.optionRight  = "certainly true";
         b.question = `------------------------------- <br/> <font size="2">
         How likely do you think it is that the statement is true, given the
         information in the background story?</font> <br/> ------------------------------- <br/>`
         +b.question_pri2;
         return(b);
}

// create relevance block
function create_rel_block(a) {
         var b = a;
         b.title = b.name;
         b.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
         b.optionLeft = "certainly false";
         b.optionRight  = "certainly true";
         b.question = `------------------------------- <br/> <font size="2">
         How likely do you think it is that the statement is true, given the
         information in the background story?</font> <br/> ------------------------------- <br/>`
         + _.sample(b.allQ)
         return(b);
}



//creating new xor block-based trial info
function create_xor_block(a) {
      var b = a;
      b.title = b.name;
      b.QUD = '<font size="6">'+ b.title + '</font>  <br /> <br />' + b.background;
      b.optionLeft = "certainly false";
      b.optionRight  = "certainly true";
      b.question = `------------------------------- <br/> <font size="2">
      How likely do you think it is that the statement is true, given the
      information in the background story?</font> <br/> ------------------------------- <br/>`
      +b.question_xor;
      return(b);
}


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

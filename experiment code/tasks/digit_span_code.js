//----- CUSTOMIZABLE VARIABLES -----------------------------------------

let nTrials = 14; // number of trials in the test
let nPracticeTrials = 3; // number of practice trials
let minSetSize = 3; // starting digit length
const stimuli_duration = 1000; // number of milliseconds to display each digit
const recall_duration = null; // number of milliseconds to allow recall. If null, there is no time limit.

if (debug) {
  nTrials = 1;
  nPracticeTrials = 1;
}

//----------------------------------------------------------------------

const possibleNumbers = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"];  // possible digits participants can get

let selection = jsPsych.randomization.sampleWithoutReplacement(possibleNumbers, minSetSize);  // chooses random digits
let selection_id = -1;

// keeps track of participant's scores:
let nError = 0;
let highest_span_score = 0;
let consec_error_score = 0;
let in_practice = true; // keeps track of whether the participant is in practice or real trials

// instruction page
const ds_instructions = {
  type: jsPsychInstructions,
  pages: [
    "Great job. You've completed the main block of tasks. Now you will complete one last unrelated task and answer a few short questionnairs. This task should take about 5 minutes.",
    "In this task, we will ask you to <b>memorize sequences of numbers</b>. The numbers will be presented on the screen one after the other. Your goal will be to memorize each sequence of numbers in the exact order they're shown.",
    `After we show you the sequence of numbers, we will show you an onscreen number-pad. It will look like this:<br><br>
    <img src="images/number-pad.png">
    <br><br><b>Your goal is to enter the numbers you saw into the onscreen number-pad in their correct order.</b> For example, if the sequence is '7 4 5 1', you would enter '7 4 5 1' in this exact order, and then press 'Continue'.
    <br><br>If you make a mistake, you can use the provided 'Backspace' button to clear the last number you entered.`,
    "You will now be given some practice trials to help you understand the task. Press 'Next' to start the practice trials."
  ],
  show_clickable_nav: true,
  button_label_previous: "",
  button_label_next: "Next"
};

const ds_instructions_node = {
  type: jsPsychInstructions,
  pages: [
    `You have completed the practice trials. <br><br> There will be ${nTrials} real trials. Click 'Next' to start.`
  ],
  show_clickable_nav: true,
  button_label_previous: "",
  button_label_next: "Next",
  on_finish: function() {
    minSetSize = 3;
    nError = 0;
    highest_span_score = 0;
    consec_error_score = 0;
    in_practice = false; // set to false to indicate that the participant is now in real trials
    selection = jsPsych.randomization.sampleWithoutReplacement(possibleNumbers, minSetSize);
  }
};

const test_stimuli = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: function() {
    selection_id += 1;
    return `<div style="font-size:70px;">${selection[selection_id]}</div>`;
  },
  choices: "NO_KEYS",
  trial_duration: stimuli_duration,
  on_finish: function() {
    if (selection_id + 1 >= selection.length) {
      jsPsych.finishCurrentTimeline();
    }
  }
};

const recall = {
  type: 'digit-span-recall', 
  correct_order: function() {
    return selection;
  },
  trial_duration: recall_duration,
  on_finish: function() {
    const ds_data = jsPsych.data.get().last(1).values()[0];
    const cur_set_size = minSetSize;

    const acc = ds_data.accuracy;
    if (acc == 1) {
      if (highest_span_score < minSetSize) {
        highest_span_score = minSetSize;
      }
      minSetSize += 1;
      nError = 0;
    } else if (nError < 1) { // checks for number of consecutive errors
      nError += 1;
    } else {
      if (consec_error_score < minSetSize) {
        consec_error_score = minSetSize;
      }
      minSetSize = Math.max(3, minSetSize - 1);
    }
    
    if (minSetSize <= 8) { // bottom code prevents immediate repetition of digits
      selection = jsPsych.randomization.sampleWithoutReplacement(possibleNumbers, minSetSize);
    } else {
      selection = jsPsych.randomization.sampleWithoutReplacement(possibleNumbers, 8);
      const extra = minSetSize - 8;
      const id = possibleNumbers.indexOf(selection[7]);
      possibleNumbers.splice(id, 1);
      const extraNumbers = jsPsych.randomization.sampleWithoutReplacement(possibleNumbers, extra);
      selection = selection.concat(extraNumbers);
      possibleNumbers = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"];
    }
    selection_id = -1;

    // record data
    const ds_data_tostore = {
      subject: ds_data.subject,
      version: ds_data.version,
      assignmentId: subject_id_url,
      condition: feedback_condition,
      timepoint: timepoint,
      trial_type: in_practice ? 'practice' : 'real',
      stimuli: JSON.stringify(ds_data.stimuli),
      recall: JSON.stringify(ds_data.recall),
      accuracy: ds_data.accuracy,
      rt: ds_data.rt,
      cur_set_size: cur_set_size,
      cur_failed_size: consec_error_score,
      cur_succeeded_size: highest_span_score,
      num_consec_error: nError
    };

    save_data(ds_data_tostore, 'digit_span');
  }
};

const ds_feedback = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: function() {
    let text = "";
    const accuracy = jsPsych.data.get().last(1).values()[0].accuracy;
    if (accuracy == 1) {
      text += '<div style="font-size:35px; color:rgb(0 220 0)"><b>Correct</div>';
    } else {
      text += '<div style="font-size:35px; color:rgb(240 0 0)"><b>Incorrect</div>';
    }
    return text;
  },
  choices: "NO_KEYS",
  trial_duration: 1000
};

const test_stack = {
  timeline: [test_stimuli],
  repetitions: nPracticeTrials + nTrials
};

const demo_procedure = {
  timeline: [test_stack, recall, ds_feedback],
  repetitions: nPracticeTrials
};

const test_procedure = {
  timeline: [test_stack, recall, ds_feedback],
  repetitions: nTrials
};

const digit_span_timeline = [ds_instructions, demo_procedure, ds_instructions_node, test_procedure];
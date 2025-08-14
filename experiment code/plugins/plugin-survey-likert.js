var jsPsychSurveyLikert = (function (jspsych) {
  'use strict';

  var version = "2.1.0-modified";

  const info = {
    name: "survey-likert",
    version,
    parameters: {
      /** Array containing one or more objects with parameters for the question(s) that should be shown on the page. */
      questions: {
        type: jspsych.ParameterType.COMPLEX,
        array: true,
        nested: {
          /** Question prompt. */
          prompt: {
            type: jspsych.ParameterType.HTML_STRING,
            default: void 0
          },
          /** Array of likert labels to display for this question. */
          labels: {
            type: jspsych.ParameterType.STRING,
            array: true,
            default: void 0
          },
          /** Whether or not a response to this question must be given in order to continue. */
          required: {
            type: jspsych.ParameterType.BOOL,
            default: false
          },
          /** Name of the question in the trial data. If no name is given, the questions are named Q0, Q1, etc. */
          name: {
            type: jspsych.ParameterType.STRING,
            default: ""
          },
          correct_response: {
            type: jspsych.ParameterType.INT,
            default: null,
            description: "The correct response for this question. If provided, the plugin will check if the participant's response matches this value before continuing. This is useful for checking if the participant has answered the question correctly, such as matching a Prolific user's answer."
          }
        }
      },
      /** If true, the order of the questions in the 'questions' array will be randomized. */
      randomize_question_order: {
        type: jspsych.ParameterType.BOOL,
        default: false
      },
      /** HTML-formatted string to display at top of the page above all of the questions. */
      preamble: {
        type: jspsych.ParameterType.HTML_STRING,
        default: null
      },
      /** Width of the likert scales in pixels. */
      scale_width: {
        type: jspsych.ParameterType.INT,
        default: null
      },
      /** Label of the button to submit responses. */
      button_label: {
        type: jspsych.ParameterType.STRING,
        default: "Continue"
      },
      /** Setting this to true will enable browser auto-complete or auto-fill for the form. */
      autocomplete: {
        type: jspsych.ParameterType.BOOL,
        default: false
      }
    },
    data: {
      /** An object containing the response for each question. The object will have a separate key (variable) for each question, with the first question in the trial being recorded in `Q0`, the second in `Q1`, and so on. The responses are recorded as integers, representing the position selected on the likert scale for that question. If the `name` parameter is defined for the question, then the response object will use the value of `name` as the key for each question. This will be encoded as a JSON string when data is saved using the `.json()` or `.csv()` functions. */
      response: {
        type: jspsych.ParameterType.OBJECT
      },
      /** The response time in milliseconds for the participant to make a response. The time is measured from when the questions first appear on the screen until the participant's response(s) are submitted. */
      rt_main_question: {
        type: jspsych.ParameterType.INT
      },
      /** An array with the order of questions. For example `[2,0,1]` would indicate that the first question was `trial.questions[2]` (the third item in the `questions` parameter), the second question was `trial.questions[0]`, and the final question was `trial.questions[1]`. This will be encoded as a JSON string when data is saved using the `.json()` or `.csv()` functions. */
      question_order: {
        type: jspsych.ParameterType.INT,
        array: true
      }
    },
    // prettier-ignore
    citations: {
      "apa": "de Leeuw, J. R., Gilbert, R. A., & Luchterhandt, B. (2023). jsPsych: Enabling an Open-Source Collaborative Ecosystem of Behavioral Experiments. Journal of Open Source Software, 8(85), 5351. https://doi.org/10.21105/joss.05351 ",
      "bibtex": '@article{Leeuw2023jsPsych,   author = {de Leeuw, Joshua R. and Gilbert, Rebecca A. and Luchterhandt, Bj{\\" o}rn},   journal = {Journal of Open Source Software},  doi = {10.21105/joss.05351},  issn = {2475-9066},   number = {85},  year = {2023},  month = {may 11},   pages = {5351},   publisher = {Open Journals},  title = {jsPsych: Enabling an {Open}-{Source} {Collaborative} {Ecosystem} of {Behavioral} {Experiments}},   url = {https://joss.theoj.org/papers/10.21105/joss.05351},  volume = {8}, }  '
    }
  };
  class SurveyLikertPlugin {
    constructor(jsPsych) {
      this.jsPsych = jsPsych;
    }
    static {
      this.info = info;
    }
    trial(display_element, trial) {
  if (trial.scale_width !== null) {
    var w = trial.scale_width + "px";
  } else {
    var w = "100%";
  }
  var html = "";
  html += '<style id="jspsych-survey-likert-css">';
  html += ".jspsych-survey-likert-statement { display:block; font-size: 16px; padding-top: 40px; margin-bottom:10px; }.jspsych-survey-likert-opts { list-style:none; width:" + w + "; margin:auto; padding:0 0 35px; display:block; font-size: 14px; line-height:1.1em; }.jspsych-survey-likert-opt-label { line-height: 1.1em; color: #444; }.jspsych-survey-likert-opts:before { content: ''; position:relative; top:11px; /*left:9.5%;*/ display:block; background-color:#efefef; height:4px; width:100%; }.jspsych-survey-likert-opts:last-of-type { border-bottom: 0; }.jspsych-survey-likert-opts li { display:inline-block; /*width:19%;*/ text-align:center; vertical-align: top; }.jspsych-survey-likert-opts li input[type=radio] { display:block; position:relative; top:0; left:50%; margin-left:-6px; }";
  html += "</style>";
  if (trial.preamble !== null) {
    html += '<div id="jspsych-survey-likert-preamble" class="jspsych-survey-likert-preamble">' + trial.preamble + "</div>";
  }
  if (trial.autocomplete) {
    html += '<form id="jspsych-survey-likert-form">';
  } else {
    html += '<form id="jspsych-survey-likert-form" autocomplete="off">';
  }
  var question_order = [];
  for (var i = 0; i < trial.questions.length; i++) {
    question_order.push(i);
  }
  if (trial.randomize_question_order) {
    question_order = this.jsPsych.randomization.shuffle(question_order);
  }
  for (var i = 0; i < trial.questions.length; i++) {
    var question = trial.questions[question_order[i]];
    html += '<label class="jspsych-survey-likert-statement">' + question.prompt + "</label>";
    var width = 100 / question.labels.length;
    var options_string = '<ul class="jspsych-survey-likert-opts" data-question-index="' + question_order[i] + '" data-name="' + question.name + '" data-radio-group="Q' + question_order[i] + '">';
    for (var j = 0; j < question.labels.length; j++) {
      options_string += '<li style="width:' + width + '%"><label class="jspsych-survey-likert-opt-label"><input type="radio" name="Q' + question_order[i] + '" value="' + j + '"';
      if (question.required) {
        options_string += " required";
      }
      options_string += ">" + question.labels[j] + "</label></li>";
    }
    options_string += "</ul>";
    html += options_string;
  }
  
  html += '<div id="jspsych-button-multi-error-message" style="color: red; margin-top: 10px;"></div>';
  html += '<input type="submit" id="jspsych-survey-likert-next" class="jspsych-survey-likert jspsych-btn" value="' + trial.button_label + '"></input>';
  html += "</form>";
  display_element.innerHTML = html;

    // Record the start time after the content is displayed.
    var startTime = performance.now();

    // 2. NOW that the form is on the page, select it and add the listener.
    display_element.querySelector("#jspsych-survey-likert-form").addEventListener("submit", (e) => {
      e.preventDefault();

      let all_correct = true;
      // Get all the question groups from the DOM
      const question_elements = display_element.querySelectorAll('.jspsych-survey-likert-opts');
      // Loop through each question to check for correctness
      for (const q_el of question_elements) {
        const question_index = parseInt(q_el.dataset.questionIndex);
        const question_info = trial.questions[question_index];
        
        
        // Only check if a correct_response is defined for this question
        if (question_info.correct_response !== null && typeof question_info.correct_response !== 'undefined') {
          const radio_group_name = q_el.dataset.radioGroup;
          const checked_el = display_element.querySelector(`input[name="${radio_group_name}"]:checked`);
          
          // If no answer is given OR the answer is wrong
          if (!checked_el || parseInt(checked_el.value) !== question_info.correct_response) {
            all_correct = false;
            break; // Stop checking as soon as one is wrong
          }
        }
      }

      // If any question was incorrect, show an error and stop.
      if (!all_correct) {
        display_element.querySelector("#jspsych-button-multi-error-message").innerHTML = "Please ensure your answers match the Prolific user's choices for all items before continuing.";
        mistakeCounter++;

        return; // Prevents the trial from finishing
      }

      // If we reach here, all checks passed.
      // Clear any previous error message.
      display_element.querySelector("#jspsych-button-multi-error-message").innerHTML = "";


      // ---- Measure response time and gather data (your original logic) ----
      var endTime = performance.now();
      var response_time = Math.round(endTime - startTime);
      var question_data = {};
      
      // Re-querying question_elements here just to be safe, though the variable from above is likely still in scope.
      const final_question_elements = display_element.querySelectorAll('.jspsych-survey-likert-opts');
      for (var i = 0; i < final_question_elements.length; i++) {
        var ul_element = final_question_elements[i];
        var id = ul_element.dataset["radioGroup"];
        var name = ul_element.dataset["name"] || id;
        var el = display_element.querySelector('input[name="' + id + '"]:checked');
        var response = el ? parseInt(el.value) : ""; // Store as integer
        var obje = {};
        obje[name] = response;
        Object.assign(question_data, obje);
      }
      display_element.innerHTML = ''; 

      // Finish the trial
      this.jsPsych.finishTrial({
        rt_main_question: response_time,
        response: question_data,
        question_order
      });
    });
    
  }
    simulate(trial, simulation_mode, simulation_options, load_callback) {
      if (simulation_mode == "data-only") {
        load_callback();
        this.simulate_data_only(trial, simulation_options);
      }
      if (simulation_mode == "visual") {
        this.simulate_visual(trial, simulation_options, load_callback);
      }
    }
    create_simulation_data(trial, simulation_options) {
      const question_data = {};
      let rt = 1e3;

      // MODIFICATION 3: Make simulation respect correct_response if specified
      let first = true;
      for (const q of trial.questions) {
        const name = q.name ? q.name : `Q${trial.questions.indexOf(q)}`;
        if (trial.correct_response !== null && first) {
          question_data[name] = trial.correct_response;
          first = false;
        } else {
          question_data[name] = this.jsPsych.randomization.randomInt(0, q.labels.length - 1);
        }
        rt += this.jsPsych.randomization.sampleExGaussian(1500, 400, 1 / 200, true);
      }
      const default_data = {
        response: question_data,
        rt,
        question_order: trial.randomize_question_order ? this.jsPsych.randomization.shuffle([...Array(trial.questions.length).keys()]) : [...Array(trial.questions.length).keys()]
      };
      const data = this.jsPsych.pluginAPI.mergeSimulationData(default_data, simulation_options);
      this.jsPsych.pluginAPI.ensureSimulationDataConsistency(trial, data);
      return data;
    }
    simulate_data_only(trial, simulation_options) {
      const data = this.create_simulation_data(trial, simulation_options);
      this.jsPsych.finishTrial(data);
    }
    simulate_visual(trial, simulation_options, load_callback) {
      const data = this.create_simulation_data(trial, simulation_options);
      const display_element = this.jsPsych.getDisplayElement();
      this.trial(display_element, trial);
      load_callback();
      const answers = Object.entries(data.response);
      for (let i = 0; i < answers.length; i++) {
        this.jsPsych.pluginAPI.clickTarget(
          display_element.querySelector(
            `input[type="radio"][name="${answers[i][0]}"][value="${answers[i][1]}"]`
          ),
          (data.rt - 1e3) / answers.length * (i + 1)
        );
      }
      this.jsPsych.pluginAPI.clickTarget(
        display_element.querySelector("#jspsych-survey-likert-next"),
        data.rt
      );
    }
  }

  return SurveyLikertPlugin;

})(jsPsychModule);
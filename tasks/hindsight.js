//#region hindsight
    
var hindsight_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, you will be asked to estimate the population of a series of countries.</p>
        <p><i>Please click the "Next" button when you are ready to see the first question.</i></p>`
    ],
    show_clickable_nav: true
};

var Peru_answer = "34,668,340 people live in Peru."
var Peru_control = "The movie, Back to the Future, cost $19,000,000 to make."
var Cameroon_answer = "29,354,570 people live in Cameroon."
var Cameroon_control = "The movie, Star Wars: Return of the Jedi, cost $32,500,000 to make."
var Italy_answer = " 58,704,136 people live in Italy."
var Italy_control ="The movie, Miss Congeniality, cost $45,000,000 to make."


var Peru_estimate_first_response = null;
var Peru_estimate = {
    timeline: [{
        type: jsPsychSurveyText,
        questions: [{
            prompt:  "How many people live in Peru?",
            required: required_general, rows: 2, columns: 10
        }],
        on_finish: function (data) {
            console.log(data.response);
            Peru_estimate_first_response = data.response.Q0;
        }
    }],
    randomize_order: false
};

var intro_slides_with_answers = {
    type: jsPsychInstructions,
    pages: [
        `<p>You will now be shown some trivia facts for seven seconds each.</p>
        <p><i>Please click the "Next" button when you are ready to see the first fact.</i></p>`
    ],
    show_clickable_nav: true
};

var Peru_answer_or_control = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: condition[0] == 'Factor-Included' ? Peru_answer : Peru_control,
    choices: "NO_KEYS",
    trial_duration: 7000,
};      

var Peru_estimate_second_response = null;
var Peru_recall_original_response = null;
var Peru_estimate_two = {
    timeline: [{
        type: jsPsychSurveyText,
        questions: [{
            prompt:  "How many people live in Peru?",
            required: required_general, rows: 2, columns: 10
        },
            {prompt: "What was your ORIGINAL ANSWER?",
                required: required_general, rows: 2, columns: 10
            }],
        on_finish: function (data) {
            console.log(data.response);
            Peru_estimate_second_response = data.response.Q0;
            Peru_recall_original_response = data.response.Q1;
        }
    }],
    randomize_order: false
};

var Cameroon_estimate_first_response = null;
var Cameroon_estimate = {
    timeline: [{
        type: jsPsychSurveyText,
        questions: [{
            prompt:  "How many people live in Cameroon?",
            required: required_general, rows: 2, columns: 10
        }],
        on_finish: function (data) {
            console.log(data.response);
            Cameroon_estimate_first_response = data.response.Q0;
        }
    }],
    randomize_order: false
};

var Cameroon_answer_or_control = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: condition[0] == 'Factor-Included' ? Cameroon_answer : Cameroon_control,
    choices: "NO_KEYS",
    trial_duration: 7000,
};      

var Cameroon_estimate_second_response = null;
var Cameroon_recall_original_response = null;
var Cameroon_estimate_two = {
    timeline: [{
        type: jsPsychSurveyText,
        questions: [{
            prompt:  "How many people live in Cameroon?",
            required: required_general, rows: 2, columns: 10
        },
            {prompt: "What was your ORIGINAL ANSWER?",
                required: required_general, rows: 2, columns: 10
            }],
        on_finish: function (data) {
            console.log(data.response);
            Cameroon_estimate_second_response = data.response.Q0;
            Cameroon_recall_original_response = data.response.Q1;
        }
    }],
    randomize_order: false
};

var Italy_estimate_first_response = null;
var Italy_estimate = {
    timeline: [{
        type: jsPsychSurveyText,
        questions: [{
            prompt:  "How many people live in Italy?",
            required: required_general, rows: 2, columns: 10
        }],
        on_finish: function (data) {
            console.log(data.response);
            Italy_estimate_first_response = data.response.Q0;
        }
    }],
    randomize_order: false
};

var Italy_answer_or_control = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: condition[0] == 'Factor-Included' ? Italy_answer : Italy_control,
    choices: "NO_KEYS",
    trial_duration: 7000,
};      

var Italy_estimate_second_response = null;
var Italy_recall_original_response = null;
var Italy_estimate_two = {
    timeline: [{
        type: jsPsychSurveyText,
        questions: [{
            prompt:  "How many people live in Italy?",
            required: required_general, rows: 2, columns: 10
        },
            {prompt: "What was your ORIGINAL ANSWER?",
                required: required_general, rows: 2, columns: 10
            }],
        on_finish: function (data) {
            console.log(data.response);
            Italy_estimate_second_response = data.response.Q0;
            Italy_recall_original_response = data.response.Q1;
        }
    }],
    randomize_order: false
};

    var hindsight_openQ_response = null;
    var hindsight_openQ = {
        type: jsPsychSurveyText,
        questions: [{
            prompt: `<p>In this exercise, you were asked to estimate the 
            population of a series of countries and then, later on, to recall 
            the estimate you made.</p><p>Describe your thought process behind 
            the number you put in when recalling your earlier estimate. How did 
            you come to remember the estimate you had made before?</p>`,
            required: required_general, rows: 5, columns: 80
        }],
        on_finish: function (data) {
            hindsight_openQ_response = data.response.Q0;
        }
    };

    var introspection_q_labels_hindsight1 = [
        `<strong>It made me more likely to misremember my intitial response as having been closer to the true value than it was.</strong>`,
        "",
        "<strong>It did not affect my response</strong>",
        "",
        `<strong>It made me more likely to misremember my intitial response as having been farther from the true value than it was.</strong>`
    ];

    var introspection_q_labels_hindsight2 = [
        `<strong>It would have made me more likely to misremember my intitial response as having been closer to the true value than it was.</strong>`,
        "",
        "<strong>It would not have affected my response</strong>",
        "",
        `<strong>It would have made me more likely to misremember my intitial response as having been farther from the true value than it was.</strong>`
    ];

    var hindsight_intro_response1 = null;
    var hindsight_introspect1 = {
        type: jsPsychHtmlSliderResponse,
        stimulus: function () {
            if (condition[0] == "Factor-Included") {
                return `<p>In this exercise, you were asked to estimate the population of a series of countries. And later on, you were asked to recall the population estimate you had made earlier.</p>
                        <p>In between these two questions, we told you the true population for each country.</p>
                        <p>Do you think <b>being told the true population for each country</b> affected your response? If so, how?</p>`;
            } else {
                return `<p>In this exercise, you were asked to estimate the population of a series of countries. And later on, you were asked to recall the population estimate you had made earlier.</p>
                        <p>Now, imagine if, between these two questions, we told you the true population for each country.</p>
                        <p>If this were the case, do you think <b>being told the true population for each country</b> would have affected your response? If so, how?</p>`;
            }
        },
        labels: condition[0] == "Factor-Included" ? introspection_q_labels_hindsight1 : introspection_q_labels_hindsight2,
        slider_width: introspection_q_slider_width,
        min: introspection_q_min,
        max: introspection_q_max,
        slider_start: 50,
        require_movement: introspection_q_require,
        prompt: "<br><br><br><br><br><br>",
        on_finish: function (data) {
            hindsight_intro_response1 = data.response;
        }
    };

    var hindsight_intro_response2 = null;
    var hindsight_introspect2 = {
        type: jsPsychSurveyText,
        questions: [{
            prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
            required: required_general, rows: 5, columns: 80
        }],
        on_finish: function (data) {
            hindsight_intro_response2 = data.response.Q0;
        }
    };

    var hindsight_intro_confidence_response = null;
    var hindsight_intro_confidence = {
        type: jsPsychHtmlSliderResponse,
        stimulus: confidence_q,
        labels: confidence_q_labels,
        slider_width: confidence_q_slider_width,
        min: confidence_q_min,
        max: confidence_q_max,
        slider_start: 50,
        require_movement: require_movement_general,
        on_finish: function (data) {
            hindsight_intro_confidence_response = data.response;
            s1_data = {
                subject: data.subject,
                version: data.version,
                factor: data.condition,
                condition: condition[0] == "Factor-Included" ? "Factor-Included" : "Factor-Excluded",
                task_name: "hindsight",
                Peru_estimate_first_response: Peru_estimate_first_response,
                Peru_estimate_second_response: Peru_estimate_second_response,
                Peru_recall_original_response: Peru_recall_original_response,
                Cameroon_estimate_first_response: Cameroon_estimate_first_response,
                Cameroon_estimate_second_response: Cameroon_estimate_second_response,
                Cameroon_recall_original_response: Cameroon_recall_original_response,
                Italy_estimate_first_response: Italy_estimate_first_response,
                Italy_estimate_second_response: Italy_estimate_second_response,
                Italy_recall_original_response: Italy_recall_original_response,
                openq_response: hindsight_openQ_response,
                introspect_rating: hindsight_intro_response1,
                introspect_open: hindsight_intro_confidence_response,
                familiarity: familiarity,
                rt: data.rt
            };
            console.log(s1_data);
            save_data(s1_data, 'introspection');
        }
    };

    var familiarity = null;
    var hindsight_familiar = {
        type: jsPsychHtmlButtonResponse,
        stimulus: `<p>Before doing this study, had you seen or heard of a task similar to this last one before?</p>`,
        choices: ["Yes", "No"],
        on_finish: function (data) {
            familiarity= data.response == 0 ? "Yes" : "No"

            
        }
    };

    var hindsight = {
        timeline: [hindsight_instructions, Peru_estimate, Cameroon_estimate, 
            Italy_estimate, intro_slides_with_answers, Peru_answer_or_control, 
            Cameroon_answer_or_control, 
            Italy_answer_or_control, Peru_estimate_two, Cameroon_estimate_two, 
            Italy_estimate_two, hindsight_familiar, hindsight_openQ, hindsight_introspect1, 
            hindsight_intro_confidence]
    };

//#endregion hindsight
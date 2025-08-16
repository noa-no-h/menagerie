//#region representativeness


subjectData = representativeness_db.find(item => item.subject === actorNumber);
observedChoice = subjectData.choice;
observedTime = subjectData.rt;

var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by the information they were told about Jack)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the information you were told about Jack)?</p>";

var rep_stimulus_factor_included =
`Imagine that a panel of psychologists have interviewed and administered 
personality tests to 30 engineers and 70 lawyers, all successful in 
their respective fields.<br><br>
On the basis of this information, thumbnail descriptions of the 30 
engineers and 70 lawyers have been written. Below is one of these 
descriptions chosen at random from the 100 available descriptions. 
The Prolific user was asked to read the description carefully and use the slider below to indicate 
your probability that the person described is an engineer, on a scale 
from 0 to 100.<br><br>
“Jack is a 45-year-old man. He is married and has four children. He 
is generally conservative, careful, and 
ambitious. He shows no interest in political and social issues and 
spends most of his free time on his many hobbies which include home 
carpentry, sailing, and mathematical puzzles.”<br><br>
We asked the Prolific user for the probability that Jack is one of the 30 engineers in the sample of 
100 is.<br><br> The Prolific user selected ` + observedChoice + `.<br><br>To demonstrate that you understand the Prolific user's choice, <b>please move the slider to the option that they selected (regardless of your own beliefs).</b> `;

var rep_stimulus_factor_excluded =
`Imagine that a panel of psychologists have interviewed and administered 
personality tests to 30 engineers and 70 lawyers, all successful in 
their respective fields.<br><br>
One individual, Jack, is chosen from the sample at random. Assuming you are given no other information about Jack, please use the slider 
below to indicate your probability that he is an engineer, on a scale 
from 0 to 100.<br><br>
The probability that Jack is one of the 30 engineers in the sample of 
100 is:<br><br> The Prolific user selected ` + observedChoice + `.<br><br>To demonstrate that you understand the Prolific user's choice, please move the slider to the option that they selected (regardless of your own beliefs). `;

var rep_instructions = {
type: jsPsychInstructions,
pages: [
    `<p>In this exercise, the Prolific user was given information about an individual within a group of people and asked to give the probability that he has a particular profession. </p>
        <p><i>Please click the "Next" button when you are ready to see the information and the Prolific user's response.</i></p>`
],
show_clickable_nav: true
};

var rep_stimulus = function() {
    return condition[0] == 'Factor-Included' ? rep_stimulus_factor_included : rep_stimulus_factor_excluded;
};
var choice = null;
var rep_trial = {
timeline: [{
    type: jsPsychHtmlSliderResponse,
    stimulus: rep_stimulus,
    labels: ['0%', '10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%'],
    min: 0,
    max: 100,
    start: 50,
    step: 10,
    enable_button_after: observedTime,
    require_movement: false,
    prompt: "<br><br><br>",
    correct_response: observedChoice,
    on_finish: function(data) {
        console.log(data.response);
        choice = data.response;
        rt_main_question = data.rt;
    }
}],
randomize_order: false
};

var rep_openQ_response = null;
var rep_openQ = {
type: jsPsychSurveyText,
questions: [{
    prompt: `<p>In this exercise, the Prolific user was given information about Jack and 
    asked to rate how likely it was that he was an engineer.
    </p><p>Describe what you think the thought 
    process was behind their decision of the probability that Jack was an engineer.</p>`,
    required: required_general, rows: 5, columns: 80
}],
on_finish: function (data) {
    rep_openQ_response = data.response.Q0;
}
};

var introspection_q_labels_rep1 = [
    `<strong>It made them say he was <u>LESS</u> likely to be an engineer.</strong>`,
    "",
    "<strong>It did not affect their response</strong>",
    "",
    `<strong>It made them say he was <u>MORE</u> likely to be an engineer.</strong>`
];

var introspection_q_labels_rep2 = [
    `<strong>It would have made me say he was <u>LESS</u> likely to be an engineer.</strong>`,
    "",
    "<strong>It would not have affected my response</strong>",
    "",
    `<strong>It would have made me say he was <u>MORE</u> likely to be an engineer.</strong>`
];
var label_order_randomized = function() {
    return Math.random() < 0.5 ? 'original' : 'flipped';
};
var rep_intro_response1 = null;
var rep_introspect1 = {
type: jsPsychHtmlSliderResponse,
stimulus: function () {
    if (condition[0] == "Factor-Included") {
        return `<p>In this exercise, the Prolific user was told about the group of people given personality tests.</p>
                    <p>Then we gave them a thumbnail description of one of them — Jack. Specifically, we told them, “Jack is a 45-year-old man. He is married and has four children. He 
                    is generally conservative, careful, and 
                    ambitious. He shows no interest in political and social issues and 
                    spends most of his free time on his many hobbies which include home 
                    carpentry, sailing, and mathematical puzzles.”</p>
                    <p>Do you think <b>being given that information about him</b> affected the Prolific user's response? If so, how?</p>`;
    } else {
        return `<p>In this exercise, you were told about the group of people given personality tests.</p>
                    <p>Now, imagine if you had also been given the following thumbnail description of Jack: “Jack is a 45-year-old man. He is married and has four children. He 
                    is generally conservative, careful, and 
                    ambitious. He shows no interest in political and social issues and 
                    spends most of his free time on his many hobbies which include home 
                    carpentry, sailing, and mathematical puzzles.”</p>
                    <p>If this were the case, do you think <b>being given that information about him</b> would have affected your response? If so, how?</p>`;
    }
},
labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_rep1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_rep1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_rep2;
        } else {
            return introspection_q_labels_rep2.slice().reverse();
        }
    },
slider_width: introspection_q_slider_width,
min: introspection_q_min,
max: introspection_q_max,
slider_start: 50,
require_movement: false,
prompt: "<br><br><br>",
on_finish: function (data) {
    rt_introspection_question = data.rt;

        if (label_order_randomized == 'original') {
            rep_intro_response1 = data.response
    }
        else {
            rep_intro_response1 = 100 - data.response;
            }
        }
};

var rep_intro_response2 = null;
var rep_introspect2 = {
type: jsPsychSurveyText,
questions: [{
    prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
    required: required_general, rows: 5, columns: 80
}],
on_finish: function (data) {
    rep_intro_response2 = data.response.Q0;
}
};
var rep_intro_confidence_response = null;
var rep_intro_confidence = {
type: jsPsychHtmlSliderResponse,
stimulus: confidence_q,
labels: confidence_q_labels,
slider_width: confidence_q_slider_width,
min: confidence_q_min,
max: confidence_q_max,
slider_start: 50,
require_movement: require_movement_general,
on_finish: function (data) {
    rep_intro_confidence_response = data.response;
    s1_data = {
        subject: data.subject,
        version: data.version,
        factor: data.condition,
        task_name: "rep",
        condition: condition[0] == "Factor-Included" ? "Factor-Included" : "Factor-Excluded",
        stimulus: rep_stimulus,
        choice: choice,
        flipped_scale: label_order_randomized,
        auxiliary_info1:  null,
        openq_response: rep_openQ_response,
        introspect_rating: rep_intro_response1,
        introspect_open: rep_intro_confidence_response,
        familiarity: familiarity,
        rt_main_question: rt_main_question,
        rt_introspection_question: rt_introspection_question
    };
    
    save_data(s1_data, 'introspection');
}
};

var familiarity = null;
var rep_familiar = {
type: jsPsychHtmlButtonResponse,
stimulus: familiarity_prompt,
choices: ["Yes", "No"],
on_finish: function (data) {
    familiarity= data.response == 0 ? "Yes" : "No"

    
}
};


if (only_main_question) {
    var representativeness = {
        timeline: [rep_instructions, rep_trial]
    };
} else {
    var representativeness = {
        timeline: [rep_instructions, rep_trial, rep_familiar, rep_openQ, rep_introspect1, rep_intro_confidence]
    };
}

//#endregion representativeness
var status_quo_stimulus_factor_included =
`The National Highway Safety Commission is deciding
how to allocate its budget between two safety research 
programs: i) improving automobile safety (bumpers,
body, gas tank configurations, seatbelts) and 
ii) improving the safety of interstate highways 
(guard rails, grading, highway interchanges, 
and implementing selective reduced
speed limits). Currently the commission allocates 
approximately 70% of its funds to auto safety and 
30% of its funds to highway safety. 
Since there is a ceiling on its total spending, these are
the commission's two options. Please select the option you would recommend.`;

var status_quo_stimulus_factor_excluded =
`The National Highway Safety Commission is deciding
how to allocate its budget between two safety research 
programs: i) improving automobile safety (bumpers,
body, gas tank configurations, seatbelts) and 
ii) improving the safety of interstate highways 
(guard rails, grading, highway interchanges, 
and implementing selective reduced
speed limits).  
Since there is a ceiling on its total spending, these are
the commission's two options. Please select the option you would recommend.`;

var status_quo_instructions = {
type: jsPsychInstructions,
pages: [
    `<p>In this exercise, you will be given a hypothetical scenario and asked what option you would recommend.</p>
    <p><i>Please click the "Next" button when you are ready to see the scenario and your options.</i></p>`
],
show_clickable_nav: true
};

choice = null;
var status_quo_trial = {
timeline: [
    {
        type: jsPsychHtmlButtonResponse,
        stimulus: condition[0] == 'Factor-Included' ? status_quo_stimulus_factor_included : status_quo_stimulus_factor_excluded,
        choices: ['Allocate 70% to auto safety and 30% to highway safety', 'Allocate 50% to auto safety and 50% to highway safety'],
        prompt: '',
        on_finish: function (data) {
            console.log(data.response ? "50/50" : "70/30")
            choice = data.response ? "50/50" : "70/30";
        }
    },
],
randomize_order: false
};

var status_quo_openQ_response = null;
var status_quo_openQ = {
type: jsPsychSurveyText,
questions: [{
    prompt: `<p>In this exercise, you were shown a scenario and asked what allocation of funds you would recommend.</p><p>Describe your thought process behind your decision about what allocation of funds you would recommend. How did you come to your eventual decision?</p>`,
    required: required_general, rows: 5, columns: 80
}],
on_finish: function (data) {
    status_quo_openQ_response = data.response.Q0;
}
};

var introspection_q_labels_status_quo1 = [
`<strong>It made me more likely to recommend the current allocation (70% auto safety / 30% highway safety)</strong>`,
"",
"<strong>It did not affect my response</strong>",
"",
`<strong>It made me more likely to recommend an equal allocation (50% auto safety / 50% highway safety)</strong>`
];

var introspection_q_labels_status_quo2 = [
`<strong>It would have made me more likely to recommend the current allocation (70% auto safety / 30% highway safety)</strong>`,
"",
"<strong>It would not have affected my response</strong>",
"",
`<strong>It would have made me more likely to recommend an equal allocation (50% auto safety / 50% highway safety)</strong>`
];

var status_quo_intro_response1 = null;
var status_quo_introspect1 = {
type: jsPsychHtmlSliderResponse,
stimulus: function () {
    if (condition[0] == "Factor-Included") {
        return `<p>In this exercise, you were asked what allocation of funds you would recommend.</p>
                <p>We first told you the current allocations of funds before asking you what you would recommend.</p>
                <p>Do you think <b>being told the current allocations</b> affected your response? If so, how?</p>`;
    } else {
        return `<p>In this exercise, you were asked what allocation of funds you would recommend.</p>
                <p>Now, imagine if you had first been told the current allocation of funds is 70% auto safety / 30% highway safety.</p>
                <p>If this were the case, do you think <b>being told those current allocations</b> would have affected your response? If so, how?</p>`;
    }
},
labels: condition[0] == "Factor-Included" ? introspection_q_labels_status_quo1 : introspection_q_labels_status_quo2,
slider_width: introspection_q_slider_width,
min: introspection_q_min,
max: introspection_q_max,
slider_start: 50,
require_movement: introspection_q_require,
prompt: "<br><br><br><br><br><br>",
on_finish: function (data) {
    status_quo_intro_response1 = data.response;
}
};

var status_quo_intro_response2 = null;
var status_quo_introspect2 = {
type: jsPsychSurveyText,
questions: [{
    prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
    required: required_general, rows: 5, columns: 80
}],
on_finish: function (data) {
    status_quo_intro_response2 = data.response.Q0;
}
};

var status_quo_intro_confidence_response = null;
var status_quo_intro_confidence = {
type: jsPsychHtmlSliderResponse,
stimulus: confidence_q,
labels: confidence_q_labels,
slider_width: confidence_q_slider_width,
min: confidence_q_min,
max: confidence_q_max,
slider_start: 50,
require_movement: require_movement_general,
on_finish: function (data) {
    status_quo_intro_confidence_response = data.response;
    s1_data = {
        subject: data.subject,
        version: data.version,
        factor: data.condition,
        condition: condition[0] == "Factor-Included" ? "Factor-Included" : "Factor-Excluded",
        task_name: "status_quo",
        choice: choice,
        familiarity: familiarity,
        openq_response: status_quo_openQ_response,
        introspect_rating: status_quo_intro_response1,
        introspect_open: status_quo_intro_confidence_response,
        rt: data.rt
    };
    console.log(s1_data);
    save_data(s1_data, 'introspection');
}
};

var familiarity = null;
var status_quo_familiar = {
type: jsPsychHtmlButtonResponse,
stimulus: `<p>Before doing this study, had you seen or heard of a task similar to this last one before?</p>`,
choices: ["Yes", "No"],
on_finish: function (data) {
    familiarity= data.response == 0 ? "Yes" : "No"
    
}
};


var status_quo = {
timeline: [status_quo_instructions, status_quo_trial, status_quo_familiar, status_quo_openQ, status_quo_introspect1, status_quo_intro_confidence]
};
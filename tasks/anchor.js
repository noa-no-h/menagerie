 ///#region 1. Anchoring Trivia (Mussweiler & Strack, 1999) - BETWEEN

 var confidence_q = condition[0] == 'Factor-Included' ? '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the initial example value)?</p>' : '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have influenced by the initial example value)?</p>';

 

var anchor_instructions = {
    type: jsPsychInstructions,
    pages: function() {
        if (trivia_question_already) {
            return [
                `<p>In this exercise, you will be asked to answer another trivia question. Please make your best guess and do not look up the answer online.</p>
                 <p><i>Please click the button below to view the question.</i></p>`
            ];
        } else {
            trivia_question_already = true;
            return [
                `<p>In this exercise, you will be asked to answer a trivia question. Please make your best guess and do not look up the answer online.</p>
                <p><i>Please click the button below to view the question.</i></p>`
            ];
        }
    },
    show_clickable_nav: true
};

const anchor_low = {
    type: jsPsychSurvey,
    survey_json: {
        showQuestionNumbers: false,
        pageNextText: 'Continue',
        pagePrevText: 'Previous',
        pages: [
            {
                name: "page1",
                elements: [
                    {
                        type: "radiogroup",
                        name: "AntarcticAnchor",
                        title: "Is the mean winter temperature in the Antarctic higher or lower than -45 degrees Fahrenheit?",
                        choices: [
                            { value: "Higher", text: "Higher" },
                            { value: "Lower", text: "Lower" }
                        ]
                    },
                    {
                        type: "text",
                        name: "AntarcticOpen",
                        title: "What is the mean winter temperature in the Antarctic (in degrees Fahrenheit)?",
                        isRequired: true,
                        inputType: "number"
                    }
                ]
            }
        ]
    },
    on_start: function() {
        console.log("here!");
    },
    on_finish: function(data) {
        // Convert boolean responses to human-readable form
        const antarcticAnchorResponse = data.response.AntarcticAnchor;
        const whaleAnchorResponse = data.response.WhaleAnchor;

        var s1_data = {
            subject: data.subject,
            version: data.version,
            task_name: "anchoring",
            condition: "Low Anchor",
            factor: data.condition,
            choice: data.response.AntarcticOpen,
            auxiliary_info1: antarcticAnchorResponse,
            stimulus: "Antarctic Temperature",
            rt: data.rt,
        };
        save_data(s1_data, 'introspection');

    }
};





const anchor_high = {
    type: jsPsychSurvey,
    survey_json: {
        showQuestionNumbers: false,
        pageNextText: 'Continue',
        pagePrevText: 'Previous',
        pages: [
            {
                name: "page1",
                elements: [
                    {
                        type: "boolean",
                        name: "AntarcticAnchor",
                        title: "Is the mean winter temperature in the Antarctic higher or lower than 1 degree Fahrenheit?",
                        labelTrue: "Lower",
                        labelFalse: "Higher"
                    },
                    {
                        type: "text",
                        name: "AntarcticOpen",
                        title: "What is the mean winter temperature in the Antarctic (in degrees Fahrenheit)?",
                        isRequired: true,
                        inputType: "number"
                    }
                ]
            },
            /*{
                name: "page2",
                elements: [
                    {
                        type: "boolean",
                        name: "WhaleAnchor",
                        title: "Was the longest recorded blue whale shorter or longer than 160 feet?",
                        labelTrue: "Longer",
                        labelFalse: "Shorter"
                    },
                    {
                        type: "text",
                        name: "WhaleOpen",
                        title: "How long was the longest recorded blue whale (in feet)?",
                        isRequired: true,
                        inputType: "number"
                    }
                ]
            }*/
        ]
    },
    on_start: function() {
        console.log("here!");
    },
    on_finish: function(data) {
        // Convert boolean responses to human-readable form
        const antarcticAnchorResponse = data.response.AntarcticAnchor ? "Lower" : "Higher";
        const whaleAnchorResponse = data.response.WhaleAnchor ? "Longer" : "Shorter";

        var s1_data = {
            subject: data.subject,
            version: data.version,
            task_name: "anchoring",
            condition: "High Anchor",
            factor: data.condition,
            choice: data.response.AntarcticOpen,
            auxiliary_info1: antarcticAnchorResponse,
            stimulus: "Antarctic Temperature",
            rt: data.rt,
        };
        save_data(s1_data, 'introspection');

        /*var s2_data = {
            subject: data.subject,
            version: data.version,
            task_name: "anchoring",
            condition: "High Anchor",
            factor: data.condition,
            choice: data.response.WhaleOpen,
            auxiliary_info1: whaleAnchorResponse,
            stimulus: "Whale Length",
            rt: data.rt,
        };*/
        //save_data(s2_data, 'introspection');
    }
};



const anchor_none = {
    type: jsPsychSurvey,
    survey_json: {
        showQuestionNumbers: false,
        completeText: 'Complete',
        pageNextText: 'Continue',
        pagePrevText: 'Previous',
        pages: [
            {
                name: "page1",
                elements: [
                    {
                        type: "text",
                        name: "AntarcticOpen",
                        title: "What is the mean winter temperature in the Antarctic (in degrees Fahrenheit)?",
                        isRequired: true,
                        inputType: "number"
                    }
                ]
            },
            
        ]
    },
    on_finish: function(data) {
        var s1_data = {
            subject: data.subject,
            version: data.version,
            task_name: "anchoring",
            condition: "No Anchor",
            choice: data.response.AntarcticOpen,
            factor: data.condition,
            auxiliary_info1: data.response.AntarcticAnchor,
            stimulus: "Antarctic Temperature",
            rt: data.rt,
        };
        save_data(s1_data, 'introspection');

        var s2_data = {
            subject: data.subject,
            version: data.version,
            task_name: "anchoring",
            condition: "No Anchor",
            choice: data.response.WhaleOpen,
            factor: data.condition,
            auxiliary_info1: data.response.WhaleAnchor,
            stimulus: "Whale Length",
            rt: data.rt,
        };
        save_data(s2_data, 'introspection');
    }
};


anchor_sample = anchor_low;//jsPsych.randomization.sampleWithoutReplacement([anchor_high, anchor_low], 1);

var anchor_trials = null;
if (condition[0] == "Factor-Included") {
    anchor_trials = anchor_low
}
else {
    anchor_trials = anchor_none
}

var anchor_openQ_response = null;
var anchor_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: function() {
            if (condition[0] == "Factor-Included"){
                return `<p>In this exercise, you were asked two related trivia questions.</p><p>Describe your thought process while answering the second question: what is the mean winter temperature in the Antarctic? How did you come to your eventual answers for each question? Please try to describe how you answered each question individually.</p>`;

            } else {
                return `<p>In this exercise, you were asked a trivia question.</p><p>Describe your thought process while answering the question. How did you come to your eventual answer?</p>`;

            }
        },
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        anchor_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_anchor1 = [`<strong>It pushed my answer <u>FURTHER</u> away from the example value (e.g., further away from -45 degrees)</strong>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It pushed my answer <u>CLOSER</u> to the example value (e.g., closer to -45 degrees)</strong>`];
var introspection_q_labels_anchor2 = [`<strong>It would have pushed my answer <u>FURTHER</u> away from the example value (e.g., further away from -45 degrees)</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have pushed my answer <u>CLOSER</u> to the example value (e.g., closer to -45 degrees)</strong>`];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var anchor_intro_response1 = null;
var anchor_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>The trivia question you saw came in pairs, where the first question would ask you if the answer was greater or less than an <b>example value</b>.
        <p>Specifically, we first asked you whether the mean winter antarctic temperature in the Antarctic was higher or lower than -45 degrees Fahrenheit. Then, we asked you to provide an estimate of the mean winter temperature in the Antarctic. 
        <p>Do you think the <b>presence of the example value (-45 degrees Fahrenheit)</b> affected your response? If so, how?`
        } else {
            return `<p>The trivia question you saw asked you to estimate a specific value: the mean winter temperature in the Antarctic.
        <p>Now, imagine if before we asked you to provide this estimate, we first asked you if you thought the value was greater or less than an <b>example value.</b>
        <p>For example, imagine that we first asked you whether the mean winter temperature in the Antarctic was higher or lower than -45 degrees Fahrenheit. Then, we asked you to provide an estimate of the mean winter temperature in the Antarctic.
        <p>Do you think the <b>presence of such an example value</b> would have affected your response? If so, how?`
        }
    },
    labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_anchor1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_anchor1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_anchor2;
        } else {
            return introspection_q_labels_anchor2.slice().reverse();
        }
    },
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {

        if (label_order_randomized == 'original') {
            anchor_intro_response1 = data.response
    }
        else {
                anchor_intro_response1 = 100 - data.response;
            }
        }

};

var anchor_intro_response2 = null;
var anchor_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        anchor_intro_response2 = data.response.Q0
    }
};

var anchor_intro_confidence_response = null;
var anchor_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        anchor_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "anchoring",
            condition: anchor_condition,
            stimulus: null,
            auxiliary_info1: null,
            openq_response: anchor_openQ_response,
            introspect_rating: anchor_intro_response1,
            introspect_open: anchor_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection');
        console.log(s1_data);
    }
};

anchor_condition = null;
if (anchor_trials == anchor_none) {
    anchor_condition = "No Anchor"
} else if (anchor_sample[0] == anchor_high) {
    anchor_condition = "High Anchor"
} else {
    anchor_condition = "Low Anchor"
}

var familiarity = null;
var anchor_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p>Before doing this study, had you seen or heard of a task similar to this last one before?</p>`,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity = data.response == 0 ? "Yes" : "No"
    }
}

if (only_main_question){
    var anchor = {
        timeline: [anchor_instructions, anchor_trials]}
}
else{var anchor = {
    timeline: [anchor_instructions, anchor_trials, anchor_familiar, anchor_openQ, anchor_introspect1, anchor_intro_confidence]}}

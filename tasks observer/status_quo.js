

var observedChoice = "CHANGE THIS"


var confidence_q = condition[0] == 'Factor-Included' ? "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by being told that the <u>current allocation</u> of funds was 50% auto safety / 50% highway safety)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by being told that the <u>current allocation</u> of funds was 50% auto safety / 50% highway safety)?</p>";


var status_quo_stimulus_factor_included =
    `We told the Prolific user that in this study we are interested in your perceptions of decision making situations. Below is a scenario. We asked the Prolific user to read the scenario carefully and answer the questions that follow. <br><br>
The National Highway Safety Commission is deciding
how to allocate its budget between two safety research 
programs: <br>
1) improving automobile safety (bumpers,
body, gas tank configurations, seatbelts) <br>

2) improving the safety of interstate highways 
(guard rails, grading, highway interchanges, 
and implementing selective reduced
speed limits). <br><br>

<b>Currently the commission allocates 
approximately 50% of its funds to auto safety and 
50% of its funds to highway safety.</b>

<br><br>To make sure you understood the scenario, the Prolific user was asked to answer the following comprehension questions. Please answer them as well:`;

var status_quo_stimulus_factor_excluded =
    `In this study we are interested in your perceptions of decision making situations. Below is a scenario, please read the scenario carefully and answer the questions that follow. <br><br>
The National Highway Safety Commission is deciding
how to allocate its budget between two safety research 
programs: <br>
1) improving automobile safety (bumpers,
body, gas tank configurations, seatbelts) <br>

2) improving the safety of interstate highways 
(guard rails, grading, highway interchanges, 
and implementing selective reduced
speed limits). <br><br>

To make sure you understood the scenario, please answer the following comprehension questions:`;

var status_quo_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, the Prolific user was given a hypothetical scenario and asked what option they would recommend.</p>
    <p><i>Please click the "Next" button when you are ready to see the scenario and their options.</i></p>`
    ],
    show_clickable_nav: true
};

var passed = null;

var stimulus = condition[0] == 'Factor-Included' ? status_quo_stimulus_factor_included : status_quo_stimulus_factor_excluded;
var choice = "not status quo";
var which_option = null;
var comprehension_outcome = null;
var status_quo_trial = {
    timeline: [
        {
            type: jsPsychSurveyMultiChoice,
            questions: [
                {
                    prompt: condition[0] === 'Factor-Included'
                        ? "Now, we asked for the Prolific user's opinion regarding The National Highway Safety Commission's decision. There is no right or wrong opinion. We asked them to answer to the best of their understanding and view. <br><br>Reminder of the scenario:<span style='color:grey;'><br> The National Highway Safety Commission is deciding how to allocate its budget between two safety research programs:<br> 1) Improving automobile safety (bumpers, body, gas tank configuration, seat-belts)<br>2) Improving the safety of interstate highways (guard rails, grading, highway interchanges, and implementing selective reduced speed limits)<br><br><b>Currently, the commission allocates approximately 50% of its funds to auto safety and 50% of its funds to highway safety.</span></b><br><br> <b> We asked the Prolific user: Since there is a ceiling on its total spending, it must choose between the options provided below. If you had to make this choice, which of the following will you choose? </b> <br><br> The Prolific user selected " + observedChoice + ". <b> To demonstrate that you understand the Prolific user's choice, please select the option that they selected (regardless of your own beliefs).</br>"
                        : "Now, we would like to ask for your opinion regarding The National Highway Safety Commission's decision. There is no right or wrong opinion, answer to the best of your understanding and view. <br><br>Reminder of the scenario:<span style='color:grey;'><br> The National Highway Safety Commission is deciding how to allocate its budget between two safety research programs:<br> 1) Improving automobile safety (bumpers, body, gas tank configuration, seat-belts)<br>2) Improving the safety of interstate highways (guard rails, grading, highway interchanges, and implementing selective reduced speed limits)</span><br><br>Since there is a ceiling on its total spending, it must choose between the options provided below. If you had to make this choice, which of the following will you choose?",
                    name: "StatusQuoAnswer",
                    options: function () {
                        if (condition[0] == 'Factor-Included') {
                            return ["Maintain present budget amounts for the programs", "Increase auto program by 10% and lower highway program by like amount", "Increase auto program by 20% and lower highway program by like amount", "Decrease auto program by 20% and raise highway program by like amount"]
                        } else {
                            return ["Allocate 70% to auto safety and 30% to highway safety", "Allocate 30% to auto safety and 70% to highway safety", "Allocate 60% to auto safety and 40% to highway safety", "Allocate 50% to auto safety and 50% to highway safety"]
                        }
                    },
                    required: true
                }],

            on_finish: function (data) {
                console.log(data.response["StatusQuoAnswer"]);
                which_option = data.response["StatusQuoAnswer"];
                if (condition[0] == 'Factor-Included') {
                    if (which_option == "Maintain present budget amounts for the programs") {
                        choice = "status quo"
                    } else {
                        choice = "not status quo"
                    }
                }
                else {
                    if (which_option == "Allocate 50% to auto safety and 50% to highway safety") {
                        choice = "status quo"
                    } else {
                        choice = "not status quo"
                    }
                }
                console.log(data.response)

                if (only_main_question) {
                    s1_data = {
                        subject: data.subject,
                        version: data.version,
                        factor: data.condition,
                        task_name: "status_quo",
                        condition: condition[0],
                        stimulus: stimulus,
                        choice: choice,
                        auxiliary_info1: which_option + " status quo: 50/50",
                        openq_response: null,
                        introspect_rating: null,
                        introspect_open: null,
                        familiarity: null,
                        rt: data.rt
                    };
                    console.log(s1_data);
                    save_data(s1_data, 'introspection');
                }
            }
        },
    ],
    randomize_order: false
};

let failed_comprehension_questions = [];
function getComprehensionQuestions(condition) {

    if (condition[0] === 'Factor-Included') {
        return {
            timeline: [
                {
                    type: jsPsychSurveyMultiChoice,
                    questions: [
                        {
                            prompt: status_quo_stimulus_factor_included + "<br><br>" +
                                "1. The National Highway Safety Commission is deciding regarding the allocation of which budget?",
                            name: "comprehensionAllocation",
                            options: [
                                "Automobile safety",
                                "Safety of interstate highway",
                                "Safety of interstate highway and automobile safety"
                            ],
                            required: true
                        },
                        {
                            prompt: "2. The budget allocated for improving <u>automobile safety</u> will be used for improving at least which of the following?",
                            name: "comprehensionAuto",
                            options: [
                                "Doors & seats",
                                "Wheels & roof",
                                "Bumpers, seat-belts, gas configuration, & body",
                                "Engine"
                            ],
                            required: true
                        },
                        {
                            prompt: "3. The budget allocated for improving <u>safety of interstate highway</u> will be used for improving at least which of the following?",
                            name: "comprehensionHighway",
                            options: [
                                "Road signs and traffic lights",
                                "Radars and traffic jams monitors",
                                "Guard rails, grading, interchanges, selective reduced speed limits",
                                "Road holes & faster turns"
                            ],
                            required: true
                        },
                        {
                            prompt: "4. How is the budget currently allocated by the commission between auto-safety and highway safety?",
                            name: "comprehensionStatusQuo",
                            options: [
                                "70% for auto safety and 30% for highway safety",
                                "30% for auto safety and 70% for highway safety",
                                "60% for auto safety and 40% for highway safety",
                                "50% for auto safety and 50% for highway safety"
                            ],
                            required: true
                        }
                    ],
                    on_finish: function (data) {
                        processComprehensionResponses(data, condition);
                    }
                }
            ]
        };
    } else {
        return {
            timeline: [
                {
                    type: jsPsychSurveyMultiChoice,
                    questions: [
                        {
                            prompt: status_quo_stimulus_factor_excluded + "<br><br>" +
                                "1. The National Highway Safety Commission is deciding regarding the allocation of which budget?",
                            name: "comprehensionAllocation",
                            options: [
                                "Automobile safety",
                                "Safety of interstate highway",
                                "Safety of interstate highway and automobile safety"
                            ],
                            required: true
                        },
                        {
                            prompt: "2. The budget allocated for improving <u>automobile safety</u> will be used for improving at least which of the following?",
                            name: "comprehensionAuto",
                            options: [
                                "Doors & seats",
                                "Wheels & roof",
                                "Bumpers, seat-belts, gas configuration, & body",
                                "Engine"
                            ],
                            required: true
                        },
                        {
                            prompt: "3. The budget allocated for improving <u>safety of interstate highway</u> will be used for improving at least which of the following?",
                            name: "comprehensionHighway",
                            options: [
                                "Road signs and traffic lights",
                                "Radars and traffic jams monitors",
                                "Guard rails, grading, interchanges, selective reduced speed limits",
                                "Road holes & faster turns"
                            ],
                            required: true
                        }
                    ],
                    on_finish: function (data) {
                        processComprehensionResponses(data, condition);
                    }
                }
            ]
        };
    }
}

var comprehension_questions = getComprehensionQuestions(condition);

function processComprehensionResponses(data, condition) {
    console.log(data.response);
    failed_comprehension_questions = [];

    if (condition[0] === 'Factor-Included') {
        if (
            data.response.comprehensionAllocation === "Safety of interstate highway and automobile safety" &&
            data.response.comprehensionAuto === "Bumpers, seat-belts, gas configuration, & body" &&
            data.response.comprehensionHighway === "Guard rails, grading, interchanges, selective reduced speed limits" &&
            data.response.comprehensionStatusQuo === "50% for auto safety and 50% for highway safety"
        ) {
            passed = true;
            comprehension_outcome = "passed";
            console.log("passed");
        } else {
            let feedback = [];

            if (data.response.comprehensionAllocation !== "Safety of interstate highway and automobile safety") {
                failed_comprehension_questions += "1";
                feedback.push("The correct answer to question 1 is 'Safety of interstate highway and automobile safety'.");
            }
            if (data.response.comprehensionAuto !== "Bumpers, seat-belts, gas configuration, & body") {
                failed_comprehension_questions += "2";
                feedback.push("The correct answer to question 2 is 'Bumpers, seat-belts, gas configuration, & body'.");
            }
            if (data.response.comprehensionHighway !== "Guard rails, grading, interchanges, selective reduced speed limits") {
                failed_comprehension_questions += "3";
                feedback.push("The correct answer to question 3 is 'Guard rails, grading, interchanges, selective reduced speed limits'.");
            }
            if (data.response.comprehensionStatusQuo !== "50% for auto safety and 50% for highway safety") {
                failed_comprehension_questions += "4";
                feedback.push("The correct answer to question 4 is '50% for auto safety and 50% for highway safety'.");
            }
            passed = false;
            comprehension_outcome = "failed";
            console.log("failed"); 

            alert("You got the following questions wrong:\n\n" + feedback.join("\n") + "\n\nPlease correct your answers and try again.");
        }
    } else {
        if (
            data.response.comprehensionAllocation === "Safety of interstate highway and automobile safety" &&
            data.response.comprehensionAuto === "Bumpers, seat-belts, gas configuration, & body" &&
            data.response.comprehensionHighway === "Guard rails, grading, interchanges, selective reduced speed limits"
        ) {
            passed = true;
            comprehension_outcome = "passed";
            console.log("passed");
        } else {
            let feedback = [];
            if (data.response.comprehensionAllocation !== "Safety of interstate highway and automobile safety") {
                failed_comprehension_questions += "1";
                feedback.push("The correct answer to question 1 is 'Safety of interstate highway and automobile safety'.");
            }
            if (data.response.comprehensionAuto !== "Bumpers, seat-belts, gas configuration, & body") {
                failed_comprehension_questions += "2";
                feedback.push("The correct answer to question 2 is 'Bumpers, seat-belts, gas configuration, & body'.");
            }
            if (data.response.comprehensionHighway !== "Guard rails, grading, interchanges, selective reduced speed limits") {
                failed_comprehension_questions += "3";
                feedback.push("The correct answer to question 3 is 'Guard rails, grading, interchanges, selective reduced speed limits'.");
            }

            passed = false;
            comprehension_outcome = "failed";
            console.log("failed");

            alert("You got the following questions wrong:\n\n" + feedback.join("\n") + "\nPlease reread the scenario and questions carefully and try again.");
        }
    }

    let s1_data = {
        subject: data.subject,
        version: data.version,
        factor: data.condition,
        task_name: "status_quo",
        condition: condition[0],
        stimulus: "comprehension",
        choice: comprehension_outcome,
        auxiliary_info1: data.response.comprehensionAllocation + ", " +
            data.response.comprehensionAuto + ", " +
            data.response.comprehensionHighway + ", " +
            (data.response.comprehensionStatusQuo || "null"),
        openq_response: null,
        introspect_rating: null,
        introspect_open: null,
        familiarity: null,
        rt: data.rt
    };

    console.log(s1_data);
    save_data(s1_data, 'introspection');
}

function formatFailedQuestions(failedQuestions) {
    // Ensure we are working with a copy of the array to avoid mutating the original
    console.log("failedQuestions", failedQuestions);
    const questions = [...failedQuestions];

    // Format the list based on the number of failed questions
    if (questions.length === 1) {
        return `You answered comprehension question ${questions[0]} wrong. Please click 'retry' to try the questions again.";
`;
    } else if (questions.length > 1) {
        const lastQuestion = questions.pop();
        return `You answered comprehension questions ${questions.join(', ')}, and ${lastQuestion} wrong. Please click 'retry' to try the questions again.`;
    } else {
        return "You answered all comprehension questions correctly.";
    }
}

function comprehension_loop() {
    return {
        timeline: [
            comprehension_questions
            /*{
                type: jsPsychHtmlButtonResponse,
                stimulus: function () {
                    if (passed) {
                        return "Correct!";
                    } else {
                        return formatFailedQuestions(failed_comprehension_questions);
                    }
                },
                choices: function () {
                    if (passed) {
                        return ["Continue"];
                    } else {
                        return ["Retry"];
                    }
                },
            }*/
        ],
        loop_function: function () {
            return !passed; // Continue the loop if comprehension is not passed
        }
    };
}


var status_quo_openQ_response = null;
var status_quo_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, the Prolific user was asked to advise the National Highway 
    Safety Commission on how to allocate its budget between improving automobile safety
    and improving the safety of interstate highway. 
    </p><p>Describe what you think. the thought process was behind their decision about what allocation of funds they would recommend. How do you think they came to their eventual decision?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        status_quo_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_status_quo1 = [
    `<strong>It made them <u>LESS</u> likely to recommend the allocation: 50% auto safety / 50% highway safety</strong>`,
    "",
    "<strong>It did not affect their response</strong>",
    "",
    `<strong>It made them <u>MORE</u> likely to recommend the allocation: 50% auto safety / 50% highway safety</strong>`
];

var introspection_q_labels_status_quo2 = [
    `<strong>It would have made me LESS likely to recommend the allocation: 50% auto safety / 50% highway safety</strong>`,
    "",
    "<strong>It would not have affected my response</strong>",
    "",
    `<strong>It would have made me MORE likely to recommend the allocation: 50% auto safety / 50% highway safety</strong>`
];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';


var status_quo_intro_response1 = null;
var status_quo_introspect1 = {
    type: 'html-slider-response',
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this exercise, the Prolific user was asked what allocation of funds they would recommend to the National Highway Safety Commission.</p>
                <p>We first told them that the <b><u>current allocations</u> of funds is 50% to auto safety and 50% to highway safety</b>  before asking you what they would recommend.</p>
                <p>Do you think <b>knowing that the <u>current allocation</u> of funds is 50% to auto safety and 50% to highway safety</b> affected their response? If so, how?</p>`;
        } else {
            return `<p>In this exercise, you were asked what allocation of funds you would recommend to the National Highway Safety Commission.</p>
                <p>Now, imagine if you had first been told the <b><u>current allocation</u> of funds</b> is 50% auto safety / 50% highway safety.</p>
                <p>If this were the case, do you think <b>knowing that the <u>current allocation</u> of funds is 50% to auto safety and 50% to highway safety</b> would have affected your response? If so, how?</p>`;
        }
    },
labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_status_quo1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_status_quo1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_status_quo2;
        } else {
            return introspection_q_labels_status_quo2.slice().reverse();
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
            status_quo_intro_response1 = data.response
    }
        else {
            status_quo_intro_response1 = 100 - data.response;
            }
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
    type: 'html-slider-response',
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        status_quo_intro_confidence_response = data.response;
        var stimulus = condition[0] == 'Factor-Included' ? status_quo_stimulus_factor_included : status_quo_stimulus_factor_excluded;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "status_quo",
            condition: condition[0],
            stimulus: stimulus,
            choice: choice,
            auxiliary_info1: which_option + " status quo: 50/50",
            openq_response: status_quo_openQ_response,
            introspect_rating: status_quo_intro_response1,
            introspect_open: status_quo_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        };
        console.log(s1_data);
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var status_quo_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity = data.response == 0 ? "Yes" : "No"

    }
};



if (only_main_question) {
    var status_quo = {
        timeline: [status_quo_instructions, comprehension_loop(), status_quo_trial]
    };
} else {
    var status_quo = {
        timeline: [status_quo_instructions, comprehension_loop(), status_quo_trial, status_quo_familiar, status_quo_openQ, status_quo_introspect1, status_quo_intro_confidence]
    };
}
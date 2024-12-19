//#region 5. hindsight Effect - BETWEEN


var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by your knowledge of the actual outcomes of the event)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by your knowledge of the actual outcomes of the event)?</p>";

var hindsight_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> A short description of a real social
or personal event appears on the next page with a
number of possible outcomes. On the basis of
these data, we ask you to evaluate the likelihood
of the outcomes listed.</p>
    <p><i>Please click the button below to view the passage.</i>`,
    ],
    show_clickable_nav: true
}

var passed = false;

var comprehension_questions = {
    type: jsPsychSurvey,
    survey_json: {
        showQuestionNumbers: false,
        pages: [
            {
                name: "comprehension",
                elements: [
                    {
                        type: "html",
                        name: "event_description",
                        html: function () {
                            if (condition[0] === "Factor-Included") {
                                return `
                                    <p><b>Please consider the following event:</b></p>
                                    <p>For some years after the arrival of Hastings as governor-general of India, the consolidation of British power involved serious war. The first of these wars took place on the northern frontier of Bengal where the British were faced by the plundering raids of the Gurkas of Nepal. Attempts had been made to stop the raids by an exchange of lands, but the Gurkas would not give up their claims to country under British control, and Hastings decided to deal with them once and for all. The campaign began in November, 1814. It was not glorious. The Gurkas were only some 12,000 strong; but they were brave fighters, fighting in territory well-suited to their raiding tactics. The older British commanders were used to war in the plains where the enemy ran away from a resolute attack. In the mountains of Nepal it was not easy even to find the enemy. The troops and transport animals suffered from the extremes of heat and cold, and the officers learned caution only after sharp revers. Major-General Sir D. Octerlony was the one commander to escape from these minor defeats.</p>
                                    <p><u>The result was a British victory.</u></p>
                                    `;
                            } else {
                                return `
                                    <p><b>Please consider the following event:</b></p>
                                    <p>For some years after the arrival of Hastings as governor-general of India, the consolidation of British power involved serious war. The first of these wars took place on the northern frontier of Bengal where the British were faced by the plundering raids of the Gurkas of Nepal. Attempts had been made to stop the raids by an exchange of lands, but the Gurkas would not give up their claims to country under British control, and Hastings decided to deal with them once and for all. The campaign began in November, 1814. It was not glorious. The Gurkas were only some 12,000 strong; but they were brave fighters, fighting in territory well-suited to their raiding tactics. The older British commanders were used to war in the plains where the enemy ran away from a resolute attack. In the mountains of Nepal it was not easy even to find the enemy. The troops and transport animals suffered from the extremes of heat and cold, and the officers learned caution only after sharp revers. Major-General Sir D. Octerlony was the one commander to escape from these minor defeats.</p>
                                    `;
                            }
                        }
                    },
                    {
                        type: "radiogroup",
                        name: "To make sure you read and understood the scenario, please answer the following comprehension question: What was the outcome of the event?",
                        choices: ["British victory", "Gurka victory", "Military stalemate with no peace settlement", "Military stalemate with a peace settlement", "The case did not indicate the outcome"],
                        isRequired: true
                    }
                ]
            }
        ]
    },
    on_finish: function (data) {
        // Extract response index and verify if the answer is correct
        console.log("response", data.response); // data.response
        const response = data.response["To make sure you read and understood the scenario, please answer the following comprehension question: What was the outcome of the event?"] ;
        passed = false;

        if (response !== null) {
            if (condition[0] === "Factor-Included") {
                passed = response === "British victory"; // British victory
            } else {
                passed = response === "The case did not indicate the outcome"; // The case did not indicate the outcome
            }
        }
        console.log("passed", passed);


        const s1_data = {
            subject: data.subject,
            version: data.version,
            factor: condition[0],
            task_name: "hindsight effect",
            condition: condition[0] === "Factor-Included" ? "knowledge of outcome" : "no knowledge of outcome",
            choice: passed.toString(),
            auxiliary_info1: response,
            openq_response: null,
            introspect_rating: null,
            introspect_open: null,
            familiarity: null,
            rt: data.rt
        };

        console.log(s1_data);
        save_data(s1_data, 'introspection');
    }
};

var probBritish = null;
var probGurka = null;
var probStalemate = null;

var hindsight_question = {
    type: jsPsychSurvey,
    survey_json: {
        showQuestionNumbers: false,
        pages: [
            {
                name: "probabilities",
                elements: [
                    {
                        type: "html",
                        name: "event_description",
                        html: function () {
                            if (condition[0] === "Factor-Included") {
                                return `
                                    <p>For some years after the arrival of Hastings as governor-general of India, the consolidation of British power involved serious war. The first of these wars took place on the northern frontier of Bengal where the British were faced by the plundering raids of the Gurkas of Nepal. Attempts had been made to stop the raids by an exchange of lands, but the Gurkas would not give up their claims to country under British control, and Hastings decided to deal with them once and for all. The campaign began in November, 1814. It was not glorious. The Gurkas were only some 12,000 strong; but they were brave fighters, fighting in territory well-suited to their raiding tactics. The older British commanders were used to war in the plains where the enemy ran away from a resolute attack. In the mountains of Nepal it was not easy even to find the enemy. The troops and transport animals suffered from the extremes of heat and cold, and the officers learned caution only after sharp revers. Major-General Sir D. Octerlony was the one commander to escape from these minor defeats.</p> <p> <u>The result was a British victory.</u></p>
                                    <p>In the light of the information appearing in the passage, please estimate the probability of occurrence of each of the four possible outcomes listed below. There are no right or wrong answers, answer based on your intuition. (The probabilities should sum to 100%). <b> Answer as if you do not know the outcome, estimating the case at that time before outcomes were known.</b></p>`;
                            } else {
                                return `
                                    <p>For some years after the arrival of Hastings as governor-general of India, the consolidation of British power involved serious war. The first of these wars took place on the northern frontier of Bengal where the British were faced by the plundering raids of the Gurkas of Nepal. Attempts had been made to stop the raids by an exchange of lands, but the Gurkas would not give up their claims to country under British control, and Hastings decided to deal with them once and for all. The campaign began in November, 1814. It was not glorious. The Gurkas were only some 12,000 strong; but they were brave fighters, fighting in territory well-suited to their raiding tactics. The older British commanders were used to war in the plains where the enemy ran away from a resolute attack. In the mountains of Nepal it was not easy even to find the enemy. The troops and transport animals suffered from the extremes of heat and cold, and the officers learned caution only after sharp revers. Major-General Sir D. Octerlony was the one commander to escape from these minor defeats.</p>
                                    <p>In the light of the information appearing in the passage, please estimate the probability of occurrence of each of the four possible outcomes listed below. There are no right or wrong answers, answer based on your intuition. (The probabilities should sum to 100%). </p>`;
                            }
                        }
                    },
                    {
                        type: "text",
                        name: "BritishVictory",
                        title: "Probability of British victory:",
                        inputType: "number",
                        isRequired: true
                    },
                    {
                        type: "text",
                        name: "GurkaVictory",
                        title: "Probability of Gurka victory:",
                        inputType: "number",
                        isRequired: true
                    },
                    {
                        type: "text",
                        name: "StalemateNoPeace",
                        title: "Probability that the two sides reached a military stalemate, but were unable to come to a peace settlement:",
                        inputType: "number",
                        isRequired: true
                    },
                    {
                        type: "text",
                        name: "StalematePeace",
                        title: "Probability that the two sides reached a military stalemate and came to a peace settlement:",
                        inputType: "number",
                        isRequired: true
                    }
                ]
            }
        ]
    },
    on_finish: function (data) {
        // Parse the responses
        probBritish = parseInt(data.response.BritishVictory, 10);
        probGurka = parseInt(data.response.GurkaVictory, 10);
        probStalemateNoPeace = parseInt(data.response.StalemateNoPeace, 10);
        probStalematePeace = parseInt(data.response.StalematePeace, 10);

        if (only_main_question) {
            //console.log("only_main_question");
            s1_data = {
                subject: data.subject,
                version: data.version,
                factor: data.condition,
                task_name: "hindsight effect",
                condition: condition[0] === "Factor-Included" ? "knowledge of outcome" : "no knowledge of outcome",
                choice: probBritish,
                auxiliary_info1: probBritish + "," + probGurka + "," + probStalemateNoPeace + "," + probStalematePeace,
                stimulus: null,
                openq_response: null,
                introspect_rating: null,
                introspect_open: null,
                familiarity: null,
                rt: data.rt
            };
            //console.log(s1_data);
            save_data(s1_data, 'introspection');

        }
    } 
}; 


var passed = null;


function comprehension_loop() {
    return {
        timeline: [
            comprehension_questions,
            {
                type: jsPsychHtmlButtonResponse,
                stimulus: function () {
                    if (passed) {
                        return "Correct!";
                    } else {
                        return "You answered the comprehension question incorrectly. Please click 'retry' to try the question again.";
                    }
                },
                choices: function () {
                    if (passed) {
                        return ["Continue"];
                    } else {
                        return ["Retry"];
                    }
                }
            }
        ],
        loop_function: function () {
            return !passed; // Keep looping until comprehension is passed
        }
    };
}


var hindsight_openQ_response = null;
var hindsight_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were asked to judge the probability of occurrence of each of the four possible outcomes.</p><p>Describe your thought process while judging the probabilities. How did you come to your eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        hindsight_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_hindsight1 = [`<strong>It made me judge the outcome of British victory as <u>LESS</u> likely </strong>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It made me judge the outcome of British victory as <u>MORE</u> likely </strong>`];
var introspection_q_labels_hindsight2 = [`<strong>It would have made me judge the outcome of British victory  as <u>LESS</u> likely </strong>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It would have made me judge the outcome of British victory as <u>MORE</u> likely </strong>`];

var hindsight_intro_response1 = null;
var hindsight_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p> After reading about the historical event, we had told you what actually happened: a British victory.</p>
            <p> You were then asked to judge the probabilities of occurrence of each of the four possible outcomes but answering <b> as if </b>you had not known the true outcome.</p>
            <p>Do you think the fact that you knew the true outcome — a British victory — influenced your judgment of how likely the outcome of British victory was?</p>`
        } else {
            return `<p>Imagine that, after reading about the historical event, we had told you what actually happened: a British victory. Then, imagine we had asked you the same question -- to judge the probabilities of the four possible outcomes -- but answering <b> as if </b>you had not known the true outcome.</p>
            <p>In this case, do you think the fact that you would have known the true outcome — a British victory — would have influenced your judgment of how likely the outcome of British victory was?</p>`
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_hindsight1 : introspection_q_labels_hindsight2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {
        hindsight_intro_response1 = data.response
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
        hindsight_intro_response2 = data.response.Q0
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
            task_name: "hindsight effect",
            condition: condition[0] == "Factor-Included" ? "knowledge of outcome" : "no knowledge of outcome",
            choice: probBritish,
            auxiliary_info1: probBritish + "," + probGurka + "," + probStalemateNoPeace + "," + probStalematePeace,
            stimulus: null,
            openq_response: hindsight_openQ_response,
            introspect_rating: hindsight_intro_response1,
            introspect_open: hindsight_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        console.log(s1_data)
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var hindsight_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity=data.response == 0 ? "Yes" : "No"

        
    }
}



if (only_main_question) {
    var hindsight2 = {
        timeline: [hindsight_instructions, comprehension_loop(),hindsight_question]
    };
} else {
    var hindsight2 = {
        timeline: [hindsight_instructions, comprehension_loop(),hindsight_question, hindsight_familiar, hindsight_openQ, hindsight_introspect1, hindsight_intro_confidence]
    };
}

//#endregion
//timeline.push(hindsight)
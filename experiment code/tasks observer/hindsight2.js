//#region 5. hindsight Effect - BETWEEN
subjectData = hindsight_db.find(item => item.subject === actorNumber);
console.log("subjectData", subjectData);
const [observedBritish, observedGurka, ObservedStalemateNoPeace, ObservedStalematePeace] = subjectData.auxiliary_info1.split(',');
observerTime = subjectData.rt;




var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by their knowledge of the actual outcomes of the event)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by your knowledge of the actual outcomes of the event)?</p>";

var hindsight_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> A short description of a real social
or personal event appears on the next page with a
number of possible outcomes. On the basis of
these data, we asked the Prolific user to evaluate the likelihood
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
                                    <p><b>We asked the Prolific user to consider the following event:</b></p>
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
                        name: "To make sure they read and understood the scenario, we asked the Prolific user to answer the following comprehension question: What was the outcome of the event? Please answer this question as well.",
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
        const response = data.response["To make sure they read and understood the scenario, we asked the Prolific user to answer the following comprehension question: What was the outcome of the event? Please answer this question as well."] ;
        passed = false;

        if (response !== null) {
            if (condition[0] === "Factor-Included") {
                passed = response === "British victory"; // British victory
            } else {
                passed = response === "The case did not indicate the outcome"; // The case did not indicate the outcome
            }
        }
        console.log("passed", passed);

        if (!passed) {
            alert("You answered the question incorrectly. The correct answer is: " + (condition[0] == 'Factor-Included' ? "British victory" : "The case did not indicate the outcome") + ".\n\nPlease reread the scenario carefully and make sure you understand, and then re-answer the question.")
        }

        const s1_data = {
            subject: data.subject,
            version: data.version,
            factor: condition[0],
            task_name: "hindsight effect",
            condition: condition[0] === "Factor-Included" ? "knowledge of outcome" : "no knowledge of outcome",
            stimulus: "comprehension",
            choice: passed.toString(),
            auxiliary_info1: response,
            openq_response: null,
            introspect_rating: null,
            introspect_open: null,
            familiarity: null,
            rt_main_question: data.rt
        };

        
        save_data(s1_data, 'introspection');
    }
};

var probBritish = null;
var probGurka = null;
var probStalemate = null;

var sum_to_hundred = false;

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
                                    <p>In the light of the information appearing in the passage, we asked the Prolific user to estimate the probability of occurrence of each of the four possible outcomes listed below. We told them that there are no right or wrong answers, and they should answer based on their intuition. (The probabilities should sum to 100%). <b> We asked them to answer as if they did not know the outcome, estimating the case at that time before outcomes were known.</b></p><br> The Prolific user responded that the probability of a British victory was ` + observedBritish + `.<br>The Prolific user responded that the probability of a Gurka victory was ` + observedGurka + `.<br>The Prolific user responded that the probability of a stalemate (no peace) was ` + ObservedStalemateNoPeace + `.<br>The Prolific user responded that the probability of a stalemate (peace) was ` + ObservedStalematePeace + `.<br><br> To demonstrate that you understand the Prolific user's choice, <b>please type the responses that they gave (regardless of your own beliefs).</b>`;
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
                        isRequired: true,

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
    survey_function: function (survey) {
        survey.onValidateQuestion.add(function (s, options) {
            if (options.name === "BritishVictory") {
                if (observedBritish === null) {
                    return;
                }
                const enteredValue = String(options.value);
                if (enteredValue !== observedBritish) {
                    options.error = `Please enter exactly what the Prolific user entered for British victory.`;
                }
            } else if (options.name === "GurkaVictory") {
                if (observedGurka === null) {
                    return;
                }
                const enteredValue = String(options.value);
                if (enteredValue !== observedGurka) {
                    options.error = `Please enter exactly what the Prolific user entered for Gurka victory.`;
                }
            } else if (options.name === "StalemateNoPeace") {
                if (ObservedStalemateNoPeace === null) {
                    return;
                }
                const enteredValue = String(options.value);
                if (enteredValue !== ObservedStalemateNoPeace) {
                    options.error = `Please enter exactly what the Prolific user entered for stalemate (no peace).`;
                }
            } else if (options.name === "StalematePeace") {
                if (ObservedStalematePeace === null) {
                    return;
                }
                const enteredValue = String(options.value);
                if (enteredValue !== ObservedStalematePeace) {
                    options.error = `Please enter exactly what the Prolific user entered for stalemate (peace).`;
                }
            }
        });
    },
    on_load: function() {
        console.log("Looking for Continue button...");
        
        // Use MutationObserver to wait for button to exist
        const observer = new MutationObserver((mutations, obs) => {
            const continueButton = document.querySelector('input.sd-btn.jspsych-nav-complete');
            if (continueButton) {
                console.log("Found Continue button! Hiding it...");
                continueButton.style.display = 'none';
                
                setTimeout(() => {
                    continueButton.style.display = 'block';
                    console.log("Continue button now visible");
                }, observerTime); 
                
                // Stop observing once found
                obs.disconnect();
            }
        });

        // Start observing the document body
        observer.observe(document.body, {
            childList: true,
            subtree: true
        });
    },
    on_finish: function (data) {
        probBritish = parseInt(data.response.BritishVictory, 10);
        probGurka = parseInt(data.response.GurkaVictory, 10);
        probStalemateNoPeace = parseInt(data.response.StalemateNoPeace, 10);
        probStalematePeace = parseInt(data.response.StalematePeace, 10);
        rt_main_question = data.rt;
        if (only_main_question) {

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
                rt_main_question: data.rt
            };
            save_data(s1_data, 'introspection');
        }
    }
};

/*var hindsight_question_loop = {
    timeline: [hindsight_question],
    loop_function: function(data) {
        return !sum_to_hundred;
    }
}*/


var passed = null;


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
            }*/
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
        prompt: `<p>In this exercise, the Prolific user was asked to judge the probability of occurrence of each of the four possible outcomes.</p><p>Describe what you think their thought process was while judging the probabilities. How do you think they came to their eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        hindsight_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_hindsight1 = [`<strong>It made them judge the outcome of British victory as <u>LESS</u> likely </strong>`, "", "<strong>It did not affect their response</strong>", "", `<strong>It made them judge the outcome of British victory as <u>MORE</u> likely </strong>`];
var introspection_q_labels_hindsight2 = [`<strong>It would have made me judge the outcome of British victory  as <u>LESS</u> likely </strong>`, "", "<strong>It did not affect their response</strong>", "", `<strong>It would have made me judge the outcome of British victory as <u>MORE</u> likely </strong>`];
var label_order_randomized = function() {
    return Math.random() < 0.5 ? 'original' : 'flipped';
};
var hindsight_intro_response1 = null;
var hindsight_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p> After reading about the historical event, we had told the Prolific user what actually happened: a British victory.</p>
            <p> They were then asked to judge the probabilities of occurrence of each of the four possible outcomes but answering <b> as if </b>they had not known the true outcome.</p>
            <p>Do you think the fact that they knew the true outcome — a British victory — influenced their judgment of how likely the outcome of British victory was? If so, how?</p>`
        } else {
            return `<p>Imagine that, after reading about the historical event, we had told you what actually happened: a British victory. Then, imagine we had asked you the same question -- to judge the probabilities of the four possible outcomes -- but answering <b> as if </b>you had not known the true outcome.</p>
            <p>In this case, do you think the fact that you would have known the true outcome — a British victory — would have influenced your judgment of how likely the outcome of British victory was?</p>`
        }
    },
labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_hindsight1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_hindsight1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_hindsight2;
        } else {
            return introspection_q_labels_hindsight2.slice().reverse();
        }
    },    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {
        rt_introspection_question = data.rt;
        if (label_order_randomized == 'original') {
            hindsight_intro_response1 = data.response
    }
        else {
            hindsight_intro_response1 = 100 - data.response;
            }
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
            observer_or_actor: observer_or_actor,
            factor: data.condition,
            task_name: "hindsight effect",
            condition: condition[0] == "Factor-Included" ? "knowledge of outcome" : "no knowledge of outcome",
            choice: probBritish,
            flipped_scale: label_order_randomized,
            auxiliary_info1: probBritish + "," + probGurka + "," + probStalemateNoPeace + "," + probStalematePeace,
            stimulus: null,
            openq_response: hindsight_openQ_response,
            introspect_rating: hindsight_intro_response1,
            introspect_open: hindsight_intro_confidence_response,
            familiarity: familiarity,
            rt_main_question: rt_main_question,
            rt_introspection_question: rt_introspection_question
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
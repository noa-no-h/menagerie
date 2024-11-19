//#region 5. Double Effect - BETWEEN

var confidence_q = "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the fact that the man's death was <b>necessary</b> in order to achieve Peter's goal)?</p>";


var double_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> A short description of a real social
or personal events appears on the next page with a
number of possible outcomes. On the basis of
these data, we ask you to evaluate the likelihood
of the outcomes listed. We thank you for your
participation.</p>
    <p><i>Please click the button below to view the passage.</i>`,
    ],
    show_clickable_nav: true
}

var probBritish = null;
var probGurka = null;
var probStalemate = null;
var double_question = {
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
                                return `<p><b>Please consider the following event:</b></p>
                                    <p>For some years after the arrival of Hastings as governor-general of India, the consolidation of British power involved serious war. The first of these wars took place on the northern frontier of Bengal where the British were faced by the plundering raids of the Gurkas of Nepal. Attempts had been made to stop the raids by an exchange of lands, but the Gurkas would not give up their claims to country under British control, and Hastings decided to deal with them once and for all. The campaign began in November, 1814. It was not glorious. The Gurkas were only some 12,000 strong; but they were brave fighters, fighting in territory well-suited to their raiding tactics. The older British commanders were used to war in the plains where the enemy ran away from a resolute attack. In the mountains of Nepal it was not easy even to find the enemy. The troops and transport animals suffered from the extremes of heat and cold, and the officers learned caution only after sharp revers. Major-General Sir D. Octerlony was the one commander to escape from these minor defeats. The two sides reached a military stalemate, but were unable to come to a peace settlement.</p>
                                    <p>In the light of the information appearing in the passage, what was the probability of occurrence of each of the four possible outcomes listed below? (The probabilities should sum to 100%) <b>Please answer as you would have had you not known what happened.</b></p>`;
                            } else {
                                return `<p><b>Please consider the following event:</b></p>
                                    <p>For some years after the arrival of Hastings as governor-general of India, the consolidation of British power involved serious war. The first of these wars took place on the northern frontier of Bengal where the British were faced by the plundering raids of the Gurkas of Nepal. Attempts had been made to stop the raids by an exchange of lands, but the Gurkas would not give up their claims to country under British control, and Hastings decided to deal with them once and for all. The campaign began in November, 1814. It was not glorious. The Gurkas were only some 12,000 strong; but they were brave fighters, fighting in territory well-suited to their raiding tactics. The older British commanders were used to war in the plains where the enemy ran away from a resolute attack. In the mountains of Nepal it was not easy even to find the enemy. The troops and transport animals suffered from the extremes of heat and cold, and the officers learned caution only after sharp revers. Major-General Sir D. Octerlony was the one commander to escape from these minor defeats.</p>
                                    <p>In the light of the information appearing in the passage, what was the probability of occurrence of each of the four possible outcomes listed below? (The probabilities should sum to 100%)</p>`;
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
                        name: "B",
                        title: "Probability of Gurka victory:",
                        inputType: "number",
                        isRequired: true
                    },
                    {
                        type: "text",
                        name: "C",
                        title: "Probability that the two sides reached a military stalemate, but were unable to come to a peace settlement:",
                        inputType: "number",
                        isRequired: true
                    }
                ]
            }
        ]
    },
    on_finish: function (data) {
        const responses = data.response;
    }
};

var double_openQ_response = null;
var double_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were asked to judge the moral permissibility of an action in a hypothetical scenario.</p><p>Describe your thought process while judging whether the action was morally permissible. How did you come to your eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        double_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_double1 = [`<strong>It made me <u>LESS</u> likely to judge Peter's action as morally permissible</strong>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It made me <u>MORE</u> likely to judge Peter's action as morally permissible</strong>`];
var introspection_q_labels_double2 = [`<strong>It would have made me <u>LESS</u> likely to judge Peter's action as morally permissible</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me <u>MORE</u> likely to judge Peter's action as morally permissible</strong>`];

var double_intro_response1 = null;
var double_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>Sometimes when someone kills other people, the deaths are "collateral damage" -- i.e., killing the other people wasn't <i>necessary</i> to achieve the person's goal. For instance, imagine a country performs a military strike on a gun manufacturing factory of an enemy country, and there happens to be a worker in the factory who gets killed. In this case, the worker's death was not necessary for the military strike to achieve its goal of blowing up the factory; the worker's death was just a side effect (i.e. "collateral damage") of the strike.
            <p>Other times when someone kills other people, killing them was <i>necessary</i> to achieve the person's goal. For instance, imagine a country performs a military strike on an enemy general in order to cripple the enemy's leadership team. In this case, killing the general was necessary in order to achieve the country's goal of crippling the enemy leadership.</p>
            <p>In the case you read, killing the man was necessary to achieve Peter's goal of breaking the window and saving the five children. Without killing the man, he wouldn't have been able to break the window and save the children; the man was used as a <b>means</b> to achieve Peter's goal.</p>
            <p>Do you think this fact -- that the man's death was <b>necessary</b> in order to achieve Peter's goal -- influenced your judgment of whether Peter's actions were morally permissible or not? If so, how?</p>`
        } else {
            return `<p>Sometimes when someone kills other people, the deaths are "collateral damage" -- i.e., killing the other people wasn't <i>necessary</i> to achieve the person's goal. For instance, imagine a country performs a military strike on a gun manufacturing factory of an enemy country, and there happens to be a worker in the factory who gets killed. In this case, the worker's death was not necessary for the military strike to achieve its goal of blowing up the factory; the worker's death was just a side effect (i.e. "collateral damage") of the strike.
            <p>Other times when someone kills other people, killing them was <i>necessary</i> to achieve the person's goal. For instance, imagine a country performs a military strike on an enemy general in order to cripple the enemy's leadership team. In this case, killing the general was necessary in order to achieve the country's goal of crippling the enemy leadership.</p>
            <p>In the case you read, killing the man was a <b>side effect</b> of achieving Peter's goal of breaking the window and saving the five children -- i.e., the man's death was "collateral damage". If the man hadn't happened to be there, Peter would still have been able to break the window and save the children.</p>
            <p>Now, imagine if killing the man <i>was</i> necessary to achieve Peter's goal of breaking the window and saving the five children. In other words, imagine if Peter would not have been able to save the five children without killing the man. In this case, the man would have been used as a <b>means</b> to achieve Peter's goal.
            <p>Do you think this fact -- that the man's death <b>would have been necessary</b> in order to achieve Peter's goal -- would have influenced your judgment of whether Peter's actions were morally permissible or not? If so, how?</p>`
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_double1 : introspection_q_labels_double2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br><br>",
    on_finish: function (data) {
        double_intro_response1 = data.response
    }
};

var double_intro_response2 = null;
var double_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        double_intro_response2 = data.response.Q0
    }
};

var double_intro_confidence_response = null;
var double_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        double_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "double effect",
            condition: condition[0] == "Factor-Included" ? "Means" : "Side Effect",
            choice: moral == 0 ? "Permissible" : "Impermissible",
            stimulus: null,
            openq_response: double_openQ_response,
            introspect_rating: double_intro_response1,
            introspect_open: double_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var double_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity=data.response == 0 ? "Yes" : "No"

        
    }
}

var hindsight2 = {
    timeline: [double_instructions, double_question, double_familiar, double_openQ, double_introspect1, double_intro_confidence]
}

//#endregion
//timeline.push(double)
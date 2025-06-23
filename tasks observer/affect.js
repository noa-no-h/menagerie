//#region 5. Affect Effect - BETWEEN

var observedBeneficial = "CHANGE THIS!"
var observedRisky = "CHANGE THIS!"

var confidence_q = condition[0] == 'Factor-Included' ? 
    "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the <b>presence of that passage</b> about the risks of natural gas?)</p>" : 
    "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the <b>presence of that passage</b> about the risks of natural gas?)</p>";

var affect_instructions = {
    type: jsPsychInstructions,
    pages: function () {
        if (condition[0] == 'Factor-Included') {
            return [
                `<p>In this task, we told the Prolific user, \"We are interested in the ways people think about the risks and benefits associated with natural gas. Therefore, in this task, you will be asked to give judgments about the benefits, the risks, and the controllability associated with this technology.\"</p>
                <p><i>Please click the button below to read the passage about natural gas we showed the Prolific user.</i></p>`
            ];
        } else {
            return [
                `<p>We are interested in the ways people think about the risks and benefits associated with several technologies natural gas. Therefore, in this task, you will be asked to give judgments about the benefits, the risks, and the controllability associated with this technology.</p>
                <p><i>Please click the button below to see the first question.</i></p>`
            ];
        }
    },
    show_clickable_nav: true
};

var risk = null;
var benefit = null;

var affect_question = {
    type: jsPsychSurveySlider,
    questions: [
        {
            prompt: `<p>We asked the prolific user, \"In general, how <b>beneficial</b> do you consider the use of natural gas to be to U.S. society as a whole?\" The Prolific user selected ` + observedBeneficial + `. Below, to demonstrate that you understand the Prolific user's choice, please select the option that they selected (regardless of your own beliefs).</p>`,
            name: "benefit",
            ticks: ["Not at all beneficial", "Moderately beneficial", "Very beneficial"],
            required: true,
            min: 0,
            slider_start: 0.5,
            max: 1,
            step: 0.01
        },
        {
            prompt: `<p>We asked the prolific user, \"In general, how <b>risky</b> do you consider the use of natural gas to be to U.S. society as a whole?\"  The Prolific user selected ` + observedRisky + `. Below, to demonstrate that you understand the Prolific user's choice, please select the option that they selected (regardless of your own beliefs).</p>`,
            name: "risk",
            ticks: ["Not at all risky", "Moderately risky", "Very risky"],
            required: true,
            min: 0,
            slider_start: 0.5,
            max: 1,
            step: 0.01
        }
    ],
    on_finish: function (data) {
        var responseObject = JSON.parse(data.response);
        benefit = responseObject["benefit"];
        risk = responseObject["risk"];
        if (only_main_question) {
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "affect heuristic",
            condition: condition[0] == "Factor-Included" ? "With passage" : "without passage",
            choice: benefit,
            stimulus: null,
            auxiliary_info1: risk,
            openq_response: null,
            introspect_rating: null,
            introspect_open: null,
            familiarity: null,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
    },
    randomize_question_order: true
};

var risk_passage = `
    The text below contains some general information about the risks associated with natural gas. We recognize that there are some benefits associated with this technology, but we are not going to deal with those right now. <b>We would like you to carefully read the information given about natural gas. You will be asked later to make a series of judgments regarding each technology.</b><br><br>
    
    Natural gas is the only energy source that is a gas at room temperatures. Because natural gas is a gas, it has unique dangers. Unlike all other forms of energy used in the household, natural gas has the ability to explode. Because it is heavier than air, a natural gas leak does not easily escape from a house or factory but instead tends to remain inside, where a single spark can cause an explosion. In addition to its explosive potential, natural gas can also kill by asphyxiation since a leak can cause the gas to replace the air in a home or factory.<br><br> 
    
    Finally, the use of natural gas produces carbon dioxide as a byproduct of its combustion. Carbon dioxide is the major greenhouse gas produced on Earth. The amount of carbon dioxide in our atmosphere is increasing daily and is already higher than at any time in human history. Many scientists feel that the entire Earth is at risk from global warming brought on by increasing greenhouse gases. The use of natural gas contributes to this problem.
`;

var natural_gas_risk = {
    type: jsPsychHtmlButtonResponse,
    stimulus: risk_passage + "<br><br>",
    choices: ["Continue to questions"],
};

  
var affect_openQ_response = null;
var affect_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, the Prolific user was asked to judge how beneficial the use of natural gas is to U.S. society as a whole, and how risky it is. <b>Focus now just on the judgment they made about how <u>beneficial</u> it is.</b></p><p>Describe what you think their thought process was while judging how beneficial it is. How do you think they came to their eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        affect_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_affect1 = [`<strong>It made me <u>MORE</u> likely to judge natural gas as beneficial</strong>`,"", `<strong>It did not affect my response</strong>`,"",`<strong>It made me <u>LESS</u> likely to judge natural gas as beneficial</strong>`];
var introspection_q_labels_affect2 = [`<strong>It would have made me <u>MORE</u> likely to judge natural gas as beneficial</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me <u>LESS</u> likely to judge natural gas as beneficial</strong>`];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var affect_intro_response1 = null;
var affect_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `
                <p>In this task, you judged how beneficial natural gas is to U.S. society, and how risky it is. <b>Again, focus just on the judgment you made about how <u>beneficial</u> it is.</b></p>
                <p>Before judging its benefits, you first read a passage about the risks of natural gas, including explosion, asphyxiation, and contribution to global warming through carbon dioxide emissions.</p>
                
                <p>Do you think <b>the presence of that passage</b> influenced your judgment of how <u>beneficial</u> natural gas is? If so, how?</p>
            `;
        } else {
            return `
                <p>In this task, you judged how beneficial natural gas is to U.S. society, and how risky it is. <b>Again, focus just on the judgment you made about how <u>beneficial</u> it is.</b></p>
                <p>Imagine that before judging its benefits, you had first read a passage about the risks of natural gas, including explosion, asphyxiation, and contribution to global warming through carbon dioxide emissions.</p>
                
                <p>Do you think <b>the presence of that passage</b> would have influenced your judgment of how <u>beneficial</u> natural gas is? If so, how?</p>
            `;
        }
    },
    labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_affect1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_affect1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_affect2;
        } else {
            return introspection_q_labels_affect2.slice().reverse();
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
            affect_intro_response1 = data.response
    }
        else {
                affect_intro_response1 = 100 - data.response;
            }

        }
};

var affect_intro_response2 = null;
var affect_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        affect_intro_response2 = data.response.Q0
    }
};

var affect_intro_confidence_response = null;
var affect_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        affect_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "affect heuristic",
            condition: condition[0] == "Factor-Included" ? "With passage" : "without passage",
            choice: benefit,
            stimulus: null,
            auxiliary_info1: null,
            openq_response: affect_openQ_response,
            introspect_rating: affect_intro_response1,
            introspect_open: affect_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var affect_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity=data.response == 0 ? "Yes" : "No"

        
    }
}



if (only_main_question) {
    if (condition[0] == "Factor-Included") {
        var affect = {
            timeline: [affect_instructions, natural_gas_risk, affect_question]
        };
    } else {
        var affect = {
            timeline: [affect_instructions, affect_question]
        };
    }
} else {
    if (condition[0] == "Factor-Included") {
        var affect = {
            timeline: [affect_instructions, natural_gas_risk, affect_question, affect_familiar, affect_openQ, affect_introspect1, affect_intro_confidence]
        };
    } else {
        var affect = {
            timeline: [affect_instructions, affect_question, affect_familiar, affect_openQ, affect_introspect1, affect_intro_confidence]
        };
    }
}


//#endregion
//timeline.push(affect)
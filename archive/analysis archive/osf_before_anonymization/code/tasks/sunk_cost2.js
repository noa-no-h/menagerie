//#region 5. sunk_cost2 Effect - BETWEEN


var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the fact that you had already spent 9 million dollars)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the fact that you had already spent 9 million dollars)?</p>";

var sunk_cost2_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> In this exercise, you will be given a hypothetical scenario and then asked what action you would take.</p>
    <p><i>Please click the button below to view the scenario.</i>`,
    ],
    show_clickable_nav: true
}

var stimulus = function(){if (condition[0] == "Factor-Included") {
    return `<p><b>Please consider the following scenario:</b></p>
<p>As the president of an airline company, you have invested 10 million dollars of the company’s money into a research project. The purpose was to build a plane that would not be detected by conventional radar, in other words, a radar-blank plane. When the project is 90% completed (and 9 million dollars have already been spent), another firm begins marketing a plane that cannot be detected by radar. Also, it is apparent that their plane is much faster and far more economical than the plane your company is building. <br><br>The question is: should you spend the last million dollars of your research fund to finish the radar-blank plane?</p>`
} else {
    return `<p><b>Please consider the following scenario:</b></p>
<p>As president of an airline company, you have received a suggestion from one of your employees. The suggestion is to use the last 1 million dollars of your research fund to develop a plane that would not be detected by conventional radar, in other words, a radar-blank plane. However, another firm has just begun marketing a plane that cannot be detected by radar. Also, it is apparent that their plane is much faster and far more economical than the plane your company could build. <br><br>The question is: should you spend the last million dollars of your research fund to build the radar-blank plane?</p>`
}}


var choice = null;
var sunk_cost2_question = {
    type: jsPsychSurveyMultiChoice,
    questions: [
        { prompt: stimulus,
        required: true, 
        name: "response",
        options: ["Yes", "No"] }],
    on_finish: function (data) {
        choice = data.response["response"]
        if (only_main_question) {
                s1_data = {
                    subject: data.subject,
                    version: data.version,
                    factor: data.condition,
                    task_name: "sunk_cost2 effect",
                    condition: condition[0] == "Factor-Included" ? "Sunk Cost" : "No Sunk Cost",
                    choice: choice == "Yes" ? "Continue Investing" : "Don't Continue Investing",
                    stimulus: null,
                    openq_response: null,
                    introspect_rating: null,
                    introspect_open: null,
                    familiarity: null,
                    rt_main_question: data.rt
                }
                save_data(s1_data, 'introspection')
            
        }
    }
}

var sunk_cost2_openQ_response = null;
var sunk_cost2_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this task, you were asked what action you would take in a hypothetical scenario.</p><p>Describe your thought process while choosing your action. How did you come to your eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        sunk_cost2_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_sunk_cost21 = [`<strong>It made me <u>MORE</u> likely to spend the last million dollars to finish the plane`, "", "<strong>It did not affect my response</strong>", "", `<strong>It made me <u>LESS</u> likely to spend the last million dollars to finish the plane`];
var introspection_q_labels_sunk_cost22 = [`<strong>It would have made me <u>MORE</u> likely to spend the last million dollars to finish the plane`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me <u>LESS</u> likely to spend the last million dollars to finish the plane`];

var sunk_cost2_intro_response1 = null;
var sunk_cost2_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this task, you were asked whether you would spend the last million dollars of your research fund to finish the radar-blank plane.</p>
                <p>You were told that you had already spent 9 million dollars on this research project.</p>
                <p>Do you think <b>the fact that you had already spent 9 million dollars on the project</b> affected your response? If so, how?</p>`;
         } else {
            return `<p>In this task, you were asked whether you would spend the last million dollars of your research fund to build the radar-blank plane.</p>
                <p>Now imagine that you had already spent 9 million dollars on this research project.</p></p>
                <p>Do you think <b>the fact that you had already spent 9 million dollars on the project</b> would have affected your response? If so, how?</p>`;
         }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_sunk_cost21 : introspection_q_labels_sunk_cost22,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {
        sunk_cost2_intro_response1 = data.response
    }
};

var sunk_cost2_intro_response2 = null;
var sunk_cost2_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        sunk_cost2_intro_response2 = data.response.Q0
    }
};

var sunk_cost2_intro_confidence_response = null;
var sunk_cost2_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        sunk_cost2_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "sunk_cost2 effect",
            condition: condition[0] == "Factor-Included" ? "Sunk Cost" : "No Sunk Cost",
            choice: choice == "Yes" ? "Continue Investing" : "Don't Continue Investing",
            stimulus: null,
            openq_response: sunk_cost2_openQ_response,
            introspect_rating: sunk_cost2_intro_response1,
            introspect_open: sunk_cost2_intro_confidence_response,
            familiarity: familiarity,
            rt_main_question: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var sunk_cost2_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity=data.response == 0 ? "Yes" : "No"

        
    }
}


if (only_main_question) {
    var sunk_cost2 = {
        timeline: [sunk_cost2_instructions, sunk_cost2_question]
    };
} else {
    var sunk_cost2 = {
        timeline: [sunk_cost2_instructions, sunk_cost2_question, sunk_cost2_familiar, sunk_cost2_openQ, sunk_cost2_introspect1, sunk_cost2_intro_confidence]
    };
}

//#endregion
//timeline.push(sunk_cost2)
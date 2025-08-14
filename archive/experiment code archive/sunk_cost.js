//#region 5. sunk_cost Effect - BETWEEN


var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the fact that you had already spent $90 on the rocket engine design)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the fact that you had already spent $90 on the rocket engine design)?</p>";

var sunk_cost_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> In this exercise, you will be given a hypothetical scenario and then asked what action you would take.</p>
    <p><i>Please click the button below to view the scenario.</i>`,
    ],
    show_clickable_nav: true
}



var choice = null;
var sunk_cost_question = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p><b>Please consider the following scenario:</b></p>
    <p>Imagine you are a student and are planning to submit an entry to the 'new invention' competition organized by the students' club.
    <p>You have spent $90 preparing a design for an innovative rocket engine and estimate that it will take you an additional $30 to finish it.
    <p> You just learned that <strong>the winner of the previous year's competition is also working on a rocket engine </strong> design similar to yours. You worry that the winner's design will make yours look worse in comparison.    
    <p>You had also thought about working on an (equally innovative and good) design for a solar-powered pump that would cost about $30 to complete.
    <p>Based on on this scenario, please complete the following sentence by clicking one of the buttons below:
    <p> You can submit only one entry, and since the deadline  is very close, you must choose now. The question is - Should you spend $30 trying to finish your rocket engine design given what you know, or would you rather work on the solar- powered pump?
    </p>`
        } else {
            return `<p><b>Please consider the following scenario:</b></p>
    <p>Imagine you are a student and are planning to submit an entry to the 'new invention' competition organized by the students' club.
    <p>You thought about preparing a design for an innovative rocket engine, and you estimate that it will cost approximately $30 to finish it.
    <p> You just learned that <strong>the winner of the previous year's competition is also working on a rocket engine </strong> design similar to yours. You worry that the winner's design will make yours look worse in comparison.    
    <p>You had also thought about working on an (equally innovative and good) design for a solar-powered pump that would cost about $30 to complete.
    <p>Based on on this scenario, please complete the following sentence by clicking one of the buttons below:
    <p> You can submit only one entry, and since the deadline  is very close, you must choose now. The question is - Should you spend $30 trying to finish your rocket engine design given what you know, or would you rather work on the solar- powered pump?
    </p>`
        }
    },
    choices: ["Rocket Engine ", "Solar-powered Pump"],
    on_finish: function (data) {
        choice = data.response
    }
}

var sunk_cost_openQ_response = null;
var sunk_cost_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were asked what action you would take in a hypothetical scenario.</p><p>Describe your thought process while choosing your action. How did you come to your eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        sunk_cost_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_sunk_cost1 = [`<strong>It made me more likely to choose the <u>ROCKET ENGINE</u>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It made me more likely to choose the <u>SOLAR-POWERED PUMP</u>`];
var introspection_q_labels_sunk_cost2 = [`<strong>It would have made me more likely to choose the <u>ROCKET ENGINE</u>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me more likely to choose the <u>SOLAR-POWERED PUMP</u>`];

var sunk_cost_intro_response1 = null;
var sunk_cost_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this exercise, you were asked which design to submit to the new invention competition, given that the winner of the previous year's competition was also working on a similar rocket engine.</p>
                <p>We also told you that you had already spent $90 on your rocket engine design.</p>
                <p>Do you think <b>the fact that you had already spent $90</b> on the rocket engine design affected your response? If so, how?</p>`;
         } else {
            return `<p>In this exercise, you were asked which design to submit to the new invention competition, given that the winner of the previous year's competition was also working on a similar rocket engine.</p>
                <p>Now imagine that you had already spent $90 on your rocket engine design.</p>
                <p>Do you think <b>the fact that you had already spent $90</b> on the rocket engine design would have affected your response? If so, how?</p>`;
         }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_sunk_cost1 : introspection_q_labels_sunk_cost2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {
        sunk_cost_intro_response1 = data.response
    }
};

var sunk_cost_intro_response2 = null;
var sunk_cost_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        sunk_cost_intro_response2 = data.response.Q0
    }
};

var sunk_cost_intro_confidence_response = null;
var sunk_cost_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        sunk_cost_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "sunk_cost effect",
            condition: condition[0] == "Factor-Included" ? "Sunk Cost" : "No Sunk Cost",
            choice: choice == 0 ? "Rocket Engine" : "Solar-powered Pump",
            stimulus: null,
            openq_response: sunk_cost_openQ_response,
            introspect_rating: sunk_cost_intro_response1,
            introspect_open: sunk_cost_intro_confidence_response,
            familiarity: familiarity,
            rt_main_question: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var sunk_cost_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity=data.response == 0 ? "Yes" : "No"

        
    }
}


if (only_main_question) {
    var sunk_cost = {
        timeline: [sunk_cost_instructions, sunk_cost_question]
    };
} else {
    var sunk_cost = {
        timeline: [sunk_cost_instructions, sunk_cost_question, sunk_cost_familiar, sunk_cost_openQ, sunk_cost_introspect1, sunk_cost_intro_confidence]
    };
}

//#endregion
//timeline.push(sunk_cost)
//#region 5. simulation Effect - BETWEEN

var simulation_instructions = {
    type: jsPsychInstructions,
    pages: [
                `<p> In this exercise, you will be given a hypothetical scenario and then asked to judge how upset someone would be in that scenario.</p>
    <p><i>Please click the button below to view the scenario.</i>`]
    ,    show_clickable_nav: true

    
};

var risk = null;
var benefit = null;
var person = condition[0] == 'Factor-Included' ? "Mr. C" : "Mr. D";

var confidence_q = condition[0] == 'Factor-Included' ? 
    `<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the <b>fact that ${person} barely missed his flight</b>?)</p>` : 
    `<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the <b>fact that ${person} barely missed his flight</b>?)</p>`;

var prompt = condition[0] == 'Factor-Included' ?
    `Mr. C traveled to the airport from town in a limousine, was caught in a traffic jam, and arrived at the airport 30 minutes after the scheduled departure of his flight. Mr. C is told that his flight was delayed, and only left 5 minutes ago. <br><br> On the following scale, how upset do you expect Mr. C to be over this episode?` :
    `Mr. D traveled to the airport from town in a limousine, was caught in a traffic jam, and arrived at the airport 30 minutes after the scheduled departure of his flight. Mr. D is told that his flight left on time. <br><br> On the following scale, how upset do you expect Mr. D to be over this episode?`;

var simulation_question = {
    type: jsPsychSurveySlider,
    questions: [
        {
            prompt: prompt,
            name: "upset",
            ticks: ["Not upset", "", "Somewhat upset", "", "Extremely upset"],
            required: true,
            min: 1,
            slider_start: 50,
            max: 100,
            step: 1,
            slider_width: 1500
        }
    ],
    on_finish: function (data) {
        var responseObject = JSON.parse(data.response);
        upset = responseObject["upset"];
        if (only_main_question) {
            s1_data = {
                subject: data.subject,
                version: data.version,
                factor: data.condition,
                task_name: "simulation",
                condition: condition[0] == "Factor-Included" ? "barely missed" : "missed",
                choice: upset,
                stimulus: null,
                auxiliary_info1: null,
                openq_response: null,
                introspect_rating: null,
                introspect_open: null,
                familiarity: null,
                rt_main_question: data.rt
            }
            //console.log("data to save: " + JSON.stringify(s1_data));
            save_data(s1_data, 'introspection')
        }
        
    }
};


var simulation_openQ_response = null;
var simulation_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were asked to judge how upset ${person} was in the scenario.</p><p>Describe your thought process while judging their level of upset. How did you come to your eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        simulation_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_simulation1 = [`<strong>It made me think ${person} would be <u>MORE</u> upset</strong>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It made me think ${person} would be <u>LESS</u> upset</strong>`];
var introspection_q_labels_simulation2 = [`<strong>It would have made me think ${person} would be <u>MORE</u> upset</strong>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It would have made me think ${person} would be <u>LESS</u> upset</strong>`];

var simulation_intro_response1 = null;
var simulation_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `
                <p>In this task, you judged how upset you expected ${person} to be after missing his plane.</p>
                <p>You were first told that ${person} barely missed his flight (his plane left 5 minutes before he arrived).</p>
                <p>Do you think <b>the fact that ${person} barely missed his flight</b> influenced your judgment of how upset he was? If so, how?</p>
            `;
        } else {
            return `
                <p>In this task, you judged how much upset you expected ${person} to be after missing his plane.</p>
                <p>Imagine you were first told that ${person} had barely missed his flight (his plane had left 5 minutes before he arrived).</p>
                <p>Do you think <b>the fact that ${person} had barely missed his flight</b> would have influenced your judgment of how upset he was? If so, how?</p>
            `;
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_simulation1 : introspection_q_labels_simulation2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {
        simulation_intro_response1 = data.response
    }
};

var simulation_intro_response2 = null;
var simulation_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        simulation_intro_response2 = data.response.Q0
    }
};

var simulation_intro_confidence_response = null;
var simulation_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        simulation_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "simulation",
            condition: condition[0] == "Factor-Included" ? "barely missed" : "missed",
            choice: upset,
            stimulus: null,
            auxiliary_info1:  null,
            openq_response: simulation_openQ_response,
            introspect_rating: simulation_intro_response1,
            introspect_open: simulation_intro_confidence_response,
            familiarity: familiarity,
            rt_main_question: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var simulation_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity=data.response == 0 ? "Yes" : "No"

        
    }
}



if (only_main_question) {
    var simulation = {
        timeline: [simulation_instructions, simulation_question]
    };
} else {
    var simulation = {
        timeline: [simulation_instructions, simulation_question, simulation_familiar, simulation_openQ, simulation_introspect1, simulation_intro_confidence]
    };
}


//#endregion
//timeline.push(simulation)
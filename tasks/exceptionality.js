//#region 5. exceptionality Effect - BETWEEN

var confidence_q = condition[0] == 'Factor-Included' ? 
    "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the <b>fact that Mr. Smith frequently takes hitch-hikers in his car</b>?)</p>" : 
    "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the <b>fact that Mr. Jones frequently takes hitch-hikers in his car</b>?)</p>";

var exceptionality_instructions = {
    type: jsPsychInstructions,
    pages: [
                `<p> In this exercise, you will be given a hypothetical scenario and then asked to judge the amount of regret someone would experience in that scenario.</p>
    <p><i>Please click the button below to view the scenario.</i>`]
    ,    show_clickable_nav: true

    
};

var risk = null;
var benefit = null;
var person = condition[0] == 'Factor-Included' ? "Mr. Smith" : "Mr. Jones";

var prompt = condition[0] == 'Factor-Included' ?
    `Mr. Smith frequently takes hitch-hikers in his car. Yesterday he gave a man a ride and was robbed.<br><br> On the following scale, how much regret do you expect Mr. Jones experienced over this episode?` :
    `Mr. Jones almost never takes hitch-hikers in his car. Yesterday he gave a man a ride and was robbed.<br><br> On the following scale, how much regret do you expect Mr. Jones experienced over this episode?`;

var exceptionality_question = {
    type: jsPsychSurveySlider,
    questions: [
        {
            prompt: prompt,
            name: "regret",
            ticks: ["Very little regret", "Moderate regret", "Very much regret"],
            required: true,
            min: 0,
            slider_start: 0.5,
            max: 1,
            step: 0.01
        }
    ],
    on_finish: function (data) {
        var responseObject = JSON.parse(data.response);
        regret = responseObject["regret"];
    }
};


var exceptionality_openQ_response = null;
var exceptionality_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were asked to judge how much regret ${person} experienced in the scenario.</p><p>Describe your thought process while judging their regret. How did you come to your eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        exceptionality_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_exceptionality1 = [`<strong>It made me likely to judge ${person} as <u>MORE</u> regretful</strong>`, "", "<strong>It did not affected my response</strong>", "", `<strong>It made me likely to judge ${person} as <u>LESS</u> regretful</strong>`];
var introspection_q_labels_exceptionality2 = [`<strong>It would have made me likely to judge ${person} as <u>MORE</u> regretful</strong>`, "", "<strong>It did not affected my response</strong>", "", `<strong>It would have made me likely to judge ${person} as <u>LESS</u> regretful</strong>`];

var exceptionality_intro_response1 = null;
var exceptionality_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `
                <p>In this task, you judged how much regret you expected ${person} to experience after being robbed by a hitch-hiker.</p>
                <p>You were first told that Mr. Smith frequently takes hitch-hikers in his car.</p>
                <p>Do you think <b>the fact that Mr. Smith frequently takes hitch-hikers in his car</b> influenced your judgment of how much regret he experienced? If so, how?</p>
            `;
        } else {
            return `
                <p>In this task, you judged how much regret you expected ${person} to experience after being robbed by a hitch-hiker.</p>
                <p>Imagine you were first told that Mr. Jones frequently takes hitch-hikers in his car.</p>
                <p>Do you think <b>the fact that Mr. Jones frequently takes hitch-hikers in his car</b> would have influenced your judgment of how much regret he experienced? If so, how?</p>
            `;
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_exceptionality1 : introspection_q_labels_exceptionality2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {
        exceptionality_intro_response1 = data.response
    }
};

var exceptionality_intro_response2 = null;
var exceptionality_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        exceptionality_intro_response2 = data.response.Q0
    }
};

var exceptionality_intro_confidence_response = null;
var exceptionality_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        exceptionality_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "exceptionality",
            condition: condition[0] == "Factor-Included" ? "frequently" : "almost never",
            choice: regret,
            stimulus: null,
            auxiliary_info1: null,
            openq_response: exceptionality_openQ_response,
            introspect_rating: exceptionality_intro_response1,
            introspect_open: exceptionality_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var exceptionality_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity=data.response == 0 ? "Yes" : "No"

        
    }
}



if (only_main_question) {
    var exceptionality = {
        timeline: [exceptionality_instructions, exceptionality_question]
    };
} else {
    var exceptionality = {
        timeline: [exceptionality_instructions, exceptionality_question, exceptionality_familiar, exceptionality_openQ, exceptionality_introspect1, exceptionality_intro_confidence]
    };
}


//#endregion
//timeline.push(exceptionality)
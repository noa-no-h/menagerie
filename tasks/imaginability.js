//#region 5. imaginability Effect - BETWEEN

var confidence_q = "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by whether the symptoms were easy to imagine)?</p>";


var imaginability_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> In this exercise, you will be given a hypothetical scenario and then asked to judge a probability.</p>
    <p><i>Please click the button below to view the scenario.</i>`,
    ],
    show_clickable_nav: true
}

var choice = null;
var imaginability_question = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `Imagine an illness called Hypo-A is becoming increasingly prevalent in your city. 
            <p>The symptoms of this disease are: low energy level, muscle aches, and frequent severe headaches. 
            <p>Now judge how likely it you could contract Hypo-A in the future`
        } else {
            return `Imagine an illness called Scenia-B is becoming increasingly prevalent in your city. 
            <p>The symptoms of this disease are: vague sense of disorientation, a malfunctioning nervous system and an inflamed liver.
            <p>Now judge how likely it you could contract Scenia-B in the future`
        }
    },
    labels: ['very unlikely (1)', '2', '3', '4', '5', '6', '7', '8', '9', 'very likely (10)'],
    slider_width: introspection_q_slider_width,
    min: -100,
    max: 0,
    slider_start: -50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br><br>",    
    on_finish: function (data) {
        choice = data.response
    }
}

var imaginability_openQ_response = null;
var imaginability_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were asked to judge the probability of contracting a disease after reading about its symptoms.</p>
        <p>Describe your thought process while judging the likelihood of catching the disease. How did you come to your eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        imaginability_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_imaginability1 = [`<strong>It made me think it was <u>LESS</u> likely I would contract the disease</strong>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It made me think it was <u>MORE</u>  likely I would contract the disease</strong>`];
var introspection_q_labels_imaginability2 = [`<strong>It would have me think it was  <u>LESS</u> likely I would contract the disease</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me think it was <u>MORE</u>  likely I would contract the disease</strong>`];

var imaginability_intro_response1 = null;
var imaginability_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this task, you read about a disease with symptoms that were likely easy to imagine experiencing (a low energy level, muscle aches, and frequent severe headaches). 
            <p>Do you think the fact that the symptoms were <b>easy to imagine</b> influenced your judgment of how likely you were to contract the disease? If so, how?</p>`
        } else {
            return `<p>In this task, you read about a disease with symptoms  that were likely difficult to imagine experiencing (a vague sense of disorientation, a malfunctioning nervous system and an inflamed liver).
            <p>Now, imagine if the symptoms had instead been <i> easy to imagine experiencing</i> (such as a low energy level, muscle aches, and frequent severe headaches). 
            <p>Do you think the fact that the symptoms would be <b>easy to imagine</b> would have influenced your judgment of how likely you were to contract the disease? If so, how?</p>`
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_imaginability1 : introspection_q_labels_imaginability2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br><br>",
    on_finish: function (data) {
        imaginability_intro_response1 = data.response
    }
};

var imaginability_intro_response2 = null;
var imaginability_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        imaginability_intro_response2 = data.response.Q0
    }
};

var imaginability_intro_confidence_response = null;
var imaginability_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        imaginability_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "imaginability effect",
            condition: condition[0] == "Factor-Included" ? "Easy to Imagine" : "Difficult to Imagine",
            choice: choice,
            stimulus: null,
            openq_response: imaginability_openQ_response,
            introspect_rating: imaginability_intro_response1,
            introspect_open: imaginability_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var imaginability_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity=data.response == 0 ? "Yes" : "No"

        
    }
}

var imaginability = {
    timeline: [imaginability_instructions, imaginability_question, imaginability_familiar, imaginability_openQ, imaginability_introspect1, imaginability_intro_confidence]
}

//#endregion
//timeline.push(imaginability)
//#region 6. Decoy Effect (Kaptein et al., 2016) - BETWEEN

var observedChoice = "CHANGE THIS"

var confidence_q = condition[0] == 'Factor-Included' ? '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by the presence of Brand W)?</p>' : '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the presence of Brand W)?</p>';

var decoy_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, the Prolific user was presented with a scenario where they had to choose between several consumer products. These products are similar to those you would see at a grocery store.<p> We told the Prolific user that, when choosing between these products, there are no right or wrong answers. We were simply looking for their opinion.</p>
    <p><i>Please click the button below to continue.</i></p>`
    ],
    show_clickable_nav: true
}

var juice = null
var decoy_present = {
    type: jsPsychSurveyMultiChoice,
    questions: [
        {
            prompt: "We asked the Prolific user to imagine that they were choosing between several brands of frozen concentrated orange juice. For each brand, they knew only the price and the quality ratings made by consumer reports. Given that they had to buy one brand based on this information alone, we asked them which one would it be? In case of the quality rating, 0 = worst quality and 100 = ideal quality. The three brands were:<br><br> Brand W: Price per can = $1.20; Quality rating = 30 <br> Brand N: Price per can = $1.20; Quality rating = 50 <br> Brand J: Price per can = $2.00; Quality rating = 70<br><br> The Prolific user selected " + observedChoice + ".<br><br>To demonstrate that you understand the Prolific user's choice, <b>please select the option that they selected (regardless of your own preferences).</b>",
            options: [
                '<b>Brand W:</b> Price per can = $1.20; Quality rating = 30',
                '<b>Brand N:</b> Price per can = $1.20; Quality rating = 50',
                '<b>Brand J:</b> Price per can = $2.00; Quality rating = 70',
            ],
            required: required_general,
        }
    ],
    on_finish: function (data) {
        if (data.response.Q0 == '<b>Brand N:</b> Price per can = $1.20; Quality rating = 50') {
            juice = 'Brand N (Target)'
        } else if (data.response.Q0 == '<b>Brand J:</b> Price per can = $2.00; Quality rating = 70') {
            juice = 'Brand J (Competitor)'
        } else {
            juice = 'Brand W (decoy)'
        }
    }
};

var decoy_absent = {
    type: jsPsychSurveyMultiChoice,
    questions: [
        {
            prompt: "Imagine that you are choosing between several brands of frozen concentrated orange juice. For each brand, you know only the price and the quality ratings made by consumer reports. Given that you had to buy one brand based on this information alone, which one would it be? In case of the quality rating, 0 = worst quality and 100 = ideal quality.",
            options: [
                '<b>Brand N:</b> Price per can = $1.20; Quality rating = 50',
                '<b>Brand J:</b> Price per can = $2.00; Quality rating = 70',
            ],
            required: required_general,
        }
    ],
    on_finish: function (data) {
        if (data.response.Q0 == '<b>Brand N:</b> Price per can = $1.20; Quality rating = 50') {
            juice = 'Brand N (Target)'
        } else {
            juice = 'Brand J (Competitor)'
        }
    }
};

var decoy_question = null;
if (condition[0] == "Factor-Included") {
    decoy_question = decoy_present
} else {
    decoy_question = decoy_absent
}

var decoy_openQ_response = null;
var decoy_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, the Prolific user was asked to choose between several brands of frozen orange juice concentrate.</p><p>Describe what you think their thought process was during this choice. How do you think they came to their eventual decision about which juice to purchase?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        decoy_openQ_response = data.response.Q0
    }
};

var introspection_q_labels_decoy1 = [`<strong>It made them <u>LESS</u> likely to choose Brand N (and more likely to choose Brand J)</strong>`, "", "<strong>It did not affect their response</strong>", "", `<strong>It made them <u>MORE</u> likely to choose Brand N (and less likely to choose Brand J)</strong>`];
var introspection_q_labels_decoy2 = [`<strong>It would have made me <u>LESS</u> likely to choose Brand N (and more likely to choose Brand J)</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me <u>MORE</u> likely to choose Brand N (and less likely to choose Brand J)</strong>`];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var decoy_intro_response1 = null;
var decoy_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>Below are the options the Prolific user was asked to choose from:</p>
        <p><b>Brand W:</b> Price per can = $1.20; Quality rating = 30</p>
        <p><b>Brand N:</b> Price per can = $1.20; Quality rating = 50</p>
        <p><b>Brand J:</b> Price per can = $2.00; Quality rating = 70</p>
        <p>Do you think the <b>presence of Brand W</b> affected their preference <b>between Brand N and Brand J?</b> If so, how?</p>`
        } else {
            return `<p>Below are the options you were asked to choose from:</p>
        <p><b>Brand N:</b> Price per can = $1.20; Quality rating = 50</p>
        <p><b>Brand J:</b> Price per can = $2.00; Quality rating = 70</p>
        <p>Now, imagine if you were given a <i>third option</i>, as follows:</p>
        <p><b>Brand W:</b> Price per can = $1.20; Quality rating = 30</p>
        <p>Do you think the <b>presence of this option</b> would have affected your preference <b>between Brand N and Brand J?</b> If so, how?</p>`
        }
    },
labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_decoy1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_decoy1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_decoy2;
        } else {
            return introspection_q_labels_decoy2.slice().reverse();
        }
    },    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {

        if (label_order_randomized == 'original') {
            decoy_intro_response1 = data.response
    }
        else {
            decoy_intro_response1 = 100 - data.response;
            }
        }
};

var decoy_intro_response2 = null;
var decoy_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        decoy_intro_response2 = data.response.Q0
    }
};

var decoy_intro_confidence_response = null;
var decoy_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        decoy_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "decoy effect",
            condition: condition[0] == "Factor-Included" ? "Decoy Present" : "Decoy Absent",
            stimulus: null,
            choice: juice,
            auxiliary_info1: null,
            openq_response: decoy_openQ_response,
            introspect_rating: decoy_intro_response1,
            introspect_open: decoy_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var decoy_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity= data.response == 0 ? "Yes" : "No";
        
    }
}



if (only_main_question) {
    var decoy = {
        timeline: [decoy_instructions, decoy_question]
    };
} else {
    var decoy = {
        timeline: [decoy_instructions, decoy_question, decoy_familiar, decoy_openQ, decoy_introspect1, decoy_intro_confidence]
    };
}

//#endregion
//timeline.push(decoy)
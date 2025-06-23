//#region Reference Price - BETWEEN (Thaler, 2008)


var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by the fanciness of the hotel selling the beer)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the fanciness of the hotel selling the beer)?</p>";

var ref_price_hotel_stimulus = `Scenario: You are lying on the beach on a hot day. All you have to drink is ice water. For the last hour you have been thinking about how much you would enjoy a nice cold bottle of your favorite brand of beer. Your friend gets up to go make a phone call and offers to bring back a beer from the only nearby place where beer is sold: a fancy 5-star hotel. He says that the beer might be expensive and so asks how much you are willing to pay for the beer. He says that he will buy the beer if it costs as much or less than the price you state. But if it costs more than the price you state he will not buy it. You trust your friend, and there is no possibility of bargaining with the bartender. The Prolific user was asked what price they would tell him. The Prolific user selected ` + observedChoice + `. Below, to demonstrate that you understand the Prolific user's choice, please move the slider to the option that they selected (regardless of your own beliefs). (Please answer in dollars with only a number)`;

var ref_price_motel_stimulus = `Scenario: You are lying on the beach on a hot day. All you have to drink is ice water. For the last hour you have been thinking about how much you would enjoy a nice cold bottle of your favorite brand of beer. Your friend gets up to go make a phone call and offers to bring back a beer from the only nearby place where beer is sold: a run-down 1-star motel. He says that the beer might be expensive and so asks how much you are willing to pay for the beer. He says that he will buy the beer if it costs as much or less than the price you state. But if it costs more than the price you state he will not buy it. You trust your friend, and there is no possibility of bargaining with the store owner. What price do you tell him? (Please answer in dollars with only a number)`;

var ref_price_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, the Prolific user was given a hypothetical scenario and asked what they would do in that scenario.</p>
        <p><i>Please click the "Next" button when you are ready to see the scenario and the Prolific user's options.</i></p>`
    ],
    show_clickable_nav: true
};


var choice = null;

var ref_price_trial = {
    timeline: [{

        type: jsPsychSurvey,
    survey_json: {
        showQuestionNumbers: false,
        focusFirstQuestionAutomatic: true,
        completeText: "Next",
        pages: [
            {
                name: "page1",
                elements: [
                    {
                        type: "text",
                        name: "referencePrice",
                        maskType: "numeric",
                        maskSettings: {
                            "precision": 1
                          },
                        title: function() {
                            return condition[0] == 'Factor-Included' ? ref_price_hotel_stimulus : ref_price_motel_stimulus;
                        },
                        isRequired: true,
                        placeHolder: "Enter your dollar amount here",
                        size: 25
                    }
                ]
            }
        ]
    },
        on_finish: function (data) {
            console.log(data.response);
            choice = data.response.referencePrice;
        }
    }],
    randomize_order: false
};

var ref_price_openQ_response = null;
var ref_price_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, the Prolific user was told about a scenario where they wanted a beer and they were asked what price they would be willing to pay for the beer.</p><p>Describe what you think the thought process was behind their decision about what price they would be willing to pay for the beer. How do you think they came to their eventual decision?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        ref_price_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_ref_price1 = ['<strong>It made the price they were willing to pay <u>LOWER</u></strong>', "", '<strong>It did not affect their response</strong>', "", '<strong>It made the price they were willing to pay <u>HIGHER</u></strong>'];
var introspection_q_labels_ref_price2 = ['<strong>It would have made the price I was willing to pay <u>LOWER</u></strong>', "", '<strong>It would not have affected my response</strong>', "", '<strong>It would have made the price I was willing to pay <u>HIGHER</u></strong>'];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';


    
var ref_price_intro_response1 = null;
var ref_price_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this exercise, the Prolific user was asked the most they would be willing to pay for the beer in a fancy 5-star hotel.</p>
            <p>How do you think the <b>quality of the location selling the beer</b> affected the response about the most they would be willing to pay for the beer?</p>`;
        } else {
            return `<p>In this exercise, you were asked the most you would be willing to pay for the beer.</p>
            <p>Now, imagine if you had instead been told your friend was going to a fancy 5-star hotel.</p>
            <p>If this were the case, do you think the <b>quality of the location selling the beer</b> would have affected your response about the most you would be willing to pay for the beer? If so, how?</p>`;
        }
    },
labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_ref_price1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_ref_price1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_ref_price2;
        } else {
            return introspection_q_labels_ref_price2.slice().reverse();
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
            ref_price_intro_response1 = data.response
    }
        else {
            ref_price_intro_response1 = 100 - data.response;
            }
        }
};

var ref_price_intro_response2 = null;
var ref_price_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        ref_price_intro_response2 = data.response.Q0;
    }
};

var ref_price_intro_confidence_response = null;
var ref_price_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        ref_price_intro_confidence_response = data.response;
        var s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "reference price",
            condition: condition[0] == "Factor-Included" ? "hotel" : "motel",
            stimulus: null,
            choice: choice,
            auxiliary_info1: null,
            openq_response: ref_price_openQ_response,
            introspect_rating: ref_price_intro_response1,
            introspect_open: ref_price_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        };
        console.log("s1_data", s1_data);
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var ref_price_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity= data.response == 0 ? "Yes" : "No";

        
    }
};


if (only_main_question) {
    var reference_price = {
        timeline: [ref_price_instructions, ref_price_trial]
    };
} else {
    var reference_price = {
        timeline: [ref_price_instructions, ref_price_trial, ref_price_familiar, ref_price_openQ, ref_price_introspect1, ref_price_intro_confidence]
    };
}

// end region reference price 
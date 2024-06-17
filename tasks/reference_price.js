//#region Reference Price - BETWEEN (Thaler, 2008)

var ref_price_hotel_stimulus = `Scenario: You are lying on the beach on a hot day. All you have to drink is ice water. For the last hour you have been thinking about how much you would enjoy a nice cold bottle of your favorite brand of beer. gets up to go make a phone call and offers to bring back a beer from the only nearby place where beer is sold — a fancy 5-star hotel. He says that the beer might be expensive and so asks how much you are willing to pay for the beer. He says that he will buy the beer if it costs as much or less than the price you state. But if it costs more than the price you state he will not buy it. You trust your friend, and there is no possibility of bargaining with the bartender. What price do you tell him? (Please answer in dollars with only a numerical value)`;

var ref_price_motel_stimulus = `Scenario: You are lying on the beach on a hot day. All you have to drink is ice water. For the last hour you have been thinking about how much you would enjoy a nice cold bottle of your favorite brand of beer. gets up to go make a phone call and offers to bring back a beer from the only nearby place where beer is sold — a run-down 1-star motel. He says that the beer might be expensive and so asks how much you are willing to pay for the beer. He says that he will buy the beer if it costs as much or less than the price you state. But if it costs more than the price you state he will not buy it. You trust your friend, and there is no possibility of bargaining with the store owner. What price do you tell him? (Please answer in dollars with only a numerical value)`;

var ref_price_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, you will be given a hypothetical scenario and asked what you would do in that scenario.</p>
        <p><i>Please click the "Next" button when you are ready to see the scenario and your options.</i></p>`
    ],
    show_clickable_nav: true
};

var choice = null;
var ref_price_trial = {
    timeline: [{
        type: jsPsychSurveyText,
        questions: [{
            prompt: condition[0] == 'Factor-Included' ? ref_price_hotel_stimulus : ref_price_motel_stimulus,
            required: required_general, rows: 2, columns: 1
        }],
        on_finish: function (data) {
            console.log(data.response);
            choice = data.response.Q0;
        }
    }],
    randomize_order: false
};

var ref_price_openQ_response = null;
var ref_price_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were shown a scenario and asked what price you would tell your friend.</p><p>Describe your thought process behind your decision about what price to tell your friend. How did you come to your eventual decision?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        ref_price_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_ref_price1 = ['<strong>It would have made the price I told my friend higher</strong>', "", '<strong>It would not have affected my response</strong>', "", '<strong>It would have made the price I told my friend lower</strong>'];
var introspection_q_labels_ref_price2 = ['<strong>It would have made the price I told my friend higher</strong>', "", '<strong>It would not have affected my response</strong>', "", '<strong>It would have made the price I told my friend lower</strong>'];

var ref_price_intro_response1 = null;
var ref_price_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this exercise, you were asked the most you would be willing to pay for the beer in a fancy 5-star hotel.</p>
            <p>Now, imagine if you had instead been told your friend was going to a run-down 1-star motel</p>
            <p>If this were the case, do you think the <b>location selling the beer</b> would have affected your response about the most you would be willing to pay for the beer? If so, how?</p>`;
        } else {
            return `<p>In this exercise, you were asked the most you would be willing to pay for the beer in a run-down 1-star motel.</p>
            <p>Now, imagine if you had instead been told your friend was going to a fancy 5-star hotel.</p>
            <p>If this were the case, do you think the <b>location selling the beer</b> would have affected your response about the most you would be willing to pay for the beer? If so, how?</p>`;
        }
    },
    labels: condition[0] == "Factor-Included" ? introspection_q_labels_ref_price1 : introspection_q_labels_ref_price2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br><br>",
    on_finish: function (data) {
        ref_price_intro_response1 = data.response;
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
            condition: condition[0] == "Factor-Included" ? "hotel" : "motel",
            task_name: "reference price",
            choice: choice,
            familiarity: familiarity,
            openq_response: ref_price_openQ_response,
            introspect_rating: ref_price_intro_response1,
            introspect_open: ref_price_intro_confidence_response,
            rt: data.rt
        };
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var ref_price_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p>Before doing this study, had you seen or heard of a task similar to this last one before?</p>`,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity= data.response == 0 ? "Yes" : "No";

        
    }
};

var reference_price = {
    timeline: [ref_price_instructions, ref_price_trial, ref_price_familiar, ref_price_openQ, ref_price_introspect1, ref_price_intro_confidence]
};

// end region reference price 
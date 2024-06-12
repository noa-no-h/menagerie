//#region Reference Price - BETWEEN (Thaler, 2008)

var ref_price_hotel_stimulus =
    `The National Highway Safety Commission is deciding
     how to allocate its budget between two safety research 
     programs: i) improving automobile  safety  (bumpers,
    body, gas tank configurations, seatbelts) and i
    i) improving the safety of interstate highways 
    (guard rails, grading, highway  interchanges, 
    and implementing  selective reduced
     speed limits). Currently the commission allocates 
     approximately 70% of its funds to auto  safety and 
     30%  of its funds to  highway  safety. 
     Since there is a ceiling on its total spending, its 
     options are:`
    
    
var ref_price_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> In this exercise, you will be given a hypothetical scenario and then asked what you would do in that situation.</p>`
        `<p><i>Please click the "Next" button when you are ready to see the scenario.</i></p>`
    ],
    show_clickable_nav: true
}

var ref_price_trial = {
    type: jsPsychSurveyText,
    questions: [
      {prompt: 'How old are you?'}
    ]
  }

var avail_trials = {
    timeline: [
        {//trials
            type: jsPsychHtmlKeyboardResponse,
            stimulus: jsPsych.timelineVariable('stimulus'),
            questions: [
                {prompt: jsPsych.timelineVariable('stimulus')}
              ]
        },
    ],
    timeline_variables: condition[0] == "Factor-Included" ? ref_price_hotel_stimulus : ref_price_motel_stimulus,
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

var introspection_q_labels_ref_price1 = [`<strong>It made me say I'd be willing to pay a higher price`, "", "<strong>It did not affect my response</strong>", "", `<strong>It made me more likely to think that the second list (where all the women were famous) contained more men</strong>`];
var introspection_q_labels_ref_price2 = [`<strong>It would have made me more likely to think that first list (where all the men were famous) contained more men</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me more likely to think that the second list (where all the women were famous) contained more men</strong>`];

var avail_intro_response1 = null;
var avail_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this exercise, you were asked the most you would be willing to pay for the beer in a fancy 5-star hotel.</p>
            <p>Now, imagine if you had instead been told your friend was going to a run-down 1-star motel</p>
            <p>If this were the case, do you think the <b>location selling the beer</b> would have affected your response about the most you would be willing to pay for the beer? If so, how?`
                } else {
            return `<p>In this exercise, you were asked the most you would be willing to pay for the beer in a run-down 1-star motel.</p>
            <p>Now, imagine if you had instead been told your friend was going to a run-down fancy 5-star hotel.</p>
            <p>If this were the case, do you think the <b>location selling the beer</b> would have affected your response about the most you would be willing to pay for the beer? If so, how?`
        }
    },
    labels: condition[0] == "Factor-Included" ? introspection_q_labels_avail1 : introspection_q_labels_avail2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br><br>",
    on_finish: function (data) {
        avail_intro_response1 = data.response
    }
};

var avail_intro_response2 = null;
var avail_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        avail_intro_response2 = data.response.Q0
    }
};

var avail_intro_confidence_response = null;
var avail_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        avail_intro_confidence_response = data.response
    }
};

var avail_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p>Before doing this study, had you seen or heard of a task similar to this last one before?</p>`,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            condition: condition[0] == "Factor-Included" ? "Famous" : "Unfamous",
            task_name: "availability",
            choice: more_men == 0 ? "List 1" : "List 2",
            openq_response: avail_openQ_response,
            introspect_rating: avail_intro_response1,
            introspect_open: avail_intro_confidence_response,
            familiarity: data.response == 0 ? "Yes" : "No",
            rt: data.rt
        }
        save_data(s1_data, 'introspection');
    }
}

var avail = {
    timeline: [avail_instructions, avail_trials, avail_question, avail_openQ, avail_introspect1, avail_intro_confidence, avail_familiar]
}

/*var avail_excluded = {
  timeline: [avail_instructions, avail_trials, avail_question, avail_openQ, avail_introspect1, avail_introspect2, avail_familiar]
}*/
//#endregion

//#region Halo

var confidence_q = "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by attractiveness of the face)?</p>";


var preload = {
    type: jsPsychPreload,
    auto_preload: true
}

var halo_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, you will be presented with a series of pictures of anonymous strangers. Based on these pictures, you will then be asked to rate your impression of these strangers.</p>
        <p>There are no right or wrong answers in this task. We are simply interested in your honest impressions of these individuals.</p>
        <p><i>Click the Next button below when you are ready to see the first stranger.</i></p>`
    ],
    show_clickable_nav: true
};


var choice = null;


var attractive_list = ['img/A1 WF-233.jpg', 'img/A2 BF-240.jpg', 'img/A3 LF-249.jpg',
                    'img/A4 BF-233.jpg'];

var unattractive_list = ['img/U1 AM-224.jpg', 'img/U2 LM-240.jpg', 'img/U3 BF-200.jpg', 'img/U4 WF-002.jpg'];

var neutral_list = ['img/M1 LF-240.jpg', 'img/M2 WM-230.jpg', 'img/M3 BM-034.jpg', 'img/M4 LF-252.jpg', 'img/U5 WF-250.jpg',
                    'img/M5 WM-221.jpg', 'img/M6 WM-214.jpg', 'img/M7 WM-254.jpg', 'img/M8 BM-232.jpg'
];

var stimuli_list = [];
var ordered_stimuli_list = [];
if (condition[0] == 'Factor-Included'){
    ordered_stimuli_list = attractive_list.concat(unattractive_list);
} else {
    ordered_stimuli_list = neutral_list;
}
stimuli_list = jsPsych.randomization.shuffle(ordered_stimuli_list);
num_stimuli = stimuli_list.length;
list_index = 0;


choice = {}
for (i = 0; i < num_stimuli; i++) {
    choice[stimuli_list[i]]=null;
}

var stimulus = null;
var halo_trial = {
    type: jsPsychSurveyLikert,
    preamble: function(){

        stimulus = '<img src="' + stimuli_list[list_index] + '" alt="Stimulus Image" style="width:45%;height:auto;"><br><br><p>Please rate your impression of how persuasive the individual pictured above is on a scale from 1 to 5.</p>'
        return stimulus;
},
    questions: [
      {prompt: "On a scale of 1-5, how persuasive do you think this person is?", labels: ["1", "2", "3", "4", "5"], required: true},
    ],
    scale_width: 500,
    on_finish: function (data) {
        //console.log(data.response);
        //choice[stimuli_list[list_index]] = data.response.Q0;
        console.log("data.response.Q0: " + data.response.Q0);

        var s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "halo",
            condition: condition[0] == "Factor-Included" ? "attractive/unattractive" : "average attractiveness",
            stimulus: stimulus,
            choice: data.response.Q0,
            auxiliary_info1: null,
            openq_response: null,
            introspect_rating: null,
            introspect_open: null,
            familiarity: null,
            rt: data.rt
        };
        save_data(s1_data, 'introspection');
    }
  };

  var loop_halo = {
    timeline: [halo_trial],
    loop_function: function(data){
        console.log("list_index", list_index,"stimuli_list.length - 1", stimuli_list.length - 1)
        if (list_index != stimuli_list.length - 1) {
            list_index = list_index + 1;
            console.log(list_index, stimuli_list[list_index]);
            return true; //loop
        } else {
            return false; // don't loop
        }
    }
}

var halo_openQ_response = null;
var halo_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were presented with a series of pictures of 
        anonymous strangers. Based on these pictures, you were then asked to rate your 
        impression how persuasive the strangers were. </p><p>Describe your thought process behind your decision about how persuasive to rate each individual. How did you come to your eventual decision?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        halo_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_ref_price1 = ['<strong>It made me think they were less persuasive</strong>', "", '<strong>It would not have affected my response</strong>', "", '<strong>It made me think they were more persuasive</strong>'];
var introspection_q_labels_ref_price2 = ['<strong>It would have made me think they were less persuasive</strong>', "", '<strong>It would not have affected my response</strong>', "", '<strong>It would have made me think they were more persuasive</strong>'];

var halo_intro_response1 = null;
var halo_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this exercise, you were presented with a series of pictures of 
            anonymous strangers. Based on these pictures, you were then asked to rate your 
            impression of how persuasive strangers were.</p>
            <p>How do you think the <b>attractiveness of the face</b> affected your impression of their persuasiveness?</p>`;
        } else {
            return `<p>In this exercise, you were presented with a series of pictures of 
            anonymous strangers. Based on these pictures, you were then asked to rate your 
            impression of how persuasive strangers were.</p>
            <p>Now, imagine if you had been shown particularly <b>attractive<b/> faces.</p>
            <p>If this were the case, do you think the <b>attractiveness of the faces</b> would have affected your impression of their persuasiveness? If so, how?</p>`;
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
        halo_intro_response1 = data.response;
    }
};

var halo_intro_response2 = null;
var halo_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        halo_intro_response2 = data.response.Q0;
    }
};

var halo_intro_confidence_response = null;
var halo_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        halo_intro_confidence_response = data.response;
        var s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "halo",
            condition: condition[0] == "Factor-Included" ? "attractive/unattractive" : "average attractiveness",
            stimulus: null,
            choice: choice,
            auxiliary_info1: null,
            openq_response: halo_openQ_response,
            introspect_rating: halo_intro_response1,
            introspect_open: halo_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        };
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var halo_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity= data.response == 0 ? "Yes" : "No";

        
    }
};

var halo = {
    timeline: [preload, halo_instructions, loop_halo, halo_familiar, halo_openQ, halo_introspect1, halo_intro_confidence]
};

// end region reference price 
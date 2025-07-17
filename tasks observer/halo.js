//#region Halo


var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by how physically attractive each stranger looked)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by how physically attractive each stranger looked)?</p>";


var preload = {
    type: jsPsychPreload,
    auto_preload: true
}

var halo_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, the Prolific user was presented with a series of pictures of anonymous strangers. Based on these pictures, they were then asked to rate their impression of these strangers.</p>
        <p>They were told that there are no right or wrong answers in this task. We were simply interested in their honest impressions of these individuals.</p>
        <p><i>Click the Next button below when you are ready to see the first stranger.</i></p>`
    ],
    show_clickable_nav: true
};


var choice = null;


var attractive_list = ['img/A1 WF-233.jpg', 'img/A2 BF-240.jpg', 'img/A3 LF-249.jpg',
                    'img/A4 BF-233.jpg'];

var unattractive_list = ['img/U1 AM-224.jpg', 'img/U2 LM-240.jpg', 'img/U3 BF-200.jpg', 'img/U4 WF-002.jpg'];

var neutral_list = ['img/M1 LF-240.jpg', 'img/M2 WM-230.jpg', 'img/M3 BM-034.jpg', 'img/M4 LF-252.jpg',
                    'img/M5 WM-221.jpg', 'img/M6 WM-214.jpg', 'img/M7 WM-254.jpg', 'img/M8 BM-232.jpg'
];

var attractiveness = {
    'img/A1 WF-233.jpg': 5.47826087, 'img/A2 BF-240.jpg': 5.310344828,
    'img/A3 LF-249.jpg': 5.24137931, 'img/A4 BF-233.jpg': 5.12,
    'img/U1 AM-224.jpg': 1.52, 'img/U2 LM-240.jpg': 1.541666667,
    'img/U3 BF-200.jpg': 1.551724138, 'img/U4 WF-002.jpg': 1.612903226,
    'img/M1 LF-240.jpg': 3.142857143, 'img/M2 WM-230.jpg':3.142857143,
    'img/M3 BM-034.jpg':3.144444444, 'img/M4 LF-252.jpg': 3.148148148,
    'img/M5 WM-221.jpg': 3.153846154, 'img/M6 WM-214.jpg': 3.12, 
    'img/M7 WM-254.jpg': 3.153846154, 'img/M8 BM-232.jpg': 3.16
}

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
    type: jsPsychHtmlSliderResponse,
    stimulus: function(){
        console.log("stimuli_list[list_index]: " + stimuli_list[list_index]);
        console.log("actorNumber: " + actorNumber);
        console.log("halo_db: " + halo_db);

        const foundEntry = halo_db.find(item =>
    item.subject === String(actorNumber) && // Ensure actorNumber is compared as a string
    item.stimulus === stimuli_list[list_index]
);

if (foundEntry) {
    observedChoice = foundEntry.choice;
} else {
    observedChoice = null; 
    console.warn(`Warning: No matching entry found in halo_db for subject ${actorNumber} and stimulus ${stimuli_list[list_index]}. 'observedChoice' set to null.`);
}

        console.log("actorNumber type and value:", typeof actorNumber, actorNumber);
console.log("stimuli_list[list_index] type and value:", typeof stimuli_list[list_index], stimuli_list[list_index]);

const tempSubjectMatch = halo_db.find(item => item.subject === actorNumber);
console.log("Subject match test:", tempSubjectMatch); // Is anything found just by subject?

const tempStimulusMatch = halo_db.find(item => item.stimulus === stimuli_list[list_index].toString());
console.log("Stimulus match test:", tempStimulusMatch); // Is anything found just by stimulus?


        stimulus = '<img src="' + stimuli_list[list_index] + '" alt="Stimulus Image" style="width:45%;height:auto;"><br><br><p>Please rate your impression of how persuasive the individual pictured above is on a scale from 1 to 5.<br><br>The Prolific user selected ' + observedChoice + '.<br><br>To demonstrate that you understand the Prolific user\'s choice, <b>please move the slider to the option that they selected (regardless of your own beliefs).</b></p>';
        
        return stimulus;
    },
    
    scale_width: 200,
    labels: ["1", "2", "3", "4", "5"],
    min: 1,
    max: 5,
    step: 1,
    slider_start: 3,
    correct_response: function() {
        return observedChoice;

    },
    on_finish: function (data) {
        //console.log(data.response);
        //choice[stimuli_list[list_index]] = data.response.Q0;
        console.log("data.response: " + data.response);
        stimulus = stimuli_list[list_index];
        console.log("stimulus: " + stimulus);
        function findListContainingString(str) {
            if (attractive_list.includes(str)) {
              return "attractive"; 
            } else if (unattractive_list.includes(str)) {
              return "unattractive"; 
            } else if (neutral_list.includes(str)) {
              return "neutral"; 
            } else {
              return null; 
            }
          }
        var s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "halo",
            condition: findListContainingString(stimulus),
            stimulus: stimulus,
            choice: data.response,
            auxiliary_info1: attractiveness[stimulus],
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
        prompt: `<p>In this exercise, the Prolific user was presented with a series of pictures of 
        anonymous strangers. Based on these pictures, they were then asked to rate their 
        impression how persuasive the strangers were. </p><p>Describe what you think their thought process was behind their decision about how persuasive to rate each individual. How do you think they came to their eventual decision?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        halo_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_halo1 = ['<strong>It made the Prolific user think the stranger was <u>LESS</u> persuasive</strong>', "", '<strong>It did not affect their response</strong>', "", '<strong>It made the Prolific user think the stranger was <u>MORE</u> persuasive</strong>'];
var introspection_q_labels_halo2 = ['<strong>It would have made me think they were <u>LESS</u> persuasive</strong>', "", '<strong>It would not have affected my response</strong>', "", '<strong>It would have made me think they were <u>MORE</u> persuasive</strong>'];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var halo_intro_response1 = null;
var halo_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this exercise, the Prolific user was presented with a series of pictures of 
            anonymous strangers. Based on these pictures, they were then asked to rate their 
            impression of how persuasive strangers were.</p>
            <p>Do you think <b>how physically attractive each stranger looked</b> affected the Prolific user's impression of the strangers' persuasiveness? If so, how?</p>`;
        } else {
            return `<p>In this exercise, you were presented with a series of pictures of 
            anonymous strangers. Based on these pictures, you were then asked to rate your 
            impression of how persuasive strangers were.</p>
            <p>Now, imagine if you had been shown particularly <b>attractive<b/> faces.</p>
            <p>If this were the case, do you think <b>how physically attractive each stranger looked</b> would have affected your impression of their persuasiveness? If so, how?</p>`;
        }
    },
labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_halo1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_halo1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_halo2;
        } else {
            return introspection_q_labels_halo2.slice().reverse();
        }
    },    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {

        if (label_order_randomized == 'original') {
            halo_intro_response1 = data.response
    }
        else {
            halo_intro_response1 = 100 - data.response;
            }
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

       halo_intro_response2 = data.response

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
            auxiliary_info1:  label_order_randomized,
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



if (only_main_question) {
    var halo = {
        timeline: [preload, halo_instructions, loop_halo]
    };
} else {
    var halo = {
        timeline: [preload, halo_instructions, loop_halo, halo_familiar, halo_openQ, halo_introspect1, halo_intro_confidence]
    };
}

// end region reference price 
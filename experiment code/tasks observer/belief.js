//#region 8. Belief Bias (Evans et al. 1983) - WITHIN
function beliefGetChoice(stimulus) {
 
    chosenArray = belief_db.find(item =>
    item.subject === String(actorNumber) && 
    item.stimulus === stimulus); 
    choice = chosenArray.choice
    return choice;
}

function beliefGetRt(stimulus) {
    chosenArray = belief_db.find(item =>
    item.subject === String(actorNumber) && 
    item.stimulus === stimulus); 
    rt = chosenArray.rt
    return rt;

}

var confidence_q = condition[0] == 'Factor-Included' ? '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by the believability of each conclusion)?</p>' : '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the believability of each conclusion)?</p>';

var belief_instructions1 = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, the Prolific user played a short logic game. They were asked to try to play the game to the best of their ability.</p>
    <p><i>Please click the button below to view the instructions for the game.</i></p>`,
        `<p>The game they played is called the <b>Alien Game</b>. In this game, the Prolific user is asked to imagine an alien from another planet has just landed on Earth. This alien's thought processes are very <b>logical</b>, but it knows nothing about Earth.</p>`,
        `<p>In every round of the Alien Game, the alien will be told a number of things about Earth.</p>
    <p>For example, the alien might be told the following statements:
    <p><i>All elephants have big ears.</p>
    <p>Some animals are elephants.</i></p>
    <p>Then, you will then be given a <b>conclusion</b> that the alien could make based on those statements.</p>
    <p>For example, a conclusion based on the above statements might be:
    <p><b><i>Therefore, some animals have big ears.</i></b></p>
    Your goal is to determine <b>whether the logical alien would be able to come to that conclusion based on what it was told before</b>. In this case, the conclusion logically follows the statements, so the alien would be <b>able</b> to make it.`,
        `<p>In some rounds, the alien might be told <i>false</i> information. That is, information that is not true about Earth.</p>
    <p>For example, the alien might be told the following statements:
    <p><i>All houses have big ears.</p>
    <p>Some places are houses.</i></p>
    <p>Even though you know that these statements are false, the alien &#8212 who does not know anything about Earth &#8212 may still be <b>able</b> to draw a logical conclusion based on those statements.</p>
    <p>For example, a conclusion based on the above statements might be:
    <p><b><i>Therefore, some places have big ears.</i></b></p>
    <p>In this case, even though the conclusion is <i>false</i>, it still logically follows from what the alien was told.</p>`,
        `<p>Now that you know how to play the Alien game, you will be see the Prolific user's response in a practice round. Then, you will see their response in the actual game. Please ensure that you have read and understood the instructions before continuing (You can click the "Previous" button below to review them again).</p>
    <p>During the practice round, the Prolific user received feedback on their responses. However, they did not receive feedback during the actual game.</p>
    <p><i>Once you are sure you know how to play the game, please click the "Next" button below to see the practice round responses.</i></p>`
    ],
    show_clickable_nav: true
};

var belief_practice1 = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p><i>This is a practice question. The Prolific user was asked to answer the question using the buttons at the bottom of the screen. They were told to click "Yes" if they thought the alien would come to the conclusion. Otherwise, they were told to click "No".</i></p><br>
<p>Imagine the logical alien is told the following statements:</p>
<p><i>All dogs are animals.</p>
<p>All cats are animals.</i></p>
<p>Based on these statements, the Prolific user was asked whether they thought the alien would be able to come to the conclusion below:</p>
<p><i><b>Therefore, all dogs are cats.</b></i></p>The Prolific user selected ` + beliefGetChoice("Practice 1") + `. <br><br> To demonstrate that you understand the Prolific user's choice, <b>please select the option that they selected</b> (regardless of your own beliefs).`,
    choices: ["Yes", "No"],
    enable_button_after: beliefGetRt("Practice 1"),
    correct_response: function() {
                string_choice = beliefGetChoice("Practice 1");
                if (string_choice == "Yes") {
                    return 0;
                } else {
                    return 1;
                }
            },
    on_finish: function (data) {
        s1_data = {
            subject: data.subject,
            version: data.version,
            task_name: "belief",
            factor: data.condition,
            choice: data.response == 0 ? "Yes" : "No",
            stimulus: "Practice 1",
            auxiliary_info1: data.response == 1 ? "Correct" : "Incorrect",
            rt: data.rt,
        },
        
        save_data(s1_data, 'introspection');
    }
};

var belief_practice1_feedback = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function (data) {
        var logic = jsPsych.data.get().last(1).values()[0].response
        if (logic == 1) {
            return `<p>The Prolific user was given the following feedback: Correct! This conclusion is illogical, so the alien would be unable to make it.</p>
        <p><i>Please click the button below to continue.</i></p>`
        } else {
            return `<p>The Prolific user was given the following feedback: Incorrect. This conclusion is illogical, so the alien would be unable to make it.</p>
        <p><i>Please click the button below to continue.</i></p>`
        }
    },
    choices: ["Continue"]
};

var belief_practice2 = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p><i>This is a practice question. Please answer the question using the buttons at the bottom of the screen. Click "Yes" if you think the alien would come to the conclusion. Otherwise, click "No".</i></p><br>
<p>Imagine the logical alien is told the following statements:</p>
<p><i>No trees are buildings.</p>
<p>All tall things are trees.</i></p>
<p>Based on these statements, do you think the alien would be able to come to the conclusion below?</p>
<p><b><i>Therefore, no tall things are buildings.</i></b></p><br>The Prolific user selected ` + beliefGetChoice("Practice 2") + `. <br><br> To demonstrate that you understand the Prolific user's choice, <b>please select the option that they selected</b> (regardless of your own beliefs).`,
    choices: ["Yes", "No"],
    enable_button_after: beliefGetRt("Practice 2"),
    correct_response: function() {
                let current_stimulus_name = jsPsych.timelineVariable('name');
                string_choice = beliefGetChoice("Practice 2");
                if (string_choice == "Yes") {
                    return 0;
                } else {
                    return 1;
                }
            },
    on_finish: function (data) {
        s1_data = {
            subject: data.subject,
            version: data.version,
            task_name: "belief",
            factor: data.condition,
            choice: data.response == 0 ? "Yes" : "No",
            stimulus: "Practice 2",
            auxiliary_info1: data.response == 0 ? "Correct" : "Incorrect",
            rt: data.rt,
        }
        save_data(s1_data, 'introspection');
    }
};

var belief_practice2_feedback = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function (data) {
        var logic = jsPsych.data.get().last(1).values()[0].response
        if (logic == 0) {
            return `<p>The Prolific user was given the following feedback: Correct! This conclusion is logical, so the alien would be able to make it.</p>
        <p><i>Please click the button below to continue.</i></p>`
        } else {
            return `<p>The Prolific user was given the following feedback: Incorrect. This conclusion is logical, so the alien would be able to make it.</p>
        <p><i>Please click the button below to continue.</i></p>`
        }
    },
    choices: ["Continue"]
};

belief_instructions2 = {
    type: jsPsychInstructions,
    pages: [
        `<p>Now you will see the Prolific user's responses in the actual trials. Please note that for these trials, they no longer received feedback on their responses.</p>
    <p><i>Please click the button below when you are ready to begin.</i></p>`
    ],
    show_clickable_nav: true
};

var belief_included_stimuli = [
    {
        stimulus: `<p>We asked the Prolific user to imagine the logical alien is told the following statements:</p>
<p><i>No police dogs are vicious.</p>
<p>Some highly trained dogs are vicious</i></p>
<p>Based on these statements, do you think the alien would be able to come to the conclusion below?</p>
<p><b><i>Therefore, some highly trained dogs are not police dogs.</i></b></p><br>`, validity: "Valid", believability: "Believable",
        name: "Therefore, some highly trained dogs are not police dogs."
    },
    {
        stimulus: `<p>We asked the Prolific user to imagine the logical alien is told the following statements:</p>
<p><i>No nutritional things are expensive.</p>
<p>Some vitamin tablets are expensive.</i></p>
<p>Based on these statements, do you think the alien would be able to come to the conclusion below?</p>
<p><b><i>Therefore, some vitamin tablets are not nutritional.</i></b></p><br>`, validity: "Valid", believability: "Unbelievable",
        name: "Therefore, some vitamin tablets are not nutritional."
    },
    {
        stimulus: `<p>We asked the Prolific user to imagine the logical alien is told the following statements:</p>
<p><i>No addictive things are inexpensive.</p>
<p>Some cigarettes are inexpensive.</i></p>
<p>Based on these statements, do you think the alien would be able to come to the conclusion below?</p>
<p><b><i>Therefore, some addictive things are not cigarettes.</i></b></p><br>`, validity: "Invalid", believability: "Believable",
        name: "Therefore, some addictive things are not cigarettes."
    },
    {
        stimulus: `<p>We asked the Prolific user to imagine the logical alien is told the following statements:</p>
<p><i>No millionaires are hard workers.</p>
<p>Some rich people are hard workers.</i></p>
<p>Based on these statements, do you think the alien would be able to come to the conclusion below?</p>
<p><b><i>Therefore, some millionaires are not rich people.</i></b></p><br>`, validity: "Invalid", believability: "Unbelievable",
        name: "Therefore, some millionaires are not rich people."
    },
]

var belief_excluded_stimuli = [
    {
        stimulus: `<p>We asked the Prolific user to imagine the logical alien is told the following statements:</p>
<p><i>No dogs are vicious.</p>
<p>Some rottweilers are vicious</i></p>
<p>Based on these statements, do you think the alien would be able to come to the conclusion below?</p>
<p><b><i>Therefore, some rottweilers are not dogs.</i></b></p><br>`, validity: "Valid", believability: "Unbelievable",
        name: "Therefore, some rottweilers are not dogs."
    },

    {
        stimulus: `<p>We asked the Prolific user to imagine the logical alien is told the following statements:</p>
<p><i>No nutritional things are expensive.</p>
<p>Some vitamin tablets are expensive.</i></p>
<p>Based on these statements, do you think the alien would be able to come to the conclusion below?</p>
<p><b><i>Therefore, some vitamin tablets are not nutritional.</i></b></p><br>`, validity: "Valid", believability: "Unbelievable",
        name: "Therefore, some vitamin tablets are not nutritional."
    },

    {
        stimulus: `<p>We asked the Prolific user to imagine the logical alien is told the following statements:</p>
<p><i>No cigarettes are inexpensive.</p>
<p>Some addictive things are inexpensive.</i></p>
<p>Based on these statements, do you think the alien would be able to come to the conclusion below?</p>
<p><b><i>Therefore, some cigarettes are not addictive things.</i></b></p><br>`, validity: "Invalid", believability: "Unbelievable",
        name: "Therefore, some cigarettes are not addictive things."
    },

    {
        stimulus: `<p>We asked the Prolific user to imagine the logical alien is told the following statements:</p>
<p><i>No millionaires are hard workers.</p>
<p>Some rich people are hard workers.</i></p>
<p>Based on these statements, do you think the alien would be able to come to the conclusion below?</p>
<p><b><i>Therefore, some millionaires are not rich people.</i></b></p><br>`, validity: "Invalid", believability: "Unbelievable",
        name: "Therefore, some millionaires are not rich people."
    },

]

var belief_stimuli = null;
if (condition[0] == "Factor-Included") {
    belief_stimuli = belief_included_stimuli
} else {
    belief_stimuli = belief_excluded_stimuli
}

var belief_trials = {
    timeline: [
        {//fixation
            type: jsPsychHtmlKeyboardResponse,
            stimulus: '<p style = "font-size:30px">+</p>',
            choices: "NO_KEYS",
            trial_duration: 500
        },
        {//trials
            type: jsPsychHtmlButtonResponse,
            stimulus: function(){
                let timeline_stimulus = jsPsych.timelineVariable('stimulus');
                console.log("timeline_stimulus", timeline_stimulus)
                let current_stimulus_name = jsPsych.timelineVariable('name');
                console.log("current_stimulus_name", current_stimulus_name)
                observed_response = beliefGetChoice(current_stimulus_name)
                

                observer_instructions = `<p>The Prolific user selected ` + observed_response + `.</p>
            <p>To demonstrate that you understand the Prolific user's choice, <b>please enter the Prolific user's choice</b> (regardless of your own beliefs):</b></p>`

                stimulus_with_observer_instructions = timeline_stimulus + observer_instructions;
                return stimulus_with_observer_instructions;
            
            },
            correct_response: function() {
                let current_stimulus_name = jsPsych.timelineVariable('name');
                string_choice = beliefGetChoice(current_stimulus_name);
                if (string_choice == "Yes") {
                    return 0;
                } else {
                    return 1;
                }
            },
            enable_button_after: function(){return beliefGetRt(jsPsych.timelineVariable('name'))},

            choices: ["Yes", "No"],
            data: { stim: jsPsych.timelineVariable('name'), aux: jsPsych.timelineVariable('validity'), con: jsPsych.timelineVariable('believability'), },
            on_finish: function (data) {
                s1_data = {
                    subject: data.subject,
                    version: data.version,
                    task_name: "belief",
                    factor: data.condition,
                    choice: data.response == 0 ? "Yes" : "No",
                    condition: data.con,
                    stimulus: data.stim,
                    auxiliary_info1: data.aux,
                    rt: data.rt,
                }
                save_data(s1_data, 'introspection');
            }
        },
    ],
    timeline_variables: belief_stimuli,
    randomize_order: true
};

var belief_openQ_response = null;
var belief_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, the Prolific user played a short game where they were given a series of conclusions and asked to determine whether a "logical alien" would make those conclusions based on certain statements.</p>
<p>Describe what you think their thought process was while deciding whether the alien would be able or unable to make the given conclusions. How do you think they came to each decision about whether or not the alien would make that conclusion?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        belief_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_belief1 = [`<strong>When the conclusion was believable, that made them <u>LESS</u> likely to think the alien would come to that conclusion</strong>`, "", "<strong>Whether the conclusion was believable did not affect their response</strong>", "", `<strong>When the conclusion was believable, that made them <u>MORE</u> likely to think the alien would come to that conclusion</strong>`];
var introspection_q_labels_belief2 = [`<strong>If the conclusion had been believable, that would have made them <u>LESS</u> likely to think the alien would come to that conclusion</strong>`, "", "<strong>Whether the conclusion was believable would not have affected their response</strong>", "", `<strong>If the conclusion had been believable, that would have made them <u>MORE</u> likely to think the alien would come to that conclusion</strong>`];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var belief_intro_response1 = null;
var belief_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `Some of the conclusions you saw in this game were <b>believable</b> based on your prior knowledge of how the world works (e.g. "some highly trained dogs are not police dogs"), while others were <b>unbelievable</b> based on your prior knowledge of how the world works (e.g. "some millionaires are not rich people").
        <p>Do you think the <b>believability</b> of each conclusion affected the Prolific user's decision about whether or not the alien would come to that conclusion? If so, how?</p>`
        } else {
            return `You may have noticed that all of the conclusions you saw in this game were <b>not believable</b> based on your prior knowledge of how the world works. For example, it is unbelievable based on prior knowledge that "some millionaires are not rich people" or that "some cigarettes are not addictive things."
        <p>Now, imagine that some of the conclusions <i>had</i> been believable, while others weren't. For example, imagine if one series of statements ended with the <b>believable</b> conclusion that "some highly trained dogs are not police dogs", while another series of statements ended with the <b>unbelievable</b> conclusion that "some millionaires are not rich people".
        <p>Do you think the <b>believability</b> of each conclusion would have affected the Prolific user's decision about whether or not the alien would come to that conclusion? If so, how?</p>`
        }
    },
labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_belief1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_belief1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_belief2;
        } else {
            return introspection_q_labels_belief2.slice().reverse();
        }
    },    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {

        if (label_order_randomized == 'original') {
            belief_intro_response1 = data.response
    }
        else {
                belief_intro_response1 = 100 - data.response;
            }
        }

};

var belief_intro_response2 = null;
var belief_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        belief_intro_response2 = data.response.Q0
    }
};

var belief_intro_confidence_response = null;
var belief_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        belief_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "belief",
            condition: data.condition,
            stimulus: null,
            choice: null,
            flipped_scale: label_order_randomized,
            auxiliary_info1:  null,
            openq_response: belief_openQ_response,
            introspect_rating: belief_intro_response1,
            introspect_open: belief_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var belief_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity=data.response == 0 ? "Yes" : "No";

        
    }
}



if (only_main_question) {
    var belief = {
        timeline: [belief_instructions1, belief_practice1, belief_practice1_feedback, belief_practice2, belief_practice2_feedback, belief_instructions2, belief_trials]
    };
} else {
    var belief = {
        timeline: [belief_instructions1, belief_practice1, belief_practice1_feedback, belief_practice2, belief_practice2_feedback, belief_instructions2, belief_trials, belief_familiar, belief_openQ, belief_introspect1, belief_intro_confidence]
    };
}
 
//#endregion
//timeline.push(belief)
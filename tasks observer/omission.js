//#region 4. omission Principle - BETWEEN


var confidence_q = condition[0] == 'Factor-Included' ?'<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by whether James actively poisoned the one villager)?</p>' : '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by whether James actively poisoned the one villager)?</p>';

var omission_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> In this exercise, the Prolific user was given a hypothetical scenario and then asked to judge the moral permissibility of the actions in that scenario.</p>
<p><i>Please click the button below to view the scenario.</i>`,
    ],
    show_clickable_nav: true
}

var choice = null;
var omission_question = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>James is hiking through a foreign land when he comes across a remote village. James learns that one of the villagers fell into a cursed pond but, contrary to local superstition, did not die. If the one villager does not die by sunrise, five other innocent and unwilling villagers will be sacrificed to the gods to thank them for sparing the one. James, who has some medical training, realizes that he can prevent the sacrifice of the five villagers if he secretly poisons the one villager. If James poisons the one, the one will be dead by sunrise, and the five will not be sacrificed. If James does not poison the one, the one will not be dead by sunrise, and the five will be sacrificed as planned. James decides to poison the one. James decides to poison the one.
            <p><i>The Prolific user was asked to rate James's action on the given scale:     
1 (Forbidden) to 7 (Obligatory)
<p>The Prolific user selected ` + observedChoice + `. Below, to demonstrate that you understand the Prolific user's choice, please move the slider to the option that they selected (regardless of your own beliefs).</p>
`
        } else {
            return `<p>James is hiking through a foreign land when he comes across a remote village. James learns that one of the villagers fell into a cursed pond but, contrary to local superstition, did not die. If the one villager does not die by sunrise, five other innocent and unwilling villagers will be sacrificed to the gods to thank them for sparing the one. James, who has some medical training, notices that the one has accidentally consumed a poisonous substance. James can administer the antidote to the one villager. If James withholds the antidote from the one, the one will die by sunrise, and the five will not be sacrificed. If James does provide the antidote to the one, the one will not be dead by sunrise, and the five will be sacrificed as planned. James decides not to provide the antidote to the one.
<p><i>Please rate James's action on the given scale:     
1 (Forbidden) to 7 (Obligatory)
<i><p>(Slide instructions: Please move the slider to one point of your choice)
</i></p>`
        }
    },
    labels: ['1<br>(Forbidden)', '2', '3', '4<br>(Permissible)','5', '6', '7<br>(Obligatory)'],
    slider_width: introspection_q_slider_width,
    min: 1,
    max: 7,
    slider_start: 4,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",    
    on_finish: function (data) {
        choice = data.response
    }
}

var omission_openQ_response = null;
var omission_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, the Prolific user was asked to judge the morality of an actor in a hypothetical scenario.</p><p>Describe what you think their thought process was while judging whether the actor was moral in the situation. How do you think they came to their eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        omission_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_omission1 = [`<strong>It made they <u>MORE</u> likely to judge the action as permissible</strong>`, "", "<strong>It did not affect their response</strong>", "", `<strong>It made them <u>LESS</u> likely to judge the action as permissible</strong>`];
var introspection_q_labels_omission2 = [`<strong>It would have made me <u>MORE</u> likely to judge the action as permissible</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me <u>LESS</u> likely to judge the action as permissible</strong>`];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var omission_intro_response1 = null;
var omission_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>Sometimes when someone harms other people, it is through actively doing something (for example, poisoning someone). Other times it is through failing to do something that would prevent harm (for example, failing to give someone who has already been poisoned an antidote). 
            <p>In the scenario the Prolific user read, James actively poisoned the one villager. Do you think the fact that he <b>actively poisoned the one villager</b> as opposed to failing to prevent the villager’s death affected their judgement of the permissibility of his action? If so, how?`
        } else {
            return `<p>Sometimes when someone harms other people, it is through actively doing something (for example, poisoning someone). Other times it is through failing to do something that would prevent harm (for example, failing to give someone who has already been poisoned an antidote). 
            <p>Now, imagine if James had actively poisoned the one villager instead of failing to give him an antidote.
            <p>Do you think the fact that James <b>actively poisoned the one villager instead of failing to give him an antidote</b> would have affected your judgement of the permissibility of his action? If so, how?`
        }
    },
    labels: function() {

            if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
                return introspection_q_labels_omission1;
            } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
                return introspection_q_labels_omission1.slice().reverse();
            } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
                return introspection_q_labels_omission2;
            } else {
                return introspection_q_labels_omission2.slice().reverse();
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
            omission_intro_response1 = data.response

    }
        else {
            omission_intro_response1 = 100 - data.response;
            }

        }
};

var omission_intro_response2 = null;
var omission_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you choose the number that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        omission_intro_response2 = data.response.Q0
    }
};

var omission_intro_confidence_response = null;
var omission_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        omission_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "omission principle",
            condition: condition[0] == "Factor-Included" ? "commission" : "omission",
            stimulus: null,
            choice: choice,
            auxiliary_info1: null,
            openq_response: omission_openQ_response,
            introspect_rating: omission_intro_response1,
            introspect_open: omission_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var omission_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity = data.response == 0 ? "Yes" : "No"
    }
}





if (only_main_question) {
    var omission = {
        timeline: [omission_instructions, omission_question]
    };
} else {
    var omission = {
        timeline: [omission_instructions, omission_question, omission_familiar, omission_openQ, omission_introspect1, omission_intro_confidence]
    };
}


//#endregion
//timeline.push(omission)
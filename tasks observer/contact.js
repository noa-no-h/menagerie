//#region 4. Contact Principle - BETWEEN
var observedChoice = "CHANGE THIS"

var confidence_q = '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by the fact that Frank needed to make physical contact with the man)?</p>';


var contact_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> In this exercise, the Prolific user was given a hypothetical scenario and then asked to judge the moral permissibility of the actions in that scenario.</p>
<p><i>Please click the button below to view the scenario.</i>`,
    ],
    show_clickable_nav: true
}

var push = null;
var contact_question = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p><b>The Prolific user was asked to consider the following scenario:</b></p>
            <p>Frank sees that an empty runaway boxcar is about to hit five people working on the track ahead. They are too far away to be warned. The only way to stop the boxcar is for it to hit something heavy.
            One person, unaware of the boxcar, is working on a side track. This person, if hit, will cause the boxcar to stop.  If Frank pushes the one person onto the main track, the boxcar will hit the one person, and therefore stop before it reaches the five people.
            If Frank does not push the one person, the boxcar will continue on the track and hit the five people and not the one person.
            <p>Based on on this scenario, the Prolific user was asked to complete the following sentence: Pushing the person is...  </p>
            <p>The two options were morally permissible or morally impermissible. We asked them: which one would it be?</p>
            <p>The Prolific user selected ` + observedChoice + `.<br><br>To demonstrate that you understand the Prolific user's choice, <b>please select the option that they selected (regardless of your own beliefs).</b></p>`
        } else {
            return `<p><b>Please consider the following scenario:</b></p>
            <p>Frank sees that an empty runaway boxcar is about to hit five people working on the track ahead. They are too far away to be warned. The only way to stop the boxcar is for it to hit something heavy.
            Next to Frank is a switch that can be flipped to turn the boxcar away from the five people onto a side track that loops and then rejoins the main track. One person, unaware of the boxcar, is working on this looped side track.
            This person, if hit, will cause the boxcar to stop before it reaches the five people on the main track.  If Frank flips the switch, the boxcar will turn onto the side track, hit the one person and therefore stop before it reaches to the five people.
            If Frank does not flip the switch, the boxcar will continue on the main track and hit the five people and not the one person.</p>
            <p>Based on on this scenario, please complete the following sentence by clicking one of the buttons below:
            <p><i>Flipping the switch is...</i></p>`
        }
    },
    choices: ["Morally Permissible", "Morally Impermissible"],
    on_finish: function (data) {
        push = data.response
    }
}

var contact_openQ_response = null;
var contact_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, the Prolific user was asked to judge the moral permissibility of an action in a hypothetical scenario.</p><p>Describe what you think their thought process was while judging whether the action was morally permissible. How do you think they came to their eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        contact_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_contact1 = [`<strong>It made them <u>MORE</u> likely to judge Frank's action as morally permissible</strong>`, "", "<strong>It did not affect their response</strong>", "", `<strong>It made them <u>LESS</u> likely to judge Frank's action as morally permissible</strong>`];
var introspection_q_labels_contact2 = [`<strong>It would have made me <u>MORE</u> likely to judge Frank's action as morally permissible</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me <u>LESS</u> likely to judge Frank's action as morally permissible</strong>`];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var contact_intro_response1 = null;
var contact_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In the scenario the Prolific user saw, Frank would have needed to make physical contact with the man (i.e. by pushing him onto the main track) in order to save the five people.</p>
                <p>Do you think the fact that Frank would have needed to make <b>physical contact</b> with the man affected the Prolific user's judgment about whether Frank's action would be morally permissible? If so, how?`
        } else {
            return `<p>In the scenario you saw, Frank would not have needed to make physical contact with the man in order to save the five people on the main track; He would have only needed to flip a switch.
            <p>Now, imagine if Frank <i>had</i> needed to make physical contact with the man. For instance, imagine if Frank had to physically push the man onto the track.
            <p>Do you think the fact that Frank would have needed to make <b>physical contact</b> with the man would have affected your judgment about whether Frank's action would be morally permissible? If so, how?`
        }
    },
    labels: function() {

            if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
                return introspection_q_labels_contact1;
            } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
                return introspection_q_labels_contact1.slice().reverse();
            } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
                return introspection_q_labels_contact2;
            } else {
                return introspection_q_labels_contact2.slice().reverse();
            }
        },    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br><br>",
    on_finish: function (data) {

        if (label_order_randomized == 'original') {
            contact_intro_response1 = data.response
    }
        else {
            contact_intro_response1 = 100 - data.response;
            }
        }
};

var contact_intro_response2 = null;
var contact_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you choose the number that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        contact_intro_response2 = data.response.Q0
    }
};

var contact_intro_confidence_response = null;
var contact_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        contact_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "contact principle",
            condition: condition[0] == "Factor-Included" ? "Contact" : "No Contact",
            stimulus: null,
            choice: push == 0 ? "Permissible" : "Impermissible",
            auxiliary_info1: null,
            openq_response: contact_openQ_response,
            introspect_rating: contact_intro_response1,
            introspect_open: contact_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var contact_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity =data.response == 0 ? "Yes" : "No"
    }
}



if (only_main_question) {
    var contact = {
        timeline: [contact_instructions, contact_question]
    };
} else {
    var contact = {
        timeline: [contact_instructions, contact_question, contact_familiar, contact_openQ, contact_introspect1, contact_intro_confidence]
    };
}

//#endregion
//timeline.push(contact)
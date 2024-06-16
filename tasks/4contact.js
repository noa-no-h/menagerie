//#region 4. Contact Principle - BETWEEN

var contact_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> In this exercise, you will be given a hypothetical scenario and then asked to judge the moral permissibility of the actions in that scenario.</p>
<p><i>Please click the button below to view the scenario.</i>`,
    ],
    show_clickable_nav: true
}

var push = null;
var contact_question = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p><b>Please consider the following scenario:</b></p>
            <p>Frank sees that an empty runaway boxcar is about to hit five people working on the track ahead. They are too far away to be warned. The only way to stop the boxcar is for it to hit something heavy.
            One person, unaware of the boxcar, is working on a side track. This person, if hit, will cause the boxcar to stop.  If Frank pushes the one person onto the main track, the boxcar will hit the one person, and therefore stop before it reaches the five people.
            If Frank does not push the one person, the boxcar will continue on the track and hit the five people and not the one person.
            <p>Based on on this scenario, please complete the following sentence by clicking one of the buttons below:
            <p><i>Pushing the person is...</i></p>`
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
        prompt: `<p>In this exercise, you were asked to judge the moral permissibility of an action in a hypothetical scenario.</p><p>Describe your thought process while judging whether the action was morally permissible. How did you come to your eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        contact_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_contact1 = [`<strong>It made me <u>LESS</u> likely to judge Frank's action as morally permissible</strong>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It made me <u>MORE</u> likely to judge Frank's action as morally permissible</strong>`];
var introspection_q_labels_contact2 = [`<strong>It would have made me <u>LESS</u> likely to judge Frank's action as morally permissible</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me <u>MORE</u> likely to judge Frank's action as morally permissible</strong>`];

var contact_intro_response1 = null;
var contact_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In the scenario you saw, Frank would have needed to make physical contact with the man (i.e. by pushing him onto the main track) in order to save the five people.</p>
                <p>Do you think the fact that Frank would have needed to make <b>physical contact</b> with the man affected your judgment about whether Frank's action would be morally permissible? If so, how?`
        } else {
            return `<p>In the scenario you saw, Frank would not have needed to make physical contact with the man in order to save the five people on the main track; He would have only needed to flip a switch.
            <p>Now, imagine if Frank <i>had</i> needed to make physical contact with the man. For instance, imagine if Frank had to physically push the man onto the track.
            <p>Do you think the fact that Frank would have needed to make <b>physical contact</b> with the man would have affected your judgment about whether Frank's action would be morally permissible? If so, how?`
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_contact1 : introspection_q_labels_contact2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br><br>",
    on_finish: function (data) {
        contact_intro_response1 = data.response
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
            task_name: "contact principle",
            choice: push == 0 ? "Permissible" : "Impermissible",
            condition: condition[0] == "Factor-Included" ? "Contact" : "No Contact",
            factor: data.condition,
            openq_response: contact_openQ_response,
            introspect_rating: contact_intro_response1,
            introspect_open: contact_intro_confidence_response,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var contact_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p>Before doing this study, had you seen or heard of a task similar to this last one before?</p>`,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity =data.response == 0 ? "Yes" : "No"
    }
}


var contact = {
    timeline: [contact_instructions, contact_question, contact_familiar, contact_openQ, contact_introspect1, contact_intro_confidence]
}

//#endregion
//timeline.push(contact)
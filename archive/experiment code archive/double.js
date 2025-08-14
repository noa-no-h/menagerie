//#region 5. Double Effect - BETWEEN


var confidence_q = condition[0] == 'Factor-Included' ? "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the fact that the man's death was <b>necessary</b> in order to achieve Peter's goal)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the fact that the man's death was <b>necessary</b> in order to achieve Peter's goal)?</p>";

var double_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> In this exercise, you will be given a hypothetical scenario and then asked to judge the moral permissibility of the actions in that scenario.</p>
    <p><i>Please click the button below to view the scenario.</i>`,
    ],
    show_clickable_nav: true
}

var moral = null;
var double_question = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p><b>Please consider the following scenario:</b></p>
    <p>Peter is a fireman trying to help five children out of a burning house.  There is only one window from which the children can be safely evacuated, and it is jammed shut.
    Peter must immediately use an object to smash open this large, heavy window or else all five children will die.  The only sufficiently large object is a man on his way towards safely escaping the burning house.
    If Peter pushes the man into the window and breaks it open, the man will fall out and die, but the five children will be safely evacuated.  If Peter does not push the man into the window the man will safely escape, but the five children will die.</p>
    <p>Based on this scenario, please complete the following sentence by clicking one of the buttons below:</p>
    <p><i>Pushing the man is...</i></p>`
        } else {
            return `<p><b>Please consider the following scenario:</b></p>
    <p>Peter is a fireman trying to help five children out of a burning house.  There is only one window from which the children can be safely evacuated, and it is jammed shut.
    Peter must immediately smash open this large, heavy window  or else all five children will die.  Outside the window, on the sill, is a man safely awaiting evacuation who Peter cannot help hitting with his fist if he breaks the window.
    Peter knows that if he breaks open the window he will hit the man with his fist, the man will fall off and die, but the five children will be safely evacuated. If Peter does not break open the window the man will be safely evacuated, but the children will die.</p>
    <p>Based on on this scenario, please complete the following sentence by clicking one of the buttons below:
    <p><i>Breaking open the window is...</i></p>`
        }
    },
    choices: ["Morally Permissible", "Morally Impermissible"],
    on_finish: function (data) {
        moral = data.response
    }
}

var double_openQ_response = null;
var double_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were asked to judge the moral permissibility of an action in a hypothetical scenario.</p><p>Describe your thought process while judging whether the action was morally permissible. How did you come to your eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        double_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_double1 = [`<strong>It made me <u>LESS</u> likely to judge Peter's action as morally permissible</strong>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It made me <u>MORE</u> likely to judge Peter's action as morally permissible</strong>`];
var introspection_q_labels_double2 = [`<strong>It would have made me <u>LESS</u> likely to judge Peter's action as morally permissible</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me <u>MORE</u> likely to judge Peter's action as morally permissible</strong>`];

var double_intro_response1 = null;
var double_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>Sometimes when someone kills other people, the deaths are "collateral damage" -- i.e., killing the other people wasn't <i>necessary</i> to achieve the person's goal. For instance, imagine a country performs a military strike on a gun manufacturing factory of an enemy country, and there happens to be a worker in the factory who gets killed. In this case, the worker's death was not necessary for the military strike to achieve its goal of blowing up the factory; the worker's death was just a side effect (i.e. "collateral damage") of the strike.
            <p>Other times when someone kills other people, killing them was <i>necessary</i> to achieve the person's goal. For instance, imagine a country performs a military strike on an enemy general in order to cripple the enemy's leadership team. In this case, killing the general was necessary in order to achieve the country's goal of crippling the enemy leadership.</p>
            <p>In the case you read, killing the man was necessary to achieve Peter's goal of breaking the window and saving the five children. Without killing the man, he wouldn't have been able to break the window and save the children; the man was used as a <b>means</b> to achieve Peter's goal.</p>
            <p>Do you think this fact -- that the man's death was <b>necessary</b> in order to achieve Peter's goal -- influenced your judgment of whether Peter's actions were morally permissible or not? If so, how?</p>`
        } else {
            return `<p>Sometimes when someone kills other people, the deaths are "collateral damage" -- i.e., killing the other people wasn't <i>necessary</i> to achieve the person's goal. For instance, imagine a country performs a military strike on a gun manufacturing factory of an enemy country, and there happens to be a worker in the factory who gets killed. In this case, the worker's death was not necessary for the military strike to achieve its goal of blowing up the factory; the worker's death was just a side effect (i.e. "collateral damage") of the strike.
            <p>Other times when someone kills other people, killing them was <i>necessary</i> to achieve the person's goal. For instance, imagine a country performs a military strike on an enemy general in order to cripple the enemy's leadership team. In this case, killing the general was necessary in order to achieve the country's goal of crippling the enemy leadership.</p>
            <p>In the case you read, killing the man was a <b>side effect</b> of achieving Peter's goal of breaking the window and saving the five children -- i.e., the man's death was "collateral damage". If the man hadn't happened to be there, Peter would still have been able to break the window and save the children.</p>
            <p>Now, imagine if killing the man <i>was</i> necessary to achieve Peter's goal of breaking the window and saving the five children. In other words, imagine if Peter would not have been able to save the five children without killing the man. In this case, the man would have been used as a <b>means</b> to achieve Peter's goal.
            <p>Do you think this fact -- that the man's death <b>would have been necessary</b> in order to achieve Peter's goal -- would have influenced your judgment of whether Peter's actions were morally permissible or not? If so, how?</p>`
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_double1 : introspection_q_labels_double2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {
        double_intro_response1 = data.response
    }
};

var double_intro_response2 = null;
var double_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        double_intro_response2 = data.response.Q0
    }
};

var double_intro_confidence_response = null;
var double_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        double_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "double effect",
            condition: condition[0] == "Factor-Included" ? "Means" : "Side Effect",
            choice: moral == 0 ? "Permissible" : "Impermissible",
            stimulus: null,
            openq_response: double_openQ_response,
            introspect_rating: double_intro_response1,
            introspect_open: double_intro_confidence_response,
            familiarity: familiarity,
            rt_main_question: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var double_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity=data.response == 0 ? "Yes" : "No"

        
    }
}



if (only_main_question) {
    var double = {
        timeline: [double_instructions, double_question]
    };
} else {
    var double = {
        timeline: [double_instructions, double_question, double_familiar, double_openQ, double_introspect1, double_intro_confidence]
    };
}

//#endregion
//timeline.push(double)
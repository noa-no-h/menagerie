//#region 3. Causal Inference (Morris et al., 2019) - BETWEEN
subjectData = cause_db.find(item => item.subject === actorNumber);
var observedChoice = subjectData.choice;
observerTime = subjectData.rt;

var confidence_q = condition[0] == 'Factor-Included' ? '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by the fact that there was only one green ball in the left box)?</p>' : '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the fact that there was only one green ball in the left box)?</p>';

var cause_instructions = {
    type: jsPsychInstructions,
    pages: [
        `In this exercise, the Prolific user was given a description of a hypothetical event. After they read the description, they were then asked a question about that event.
    <p><i>Please click the button below to view the description.</i></p>`
    ],
    show_clickable_nav: true
}

var cause_one = {
    type: jsPsychInstructions,
    pages: [`<p><b>Please consider the following event:</b></p>
    <p>Imagine a person, Joe, is playing a casino game where he is presented with two boxes, shown below:<p><img src = "img/oneball.png" style="width:700px;"><img><p>Without looking, Joe must reach into both boxes <i>simultaneously</i> and pull out a ball from each box.
    <p>Joe wins a dollar if and only if he gets a <font color = "GREEN"><b>green ball</b></font> from the left box <b><u>AND</u></b> a <font color = "blue"><b>blue ball</b></font> from the right box.</p>`,
        `<p>Imagine that Joe closes his eyes, reaches into both boxes at the same time, and randomly chooses a ball from each box.<p><img src = "img/oneball.png" style="width:700px;"><img></p>
    <p>When he opens his eyes, he sees that he has drawn a <font color = "GREEN"><b>green ball</b></font> from the left box <b><u>AND</u></b> a <font color = "blue"><b>blue ball</b></font> from the right box. So, <b>Joe wins a dollar.</b></p>` ],
    show_clickable_nav: true
}

var cause_nine = {
    type: jsPsychInstructions,
    pages: [
        `<p><b>Please consider the following event:</b></p>
    <p>Imagine a person, Joe, is playing a casino game where he is presented with two boxes, shown below:<p><img src = "img/nineballs.png" style="width:700px;"><img><p>Without looking, Joe must reach into both boxes <i>simultaneously</i> and pull out a ball from each box.
    <p>Joe wins a dollar if and only if he gets a <font color = "GREEN"><b>green ball</b></font> from the left box <b><u>AND</u></b> a <font color = "blue"><b>blue ball</b></font> from the right box.</p>`,
        `<p>Imagine that Joe closes his eyes, reaches into both boxes at the same time, and randomly chooses a ball from each box.<p><img src = "img/nineballs.png" style="width:700px;"><img></p>
    <p>When he opens his eyes, he sees that he has drawn a <font color = "GREEN"><b>green ball</b></font> from the left box <b><u>AND</u></b> a <font color = "blue"><b>blue ball</b></font> from the right box. So, <b>Joe wins a dollar.</b></p>
    <p><i>Once you have read and fully understood the event, please click the "Next" button below to continue.</i></p>`
    ],
    show_clickable_nav: true
}

var cause_condition = null;
if (condition[0] == "Factor-Included") {
    cause_condition = cause_one
} else {
    cause_condition = cause_nine
}

var cause = null
var cause_question = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function(){
        image = null;
        if (cause_condition == cause_one) {
            image = "img/oneball.png"
        } else {
            image = "img/nineballs.png"
        }
        return `<b>The Prolific user was asked to tell us how much they agree or disagree with the statement below.</b> They were told there were no right or wrong answers and that we were simply interested in their opinion.
<p>Joe's choice from the left box (where he chose a <font color = "GREEN"><b>green ball</b></font>) caused him to win the dollar.</p><p><img src = "${image}" style="width:700px;"><img></p> <br> The Prolific user selected ` + observedChoice/10 + `.<br><br> To demonstrate that you understand the Prolific user's choice, <b>please select the option that they selected (regardless of your own beliefs).</b>`
    },
    labels: [`<strong>1<br>totally disagree</strong>`, "2", "3", "4", "5", "6", "7", "8", `<strong>9<br>totally agree</strong>`],
    slider_width: 750,
    min: 10,
    max: 90,
    slider_start: 50,
    enable_button_after: function(){return observerTime;},
    correct_response: observedChoice,
    allowed_margin: 7,
    require_movement: require_movement_general,
    on_finish: function (data) {
        cause = data.response
    }
}

var cause_openQ_response = null;
var cause_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, the Prolific user was asked to judge the extent to which Joe's choice from the left box (where he chose a green ball) caused him to win the dollar.</p><p>Describe what you think their thought process was while making this judgment. How do you think they came to their eventual judgment about how much the green ball caused the win?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        cause_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_cause1 = [`<strong>It made them <u>LESS</u> likely to think that Joe's choice of a green ball from the left box caused him to win</strong>`, "", "<strong>It did not affect their response</strong>", "", `<strong>It made them <u>MORE</u> likely to think that Joe's choice of a green ball from the left box caused him to win</strong>`];
var introspection_q_labels_cause2 = [`<strong>It would have made me <u>LESS</u> likely to think that Joe's choice of a green ball from the left box caused him to win</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me <u>MORE</u> likely to think that Joe's choice of a green ball from the left box caused him to win</strong>`];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var cause_intro_response1 = null;
var cause_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In the scenario the Prolific user saw, these were the boxes that Joe chose from: <p><img src = "img/oneball.png" style="width:700px;"><img></p>
        <p>As you can see, there was exactly <b>one</b> <font color = "GREEN">green</font> ball that Joe could have chosen.
        <p>Do you think the fact that there was only one green ball in the left box affected the Prolific user's judgment about whether <b>Joe's choice of a green ball from the left box</b> caused him to win? If so, how?`
        } else {
            return `<p>In the scenario you saw, these were the boxes that Joe chose from: <p><img src = "img/nineballs.png" style="width:700px;"><img></p>
        <p>As you can see, there were <b>nine</b> <font color = "GREEN">green</font> balls that Joe could have chosen.</b><hr><b>Now</b>, imagine if the boxes had looked like this: <p><img src = "img/oneball.png" style="width:700px;"><img></p>
        <p>In this case, there would have only been <b>one</b> <font color = "GREEN">green</font> ball that Joe could have chosen.</p>
        <p>If this were the case, do you think the fact that there were only one green ball in the left box would have affected your judgment about whether <b>Joe's choice of a green ball from the left box</b> caused him to win? If so, how?</p>`
        }
    },
    labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_cause1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_cause1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_cause2;
        } else {
            return introspection_q_labels_cause2.slice().reverse();
        }
    },
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br>",
    on_finish: function (data) {

        if (label_order_randomized == 'original') {
            cause_intro_response1 = data.response
    }
        else {
            cause_intro_response1 = 100 - data.response;
            }
        }

};

var cause_intro_response2 = null;
var cause_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        cause_intro_response2 = data.response.Q0
    }
};

var cause_intro_confidence_response = null;
var cause_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        cause_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "causal inference",
            condition: condition[0] == "Factor-Included" ? "One" : "Nine",
            stimulus: null,
            choice: cause,
            flipped_scale: label_order_randomized,
            auxiliary_info1:  null,
            openq_response: cause_openQ_response,
            introspect_rating: cause_intro_response1,
            introspect_open: cause_intro_confidence_response,
            familiarity: familiarity,
            rt_main_question: data.rt
        }
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var cause_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity= data.response == 0 ? "Yes" : "No"
    }
}


if (only_main_question) {
    var cause = {
        timeline: [cause_instructions, cause_condition, cause_question]
    };
} else {
    var cause = {
        timeline: [cause_instructions, cause_condition, cause_question, cause_familiar, cause_openQ, cause_introspect1, cause_intro_confidence]
    };
}

//#endregion
//timeline.push(cause)
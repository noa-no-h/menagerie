//#region 7. Associative Memory (Roediger & McDermott, 1995) - WITHIN

var confidence_q = condition[0] == 'Factor-Included' ? '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by whether the word was related to the topic of sleep)?</p>' : '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by whether the word was related to the topic of sleep)?</p>';

var assoc_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, you will be shown a series of words in quick succession. Your goal is to <b>memorize as many words as you can</b>.</p>
        <p>After all the words are shown, your memory will be tested through a series of questions.</p>`,
        `<p>On the next screen, you will be shown the series of words. Each word will appear once in the center of the screen, for example:</p><br><br><br><br>
        <p style = "font-size:30px">house</p>
        <br><br><br>
        <p>The words will appear relatively <b>quickly</b> so please pay attention to the screen at all times.</p>
        <p><i>Please click the button below to begin viewing the words.</i></p>`
    ],
    show_clickable_nav: true
};

var assoc_list = [
    { stimulus: `<p style = "font-size:30px">bed</p>` },
    { stimulus: `<p style = "font-size:30px">rest</p>` },
    { stimulus: `<p style = "font-size:30px">awake</p>` },
    { stimulus: `<p style = "font-size:30px">tired</p>` },
    { stimulus: `<p style = "font-size:30px">dream</p>` },
    { stimulus: `<p style = "font-size:30px">wake</p>` },
    { stimulus: `<p style = "font-size:30px">snooze</p>` },
    { stimulus: `<p style = "font-size:30px">blanket</p>` },
    { stimulus: `<p style = "font-size:30px">doze</p>` },
    { stimulus: `<p style = "font-size:30px">slumber</p>` },
    { stimulus: `<p style = "font-size:30px">snore</p>` },
    { stimulus: `<p style = "font-size:30px">nap</p>` },
    { stimulus: `<p style = "font-size:30px">yawn</p>` },
    { stimulus: `<p style = "font-size:30px">drowsy</p>` },
]

var assoc_listTrials = {
    timeline: [
        {//fixation
            type: jsPsychHtmlKeyboardResponse,
            stimulus: '<p style = "font-size:30px">+</p>',
            choices: "NO_KEYS",
            trial_duration: 500
        },
        {//trials
            type: jsPsychHtmlKeyboardResponse,
            stimulus: jsPsych.timelineVariable('stimulus'),
            choices: "NO_KEYS",
            trial_duration: 1000
        },
    ],
    timeline_variables: assoc_list,
    randomize_order: true
};

var assoc_instructions2 = {
    type: jsPsychInstructions,
    pages: [
        `<p>Now you will be asked a series of recall questions. In each question, you will be presented with a word and asked to determine whether it was present in the original list (<b>"original"</b>) or not present in the original list (<b>"new"</b>).</p>
    <p><i>Please click the button below to continue.</i></p>`,
    ],
    show_clickable_nav: true
};

var assoc_practice = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p><i>This is a practice question. Please read the word below and judge whether it was present in the original list. If so, click "Original." Otherwise, click "New."</i></p><br><br>
<p style = "font-size:30px">doze</p>`,
    choices: ["Original", "New"],
    prompt: `<br> Is this word original or new?`
};

var assoc_practice_feedback = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function (data) {
        var original = jsPsych.data.get().last(1).values()[0].response
        if (original == 0) {
            return `<p>Correct! This word is original.</p>
        <p> Now you will begin the real trials. </p>
        <p><i>Please click the button below to continue.</i></p>`
        } else {
            return `<p>Incorrect. This word is original.</p>
        <p> Now you will begin the real trials. </p>
        <p><i>Please click the button below to continue.</i></p>`
        }
    },
    choices: ["Continue"]
};

var assoc_included_target = [
    //original words
    {
        stimulus: `<p style = "font-size:30px">bed</p>`,
        name: "bed", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">rest</p>`,
        name: "rest", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">awake</p>`,
        name: "awake", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">snore</p>`,
        name: "snore", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">slumber</p>`,
        name: "slumber", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">nap</p>`,
        name: "nap", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">dream</p>`,
        name: "dream", category: "Original", topic: "Sleep"
    },
    //new words
    {
        stimulus: `<p style = "font-size:30px">sleep</p>`,
        name: "sleep", category: "New", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">pillow</p>`,
        name: "pillow", category: "New", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">sheet</p>`,
        name: "sheet", category: "New", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">nurse</p>`,
        name: "nurse", category: "New", topic: "NonSleep"
    },
    {
        stimulus: `<p style = "font-size:30px">shade</p>`,
        name: "shade", category: "New", topic: "NonSleep"
    },
    {
        stimulus: `<p style = "font-size:30px">bitter</p>`,
        name: "bitter", category: "New", topic: "NonSleep"
    },
    {
        stimulus: `<p style = "font-size:30px">heart</p>`,
        name: "heart", category: "New", topic: "NonSleep"
    },
]

var assoc_excluded_target = [
    //original words
    {
        stimulus: `<p style = "font-size:30px">bed</p>`,
        name: "bed", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">rest</p>`,
        name: "rest", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">awake</p>`,
        name: "awake", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">snore</p>`,
        name: "snore", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">slumber</p>`,
        name: "slumber", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">nap</p>`,
        name: "nap", category: "Original", topic: "Sleep"
    },
    {
        stimulus: `<p style = "font-size:30px">dream</p>`,
        name: "dream", category: "Original", topic: "Sleep"
    },
    //new words
    {
        stimulus: `<p style = "font-size:30px">suit</p>`,
        name: "suit", category: "New", topic: "NonSleep"
    },
    {
        stimulus: `<p style = "font-size:30px">walk</p>`,
        name: "walk", category: "New", topic: "NonSleep"
    },
    {
        stimulus: `<p style = "font-size:30px">kneel</p>`,
        name: "kneel", category: "New", topic: "NonSleep"
    },
    {
        stimulus: `<p style = "font-size:30px">nurse</p>`,
        name: "nurse", category: "New", topic: "NonSleep"
    },
    {
        stimulus: `<p style = "font-size:30px">shade</p>`,
        name: "shade", category: "New", topic: "NonSleep"
    },
    {
        stimulus: `<p style = "font-size:30px">bitter</p>`,
        name: "bitter", category: "New", topic: "NonSleep"
    },
    {
        stimulus: `<p style = "font-size:30px">heart</p>`,
        name: "heart", category: "New", topic: "NonSleep"
    },
]

var assoc_targetTrials = {
    timeline: [
        {//fixation
            type: jsPsychHtmlKeyboardResponse,
            stimulus: '<p style = "font-size:30px">+</p>',
            choices: "NO_KEYS",
            trial_duration: 500
        },
        {//trials
            type: jsPsychHtmlButtonResponse,
            stimulus: jsPsych.timelineVariable('stimulus'),
            choices: ["Original", "New"],
            prompt: `<br> Is this word original or new?`,
            data: { stim: jsPsych.timelineVariable('name'), aux: jsPsych.timelineVariable('category'), con: jsPsych.timelineVariable('topic') },
            on_finish: function (data) {
                s1_data = {
                    subject: data.subject,
                    version: data.version,
                    task_name: "associative memory",
                    factor: data.condition,
                    condition: data.con,
                    choice: data.response == "0" ? "Original" : "New",
                    stimulus: data.stim,
                    auxiliary_info1: data.aux,
                    rt: data.rt,
                }
                save_data(s1_data, 'introspection');
            }
        },
    ],
    timeline_variables: condition[0] == "Factor-Included" ? assoc_included_target : assoc_excluded_target,
    randomize_order: true
};

var assoc_openQ_response = null;
var assoc_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were asked to memorize a series of words. Then, you were shown another series of words and asked to determine if they were original (present in the original list) or new (not present in the original list).</p>
<p>Describe your thought process during this exercise. How did you come to your eventual decisions about which words were original versus new?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        assoc_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_assoc1 = [`<strong>When the word was related to sleep, that made me <u>LESS</u> likely to judge it as new (and more likely to judge it as original)</strong>`, "", "<strong>Whether the word was related to sleep did not affect my response</strong>", "", `<strong>When the word was related to sleep, that made me <u>MORE</u> likely to judge it as new (and less likely to judge it as original)</strong>`];
var introspection_q_labels_assoc2 = [`<strong>If the word had been related to sleep, that would have made me <u>LESS</u> likely to judge it as new (and more likely to judge it as original)</strong>`, "", "<strong>Whether the word was related to sleep would not have affected my response</strong>", "", `<strong>If the word had been related to sleep, that would have made me <u>MORE</u> likely to judge it as new (and less likely to judge it as original)</strong>`];

var assoc_intro_response1 = null;
var assoc_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `Below are the lists of both the original and new words:<p>
        <font color = "RED"><p style="font-size:17px"><b>Original Words:</b> bed, rest, awake, tired, dream, wake, snooze, blanket, doze, slumber, snore, nap, yawn, drowsy</font>
        <font color = "BLUE"><p style="font-size:17px"><b>New Words:</b> suit, walk, kneel, nurse, shade, bitter, heart</font>
        <p>While doing this exercise, you might have noticed that <i>all</i> of the words in the <b>original list</b> were <b><i>related to the topic of sleep</i></b> (for example, "bed" and "doze"), while only <i>some</i> of the words in the <b>new list</b> were related to sleep (for example, "pillow").
        <p>While you were determining whether each word was original or new, do you think your decision-making process was influenced by whether the word was <b>related to the topic of sleep?</b> If so, how?</p>`
        } else {
            return `Below are the lists of both the original and new words:<p>
        <font color = "RED"><p style="font-size:17px"><b>Original Words:</b> bed, rest, awake, tired, dream, wake, snooze, blanket, doze, slumber, snore, nap, yawn, drowsy</font>
        <font color = "BLUE"><p style="font-size:17px"><b>New Words:</b> suit, walk, kneel, nurse, shade, bitter, heart</font>
        <p>While doing this exercise, you might have noticed that <i>all</i> of the words in the <b>original list</b> were <b><i>related to the topic of sleep</i></b> (for example, "bed" and "doze"), while <i>none</i> of the words in the <b>new list</b> were related to sleep.
        <p>Now, imagine if some of the words in the new list <i>had</i> been related to sleep. For example, imagine if you were asked to decide if the word "pillow" was new or original.
        <p>If this were the case, do you think your decision-making process would have been influenced by whether the word was <b>related to the topic of sleep?</b> If so, how?</p>`
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_assoc1 : introspection_q_labels_assoc2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>>",
    on_finish: function (data) {
        assoc_intro_response1 = data.response
    }
}

var assoc_intro_response2 = null;
var assoc_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        assoc_intro_response2 = data.response.Q0
    }
};

var assoc_intro_confidence_response = null;
var assoc_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        assoc_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "associative memory",
            condition: null,
            stimulus: null,
            choice: null,
            auxiliary_info1: null,
            openq_response: assoc_openQ_response,
            introspect_rating: assoc_intro_response1,
            introspect_open: assoc_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var assoc_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity= data.response == 0 ? "Yes" : "No";

    }
}


if (only_main_question) {
    var assoc = {
        timeline: [assoc_instructions, assoc_listTrials, assoc_instructions2, assoc_practice, assoc_practice_feedback, assoc_targetTrials]
    };
} else {
    var assoc = {
        timeline: [assoc_instructions, assoc_listTrials, assoc_instructions2, assoc_practice, assoc_practice_feedback, assoc_targetTrials, assoc_familiar, assoc_openQ, assoc_introspect1, assoc_intro_confidence]
    };
}

//#endregion
//timeline.push(assoc)

//#region 10. Numerical Distance Effect (Maloney et al., 2010),
var number_instructions1 = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, you will be given a series of trials where you will see a two numbers on either side of the screen. Your task is to determine which number is larger than the other.</p>
    <p>If the <b>left number is larger</b> than the right number, press the <b>F key</b> on your keyboard as fast as you can.</p>
    <p>If the <b>right number is larger</b> than the left number, press the <b>J key</b> on your keyboard as fast as you can.</p>`,
        `<p>Below is an example of how the numbers will appear on your screen:</p>
    <img src='img/14.png'; style='width:1000px'></img>
    <p>In this case, the <b>right number (4) is larger</b> than the left number (1), so you would press the <b>J key</b> on your keyboard as fast as you can.</p>`,
        `<p>You will now begin a practice round. Remember, if the <b>left number is larger</b> than the right number, press the <b>F key</b> on your keyboard as fast as you can.
    If the <b>right number is larger</b> than the left number, press the <b>J key</b> on your keyboard as fast as you can.</p>
    <p>For the practice round, you will recieve feedback for every trial. However, in the actual trials, you will not.</p>
    <p><i>Please click the "Next" button to begin the practice round.</i></p>`
    ],
    show_clickable_nav: true
}

var number_included_stimuli = [
    { stimulus: `<img src='img/23.png'; style='width:1000px'></img`, name: "2,3", difference: "1", correct_response: "j" },
    { stimulus: `<img src='img/34.png'; style='width:1000px'></img`, name: "3,4", difference: "1", correct_response: "j" },
    { stimulus: `<img src='img/43.png'; style='width:1000px'></img`, name: "4,3", difference: "1", correct_response: "f" },
    { stimulus: `<img src='img/67.png'; style='width:1000px'></img`, name: "6,7", difference: "1", correct_response: "j" },
    { stimulus: `<img src='img/76.png'; style='width:1000px'></img`, name: "7,6", difference: "1", correct_response: "f" },
    { stimulus: `<img src='img/87.png'; style='width:1000px'></img`, name: "8,7", difference: "1", correct_response: "f" },
    { stimulus: `<img src='img/26.png'; style='width:1000px'></img`, name: "2,6", difference: "4", correct_response: "j" },
    { stimulus: `<img src='img/37.png'; style='width:1000px'></img`, name: "3,7", difference: "4", correct_response: "j" },
    { stimulus: `<img src='img/48.png'; style='width:1000px'></img`, name: "4,8", difference: "4", correct_response: "j" },
    { stimulus: `<img src='img/62.png'; style='width:1000px'></img`, name: "6,2", difference: "4", correct_response: "f" },
    { stimulus: `<img src='img/73.png'; style='width:1000px'></img`, name: "7,3", difference: "4", correct_response: "f" },
    { stimulus: `<img src='img/84.png'; style='width:1000px'></img`, name: "8,4", difference: "4", correct_response: "f" },
]

var number_excluded_stimuli = [
    { stimulus: `<img src='img/23.png'; style='width:1000px'></img`, name: "2,3", difference: "1", correct_response: "j" },
    { stimulus: `<img src='img/34.png'; style='width:1000px'></img`, name: "3,4", difference: "1", correct_response: "j" },
    { stimulus: `<img src='img/43.png'; style='width:1000px'></img`, name: "4,3", difference: "1", correct_response: "f" },
    { stimulus: `<img src='img/67.png'; style='width:1000px'></img`, name: "6,7", difference: "1", correct_response: "j" },
    { stimulus: `<img src='img/76.png'; style='width:1000px'></img`, name: "7,6", difference: "1", correct_response: "f" },
    { stimulus: `<img src='img/87.png'; style='width:1000px'></img`, name: "8,7", difference: "1", correct_response: "f" },
    { stimulus: `<img src='img/23.png'; style='width:1000px'></img`, name: "2,3", difference: "1", correct_response: "j" },
    { stimulus: `<img src='img/34.png'; style='width:1000px'></img`, name: "3,4", difference: "1", correct_response: "j" },
    { stimulus: `<img src='img/43.png'; style='width:1000px'></img`, name: "4,3", difference: "1", correct_response: "f" },
    { stimulus: `<img src='img/67.png'; style='width:1000px'></img`, name: "6,7", difference: "1", correct_response: "j" },
    { stimulus: `<img src='img/76.png'; style='width:1000px'></img`, name: "7,6", difference: "1", correct_response: "f" },
    { stimulus: `<img src='img/87.png'; style='width:1000px'></img`, name: "8,7", difference: "1", correct_response: "f" },
]

var number_stimuli = null;
if (condition[0] == "Factor-Included") {
    number_stimuli = number_included_stimuli
} else {
    number_stimuli = number_excluded_stimuli
}

var number_correct = null
var number_practice = {
    timeline: [
        {//fixation
            type: jsPsychHtmlKeyboardResponse,
            stimulus: '<p style = "font-size:30px">+</p>',
            choices: "NO_KEYS",
            trial_duration: 500
        },
        {//stimulus
            type: jsPsychHtmlKeyboardResponse,
            stimulus: jsPsych.timelineVariable('stimulus'),
            choices: ['f', 'j'],
            on_finish: function (data) {
                number_correct = jsPsych.timelineVariable('correct_response')
            }
        },
        {//feedback
            type: jsPsychHtmlKeyboardResponse,
            stimulus: function () {
                var last_trial = jsPsych.data.get().last(1).values()[0].response
                if (last_trial == number_correct) {
                    return `<p style = "font-size:30px"><font color = "GREEN">Correct!</font></p>`
                } else {
                    return `<p style = "font-size:30px"><font color = "RED">Incorrect!</font></p>
                <p>Remember:</p>
                <p>Press the <b>F key</b> if the <b>left number is larger</b>.
                <p>Press the <b>J key</b> if the <b>right number is larger</b>.`
                }
            },
            choices: "NO_KEYS",
            trial_duration: 1500
        }
    ],
    timeline_variables: number_stimuli,
    sample: {
        type: 'without-replacement',
        size: 5
    }
}

number_instructions2 = {
    type: jsPsychInstructions,
    pages: [
        `<p>Now you will begin the actual trials. For these trials, you will no longer receive feedback on your responses.</p>
    <p><i>Please click the button below when you are ready to begin.</i></p>`
    ],
    show_clickable_nav: true,
}

var number_trials = {
    timeline: [
        {//fixation
            type: jsPsychHtmlKeyboardResponse,
            stimulus: '<p style = "font-size:30px">+</p>',
            choices: "NO_KEYS",
            trial_duration: 500
        },
        {//stimulus
            type: jsPsychHtmlKeyboardResponse,
            stimulus: jsPsych.timelineVariable('stimulus'),
            choices: ['f', 'j'],
            data: { stim: jsPsych.timelineVariable('name'), con: jsPsych.timelineVariable('difference'), correct_response: jsPsych.timelineVariable('correct_response') },
            on_finish: function (data) {
                s1_data = {
                    subject: data.subject,
                    version: data.version,
                    task_name: "numerical distance effect",
                    factor: data.condition,
                    condition: data.con,
                    stimulus: data.stim,
                    choice: data.response,
                    auxiliary_info1: data.response == data.correct_response ? "Correct" : "Incorrect",
                    rt: data.rt
                }
                save_data(s1_data, 'introspection');
            }
        }
    ],
    timeline_variables: number_stimuli,
    randomize_order: true
}

var number_openQ_response = null;
var number_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were shown a series of number pairs and asked to judge which number in the pair was larger (left or right). Describe your thought process during this exercise. Did you notice if there were any trials where you were able to make those judgments <i>faster</i> than in other trials? If so, what factors do you think influenced <b>how fast or slow</b> you were able to make each judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        number_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_number1 = [`<strong>When the numbers were further apart, that made me respond <u>SLOWER</u></strong>`, "", "<strong>The difference between the numbers did not affect my response</strong>", "", `<strong>When the numbers were further apart, that made me respond <u>FASTER</u></strong>`];
var introspection_q_labels_number2 = [`<strong>If the numbers had been further apart, that would have made me respond <u>SLOWER</u></strong>`, "", "<strong>The difference between the numbers would not have affected my response</strong>", "", `<strong>If the numbers had been further apart, that would have made me respond <u>FASTER</u></strong>`];

var number_intro_response1 = null;
var number_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>During this exercise, you might have noticed that these number pairs varied in the difference between them. For example, some number pairs were farther apart from each other (e.g. <u>3 and 7</u>) while others were closer to each other (e.g. <u>3 and 4</u>).
        <p>Do you think the <b>difference</b> between the numbers in each number pair affected <b>how fast</b> you were able to make your judgment for each trial? If so, how?`
        } else {
            return `During this exercise, you might have noticed that all of the number pairs had a difference of exactly 1 between each number (e.g. <u>3 and 4</u>, or <u>1 and 2</u>).
        <p>Now imagine if only some of these pairs had a difference of 1, while others had a difference of 4 between them. For example, one trial might ask you to choose the larger number between <u>1 and 2</u>, whereas others would ask you to choose the larger number between <u>4 and 7</u>.
        <p>If this were the case, do you think the <b>difference</b> between the numbers in each number pair (e.g. a difference of 4 vs. a difference of 1) would have affected <b>how fast</b> you were able to make your judgment for each trial? If so, how?`
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_number1 : introspection_q_labels_number2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br><br>",
    on_finish: function (data) {
        number_intro_response1 = data.response
    }
}

var number_intro_response2 = null;
var number_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you choose the number that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        number_intro_response2 = data.response.Q0
    }
};

var number_intro_confidence_response = null;
var number_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        number_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            task_name: "numerical distance effect",
            factor: data.condition,
            familiarity: familiarity,
            openq_response: number_openQ_response,
            introspect_rating: number_intro_response1,
            introspect_open: number_intro_confidence_response,
            rt: data.rt
        }
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var number_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity=data.response == 0 ? "Yes" : "No"

        
    }
}

var number = {
    timeline: [number_familiar]

    //timeline: [number_instructions1, number_practice, number_instructions2, number_trials, number_familiar, number_openQ, number_introspect1, number_introspect2]
}
//#endregion
//timeline.push(preload, numb)
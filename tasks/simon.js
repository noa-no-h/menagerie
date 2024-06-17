//#region 11. Simon Task - WITHIN
var simon_instructions1 = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, you will be given a series of trials where you will see a box on either side of the screen.</p>
    <p>If the box is <font color="RED"><b>red</b></font>, press the letter <b>F</b> on your keyboard as fast as you can.</p>
    <p>If the box is <font color="BLUE"><b>blue</b></font>, press the letter <b>J</b> on your keyboard as fast as you can.</p>
    <div style='width:500px; height:350px; margin:auto'>
    <div style='float: left;'><img src='img/redbox.png'; style='width:200px'></img>
    <p class='small'><strong>Press the F key</strong></p></div>
    <div style='float: right;'><img src='img/bluebox.png'; style='width:200px'></img>
    <p class='small'><strong>Press the J key</strong></p></div>
    </div>
    <p>Please note that it <i>does not matter which side the box appears on</i>. Just press the key that corresponds to the box's <b>color.</b>`,
        `<p>You will now begin a practice round.</p><p>Remember, if you see a <font color="RED"><b>red</b></font> box, press the letter <b>F</b> on your keyboard.
    If you see a <font color="BLUE"><b>blue</b></font> box, press the letter <b>J</b> on your keyboard. Please try to respond to each trial as fast as you can.
    For the practice round, you will recieve feedback for every trial. However, in the actual trials, you will not.</p>
    <p><i>Please click the "Next" button to begin the practice round.</i></p>`
    ],
    show_clickable_nav: true,
}

var simon_included_stimuli = [
    {
        stimulus:
            `<div style='width:700px; height:350px; margin:auto'>
    <div style='float: left;'><img src='img/redbox.png'; style='width:300px'></img>`,
        correct_response: 'f', color: "Red", type: "Congruent"
    },
    {
        stimulus:
            `<div style='width:700px; height:350px; margin:auto'>
    <div style='float: right;'><img src='img/bluebox.png'; style='width:300px'></img>`,
        correct_response: 'j', color: "Blue", type: "Congruent"
    },
    {
        stimulus:
            `<div style='width:700px; height:350px; margin:auto'>
    <div style='float: left;'><img src='img/bluebox.png'; style='width:300px'></img>`,
        correct_response: 'j', color: "Blue", type: "Incongruent"
    },
    {
        stimulus:
            `<div style='width:700px; height:350px; margin:auto'>
    <div style='float: right;'><img src='img/redbox.png'; style='width:300px'></img>`,
        correct_response: 'f', color: "Red", type: "Incongruent"
    }
]

var simon_excluded_stimuli = [
    {
        stimulus:
            `<div style='width:700px; height:350px; margin:auto'>
    <div style='float: left;'><img src='img/redbox.png'; style='width:300px'></img>`,
        correct_response: 'f', color: "Red", type: "Congruent"
    },
    {
        stimulus:
            `<div style='width:700px; height:350px; margin:auto'>
    <div style='float: right;'><img src='img/bluebox.png'; style='width:300px'></img>`,
        correct_response: 'j', color: "Blue", type: "Congruent"
    },
    {
        stimulus:
            `<div style='width:700px; height:350px; margin:auto'>
    <div style='float: left;'><img src='img/redbox.png'; style='width:300px'></img>`,
        correct_response: 'f', color: "Red", type: "Congruent"
    },
    {
        stimulus:
            `<div style='width:700px; height:350px; margin:auto'>
    <div style='float: right;'><img src='img/bluebox.png'; style='width:300px'></img>`,
        correct_response: 'j', color: "Blue", type: "Congruent"
    }
]

var simon_stimuli = null;
if (condition[0] == "Factor-Included") {
    simon_stimuli = simon_included_stimuli
} else {
    simon_stimuli = simon_excluded_stimuli
}

var simon_correct = null
var simon_practice = {
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
            trial_duration: 1000,
            on_finish: function (data) {
                simon_correct = jsPsych.timelineVariable('correct_response')
            }
        },
        {//feedback
            type: jsPsychHtmlKeyboardResponse,
            stimulus: function () {
                var last_trial = jsPsych.data.get().last(1).values()[0].response
                if (last_trial == simon_correct) {
                    return `<p style = "font-size:30px"><font color = "GREEN">Correct!</font></p>`
                } else if (last_trial == null) {
                    return `<p style = "font-size:30px">Respond faster!</p>`
                } else {
                    return `<p style = "font-size:30px"><font color = "RED">Incorrect</font></p>`
                }
            },
            choices: "NO_KEYS",
            trial_duration: 1000
        }
    ],
    timeline_variables: simon_stimuli,
    randomize_order: true,
    repetitions: 2
}

simon_instructions2 = {
    type: jsPsychInstructions,
    pages: [
        `<p>Now you will begin the actual trials. For these trials, you will no longer receive feedback on your responses.</p>
    <p><i>Please click the button below when you are ready to begin.</i></p>`
    ],
    show_clickable_nav: true,
}

var simon_aux = null
var simon_trials = {
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
            trial_duration: 1000,
            data: { stim: jsPsych.timelineVariable('color'), con: jsPsych.timelineVariable('type'), correct_response: jsPsych.timelineVariable('correct_response') },
            on_finish: function (data) {
                if (data.response == data.correct_response) {
                    simon_aux = "Correct"
                } else if (data.response == null) {
                    simon_aux = "Failure"
                } else {
                    simon_aux = "Incorrect"
                }
                s1_data = {
                    subject: data.subject,
                    version: data.version,
                    factor: data.condition,
                    task_name: "simon task",
                    condition: data.con,
                    stimulus: data.stim,
                    choice: data.response,
                    auxiliary_info1: simon_aux,
                    rt: data.rt
                }
                save_data(s1_data, 'introspection');
            }
        },
    ],
    timeline_variables: simon_stimuli,
    sample: {
        type: 'with-replacement',
        size: 20
    }
}

var simon_openQ_response = null;
var simon_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were shown a series of boxes. You were asked to press the F key if you saw a red box and the J key if you saw a blue box.</p>
    <p>Describe your thought process during each trial of this exercise. Did you notice if there were any trials where you were <i>faster</i> at pressing the correct key than in other trials? If so, what factors do you think influenced <b>how fast or slow</b> you were able to press the correct key?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        simon_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_simon1 = [`<strong>When the box was on the opposite side from the key I had to press, that made me respond <u>SLOWER</u></strong>`, "", "<strong>The location of the boxes did not affect my response</strong>", "", `<strong>When the box was on the opposite side from the key I had to press, that made me respond <u>FASTER</u></strong>`];
var introspection_q_labels_simon2 = [`<strong>If the box had been on on the opposite side from the key I had to press, that would have made me respond <u>SLOWER</u></strong>`, "", "<strong>The location of the boxes would not have not affected my response</strong>", "", `<strong>If the box had been on the opposite side from the key I had to press, that would have made me respond <u>FASTER</u></strong>`];

var simon_intro_response1 = null;
var simon_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>During this exercise, you might have noticed that sometimes the boxes appeared on the same side of the screen as the key you had to press, and other times they appeared on the opposite side. 
            For instance, sometimes the red box appeared on the left side of the screen, requiring you to press the F key (which is on the left side of the keyboard). But sometimes it appeared on the right side of the screen, which is the opposite side from the F key.</p>
            <p>Similarly, sometimes the blue box appeared on the right side of the screen, requiring you to press the J key (which is on the right side of the keyboard). But sometimes it appeared on the left side of the screen, which is the opposite side from the J key.</p>
        <p>Do you think the <b>location</b> of the boxes (i.e., left vs. right side) affected <b>how fast</b> you were able to complete each trial? If so, how?</p>`
        } else {
            return `<p>During this exercise, you might have noticed that the boxes always appeared on the same side of the screen as the key you had to press. For instance, red boxes always appeared on the left side of the screen, requiring you to press the F key (which is on the left side of the keyboard); and blue boxes always appeared on the right side of the screen, requiring you to press the J key (which is on the right side of the keyboard).</p>
        <p>Imagine if, in some trials, the boxes switched locations. For example, imagine that the red box appeared on the right side of the screen, which is the opposite side from the F key.
            Similarly, imagine if the <b>blue</b> box had appeared on the <b>left</b> side of the screen, which is the opposite side from the J key.</p>
        <p>Do you think the <b>location</b> of the boxes (i.e., left vs. right side) would have affected <b>how fast</b> you'd be able to complete each trial? If so, how?</p>`
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_simon1 : introspection_q_labels_simon2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br><br>",
    on_finish: function (data) {
        simon_intro_response1 = data.response
    }
}

var simon_intro_response2 = null;
var simon_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        simon_intro_response2 = data.response.Q0
    }
};

var simon_intro_confidence_response = null;
var simon_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        simon_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "simon task",
            openq_response: simon_openQ_response,
            introspect_rating: simon_intro_response1,
            introspect_open: simon_intro_confidence_response,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var simon_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p>Before doing this study, had you seen or heard of a task similar to this last one before?</p>`,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity = data.response == 0 ? "Yes" : "No";

        
    }
}

var simon = {
    timeline: [simon_instructions1, simon_practice, simon_instructions2, simon_trials, simon_familiar, simon_openQ, simon_introspect1, simon_intro_confidence]
}

//#endregion
//timeline.push(simon)
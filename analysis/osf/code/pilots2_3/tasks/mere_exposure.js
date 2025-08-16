 //#region 9. Mere Exposure (Stang (1974)


var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the number of times you saw each word)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the number of times you saw each word)?</p>";
 

//preparing stimuli
var stimulus_id_array = [
    `<p style = "font-size:30px">kitap</p>`,
    `<p style = "font-size:30px">tatil</p>`,
    `<p style = "font-size:30px">mimar</p>`,
    `<p style = "font-size:30px">dudak</p>`,
    `<p style = "font-size:30px">sokak</p>`,
    `<p style = "font-size:30px">kulak</p>`,
    `<p style = "font-size:30px">bacak</p>`,
    `<p style = "font-size:30px">hafta</p>`,
]
// [0, 1, 2, 3, 4, 5, 6, 7]
var stimulus_id_array_shuffled = _.shuffle(stimulus_id_array)

var one_array1 = Array(1).fill(stimulus_id_array_shuffled[0]);
var one_array2 = Array(1).fill(stimulus_id_array_shuffled[1]);
var one_array3 = Array(1).fill(stimulus_id_array_shuffled[2]);
var one_array4 = Array(1).fill(stimulus_id_array_shuffled[3]);

var one_array5 = Array(1).fill(stimulus_id_array_shuffled[4]);
var one_array6 = Array(1).fill(stimulus_id_array_shuffled[5]);
var one_array7 = Array(1).fill(stimulus_id_array_shuffled[6]);
var one_array8 = Array(1).fill(stimulus_id_array_shuffled[7]);

var twentyfive_array1 = Array(25).fill(stimulus_id_array_shuffled[4]);
var twentyfive_array2 = Array(25).fill(stimulus_id_array_shuffled[5]);
var twentyfive_array3 = Array(25).fill(stimulus_id_array_shuffled[6]);
var twentyfive_array4 = Array(25).fill(stimulus_id_array_shuffled[7]);

var thirteen_array1 = Array(13).fill(stimulus_id_array_shuffled[0]);
var thirteen_array2 = Array(13).fill(stimulus_id_array_shuffled[1]);
var thirteen_array3 = Array(13).fill(stimulus_id_array_shuffled[2]);
var thirteen_array4 = Array(13).fill(stimulus_id_array_shuffled[3]);

var thirteen_array5 = Array(13).fill(stimulus_id_array_shuffled[4]);
var thirteen_array6 = Array(13).fill(stimulus_id_array_shuffled[5]);
var thirteen_array7 = Array(13).fill(stimulus_id_array_shuffled[6]);
var thirteen_array8 = Array(13).fill(stimulus_id_array_shuffled[7]);

var included_stimulus_array = one_array1.concat(one_array2, one_array3, one_array4, twentyfive_array1, twentyfive_array2, twentyfive_array3, twentyfive_array4);
var included_stimulus_array = _.shuffle(included_stimulus_array);

//var excluded_stimulus_array = one_array1.concat(one_array2, one_array3, one_array4, one_array5, one_array6, one_array7, one_array8)
var excluded_stimulus_array = thirteen_array1.concat(thirteen_array2, thirteen_array3, thirteen_array4, thirteen_array5, thirteen_array6, thirteen_array7, thirteen_array8);
var excluded_stimulus_array = _.shuffle(excluded_stimulus_array);

var stimulus_array = null;
if (condition[0] == "Factor-Included") {
    stimulus_array = included_stimulus_array
} else {
    stimulus_array = excluded_stimulus_array
}

function countInArray(array, what) {
    var count = 0;
    for (var i = 0; i < array.length; i++) {
        if (array[i] === what) {
            count++;
        }
    }
    return count;
}

var mere_exposure_trials = [];
for (var i = 0; i < stimulus_array.length; i++) {
    mere_exposure_trials.push({
        type: jsPsychHtmlKeyboardResponse,
        stimulus: stimulus_array[i],
        choices: "NO_KEYS",
        trial_duration: 1000
    },
        {
            type: jsPsychHtmlKeyboardResponse,
            stimulus: '',
            choices: "NO_KEYS",
            trial_duration: 100
        })
};

var mere_exposure_instructions1 = {
    type: jsPsychInstructions,
    pages: [
        `<p>This exercise comes in two parts. In the first part of this exercise, you will be shown a series of Turkish words. Then, in the second part of this exercise, you will be asked to answer a few questions about the words you were shown.</p>`,
        `<p>For the first part of this exercise, you will see a series of Turkish words. It is not expected that you be able to speak Turkish. We are simply interested in your impression of the words.<p>Each word will appear once in the center of the screen, as follows:</p><br><br><br><br>
<p style = "font-size:30px">kalem</p>
<br><br><br>
<p>The words will appear relatively <b>quickly</b>, so please pay attention to the screen throughout the exercise. Note that some of the words may appear more than once.</p>
        <p>This part of the exercise should take about five minutes.</p>
        <p>You will now begin the first part of the exercise. The words will appear as soon as you press the "Next" button below.</p>
<p><i>Please click the "Next" button when you are ready to see the words.</i></p>`,
    ],
    show_clickable_nav: true
};

var mere_exposure_exposure = {
    timeline: mere_exposure_trials
}

var mere_exposure_stimuli = [
    { stimulus: `<p style = "font-size:30px">kitap</p>`, name: "kitap" },
    { stimulus: `<p style = "font-size:30px">tatil</p>`, name: "tatil" },
    { stimulus: `<p style = "font-size:30px">mimar</p>`, name: "mimar" },
    { stimulus: `<p style = "font-size:30px">dudak</p>`, name: "dudak" },
    { stimulus: `<p style = "font-size:30px">sokak</p>`, name: "sokak" },
    { stimulus: `<p style = "font-size:30px">kulak</p>`, name: "kulak" },
    { stimulus: `<p style = "font-size:30px">bacak</p>`, name: "bacak" },
    { stimulus: `<p style = "font-size:30px">hafta</p>`, name: "hafta" }
]

var mere_exposure_instructions2 = {
    type: jsPsychInstructions,
    pages: [`<p>You will now begin the second part of the exercise. For this part, you will be shown a series of words and be asked to rate how much you <b>like</b> each one. Note that several of these words may be ones that you have seen before. There are no right or wrong answers so try to be as honest as possible.</p>
<p><i>Please click the button below to continue.</i></p>`,],
    show_clickable_nav: true
};

var mere_exposure_questions = {
    timeline: [
        {
            type: jsPsychHtmlSliderResponse,
            stimulus: jsPsych.timelineVariable('stimulus'),
            stimulus_height: 350,
            labels: [`<strong>1<br>not at all</strong>`, "2", "3", "4", "5", "6", "7", "8", `<strong>9<br>very much</strong>`],
            prompt: `On a scale from 1 (not at all) to 9 (very much), rate how much you <b>like</b> this word.<br><br>`,
            slider_width: 750,
            min: 10,
            max: 90,
            slider_start: 50,
            require_movement: require_movement_general,
            data: { stim: jsPsych.timelineVariable('name'), aux: jsPsych.timelineVariable('stimulus') },
            on_finish: function (data) {
                var s1_data = {
                    subject: data.subject,
                    version: data.version,
                    factor: data.condition,
                    task_name: "mere exposure",
                    choice: data.response,
                    stimulus: data.stim,
                    condition: countInArray(stimulus_array, data.aux),
                }
                save_data(s1_data, 'introspection');
            }
        }
    ],
    timeline_variables: mere_exposure_stimuli,
    randomize_order: true
};

var mere_exposure_openQ_response = null;
var mere_exposure_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were shown a series of words and asked to rate how much you liked them.</p>
<p>Describe your thought process during this exercise. How did you come to your eventual ratings for each word?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        mere_exposure_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_mee1 = [`<strong>When I saw a word a lot of times, that made me like the word <u>LESS</u></strong>`, "", "<strong>The number of times I saw a word did not affect my response</strong>", "", `<strong>When I saw a word a lot of times, that made me like the word <u>MORE</u></strong>`];
var introspection_q_labels_mee2 = [`<strong>If I had seen a word a lot of times, that would have made me like the word <u>LESS</u></strong>`, "", "<strong>The number of times I saw a word would not have affected my response</strong>", "", `<strong>If I had seen a word a lot of times, that would have made me like the word <u>MORE</u></strong>`];

var mere_exposure_intro_response1 = null;
var mere_exposure_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `During this exercise, you were initially shown a series of foreign words. Then, you were asked to rate how much you liked each of these words. When you were originally shown these words, you may have noticed that some of these words appeared many times, while others appeared only a few times or not at all.
            <p>Do you think the <b>number of times</b> you saw each word affected how highly you rated it? If so, how?`
        } else {
            return `During this exercise, you were initially shown a series of foreign words. Then, you were asked to rate how much you liked each of these words. When you were originally shown these words, you may have noticed that each word (e.g. "dudak") appeared exactly <i>one</i> time.
            <p>Now, imagine if some of the words you saw appeared more often than others. For example, imagine if the word "mimar" had appeared twenty-five times while the word "dudak" only appeared once.
            <p>Do you think the <b>number of times</b> you saw each word would have affected how highly you rated each word? If so, how?`
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_mee1 : introspection_q_labels_mee2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br>",
    on_finish: function (data) {
        mere_exposure_intro_response1 = data.response
    }
};

var mere_exposure_intro_response2 = null;
var mere_exposure_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        mere_exposure_intro_response2 = data.response.Q0
    }
};

var mere_exposure_intro_confidence_response = null;
var mere_exposure_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        mere_exposure_intro_confidence_response = data.response; 
        s1_data = {
            subject: data.subject,
            version: data.version,
            observer_or_actor: observer_or_actor,
            factor: data.condition,
            task_name: "mere exposure",
            condition: condition[0],
            stimulus: null,
            choice: null,
            auxiliary_info1: null,
            openq_response: mere_exposure_openQ_response,
            introspect_rating: mere_exposure_intro_response1,
            introspect_open: mere_exposure_intro_confidence_response,
            familiarity: familiarity,
            rt_main_question: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var mere_exposure_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity= data.response == 0 ? "Yes" : "No"

        
    }
}



if (only_main_question) {
    var mere_exposure = {
        timeline: [mere_exposure_instructions1, mere_exposure_exposure, mere_exposure_instructions2, mere_exposure_questions]
    };
} else {
    var mere_exposure = {
        timeline: [mere_exposure_instructions1, mere_exposure_exposure, mere_exposure_instructions2, mere_exposure_questions, mere_exposure_familiar, mere_exposure_openQ, mere_exposure_introspect1, mere_exposure_intro_confidence]
    };
}

//#endregion
//timeline.push(mee)
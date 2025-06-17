//#region 9. Illusion of truth
//Supplementary Material
//Ongoing Secondary Tasks Can Reduce the Illusory Truth Effect
//Deva P. Ly, Daniel M. Bernstein, Eryn J. Newman*

var confidence_q = condition[0] == 'Factor-Included' ? "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by whether you had seen the statement earlier in this study)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by whether you had seen the statement earlier in this study)?</p>";


//preparing stimuli
var true_old = 
    [`<p style = "font-size:30px">Volleyball was originally called mintonette</p>`,
    `<p style = "font-size:30px">The flamingo’s pink color comes from carotenoid pigments in its food</p>`,
    `<p style = "font-size:30px">Canada is the second largest country in the world in area</p>`,
    `<p style = "font-size:30px">The largest European glacier is Vatnajökull on Iceland</p>`,
    `<p style = "font-size:30px">In almost all human populations of newborns, there is a slight excess of males</p>`,
    `<p style = "font-size:30px">Domesticated goats are descended from the pasang</p>`,
    `<p style = "font-size:30px">Most limes have more acid than lemon</p>`,
    `<p style = "font-size:30px">Lake Baikal is the world's largest freshwater lake by volume</p>`,
    `<p style = "font-size:30px">Female turkeys generally weigh half as much as males</p>`
    ]
true_old = _.shuffle(true_old);

var false_old = [
    `<p style = "font-size:30px">The mouth of a sea urchin is on its top</p>`,
    `<p style = "font-size:30px">The grape plant is a large herb</p>`,
    `<p style = "font-size:30px">Dough is boiled in the process of making croissants</p>`,
    `<p style = "font-size:30px">Neptune is part of the Kuiper belt</p>`,
    `<p style = "font-size:30px">The longbow was invented after the crossbow</p>`,
    `<p style = "font-size:30px">Sheep are a type of tylopod mammal</p>`,
    `<p style = "font-size:30px">The Caspian Sea is the lowest body of water on the surface of the Earth</p>`,
    `<p style = "font-size:30px">The Nile river flows southward</p>`,
    `<p style = "font-size:30px">Bike riding is the first event in a triathlon</p>`
    ]  
false_old = _.shuffle(false_old);




var true_new =
    ['<p style = "font-size:30px">Moose may dive underwater while feeding</p>',
    '<p style = "font-size:30px">The Colchester is a popular type of oyster</p>',
    '<p style = "font-size:30px">Taboga Island is in Panama</p>',
    '<p style = "font-size:30px">Xylem is the water-transporting tissue in plants</p>',
    '<p style = "font-size:30px">Dart boards are commonly made of sisal</p>',
    '<p style = "font-size:30px">Snakes lack moveable eyelids</p>',
    '<p style = "font-size:30px">Cabbages are in the mustard family</p>',
    '<p style = "font-size:30px">The sun constitutes more than 99 percent of the entire mass of the solar system</p>',
    '<p style = "font-size:30px">The otter belongs to the squirrel family</p>'
    ]
true_new = _.shuffle(true_new);

var false_new =
    ['<p style = "font-size:30px">The corn was first domesticated by native peoples in Argentina</p>',
    '<p style = "font-size:30px">The monetary unit in Afghanistan is the rupee</p>',
    '<p style = "font-size:30px">Spain produces most of the world’s almonds</p>',
    '<p style = "font-size:30px">The sport involving a snowboard is believed to have originated in Europe</p>',
    '<p style = "font-size:30px">The highest waterfall in the world is in Argentina</p>',
    '<p style = "font-size:30px">Candlepins is the most widely played variation of bowling</p>',
    '<p style = "font-size:30px">In chemistry, a mass spectrometer is used to separate substances into its constituent parts according to color</p>',
    '<p style = "font-size:30px">The Carpathian Mountains form a high wall between France and Spain</p>',
    '<p style = "font-size:30px">Giraffes have terrible eyesight</p>'
     ]
false_new = _.shuffle(false_new);

// Function to transform the list
function transformList(htmlList, listType) {
    return htmlList.map(html => {
        // Extract the text content from the HTML string
        var text = html.replace(/<\/?[^>]+(>|$)/g, "");
        
        return { stimulus: html, name: text, type: listType };
    });
}

// Transform each list and concatenate the results
var transformedFalseNew = transformList(false_new, 'false_new');
var transformedFalseOld = transformList(false_old, 'false_old');
var transformedTrueNew = transformList(true_new, 'true_new');
var transformedTrueOld = transformList(true_old, 'true_old');

// Combine the transformed lists
var included_twelve_statements = _.shuffle(transformedFalseNew.slice(0,3).concat(transformedFalseOld.slice(0,3), transformedTrueNew.slice(0,3), transformedTrueOld.slice(0,3)));
var excluded_twelve_statements = _.shuffle(transformedFalseNew.slice(0,6).concat(transformedTrueNew.slice(0,6)));
console.log("included: ", included_twelve_statements);
console.log("excluded: ", excluded_twelve_statements);

// Shuffle the combined list to get the final twelve statements
var stimulus_form_twelve_statements = null;
if (condition[0] == "Factor-Included") {
    stimulus_form_twelve_statements = _.shuffle(included_twelve_statements)
} else {
    stimulus_form_twelve_statements = _.shuffle(excluded_twelve_statements)
}

//stimulus_form_twelve_statements = 

console.log(stimulus_form_twelve_statements);

function countInArray(array, what) {
    var count = 0;
    for (var i = 0; i < array.length; i++) {
        if (array[i] === what) {
            count++;
        }
    }
    return count;
}

var illusion_of_truth_trials = [];
for (var i = 0; i < included_twelve_statements.length; i++) {
    illusion_of_truth_trials.push({
        type: jsPsychHtmlKeyboardResponse,
        stimulus: stimulus_array[i],
        choices: "NO_KEYS",
        trial_duration: 3000
    },
        {
            type: jsPsychHtmlKeyboardResponse,
            stimulus: '',
            choices: "NO_KEYS",
            trial_duration: 100
        })
};

var illusion_of_truth_instructions1 = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this next task, you will see series of statements appear on the screen. Half of these statements are ones that you have already seen, and half are new. Some of these trivia statements are true and some of these trivia statements are false.
        <br><p>You will be asked to assess whether each claim is true or false. When you see each statement appear on the screen, please read it carefully and answer the following question:
<br> <p><strong> Is this statement true or false? </strong>
<br><p>You will be asked to answer this question on a scale from definitely false to definitely true.</p>
        <br><p>It is important that you respond as quickly as possible, but not so quickly that you start making errors.
        <br><p>Please do not search the answers online while you are completing the study; if you are unsure of an answer, please just make your best guess.
        <p><i>Press the next button to begin.</i></p>`,
    ],
    show_clickable_nav: true
};


var illusion_of_truth_questions = {
    timeline: [
        {
            type: jsPsychHtmlSliderResponse,
            stimulus: jsPsych.timelineVariable('stimulus'),
            stimulus_height: 350,
            labels: [`<strong>1<br>Definitely False</strong>`, "2", "3", "4", "5", "6", "7", "8", `<strong>9<br>Definitely True</strong>`],
            prompt: `Is this statement true or false?<br><br>`,
            slider_width: 750,
            min: 10,
            max: 90,
            slider_start: 50,
            require_movement: require_movement_general,
            data: { stim: jsPsych.timelineVariable('name'), aux: jsPsych.timelineVariable('type') },
            on_finish: function (data) {
                function isFalsePositive(response, trivia_type) {
                    if (response < 50 && (trivia_type === "false_new" || trivia_type === "false_old")) {
                        return 'false positive';
                    }
                    return 'not false positive';
                }
                

                var s1_data = {
                    subject: data.subject,
                    version: data.version,
                    factor: data.condition,
                    task_name: "illusion of truth pt2",
                    choice: data.response,
                    stimulus: data.stim,
                    auxiliary_info1: isFalsePositive(data.response, data.aux),
                    condition: data.aux,
                }
                save_data(s1_data, 'introspection');
            }
        }
    ],
    timeline_variables: stimulus_form_twelve_statements,
    randomize_order: true
};

var illusion_of_truth_openQ_response = null;
var illusion_of_truth_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were shown a series of trivia statements and asked to assess whether each claim was true or false.</p>
        <p>Describe your thought process during this exercise. How did you come to your eventual judgement whether each claim was true or false?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        illusion_of_truth_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_illusion_of_truth1 = [`<strong>When I had seen a trivia statement earlier in this study, that made me judge the statement as <u>LESS</u> likely to be true </strong>`, "", "<strong>Whether I had seen a statement earlier in this study did not affect my response</strong>", "", `<strong>When I had seen a trivia statement earlier in this study, that made me judge the statement as <u>MORE</u> likely to be true</strong>`];
var introspection_q_labels_illusion_of_truth2 = [`<strong>If I had seen a trivia statement earlier in this study, that would have made me judge the statement as <u>LESS</u> likely to be true </strong>`, "", "<strong>Whether I had seen a statement earlier in this study would not have affected my response</strong>", "", `<strong>If I had seen a trivia statement earlier in this study, that would have made me judge the statement as <u>MORE</u> likely to be true</strong>`];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var illusion_of_truth_intro_response1 = null;
var illusion_of_truth_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `During this exercise, you were shown a series of trivia statements and you were asked to assess whether each claim was true or false. 
                    <p>Some of these claims had been shown to you earlier in the experiment and some you just saw for the first time.
                    <p>Do you think <b>seeing the statement earlier in this study</b> affected  your assessment of whether it was true or false? If so, how?`
        } else {
            return `During this exercise, you were shown a series of trivia statements and you were asked to assess whether each claim was true or false. 
                    <p>Earlier in this experiment, you were shown another series of trivia statements. Now, imagine if some of the trivia statements you were just asked to assess had also been shown to you earlier in the experiment. 
                    Do you think <b>seeing the statement earlier in this study</b> would have  affected your assessment of whether it was true or false? If so, how?`
                        }
    },
labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_illusion_of_truth1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_illusion_of_truth1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_illusion_of_truth2;
        } else {
            return introspection_q_labels_illusion_of_truth2.slice().reverse();
        }
    },    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br>",
    on_finish: function (data) {

        if (label_order_randomized == 'original') {
            illusion_of_truth_intro_response1 = data.response
    }
        else {
            illusion_of_truth_intro_response1 = 100 - data.response;
            }
        }
};

var illusion_of_truth_intro_response2 = null;
var illusion_of_truth_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        illusion_of_truth_intro_response2 = data.response.Q0
    }
};

var illusion_of_truth_intro_confidence_response = null;
var illusion_of_truth_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        illusion_of_truth_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "illusion of truth pt2",
            condition: condition[0],
            stimulus: null,
            choice: null,
            auxiliary_info1: null,
            openq_response: illusion_of_truth_openQ_response,
            introspect_rating: illusion_of_truth_intro_response1,
            introspect_open: illusion_of_truth_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var illusion_of_truth_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity = data.response == 0 ? "Yes" : "No"


    }
}


if (only_main_question) {
    var illusion_of_truth_pt2 = {
        timeline: [illusion_of_truth_instructions1, illusion_of_truth_questions]
    };
} else {
    var illusion_of_truth_pt2 = {
        timeline: [illusion_of_truth_instructions1, illusion_of_truth_questions, illusion_of_truth_familiar, illusion_of_truth_openQ, illusion_of_truth_introspect1, illusion_of_truth_intro_confidence]
    };
}

//#endregion
//timeline.push(mee)
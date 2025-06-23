//#region 9. Mere Exposure (Stang (1974)


var confidence_q = condition[0] == 'Factor-Included' ? "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by the order of the facts)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the order of the facts)?</p>";
console.log("condition[0]: ", condition[0])
console.log("condition :", condition)

var car_names = ["Hatsdun", "Nabusi", "Kaiwa", "Dasuka"]
var car_names_shuffled = _.shuffle(car_names)
console.log("car_names_shuffled: ", car_names_shuffled)
var car_names_shuffled_string = car_names_shuffled.join(",")
console.log("car_names_shuffled_string: ", car_names_shuffled_string)

var car1 = car_names_shuffled[0]
var car2 = car_names_shuffled[1]
var car3 = car_names_shuffled[2]
var car4 = car_names_shuffled[3]

console.log(car1, car2, car3, car4)

//preparing stimuli
var car1_positive_attributes = [
    `<p style = "font-size:30px">The ${car1} has good mileage</p>`,
    `<p style = "font-size:30px">The ${car1} has good handling</p>`,
    `<p style = "font-size:30px">The ${car1} is good for the environment</p>`,
    `<p style = "font-size:30px">The ${car1} has a good sound system</p>`,
    `<p style = "font-size:30px">With the ${car1} it is easy to shift gears</p>`,
    `<p style = "font-size:30px">The ${car1} has a large trunk</p>`,
    `<p style = "font-size:30px">For the ${car1}, service is good</p>`
]

var shuffled_car1_positive_attributes = _.shuffle(car1_positive_attributes)

var car1_negative_attributes = [
    `<p style = "font-size:30px">The ${car1} has little legroom</p>`,
    `<p style = "font-size:30px">The ${car1} is very old</p>`,
    `<p style = "font-size:30px">The ${car1} is not available in many different colors</p>`,
    `<p style = "font-size:30px">The ${car1} does not have a sunroof</p>`,
    `<p style = "font-size:30px">The ${car1} does not have cupholders</p>`
]

var shuffled_car1_negative_attributes = _.shuffle(car1_negative_attributes)


var car2_positive_attributes = [
    `<p style = "font-size:30px">The ${car2} has good mileage</p>`,
    `<p style = "font-size:30px">The ${car2} has good handling</p>`,
    `<p style = "font-size:30px">The ${car2} is relatively good for the environment</p>`,
    `<p style = "font-size:30px">The ${car2} has a good sound system</p>`,
    `<p style = "font-size:30px">With the ${car2} it is easy to shift gears</p>`,
    `<p style = "font-size:30px">The ${car2} has a large trunk</p>`,
    `<p style = "font-size:30px">For the ${car2}, service is good</p>`
]

var shuffled_car2_positive_attributes = _.shuffle(car2_positive_attributes)

var car2_negative_attributes = [
    `<p style = "font-size:30px">The ${car2} has little legroom</p>`,
    `<p style = "font-size:30px">The ${car2} is very old</p>`,
    `<p style = "font-size:30px">The ${car2} is not available in many different colors</p>`,
    `<p style = "font-size:30px">The ${car2} does not have a sunroof</p>`,
    `<p style = "font-size:30px">The ${car2} does not have cupholders</p>`
]

var shuffled_car2_negative_attributes = _.shuffle(car2_negative_attributes)

var car3_attributes = [
    `<p style = "font-size:30px">The ${car3} has plenty of legroom</p>`,
    `<p style = "font-size:30px">The ${car3} is new</p>`,
    `<p style = "font-size:30px">For the ${car3}, service is good</p>`,
    `<p style = "font-size:30px">With the ${car3} it is easy to change gears</p>`,
    `<p style = "font-size:30px">The ${car3} has a small trunk</p>`,
    `<p style = "font-size:30px">The ${car3} has no sunroof</p>`,
    `<p style = "font-size:30px">The ${car3} has no cup holders</p>`,
    `<p style = "font-size:30px">The ${car3} has poor mileage</p>`,
    `<p style = "font-size:30px">The ${car3} has a poor sound system</p>`,
    `<p style = "font-size:30px">The ${car3} has poor handling</p>`,
    `<p style = "font-size:30px">The ${car3} is not very good for the environment</p>`,
    `<p style = "font-size:30px">The ${car3} is available in very few different colors</p>`
]

var shuffled_car3_attributes = _.shuffle(car3_attributes);

var shuffled_car1_positive_car2_negative = _.shuffle(shuffled_car1_positive_attributes.concat(shuffled_car2_negative_attributes).concat(shuffled_car3_attributes.slice(0, 6)))
var shuffled_car1_negative_car2_positive = _.shuffle(shuffled_car1_negative_attributes.concat(shuffled_car2_positive_attributes).concat(shuffled_car3_attributes.slice(6, 13)))
var good_1_first = [];
var thirty_eight_random = _.shuffle(car1_positive_attributes.concat(car1_negative_attributes).concat(car2_positive_attributes).concat(car2_negative_attributes).concat(car3_attributes));
var random = thirty_eight_random.slice(0, 22);

for (var i = 0; i < 11; i++) {
    good_1_first.push(shuffled_car1_positive_car2_negative[i]);
}

for (var i = 0; i < 11; i++) {
    good_1_first.push(shuffled_car1_negative_car2_positive[i]);
}

console.log(good_1_first)

stimulus_array = condition[0] == "Factor-Included" ? good_1_first : random

var primacy_order_trials = [];
for (var i = 0; i < stimulus_array.length; i++) {
    primacy_order_trials.push({
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

var primacy_order_instructions1 = {
    type: jsPsychInstructions,
    pages: [
        `<p> In this task, the Prolific user saw a series of facts about three different cars. Each fact was shown on the screen for 3 seconds. There were 36 facts total.</p>
         <p>Afterward, they chose the car they thought is best.</p>
        <p><i>Please click the "Next" button when you are ready to see the facts about the cars.</i></p>`,
    ],
    show_clickable_nav: true
};

var primacy_order_exposure = {
    timeline: primacy_order_trials
}



var choice = null;
var primacy_order_question = {
    type: jsPsychSurveyMultiChoice,
    questions: [
        {
            prompt: "The Prolific user was asked which car they thought is best. They selected " + observedChoice + ". Below, to demonstrate that you understand the Prolific user's choice, please move the slider to the option that they selected (regardless of your own beliefs).",
            name: 'choice',
            options: _.shuffle([car1, car2, car3]),
            required: true,
            horizontal: false
        },
    ],
    on_finish: function (data) {

        if (data.response.choice == car1) {
            choice = "car1";
        } else if (data.response.choice == car2) {
            choice = "car2";
        } else {
            choice = "car3";
        }

        if (only_main_question) {
            s1_data = {
                subject: data.subject,
                version: data.version,
                factor: data.condition,
                task_name: "primacy order",
                condition: condition[0],
                stimulus: null,
                choice: choice,
                auxiliary_info1: car_names_shuffled_string,
                openq_response: null,
                introspect_rating: null,
                introspect_open: null,
                familiarity: null,
                rt: data.rt
            }
            console.log("data to save: " + JSON.stringify(s1_data));
            save_data(s1_data, 'introspection')
        }
    }
}

var primacy_order_openQ_response = null;
var primacy_order_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, the Prolific user was shown a series of facts about different cars and asked to choose the car they thought was best.</p>
 <p>Describe what you think was their thought process during this exercise. How do you think they came to their eventual choice about which car was best?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        primacy_order_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_primacy_order1 = [`<strong>This ordering made them like the ${car1} <u>LESS</u> than the ${car2}</strong>`, "", "<strong>The ordering of the facts did not affect their response</strong>", "", `<strong>This ordering made them like the ${car1} <u>MORE</u> than the ${car2}</strong>`];
var introspection_q_labels_primacy_order2 = [`<strong>This ordering would have made me like the ${car1} <u>LESS</u> than the ${car2}</strong>`, "", "<strong>The ordering of the facts would not have affected my response</strong>", "", `<strong>This ordering would have made me like the ${car1} <u>MORE</u> than the ${car2}</strong>`];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var primacy_order_intro_response1 = null;
var primacy_order_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `During this exercise, the Prolific user was shown a series of facts about each car and asked to choose the car they liked best.<br><br>
             It turns out these facts were presented in a specific ordering. <b>We showed them all the positive info about the ${car1} at the beginning, and all the negative info about it at the end.</b> The order for the ${car2} was flipped: <b>We showed them all the negative info about the ${car2} at the beginning, and all the positive info about it at the end.</b><br><br>
             In other words, for the ${car1} it was "positive first, negative last", and for the ${car2} it was "negative first, positive last".
             <p>Do you think this ordering affected the Prolific user's choice about which car they liked best? If so, how?`
        } else {
            return `During this exercise, you were shown a series of facts about each car and asked to choose the car you liked best.<br><br>
             Now imagine that the facts about each car had been presented in a specific ordering. <b>Imagine that we had showed you all the positive info about the ${car1} at the beginning, and all the negative info about it at the end.</b> In contrast, imagine that the order for the ${car2} had been flipped: <b>Imagine we'd shown you all the negative info about the ${car2} at the beginning, and all the positive info about it at the end.</b><br><br>
             In other words, imagine that for the ${car1} it had been "positive first, negative last", and for the ${car2} it had been "negative first, positive last".
             <p>Do you think this ordering would have affected your choice about which car you liked best? If so, how?`
        }
    },
    labels: function () {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_primacy_order1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_primacy_order1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_primacy_order2;
        } else {
            return introspection_q_labels_primacy_order2.slice().reverse();
        }
    },
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br>",
    on_finish: function (data) {

        if (label_order_randomized == 'original') {
            primacy_order_intro_response1 = data.response
            
        }
        else {
            primacy_order_intro_response1 = 100 - data.response;
        }
    }
};

var primacy_order_intro_response2 = null;
var primacy_order_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        primacy_order_intro_response2 = data.response.Q0
    }
};

var primacy_order_intro_confidence_response = null;
var primacy_order_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {

        primacy_order_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "primacy order",
            condition: condition[0],
            stimulus: null,
            choice: choice,
            auxiliary_info1: car_names_shuffled_string,
            openq_response: primacy_order_openQ_response,
            introspect_rating: primacy_order_intro_response1,
            introspect_open: primacy_order_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var primacy_order_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity = data.response == 0 ? "Yes" : "No"


    }
}



if (only_main_question) {
    var primacy_order = {
        timeline: [primacy_order_instructions1, primacy_order_exposure, primacy_order_question]
    };
} else {
    var primacy_order = {
        timeline: [primacy_order_instructions1, primacy_order_exposure, primacy_order_question, primacy_order_familiar, primacy_order_openQ, primacy_order_introspect1, primacy_order_intro_confidence]
    };
}


//#endregion
//timeline.push(mee)
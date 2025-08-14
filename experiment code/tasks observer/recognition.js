//#region recognition
// Pachur, Mata, Schooler 2009


var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by how much they recognized the name of the city)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by how much you recognized the name of the city)?</p>";

var preload = {
    type: jsPsychPreload,
    auto_preload: true
}

var recognition_instructions = {
    type: jsPsychInstructions,
    pages: function() {
        if (trivia_question_already) {
            return [
                `<p>In this exercise, the Prolific user was asked to answer another series of trivia questions.</p>
            <br><p>They were asked not to search the answers online while they were completing the study; if they were unsure of an answer, they were asked to just make their best guess.
            <p><i>Click the Next button below when you are ready to see the first question.</i></p>`
            ];
        } else {
            trivia_question_already = true;
            return [
                `<p>In this exercise, the Prolific user was asked to answer a series of trivia questions.</p>
            <br><p>They were told not to search the answers online while they were completing the study; if they were unsure of an answer, they were told to just make their best guess.
            <p><i>Click the Next button below when you are ready to see the first question.</i></p>`
            ];
        }
    },
    show_clickable_nav: true
    };




var choice = null;


var eight_recognize_cities = jsPsych.randomization.shuffle([
    'Los Angeles', 'Denver', 'Houston', 'Philadelphia',
    'San Diego', 'Nashville', 'Detroit', 'Minneapolis']);

var eight_non_recognize_cities = jsPsych.randomization.shuffle([
    'Spokane', 'Lexington–Fayette', 'Raleigh', 'Mesa',
    'Fort Worth', 'Fresno', 'Garland', 'Grand Rapids']);

var eight_recognize_diseases = jsPsych.randomization.shuffle([
    'Diphtheria', 'Cholera', 'Typhoid and paratyphoid','Leprosy',
    'Tetanus', 'Malaria', 'Syphilis','Gonorrhea'
]);
var eight_non_recognize_diseases = jsPsych.randomization.shuffle([
    'Trachoma', 'Tularemia', 'Hemorrhagic fever', 'Q fever', 
    'Shigellosis', 'Ornithosis', 'Leptospirosis', 'Brucellosis'
]);

console.log('eight_recognize_cities', eight_recognize_cities);
console.log('eight_non_recognize_cities', eight_non_recognize_cities);
console.log('eight_recognize_diseases', eight_recognize_diseases);
console.log('eight_non_recognize_diseases', eight_non_recognize_diseases);

var city_list = [];
var disease_list = [];

function formatListWithNumbers(list) {
    return list.map((item, index) => `${index + 1}. ${item}`).join(' ');
}

if (condition[0] == 'Factor-Included'){
    for (var i = 0; i < eight_recognize_cities.length; i++) {
        city_list.push([eight_recognize_cities[i], eight_non_recognize_cities[i]]);
    }
    city_list = city_list.slice(0,4);
    console.log(city_list);

    for (var i = 0; i < eight_recognize_cities.length; i++) {
        disease_list.push([eight_recognize_diseases[i], eight_non_recognize_diseases[i]]);
    }
    disease_list = disease_list.slice(0,4);
    console.log(disease_list);
} else {
    for (var i = 0; i < eight_non_recognize_cities.length; i += 2) {
        city_list.push([eight_non_recognize_cities[i], eight_non_recognize_cities[i + 1]]);
    }
    console.log(city_list);
    
    for (var i = 0; i < eight_non_recognize_cities.length; i += 2) {
        disease_list.push([eight_non_recognize_diseases[i], eight_non_recognize_diseases[i + 1]]);
    }
    console.log(disease_list);
}

city_index = 0;
disease_index = 0;


choice = null;
var stimulus = null;

function getCategory(name) {
    if (eight_recognize_cities.includes(name)) {
        return 'recognizable city';
    } else if (eight_non_recognize_cities.includes(name)) {
        return 'non-recognizable city';
    } else if (eight_recognize_diseases.includes(name)) {
        return 'recognizable disease';
    } else if (eight_non_recognize_diseases.includes(name)) {
        return 'non-recognizable disease';
    } else {
        return 'unknown';
    }
}

// Function to check if the chosen city is recognizable
function checkRecognizableCity(city) {
    if (eight_recognize_cities.includes(city)) {
        return 'chose recognizable';
    } else {
        return 'did not choose recognizable';
    }
}

// Function to check if the chosen city is recognizable
function checkRecognizableDisease(disease) {
    if (eight_recognize_diseases.includes(disease)) {
        return 'chose recognizable';
    } else {
        return 'did not choose recognizable';
    }
}
let recognition_stimulus_array = [];
for (const entry of recognition_db) {
  if (entry.subject === actorNumber && entry.choice != "0") {
    recognition_stimulus_array.push({
      stimulus: entry.stimulus, 
      choice: entry.choice,
    rt_main_question: entry.rt
    });
  }
}
recognition_stimulus_array_length = recognition_stimulus_array.length;



var recognition_list_index = 0;
var cities_with_commas = null;
var list_of_cities = null;
var townOrTown = null; 

var city_trial = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function(){
        let observedChoice = recognition_stimulus_array[recognition_list_index]["choice"];
        let this_city_list_raw = recognition_stimulus_array[recognition_list_index]["stimulus"];
        let this_city_list = this_city_list_raw.includes("Lexingtonâ€“Fayette") ?
                             this_city_list_raw.replace("Lexingtonâ€“Fayette", "Lexington-Fayette") :
                             this_city_list_raw;
        let firstTown = this_city_list.split(',')[0];
        let secondTown = this_city_list.split(',')[1];
        let townOrTown = firstTown + " or " + secondTown;
        return "The Prolific user was told to guess which city has the larger population: " + townOrTown + ".<br><br>The Prolific user selected " + observedChoice + ".<br><br>To demonstrate that you understand the Prolific user\'s choice, <b>please select the option that they selected (regardless of your own beliefs).</b>";
    },
    choices: function(){
        let cities_with_commas = recognition_stimulus_array[recognition_list_index]["stimulus"];
        let list_of_cities = cities_with_commas.split(',');
        list_of_cities = list_of_cities.map(city => {
            if (city.includes("Lexingtonâ€“Fayette")) {
                return city.replace("Lexingtonâ€“Fayette", "Lexington-Fayette");
            }
            return city;
        });
        return list_of_cities;
    },
    correct_response: function(){
        let current_stimulus_data = recognition_stimulus_array[recognition_list_index];
        let observedChoice = current_stimulus_data["choice"];
        let cities_with_commas = current_stimulus_data["stimulus"];
        let list_of_cities = cities_with_commas.split(',');
        list_of_cities = list_of_cities.map(city => {
            if (city.includes("Lexingtonâ€“Fayette")) {
                return city.replace("Lexingtonâ€“Fayette", "Lexington-Fayette");
            }
            return city;
        });
        if (list_of_cities[0] === observedChoice){
            return 0;
        } else {
            return 1;
        }
    },
    enable_button_after: function(){
        observedTime= recognition_stimulus_array[recognition_list_index]["rt"];
        return observedTime;
    },
    on_finish: function (data) {
        let current_stimulus_data = recognition_stimulus_array[recognition_list_index];
        let raw_cities_stimulus = current_stimulus_data["stimulus"];
        let list_of_cities_for_finish = raw_cities_stimulus.split(',');
        list_of_cities_for_finish = list_of_cities_for_finish.map(city => {
            if (city.includes("Lexingtonâ€“Fayette")) {
                return city.replace("Lexingtonâ€“Fayette", "Lexington-Fayette");
            }
            return city;
        });
        let participant_choice_text = list_of_cities_for_finish[data.response];
        let stimulus_category = [getCategory(list_of_cities_for_finish[0]), getCategory(list_of_cities_for_finish[1])];
        let recognizable = checkRecognizableCity(participant_choice_text);
        let s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "recognition: city",
            condition: stimulus_category.toString(),
            stimulus: raw_cities_stimulus,
            choice: participant_choice_text,
            auxiliary_info1: recognizable,
            openq_response: null,
            introspect_rating: null,
            introspect_open: null,
            familiarity: null,
            rt_main_question: data.rt
        };
        save_data(s1_data, 'introspection');
    }
};

  var loop_city = {
    timeline: [city_trial],
    loop_function: function(data){
        if (recognition_list_index != recognition_stimulus_array_length - 1) {
        if (recognition_list_index != recognition_stimulus_array_length - 1) {
            recognition_list_index = recognition_list_index + 1;
            return true; //loop
        } else {
            return false; // don't loop
        }
    }
}
  };



var recognition_openQ_response = null;
var recognition_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, the Prolific user was presented with a series of trivia questions. In each one, they were either asked to pick the city with the larger population. </p><p>Describe what you think the thought process was behind their decision about which city to select. How did they come to their eventual decision?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        recognition_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_recognition1 = ['<strong>It made them think the city had a <u>SMALLER </u> population. </strong>', "", '<strong>It did not affect their response</strong>', "", '<strong>It made them think the city had a <u>LARGER </u> population.'];
var introspection_q_labels_recognition2 = ['<strong>It would have made me think the city had a <u>SMALLER </u> population.', "", '<strong>It would not have affected my response</strong>', "", '<strong>It would have made me think the city had a <u>LARGER </u> population.'];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var recognition_intro_response1 = null;
var recognition_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this exercise, the Prolific user was presented with a series of trivia questions.
            <p>In each one, they were either asked to pick the city with the larger population. </p>
            <p>Some of the cities (like New York) they likely recognized the names of. While others (like Lexington–Fayette) they may have recognized less well. How do you think <b>how recognizable the city was</b> affected their impression of its population?</p>`;
        } else {
            return `<p>In this exercise, you were presented with a series of trivia questions.
            <p>In each one, you were either asked to pick the city with the larger population. </p>
            <p>Imagine if some of the  cities (like New York) were ones you likely recognized the names of. While others (likeLexington–Fayette) were ones you likely recognized less well. How do you think <b>how much you'd heard of the city before</b> would have affected your impression of its population?</p>`;
        }
    },
labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_recognition1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_recognition1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_recognition2;
        } else {
            return introspection_q_labels_recognition2.slice().reverse();
        }
    },
        slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {

        if (label_order_randomized == 'original') {
            recognition_intro_response1 = data.response
    }
        else {
            recognition_intro_response1 = 100 - data.response;
            }
        }
};

var recognition_intro_response2 = null;
var recognition_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        recognition_intro_response2 = data.response.Q0;
    }
};

var recognition_intro_confidence_response = null;
var recognition_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: false,
    on_finish: function (data) {
        recognition_intro_confidence_response = data.response;
        var s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "recognition",
            condition: condition[0],
            stimulus: null,
            choice: choice,
            flipped_scale: label_order_randomized,
            auxiliary_info1:  null,
            openq_response: recognition_openQ_response,
            introspect_rating: recognition_intro_response1,
            introspect_open: recognition_intro_confidence_response,
            familiarity: familiarity,
            rt_main_question: data.rt
        };
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var recognition_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity= data.response == 0 ? "Yes" : "No";

        
    }
};



if (only_main_question) {
    var recognition = {
        timeline: [preload, recognition_instructions, loop_city]
    };
} else {
    var recognition = {
        timeline: [preload, recognition_instructions, loop_city, recognition_familiar, recognition_openQ, recognition_introspect1, recognition_intro_confidence]
    };
}


// end region reference price 
//#region recognition
// Pachur, Mata, Schooler 2009


var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by how much you recognized the name of the city)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by how much you recognized the name of the city)?</p>";

var preload = {
    type: jsPsychPreload,
    auto_preload: true
}

var recognition_instructions = {
    type: jsPsychInstructions,
    pages: function() {
        if (trivia_question_already) {
            return [
                `<p>In this exercise, you will be asked to answer another series of trivia questions.</p>
            <br><p>Please do not search the answers online while you are completing the study; if you are unsure of an answer, please just make your best guess.
            <p><i>Click the Next button below when you are ready to see the first question.</i></p>`
            ];
        } else {
            trivia_question_already = true;
            return [
                `<p>In this exercise, you will be asked to answer a series of trivia questions.</p>
            <br><p>Please do not search the answers online while you are completing the study; if you are unsure of an answer, please just make your best guess.
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


var recognition_list_index = 0;
var city_trial = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function(){
        console.log("recognition_list_index:", recognition_list_index);
        console.log("city_list: " + city_list);
        console.log("stimulus:", city_list[recognition_list_index]);
        return("Please guess which city has the larger population.");

    },
    choices: function(){
        console.log("recognition_list_index:", recognition_list_index);
        console.log("stimulus:", city_list[recognition_list_index]);
        return([city_list[recognition_list_index][0], city_list[recognition_list_index][1]]);
    },
    on_finish: function (data) {
        //console.log("data.response: " + data.response);
        var response = city_list[recognition_list_index][data.response]
        //console.log("response: " + response);
        var stimulus_category = [getCategory(city_list[recognition_list_index][0]),getCategory(city_list[recognition_list_index][1])];
        //console.log("city_list[recognition_list_index]: " + city_list[recognition_list_index]);
        //console.log("typeof city_list[recognition_list_index]: " + typeof city_list[recognition_list_index]);
        //console.log("recognition_list_index[0]: " + recognition_list_index[0]);
        //console.log("stimulus_category: " + stimulus_category);
        var recognizable = checkRecognizableCity(response)
        console.log("recognizable: " + recognizable);

        var s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "recognition: city",
            condition: stimulus_category.toString(),
            stimulus: city_list[recognition_list_index].toString(),
            choice: response,
            auxiliary_info1: recognizable,
            openq_response: null,
            introspect_rating: null,
            introspect_open: null,
            familiarity: null,
            rt: data.rt
        };
        save_data(s1_data, 'introspection');
    }
  };

  var loop_city = {
    timeline: [city_trial],
    loop_function: function(data){
        console.log("recognition_list_index", recognition_list_index,"city_list.length - 1", city_list.length - 1)
        if (recognition_list_index != city_list.length - 1) {
            recognition_list_index = recognition_list_index + 1;
            console.log(recognition_list_index, city_list[recognition_list_index]);
            return true; //loop
        } else {
            return false; // don't loop
        }
    }
}

var disease_recognition_list_index = 0;
var disease_trial = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function(){
        //console.log("disease_recognition_list_index:", disease_recognition_list_index);
        //console.log("stimulus:", disease_list[disease_recognition_list_index]);
        return("Please guess which disease has the highest annual incidence rate in a typical year in the US.");

    },
    choices: function(){
        //console.log("disease_recognition_list_index:", disease_recognition_list_index);
        //console.log("stimulus:", disease_list[disease_recognition_list_index]);
        return([disease_list[disease_recognition_list_index][0], disease_list[disease_recognition_list_index][1]]);
    },
    on_finish: function (data) {
        console.log("data.response: " + data.response);
        var response = disease_list[disease_recognition_list_index][data.response]
        console.log("response: " + response);
        var stimulus_category = [getCategory(disease_list[disease_recognition_list_index[0]]),getCategory(disease_list[disease_recognition_list_index[1]])];
        var recognizable = checkRecognizableDisease(response)
        console.log("recognizable: " + recognizable);

        var s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "recognition: disease",
            condition: stimulus_category,
            stimulus: stimulus,
            choice: data.response,
            auxiliary_info1: recognizable,
            openq_response: null,
            introspect_rating: null,
            introspect_open: null,
            familiarity: null,
            rt: data.rt
        };
        save_data(s1_data, 'introspection');
    }
  };

  var loop_disease = {
    timeline: [disease_trial],
    loop_function: function(data){
        console.log("disease_recognition_list_index", disease_recognition_list_index)
        if (disease_recognition_list_index != disease_list.length - 1) {
            disease_recognition_list_index = disease_recognition_list_index + 1;
            console.log(disease_recognition_list_index, disease_list[disease_recognition_list_index]);
            return true; //loop
        } else {
            return false; // don't loop
        }
    }
}

var recognition_openQ_response = null;
var recognition_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were presented with a series of trivia questions. In each one, you were either asked to pick the city with the larger population. </p><p>Describe your thought process behind your decision about which city to select. How did you come to your eventual decision?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        recognition_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_recognition1 = ['<strong>It made me think the city had a <u>SMALLER </u> population. </strong>', "", '<strong>It did not affect my response</strong>', "", '<strong>It made me think the city had a <u>LARGER </u> population.'];
var introspection_q_labels_recognition2 = ['<strong>It would have made me think the city had a <u>SMALLER </u> population.', "", '<strong>It would not have affected my response</strong>', "", '<strong>It would have made me think the city had a <u>LARGER </u> population.'];
var label_order_randomized = Math.random() < 0.5 ? 'original' : 'flipped';

var recognition_intro_response1 = null;
var recognition_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this exercise, you were presented with a series of trivia questions.
            <p>In each one, you were either asked to pick the city with the larger population. </p>
            <p>Some of the cities (like New York) you likely recognized the names of. While others (like Lexington–Fayette) you may have recognized less well. How do you think <b>how much you'd heard of the city before</b> affected your impression of its population?</p>`;
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
    require_movement: require_movement_general,
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
            rt: data.rt
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
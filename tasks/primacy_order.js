 //#region 9. Mere Exposure (Stang (1974)


 var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the order of the facts)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the order of the facts)?</p>";
 
var car_names = ["Hatsdun", "Nabusi", "Kaiwa", "Dasuka"]
var car_names_shuffled = _.shuffle(car_names)
var car1 = car_names_shuffled[0]
var car2 = car_names_shuffled[1]
var car3 = car_names_shuffled[2]
var car4 = car_names_shuffled[3]

 //preparing stimuli
 var car1_positive_attributes = [
     `<p style = "font-size:30px">The ${car1} has good mileage</p>`,
     `<p style = "font-size:30px">The ${car1} has good handling</p>`,
     `<p style = "font-size:30px">The ${car1} is relatively good for the environment</p>`,
     `<p style = "font-size:30px">The ${car1} has a good sound system</p>`,
     `<p style = "font-size:30px">With the ${car1} it is easy to shift gears</p>`,
     `<p style = "font-size:30px">The ${car1} has a large trunk</p>`
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
    `<p style = "font-size:30px">The ${car2} has a large trunk</p>`
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

var shuffled_car1_positive_car_two_negative = _.shuffle(shuffled_car1_positive_attributes.concat(shuffled_car2_negative_attributes))
var shuffled_car1_negative_car_two_positive = _.shuffle(shuffled_car1_negative_attributes.concat(shuffled_car2_positive_attributes))
var bad_1_first = [];
var random = _.shuffle(car1_positive_attributes.concat(car1_positive_attributes).concat(car2_positive_attributes).concat(car2_negative_attributes));

for (var i = 0; i < 11; i++) {
    bad_1_first.push(shuffled_car1_positive_car_two_negative[i]);
}

for (var i = 0; i < 11; i++) {
    bad_1_first.push(shuffled_car1_negative_car_two_positive[i]);
}

console.log(bad_1_first)

stimulus_array = condition[0] == "Factor-Included" ? bad_1_first : random

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
         `<p> In this task, you will see 22 facts about two cars with each fact shown for 3 seconds each. Then you will choose the car you think is best.</p>
 <p><i>Please click the "Next" button when you are ready to see the descriptions of the cars.</i></p>`,
     ],
     show_clickable_nav: true
 };
 
 var primacy_order_exposure = {
     timeline: primacy_order_trials
 }
 
 

 var choice = null;
 var primacy_order_question = {
     type: jsPsychHtmlButtonResponse,
     stimulus: "Choose the car you think is best",
     choices: [car1, car2],
     on_finish: function (data) {
        choice = data.response == 0 ? "car1" : "car2"
 
         
     }
 }
 
 var primacy_order_openQ_response = null;
 var primacy_order_openQ = {
     type: jsPsychSurveyText,
     questions: [{
         prompt: `<p>In this exercise, you were shown a series of facts about each car and then asked to choose the car you thought was best.</p>
 <p>Describe your thought process during this exercise. How did you come to your eventual choice about which car was best?</p>`,
         required: required_general, rows: 5, columns: 80
     }],
     on_finish: function (data) {
         primacy_order_openQ_response = data.response.Q0;
     }
 };
 
 var introspection_q_labels_mee1 = [`<strong>The fact that all the positive facts about the ${car1} and the negative facts about the ${car2} showed up towards the beginning of the list, made me think the ${car1} was <u>THE BETTER</u> car</strong>`, "", "<strong>The order of the facts did not affect my response</strong>", "", `<strong>The fact that all the positive facts about the ${car1} and the negative facts about the ${car2} showed up towards the beginning of the list, made me think the ${car1} was <u>THE WORSE</u>car</strong>`];
 var introspection_q_labels_mee2 = [`<strong>If all the positive facts about the ${car1} and the negative facts about the ${car2} showed up towards the beginning of the list, that would have made me think the ${car1} was <u>THE BETTER</u> car</strong>`, "", "<strong>The order of the facts would not have affected my response</strong>", "", `<strong>If all the positive facts about the ${car1} and the negative facts about the ${car2} showed up towards the beginning of the list, that would have made me think the ${car1} was <u>THE WORSE</u> car</strong>`];
 
 var primacy_order_intro_response1 = null;
 var primacy_order_introspect1 = {
     type: jsPsychHtmlSliderResponse,
     stimulus: function () {
         if (condition[0] == "Factor-Included") {
             return `During this exercise, you were initially shown a series of facts about each car. Then, you were asked to choose the car you liked best. In the facts about each car, all the positive facts about the ${car1} and the negative facts about the ${car2} showed up towards the beginning of the list and all the positive facts about the ${car2} and the negative facts about the ${car1} showed up towards the end of the list.
             <p>Do you think the <b>fact that all the positive facts about the ${car1} and the negative facts about the ${car2} showed up towards the beginning of the list</b> about each car affected your choice about which car you liked best? If so, how?`
         } else {
             return `During this exercise, you were initially shown a series of facts about each car. Then, you were asked to choose the car you liked best. Now imagine that the facts about each car were presented in a specific order: Imagine that all the positive facts about the ${car1} and the negative facts about the ${car2} showed up towards the beginning of the list and all the positive facts about the ${car2} and the negative facts about the ${car1} showed up towards the end of the list.
             <p>Do you think the <b>fact that all the positive facts about the ${car1} and the negative facts about the ${car2} showed up towards the beginning of the list</b> about each car would have affected your choice about which car you liked best? If so, how?`
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
         primacy_order_intro_response1 = data.response
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
             auxiliary_info1: null,
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
         familiarity= data.response == 0 ? "Yes" : "No"
 
         
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
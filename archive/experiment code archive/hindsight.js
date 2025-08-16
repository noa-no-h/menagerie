//#region hindsight
var confidence_q = "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by being told the true population for each country)?</p>";

    
var hindsight_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, you will be asked to estimate the population of a series of countries.</p>
        <p><i>Please click the "Next" button when you are ready to see the first question.</i></p>`
    ],
    show_clickable_nav: true
};

//var country_list=["Pakistan", "Nigeria"]

var country_list=["Pakistan", "Nigeria", "Mexico", 
                "Vietnam", "The_Democratic_Republic_of_the_Congo", "Thailand", "Tanzania", 
                "South_Korea", "Colombia", "Uganda", "Ukraine", "Malaysia", 
                "North_Korea", "Niger", "Burkina_Faso", "Romania", "Zimbabwe", 
                "The_Netherlands", "Somalia", "Guinea", "Benin", "Haiti", "Greece",
                "The_Czech_Republic", "Azerbaijan"];

var country_list_shuffled = jsPsych.randomization.shuffle(country_list);

var answer = {Pakistan:"203, 177,034 people live in Pakistan.",
            Nigeria: "199,045,324 people live in Nigeria.",
            Mexico: "131,738,729 people live in Mexico.",
            Vietnam: "97,074,662 people live in Vietnam.",
            The_Democratic_Republic_of_the_Congo: "85,705,256 people live in Democratic Republic of the Congo.",
            Thailand: "69,256,846 people live in Thailand.",
            Tanzania: "60,229,204 people live in Tanzania.",
            South_Korea: "51,273,440 people live in South Korea.",
            Colombia: "49,705,306 people live in Colombia.",
            Uganda: "45,169,147 people live in Uganda.",
            Ukraine: "43,877,093 people live in Ukraine.",
            Malaysia: "32,294,009 people live in Malaysia.",
            North_Korea: "25,683,863 people live in North Korea.",
            Niger: "22,850,032 people live in Niger.",
            Burkina_Faso: "20,106,983 people live in Burkina Faso.",
            Romania: "19,519,762 people live in Romania.",
            Zimbabwe: "17,154,637 people live in Zimbabwe.",
            The_Netherlands: "17,114,912 people live in the Netherlands.",
            Somalia: "14,600,000 people live in Somalia.",
            Guinea: "13,270,289 people live in Guinea.",
            Benin: "11,683,042 people live in Benin.",
            Haiti: "11,193,952 people live in Haiti.",
            Greece: "11,133,944 people live in Greece.",
            The_Czech_Republic: "10,629,078 people live in the Czech Republic.",
            Azerbaijan: "9,980,369 people live in Azerbaijan.",
        };

var control = {Pakistan:"The movie, Avatar, cost $237,000,000 to make.",
            Nigeria: "The movie, Interstellar, cost $165,000,000 to make.",
            Mexico: "The movie, Harry Potter and the Sorcerer's Stone, cost $125,000,000 to make.",
            Vietnam: "The movie, Gladiator, cost $103,000,000 to make.",
            The_Democratic_Republic_of_the_Congo: "The movie, Bruce Almighty, cost $81,000,000 to make.",
            Thailand: "The movie, Les Miserables, cost $65,000,000 to make.",
            Tanzania: "The movie, Gone Girl, cost $61,000,000 to make.",
            South_Korea: "The movie, Moulin Rouge, cost $53,000,000 to make.",
            Colombia: "The movie, Miss Congeniality, cost $45,000,000 to make.",
            Uganda: "The movie, Little Women, cost $42,000,000 to make.",
            Ukraine: "The movie, Fifty Shades of Grey, cost $40,000,000 to make.",
            Malaysia: "The movie, Hangover, cost $33,000,000 to make.",
            North_Korea: "The movie, Star Wars: Return of the Jedi, cost $32,500,000 to make.",
            Niger: "The movie, La La Land, cost $30,000,000 to make.",
            Burkina_Faso: "The movie, James Bond -- Octopussy, cost $27,500,000 to make.",
            Romania: "The movie, Bodyguard, cost $25,000,000 to make.",
            Zimbabwe: "The movie, 12 Years a Slave, cost $22,000,000 to make.",
            The_Netherlands: "The movie, Back to the Future, cost $19,000,000 to make.",
            Somalia: "The movie, Borat, cost $18,000,000 to make.",
            Guinea: "The movie, Legally Blonde, cost $18,000,000 to make.",
            Benin: "The movie, Pitch Perfect, cost $17,000,000 to make.",
            Haiti: "The movie, Black Swan, cost $13,000,000 to make.",
            Greece: "The movie, The Intouchables, cost $12,800,000 to make.",
            The_Czech_Republic: "The movie, Brokeback Mountain, cost $14,000,000 to make.",
            Azerbaijan: "The movie, Scream, cost $15,000,000 to make.",
        };


true_values = {
    Pakistan: 203177034,
    Nigeria: 199045324,
    Mexico: 131738729,
    Vietnam: 97074662,
    The_Democratic_Republic_of_the_Congo: 85705256,
    Thailand: 69256846,
    Tanzania: 60229204,
    South_Korea: 51273440,
    Colombia: 49705306,
    Uganda: 45169147,
    Ukraine: 43877093,
    Malaysia: 32294009,
    North_Korea: 25683863,
    Niger: 22850032,
    Burkina_Faso: 20106983,
    Romania: 19519762,
    Zimbabwe: 17154637,
    The_Netherlands: 17114912,
    Somalia: 14600000,
    Guinea: 13270289,
    Benin: 11683042,
    Haiti: 11193952,
    Greece: 11133944,
    The_Czech_Republic: 10629078,
    Azerbaijan: 9980369
}
          

var responses = {};
for (var i = 0; i < country_list_shuffled.length; i++) {
    var country = country_list_shuffled[i];
    responses[country + "_estimate_first_response"] = null;
    responses[country + "_recall_original_response"] = null;
}

var current_country = country_list_shuffled[0];

var stim = null;
var country_display = null;
/*var first_estimate = {
    timeline: [{
        type: jsPsychSurveyText,
        questions: function() {
            country_display = current_country.replace(/_/g, ' ');
            stim = "How many people live in " + country_display + "?"
            return [{
                prompt: stim,
                required: required_general,
                rows: 2,
                columns: 10
            }];
        },
        placeholder: "Enter your estimate here",
        rows: 1,
        columns: 20,
        require_movement: true,
        on_finish: function (data) {
            responses[current_country + "_estimate_first_response"] = data.response.Q0;
            console.log(responses);
            let result = current_country + "_estimate_first_response = " + data.response.Q0;
            s1_data = {
                subject: data.subject,
                version: data.version,
                task_name: "hindsight",
                factor: data.condition,
                condition: data.con,
                choice: data.response.Q0,
                stimulus: stim,
                auxiliary_info1: result,
                rt_main_question: data.rt,
            }
            save_data(s1_data, 'introspection');

        }
    }],
    randomize_order: false
};*/


var first_estimate = {
    timeline: [{
        type: jsPsychSurvey,
        survey_json: {
            showQuestionNumbers: false,
            focusFirstQuestionAutomatic: true,
            completeText: "Next",
            pages: [
                {
                    name: "page1",
                    elements: [
                        {
                            type: "text",
                            name: "countryEstimate",
                            maskType: "numeric",
                            maskSettings: {
                                "precision": 1
                              },
                            title: function() {
                                country_display = current_country.replace(/_/g, ' ');
                                return "How many people live in " + country_display + "?";
                            },
                            isRequired: true,
                            placeHolder: "Enter your estimate here",
                            size: 25
                        }
                    ]
                }
            ]
        },
        
        on_finish: function(data) {
            var responses = data.response;
            var estimate = responses.countryEstimate;
            var difference = Math.abs(true_values[current_country] - estimate);
            console.log("difference: ", difference);
            //var result = current_country + "_estimate_first_response = " + estimate;
            s1_data = {
                subject: data.subject,
                version: data.version,
                task_name: "hindsight",
                factor: data.condition,
                condition: condition[0] == "Factor-Included" ? "Factor-Included" : "Factor-Excluded",
                choice: estimate,
                stimulus: current_country + " first estimate",
                auxiliary_info1: difference,
                rt_main_question: data.rt,
            };
            save_data(s1_data, 'introspection');
            console.log(responses);
        }
    }],
    randomize_order: false
};





var loop_estimates = {
    timeline: [first_estimate],
    loop_function: function(data){
        if (current_country != country_list_shuffled[country_list_shuffled.length - 1]) {
            var currentIndex = country_list_shuffled.indexOf(current_country);
            if (currentIndex >= 0 && currentIndex < country_list_shuffled.length - 1) {
                // Increment the index to get the next country
                current_country = country_list_shuffled[currentIndex + 1];
                console.log(current_country);
            } else {
                // Handle the case where the current country is the last in the list
                console.log("Current country is the last in the list or not found.");
            }
            return true; //loop
        } else {
            current_country=country_list_shuffled[0];
            return false; // don't loop
        }
    }
}

var intro_slides_with_answers = {
    type: jsPsychInstructions,
    pages: [
        `<p>You will now be shown some trivia facts for seven seconds each.</p>
        <p><i>Please click the "Next" button when you are ready to see the first fact.</i></p>`
    ],
    show_clickable_nav: true
};
var country_answer_or_control = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: function() {
        return condition[0] == 'Factor-Included' ? answer[current_country] : control[current_country];
    },
    choices: "NO_KEYS",
    trial_duration: 7000,
};    

var loop_answer_or_control = {
    timeline: [country_answer_or_control],
    loop_function: function(data){
        if (current_country != country_list_shuffled[country_list_shuffled.length - 1]) {
            var currentIndex = country_list_shuffled.indexOf(current_country);
            if (currentIndex >= 0 && currentIndex < country_list_shuffled.length - 1) {
                // Increment the index to get the next country
                current_country = country_list_shuffled[currentIndex + 1];
            } else {
                // Handle the case where the current country is the last in the list
                console.log("Current country is the last in the list or not found.");
            }
            
            return true; //loop
        } else {
            current_country=country_list_shuffled[0];
            return false; // don't loop
        }
    }
}

var country_memory = {
    timeline: [{
        type: jsPsychSurvey,
        survey_json: {
            showQuestionNumbers: false,
            focusFirstQuestionAutomatic: true,
            pages: [
                {
                    name: "page1",
                    elements: [
                        {
                            type: "text",
                            name: "countryMemory",
                            maskType: "numeric",
                            maskSettings: {
                                "precision": 1
                              },
                            title: function() {
                                country_display = current_country.replace(/_/g, ' ');
                                return "What was your ORIGINAL ANSWER when we asked you to estimate how many people lived in " + country_display + "?";
                             },
                            isRequired: true,
                            placeHolder: "Enter your original answer here",
                            size: 25
                        }
                    ]
                }
            ]
        },

        on_finish: function (data) {
            var memory = data.response.countryMemory
            console.log("memory: ", memory);
            responses[current_country + "_recall_original_response"] = data.response.Q0;
            let result = current_country + "_recall_original_response = " + data.response.Q0;
            var difference = Math.abs(true_values[current_country] - memory);
            console.log("difference: ", difference);
            s1_data = {
                subject: data.subject,
                version: data.version,
                task_name: "hindsight",
                factor: data.condition,
                condition: condition[0] == "Factor-Included" ? "Factor-Included" : "Factor-Excluded",
                choice: memory,
                stimulus: current_country + " memory",
                auxiliary_info1: difference,
                rt_main_question: data.rt,
            }
            save_data(s1_data, 'introspection');
        }
    }],
    randomize_order: false
};
var loop_country_memory = {
    timeline: [country_memory],
    loop_function: function(data){
        if (current_country != country_list_shuffled[country_list_shuffled.length - 1]) {
            var currentIndex = country_list_shuffled.indexOf(current_country);
            if (currentIndex >= 0 && currentIndex < country_list_shuffled.length - 1) {
                // Increment the index to get the next country
                current_country = country_list_shuffled[currentIndex + 1];
            } else {
                // Handle the case where the current country is the last in the list
                console.log("Current country is the last in the list or not found.");
            }
            
            return true; //loop
        } else {
            current_country=country_list_shuffled[0];
            return false; // don't loop
        }
    }
}

    var hindsight_openQ_response = null;
    var hindsight_openQ = {
        type: jsPsychSurveyText,
        questions: [{
            prompt: `<p>In this exercise, you were asked to estimate the 
            population of a series of countries and then, later on, to recall 
            the estimate you made.</p><p>Describe your thought process behind 
            the number you put in when recalling your earlier estimate. How did 
            you come to remember the estimate you had made before?</p>`,
            required: required_general, rows: 5, columns: 80
        }],
        on_finish: function (data) {
            hindsight_openQ_response = data.response.Q0;
        }
    };

    var introspection_q_labels_hindsight1 = [
        `<strong>It pushed my memory <u>TOWARDS</u> the country's true population value.</strong>`,
        "",
        "<strong>It did not affect my response</strong>",
        "",
        `<strong>It pushed my memory <u>AWAY FROM</u> the country's true population value.</strong>`
    ];

    var introspection_q_labels_hindsight2 = [
        `<strong>It would have pushed my memory <u>TOWARDS</u> the country's true population value.</strong>`,
        "",
        "<strong>It would not have affected my response</strong>",
        "",
        `<strong>It would have pushed my memory <u>AWAY FROM</u> the country's true population value.</strong>`
    ];

    var hindsight_intro_response1 = null;
    var hindsight_introspect1 = {
        type: jsPsychHtmlSliderResponse,
        stimulus: function () {
            if (condition[0] == "Factor-Included") {
                return `<p>In this exercise, you were asked to estimate the population of a series of countries. And later on, you were asked to recall the population estimate you had made earlier.</p>
                        <p>In between these two sets of questions, <b>we told you the true population for each country.</b></p>
                        <p>For example, early on, we asked you to estimate the population of Peru. Later on in the exercise, we told you the true population of Peru. Near the end of the exercise, we asked you to recall the population estimate you had made earlier for Peru. </p>
                        <p>Do you think <b>being told the true population for each country</b> affected your response? If so, how?</p>`;
            } else {
                return `<p>In this exercise, you were asked to estimate the population of a series of countries. And later on, you were asked to recall the population estimate you had made earlier.</p>
                        <p>Now, imagine that in between these two sets of questions, <b>we told you the true population for each country.</b> instead of showing you facts about movie budgets.</p>
                        <p>For example, imagine the following scenario: Early on, we asked you to estimate the population of Peru. Later on in the exercise, we told you the true population of Peru. Near the end of the exercise, we asked you to recall the population estimate you had made earlier for Peru. </p>
                        <p>If this were the case, do you think <b>being told the true population for each country</b> would have affected your response? If so, how?</p>`;
            }
        },
        labels: condition[0] == "Factor-Included" ? introspection_q_labels_hindsight1 : introspection_q_labels_hindsight2,
        slider_width: introspection_q_slider_width,
        min: introspection_q_min,
        max: introspection_q_max,
        slider_start: 50,
        require_movement: introspection_q_require,
        prompt: "<br><br><br><br><br><br>",
        on_finish: function (data) {
            hindsight_intro_response1 = data.response;
        }
    };

    var hindsight_intro_response2 = null;
    var hindsight_introspect2 = {
        type: jsPsychSurveyText,
        questions: [{
            prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
            required: required_general, rows: 5, columns: 80
        }],
        on_finish: function (data) {
            hindsight_intro_response2 = data.response.Q0;
        }
    };

    var hindsight_intro_confidence_response = null;
    var hindsight_intro_confidence = {
        type: jsPsychHtmlSliderResponse,
        stimulus: confidence_q,
        labels: confidence_q_labels,
        slider_width: confidence_q_slider_width,
        min: confidence_q_min,
        max: confidence_q_max,
        slider_start: 50,
        require_movement: require_movement_general,
        on_finish: function (data) {
            choice=JSON.stringify(responses);
            console.log(choice);
            hindsight_intro_confidence_response = data.response;
            s1_data = {
                subject: data.subject,
                version: data.version,
                factor: data.condition,
                task_name: "hindsight",
                condition: condition[0] == "Factor-Included" ? "Factor-Included" : "Factor-Excluded",
                stimulus: null,
                choice: null,
                auxiliary_info1: null,
                openq_response: hindsight_openQ_response,
                introspect_rating: hindsight_intro_response1,
                introspect_open: hindsight_intro_confidence_response,
                familiarity: familiarity,
                rt_main_question: data.rt
            };
            
            save_data(s1_data, 'introspection');
        }
    };

    var familiarity = null;
    var hindsight_familiar = {
        type: jsPsychHtmlButtonResponse,
        stimulus: familiarity_prompt,
        choices: ["Yes", "No"],
        on_finish: function (data) {
            familiarity= data.response == 0 ? "Yes" : "No"

            
        }
    };

    var hindsight = {
        timeline: [hindsight_instructions, loop_estimates, intro_slides_with_answers,
            loop_answer_or_control, loop_country_memory, hindsight_familiar, 
            hindsight_openQ, hindsight_introspect1, hindsight_intro_confidence]
    };

    /*var hindsight = {
        timeline: [loop_country_memory, hindsight_familiar, 
            hindsight_openQ, hindsight_introspect1, hindsight_intro_confidence]
    };*/

//#endregion hindsight
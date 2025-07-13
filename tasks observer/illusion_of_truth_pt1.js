 //#region 9. Illusion of truth
 //Supplementary Material
//Ongoing Secondary Tasks Can Reduce the Illusory Truth Effect
//Deva P. Ly, Daniel M. Bernstein, Eryn J. Newman*

 
var confidence_q = condition[0] == 'Factor-Included' ?"<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the number of times you saw each statement)?</p>" : "<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the number of times you saw each statement)?</p>";

    //preparing stimuli
    var nine_true_statements = [
        `<p style = "font-size:30px">Volleyball was originally called mintonette.</p>`,
        `<p style = "font-size:30px">The flamingo’s pink color comes from carotenoid pigments in its food</p>`,
        `<p style = "font-size:30px">Canada is the second largest country in the world in area</p>`,
        `<p style = "font-size:30px">The largest European glacier is Vatnajökull on Iceland</p>`,
        `<p style = "font-size:30px">In almost all human populations of newborns, there is a slight excess of males.</p>`,
        `<p style = "font-size:30px">Domesticated goats are descended from the pasang</p>`,
        `<p style = "font-size:30px">Most limes have more acid than lemon</p>`,
        `<p style = "font-size:30px">Lake Baikal is the world's largest freshwater lake by volume</p>`,
        `<p style = "font-size:30px">Female turkeys generally weigh half as much as males</p>`
    ]
    
    var nine_false_statements = [
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
    
    var shuffled_eighteen_statements = _.shuffle(nine_true_statements.concat(nine_false_statements))
    console.log(shuffled_eighteen_statements);

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
        for (var i = 0; i < shuffled_eighteen_statements.length; i++) {
            illusion_of_truth_trials.push({
                type: jsPsychHtmlKeyboardResponse,
                stimulus: shuffled_eighteen_statements[i],
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
                `<p>For approximately the next three minutes, you will see a series of trivia statements. 
                <br><p>The trivia statements will be presented automatically - there is no need to press any buttons.
Please read the trivia statements carefully as they are presented, but do not do anything else. You
will not be able to pause the study, so make sure you have no distractions.</p>
        <p><i>Press the next button to begin.</i></p>`,
            ],
            show_clickable_nav: true
        };

        var illusion_of_truth_exposure = {
            timeline: illusion_of_truth_trials
        }


        var illusion_of_truth_instructions2 = {
            type: jsPsychInstructions,
            pages: [`<p>You will now begin the second part of the exercise. For this part, you will be shown a series of words and be asked to rate how much you <b>like</b> each one. Note that several of these words may be ones that you have seen before. There are no right or wrong answers so try to be as honest as possible.</p>
        <p><i>Please click the button below to continue.</i></p>`,],
            show_clickable_nav: true
        };

        var illusion_of_truth_questions = {
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
                            task_name: "illusion of truth pt1",
                            choice: data.response,
                            stimulus: data.stim,
                            condition: countInArray(stimulus_array, data.aux),
                        }
                        save_data(s1_data, 'introspection');
                    }
                }
            ],
            timeline_variables: shuffled_eighteen_statements,
            randomize_order: true
        };

        var illusion_of_truth_openQ_response = null;
        var illusion_of_truth_openQ = {
            type: jsPsychSurveyText,
            questions: [{
                prompt: `<p>In this exercise, you were shown a series of words and asked to rate how much you liked them.</p>
        <p>Describe your thought process during this exercise. How did you come to your eventual ratings for each word?</p>`,
                required: required_general, rows: 5, columns: 80
            }],
            on_finish: function (data) {
                illusion_of_truth_openQ_response = data.response.Q0;
            }
        };

        var introspection_q_labels_mee1 = [`<strong>When they saw a word a lot of times, that made them like the word <u>LESS</u></strong>`, "", "<strong>The number of times they saw a word did not affect their response</strong>", "", `<strong>When they saw a word a lot of times, that made them like the word <u>MORE</u></strong>`];
        var introspection_q_labels_mee2 = [`<strong>If they had seen a word a lot of times, that would have made them like the word <u>LESS</u></strong>`, "", "<strong>The number of times they saw a word would not have affected their response</strong>", "", `<strong>If they had seen a word a lot of times, that would have made them like the word <u>MORE</u></strong>`];

        var illusion_of_truth_intro_response1 = null;
        var illusion_of_truth_introspect1 = {
            type: jsPsychHtmlSliderResponse,
            stimulus: function () {
                if (condition[0] == "Factor-Included") {
                    return `During this exercise, you were initially shown a series of foreign words. Then, you were asked to rate how much you liked each of these words. When you were originally shown these words, some of these words appeared many times, while others appeared only a few times or not at all.
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
            prompt: "<br><br><br><br><br><br><br>",
            on_finish: function (data) {
                illusion_of_truth_intro_response1 = data.response
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
                    task_name: "illusion of truth pt1",
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
                familiarity= data.response == 0 ? "Yes" : "No"

               
            }
        }

        var illusion_of_truth_pt1 = {
            timeline: [illusion_of_truth_instructions1, illusion_of_truth_exposure]
        }

        //#endregion
        //timeline.push(mee)
var jsPsychSurveySlider = (function (jspsych) {
    "use strict"




    const info = {
        name: "survey-slider",
        description: "",
        parameters: {
            questions: {
                type: jspsych.ParameterType.COMPLEX,
                array: true,
                pretty_name: "Questions",
                nested: {
                    /** The HTML string to be displayed */
                    stimulus: {
                        type: jspsych.ParameterType.HTML_STRING,
                        pretty_name: "Stimulus",
                        default: "",
                    },
                    prompt: {
                        type: jspsych.ParameterType.STRING,
                        pretty_name: "Prompt",
                        default: undefined,
                        description:
                            "Content to be displayed below the stimulus and above the slider",
                    },
                    // Labels to appear to the left of each slider, one in line with the top row ticks and one in line with the bottom
                    labels: {
                        type: jspsych.ParameterType.STRING,
                        pretty_name: "Labels",
                        default: [],
                        array: true,
                        description: "Labels of the sliders.",
                    },
                    /** Array containing the ticks to show along the slider. Ticks will be displayed at equidistant locations along the slider. Note this parameter is called Labels in the original plugin.*/
                    ticks: {
                        type: jspsych.ParameterType.HTML_STRING,
                        pretty_name: "Ticks",
                        default: [],
                        array: true,
                        description: "Ticks of the sliders.",
                    },
                    name: {
                        type: jspsych.ParameterType.STRING,
                        pretty_name: "Question Name",
                        default: "",
                        description:
                            "Controls the name of data values associated with this question",
                    },
                    min: {
                        type: jspsych.ParameterType.INT,
                        pretty_name: "Min slider",
                        default: 0,
                        description: "Sets the minimum value of the slider.",
                    },
                    max: {
                        type: jspsych.ParameterType.INT,
                        pretty_name: "Max slider",
                        default: 100,
                        description: "Sets the maximum value of the slider",
                    },
                    slider_start: {
                        type: jspsych.ParameterType.INT,
                        pretty_name: "Slider starting value",
                        default: 50,
                        description: "Sets the starting value of the slider",
                    },
                    step: {
                        type: jspsych.ParameterType.INT,
                        pretty_name: "Step",
                        default: 1,
                        description: "Sets the step of the slider",
                    },
                    correct_response: {
                        type: jspsych.ParameterType.FLOAT, // Use FLOAT to allow for decimal values
                        pretty_name: "Correct Response",
                        default: null,
                        description: "The correct value for the slider. If provided, validation will occur."
                    },
                     allowed_margin: {
                        type: jspsych.ParameterType.FLOAT,
                        pretty_name: "Allowed Margin",
                        default: 0,
                        description: "The margin of error allowed for a correct response."
                    }
                },
            },
            randomize_question_order: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "Randomize Question Order",
                default: false,
                description:
                    "If true, the order of the questions will be randomized",
            },
            preamble: {
                type: jspsych.ParameterType.HTML_STRING,
                pretty_name: "Preamble",
                default: null,
                description: "String to display at top of the page.",
            },
            button_label: {
                type: jspsych.ParameterType.STRING,
                pretty_name: "Button label",
                default: "Continue",
                description: "Label of the button.",
            },
            autocomplete: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "Allow autocomplete",
                default: false,
                description:
                    "Setting this to true will enable browser auto-complete or auto-fill for the form.",
            },
            require_movement: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "Require movement",
                default: false,
                description:
                    "If true, the participant will have to move the slider before continuing.",
            },
            slider_width: {
                type: jspsych.ParameterType.INT,
                pretty_name: "Slider width",
                default: 500,
                description: "Width of the slider in pixels.",
            },
            // ---- NEW PARAMETER ADDED HERE ----
            enable_button_after: {
                type: jspsych.ParameterType.INT,
                pretty_name: "Enable button after",
                default: 0,
                description: "How long to wait before enabling the button, in milliseconds."
            }
        },
    }

    class SurveySliderPlugin {
        constructor(jsPsych) {
            this.jsPsych = jsPsych
        }

        trial(display_element, trial) {
            // half of the thumb width value from jspsych.css, used to adjust the label positions
            var half_thumb_width = 7.5

            var html =
                '<div id="jspsych-html-slider-response-wrapper" style="margin: 100px 0px;">'

            html +=
                '<div class="jspsych-html-slider-response-container" style="position:relative; margin: 0 auto 3em auto; '
            if (trial.slider_width !== null) {
                html += "width:" + trial.slider_width + "px;"
            } else {
                html += "width:auto;"
            }
            html += '">'

            // show preamble text
            if (trial.preamble !== null) {
                html +=
                    '<div style="position: relative; width:100%" id="jspsych-survey-slider-preamble" class="jspsych-survey-slider-preamble">' +
                    trial.preamble +
                    "</div><br>"
            }

            if (trial.autocomplete) {
                html += '<form id="jspsych-survey-slider-form">'
            } else {
                html +=
                    '<form id="jspsych-survey-slider-form" autocomplete="off">'
            }

            // add sliders questions
            var question_order = []
            for (var i = 0; i < trial.questions.length; i++) {
                question_order.push(i)
            }
            if (trial.randomize_question_order) {
                question_order =
                    this.jsPsych.randomization.shuffle(question_order)
            }

            for (var i = 0; i < trial.questions.length; i++) {
                var question = trial.questions[question_order[i]]
                html +=
                    '<div id="jspsych-html-slider-response-stimulus">' +
                    question.stimulus +
                    "</div>"
                html +=
                    '<label class="jspsych-survey-slider-statement">' +
                    question.prompt +
                    "</label><br>"
                if (question.labels.length > 0) {
                    html +=
                        '<div style="font-size: 100%; font-weight: bold; position: absolute; left: calc(-15%)">' +
                        question.labels[0] +
                        "</div>"
                }
                html +=
                    '<input style="width: 100%" type="range" class="jspsych-slider" value="' +
                    question.slider_start +
                    '" min="' +
                    question.min +
                    '" max="' +
                    question.max +
                    '" step="' +
                    question.step +
                    '" id="jspsych-html-slider-response-response-' +
                    i +
                    '" name="Q' +
                    i +
                    '" data-name="' +
                    question.name +
                    '"></input><br>'
                if (question.labels.length > 0) {
                    html +=
                        '<div style="font-size: 100%; font-weight: bold; position: absolute; left: calc(-15%)">' +
                        question.labels[1] +
                        "</div>"
                }
                for (var j = 0; j < question.ticks.length; j++) {
                    var label_width_perc = 100 / (question.ticks.length - 1)
                    var percent_of_range =
                        j * (100 / (question.ticks.length - 1))
                    var percent_dist_from_center =
                        ((percent_of_range - 50) / 50) * 100
                    var offset =
                        (percent_dist_from_center * half_thumb_width) / 100
                    html +=
                        '<div style="border: 1px solid transparent; position: absolute; ' +
                        "left:calc(" +
                        percent_of_range +
                        "% - (" +
                        label_width_perc +
                        "% / 2) - " +
                        offset +
                        "px); text-align: center; width: " +
                        label_width_perc +
                        '%;">'
                    html +=
                        '<span style="text-align: center; font-size: 100%;">' +
                        question.ticks[j] +
                        "</span>"
                    html += "</div>"
                }
                html += "<br/><br/>"
            }
            html += "<br/>"

            html += '<div id="jspsych-survey-slider-error-message" style="color: red; margin-top: 10px; height: 1.2em;"></div>';


            // add submit button
            html +=
                '<input type="submit" id="jspsych-survey-slider-next" class="jspsych-survey-slider jspsych-btn" value="' +
                trial.button_label +
                '"></input>'

            html += "</form>"
            html += "</div>"
            html += "</div>"

            display_element.innerHTML = html


            // ---- NEW LOGIC TO HANDLE BUTTON ENABLING ----

            let timer_elapsed = false;
            const submit_button = document.getElementById("jspsych-survey-slider-next");

            // Master function to check if the button should be enabled
            const check_and_enable_button = () => {
                // Condition 1: Timer must have elapsed (or not exist)
                const timer_condition_met = trial.enable_button_after <= 0 || timer_elapsed;

                // Condition 2: Movement must have occurred (or not be required)
                let movement_condition_met = !trial.require_movement;
                if (trial.require_movement) {
                    const all_sliders = document.querySelectorAll(".jspsych-slider");
                    // Use Array.every() to check if all sliders have the 'clicked' class
                    const all_clicked = Array.from(all_sliders).every(slider => slider.classList.contains("clicked"));
                    if (all_clicked) {
                        movement_condition_met = true;
                    }
                }

                // Enable button only if both conditions are met
                if (timer_condition_met && movement_condition_met) {
                    submit_button.disabled = false;
                }
            };

            // Disable button initially if either condition requires it
            if (trial.require_movement || trial.enable_button_after > 0) {
                submit_button.disabled = true;
            }

            // Set up timer if needed
            if (trial.enable_button_after > 0) {
                this.jsPsych.pluginAPI.setTimeout(() => {
                    timer_elapsed = true;
                    check_and_enable_button();
                }, trial.enable_button_after);
            }

            // Set up movement listeners if needed
            if (trial.require_movement) {
                const all_sliders = document.querySelectorAll(".jspsych-slider");
                all_sliders.forEach((slider) => {
                    const on_interaction = () => {
                        slider.classList.add("clicked");
                        check_and_enable_button();
                    };
                    slider.addEventListener("click", on_interaction);
                    slider.addEventListener("change", on_interaction);
                });
            }

            display_element.querySelector("#jspsych-survey-slider-form").addEventListener("submit", (e) => {
                e.preventDefault();

                // Get all interactive sliders
                const matches = display_element.querySelectorAll('input[type="range"]:not([disabled])');
                const question_data = {};
                let all_correct = true;

                // Loop through sliders to get responses and perform validation
                for (let i = 0; i < matches.length; i++) {
                    const slider = matches[i];
                    const original_question_index = question_order[i];
                    const question = trial.questions[original_question_index];
                    
                    const response = slider.valueAsNumber;
                    const name = question.name || `Q${original_question_index}`;
                    question_data[name] = response;

                    // Check against correct_response if it exists
                    if (question.correct_response !== null) {
                        // Get the allowed margin for this question (defaults to 0)
                        const margin = question.allowed_margin;

                        // Calculate the lower and upper bounds of the correct range
                        const lower_bound = question.correct_response - margin;
                        const upper_bound = question.correct_response + margin;

                        // Check if the response is outside the allowed range
                        if (response < lower_bound || response > upper_bound) {
                            all_correct = false;
                        }
                    }
                }

                const errorMessageDiv = display_element.querySelector('#jspsych-survey-slider-error-message');

                if (all_correct) {
                    // measure response time
                    var endTime = performance.now();
                    var response_time = endTime - startTime;

                    // save data
                    var trial_data = {
                        rt: response_time,
                        response: JSON.stringify(question_data),
                        question_order: JSON.stringify(question_order),
                    };

                    display_element.innerHTML = "";
                    // next trial
                    this.jsPsych.finishTrial(trial_data);
                } else {
                    // if any response is incorrect, show the error message and do not end the trial
                    errorMessageDiv.textContent = "Please match the Prolific user's answers.";
                }
            });

            var startTime = performance.now();
        }
    }

    SurveySliderPlugin.info = info;

    return SurveySliderPlugin;
})(jsPsychModule);
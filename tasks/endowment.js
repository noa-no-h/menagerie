
/* Endowment */
var endowment_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p>In this exercise, you will be asked to make some decisions about the price of an object.</p>
         <p><i>Please click the button below when you are ready to begin the exercise.</i></p>`
    ],
    show_clickable_nav: true
}

var juice = null;

var endowment_instructions2 = {
    type: jsPsychInstructions,
    pages: function () {
        var instructions = [];

        if (condition[0] == 'Factor-Included') {
            instructions.push(`Imagine you possess <b>one year's worth of a rare and safe pill that enables a person to maintain ideal weight</b>.<br><br>
            You have the option to <b>sell this year's supply of pills</b> in exchange for a certain amount of money. You get to decide how much money this set of pills is worth to you.`);
        } else {
            instructions.push(`You have the option to sell an item you possess in exchange for a certain amount of money. You get to decide how much money this item is worth to you.`);
        }
        return instructions;
    },
    show_clickable_nav: true
}

var task_list = [
    {
        type: jsPsychSurveyMultiChoice,
        questions: [
            {
                prompt: "Imagine that you are choosing between several brands of frozen concentrated orange juice. For each brand, you know only the price and the quality ratings made by consumer reports. Given that you had to buy one brand based on this information alone, which one would it be? In case of the quality rating, 0 = worst quality and 100 = ideal quality.",
                options: [
                    '<b>Brand W:</b> Price per can = $1.20; Quality rating = 30',
                    '<b>Brand N:</b> Price per can = $1.20; Quality rating = 50',
                    '<b>Brand J:</b> Price per can = $2.00; Quality rating = 70',
                ],
                required: required_general,
            }
        ],
        on_finish: function (data) {
            if (data.response.Q0 == '<b>Brand N:</b> Price per can = $1.20; Quality rating = 50') {
                juice = 'Brand N (Target)';
            } else if (data.response.Q0 == '<b>Brand J:</b> Price per can = $2.00; Quality rating = 70') {
                juice = 'Brand J (Competitor)';
            } else {
                juice = 'Brand W (Decoy)';
            }
        }
    }
];

var decoy_absent = {
    type: jsPsychSurveyMultiChoice,
    questions: [
        {
            prompt: "Imagine that you are choosing between several brands of frozen concentrated orange juice. For each brand, you know only the price and the quality ratings made by consumer reports. Given that you had to buy one brand based on this information alone, which one would it be? In case of the quality rating, 0 = worst quality and 100 = ideal quality.",
            options: [
                '<b>Brand N:</b> Price per can = $1.20; Quality rating = 50',
                '<b>Brand J:</b> Price per can = $2.00; Quality rating = 70',
            ],
            required: required_general,
        }
    ],
    on_finish: function (data) {
        if (data.response.Q0 == '<b>Brand N:</b> Price per can = $1.20; Quality rating = 50') {
            juice = 'Brand N (Target)';
        } else {
            juice = 'Brand J (Competitor)';
        }
    }
};

var decoy_question = (condition[0] == "Factor-Included") ? task_list[0] : decoy_absent;

var decoy_openQ_response = null;
var decoy_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were asked to choose between several brands of frozen orange juice concentrate.</p><p>Describe your thought process during this choice. How did you come to your eventual decision about which juice to purchase?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        decoy_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_decoy1 = [`<strong>It made me <u>LESS</u> likely to choose Brand N (and more likely to choose Brand J)</strong>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It made me <u>MORE</u> likely to choose Brand N (and less likely to choose Brand J)</strong>`];
var introspection_q_labels_decoy2 = [`<strong>It would have made me <u>LESS</u> likely to choose Brand N (and more likely to choose Brand J)</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me <u>MORE</u> likely to choose Brand N (and less likely to choose Brand J)</strong>`];

var decoy_intro_response1 = null;
var decoy_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>Below are the options you were asked to choose from:</p>
            <p><b>Brand W:</b> Price per can = $1.20; Quality rating = 30</p>
            <p><b>Brand N:</b> Price per can = $1.20; Quality rating = 50</p>
            <p><b>Brand J:</b> Price per can = $2.00; Quality rating = 70</p>
            <p>Do you think the <b>presence of Brand W</b> affected your preference <b>between Brand N and Brand J?</b> If so, how?</p>`;
        } else {
            return `<p>Below are the options you were asked to choose from:</p>
            <p><b>Brand N:</b> Price per can = $1.20; Quality rating = 50</p>
            <p><b>Brand J:</b> Price per can = $2.00; Quality rating = 70</p>
            <p>Now, imagine if you were given a <i>third option</i>, as follows:</p>
            <p><b>Brand W:</b> Price per can = $1.20; Quality rating = 30</p>
            <p>Do you think the <b>presence of this option</b> would have affected your preference <b>between Brand N and Brand J?</b> If so, how?</p>`;
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_decoy1 : introspection_q_labels_decoy2,
    slider_width: 600,  
    min: 0,
    max: 100,
    slider_start: 50,
    require_movement: true,
    prompt: "<br><br><br><br><br><br>",
    on_finish: function (data) {
        decoy_intro_response1 = data.response;
    }
};

var decoy_intro_response2 = null;
var decoy_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        decoy_intro_response2 = data.response.Q0;
    }
};

var decoy_intro_confidence_response = null;
var decoy_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: `<p>How confident are you in the choices you made during this task?</p>`,
    labels: ["Not at all confident", "Extremely confident"],
    slider_width: 600,  
    min: 0,
    max: 100,
    slider_start: 50,
    require_movement: true,
    on_finish: function (data) {
        decoy_intro_confidence_response = data.response;
        var s1_data = {
            subject: jsPsych.data.getURLVariable('subject'),
            version: jsPsych.data.getURLVariable('version'),
            task_name: "decoy effect",
            choice: juice,
            condition: condition[0] == "Factor-Included" ? "Decoy Present" : "Decoy Absent",
            factor: condition[0],
            familiarity: familiarity,
            openq_response: decoy_openQ_response,
            introspect_rating: decoy_intro_response1,
            introspect_open: decoy_intro_response2,
            confidence: decoy_intro_confidence_response,
            rt: data.rt
        };
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var decoy_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p>Before doing this study, had you seen or heard of a task similar to this last one before?</p>`,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity= data.response == 0 ? "Yes" : "No";

        
    }
};

var endowment = {
    timeline: [endowment_instructions, endowment_instructions2, decoy_question, decoy_familiar, decoy_openQ, decoy_introspect1, decoy_introspect2, decoy_intro_confidence]
};


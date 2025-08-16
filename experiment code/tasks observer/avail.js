//#region 2. Famous Names (Tversky & Kahneman, 1973) - BETWEEN
subjectData = availability_db.find(item => item.subject === actorNumber);
var observedList = subjectData.choice;
var confidence_q = condition[0] == 'Factor-Included' ? '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way the Prolific user was influenced by the fame of the people in each list)?</p>' : '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you would have been influenced by the fame of the people in each list)?</p>';
observerTime = subjectData.rt;

var avail_included_stimuli = [
    {
        stimulus: //men are famous, fewer men (6 men, 9 women)
            `<table style = "font-size:22px;border-collapse:separate;border-spacing:1.5em">
    <tr>
      <td>Ben Affleck</td>
      <td>Nicole Sullivan</td>
      <td>Caitlin Glass</td>
    </tr>
    <tr>
        <td>Tom Hanks</td>
        <td>Elizabeth Reaser</td>
        <td>Daniel Craig</td>
    </tr>
    <tr>
        <td>Lauren Tom</td>
        <td>Annie Potts</td>
        <td>June Foray</td>
    </tr>
    <tr>
        <td>Ashley Johnson</td>
        <td>George Clooney</td>
        <td>Johnny Depp</td>
    </tr>
    <tr>
        <td>Andrea Libman</td>
        <td>Kath Soucie</td>
        <td>Brad Pitt</td>
    </tr>
</table>`
    },
    {
        stimulus: //women are famous, more men (9 men, 6 women)
            `<table style = "font-size:22px;border-collapse:separate;border-spacing:1.5em">
    <tr>
        <td>Billy West</td>
        <td>Emma Stone</td>
        <td>Scarlett Johansson</td>
    </tr>
    <tr>
        <td>Frank Welker</td>
        <td>Carrie Fisher</td>
        <td>Rob Paulsen</td>
    </tr>
    <tr>
        <td>Vincent Martella</td>
        <td>Scott Menville</td>
        <td>Greg Cipes</td>
    </tr>
    <tr>
        <td>Natalie Portman</td>
        <td>Meryl Streep</td>
        <td>Ron Perlman</td>
    </tr>
    <tr>
        <td>Wallace Shawn</td>
        <td>Nolan North</td>
        <td>Anne Hathaway</td>
    </tr>
</table>`
    }
];

var avail_excluded_stimuli = [
    {
        stimulus: //fewer men (6 men, 9 women)
            `<table style = "font-size:22px;border-collapse:separate;border-spacing:1.5em">
      <tr>
          <td>Andrew Marks</td>
          <td>Nicole Sullivan</td>
          <td>Caitlin Glass</td>
      </tr>
      <tr>
          <td>Zach Aguilar</td>
          <td>Elizabeth Reaser</td>
          <td>Gary Cole</td>
      </tr>
      <tr>
          <td>Lauren Tom</td>
          <td>Annie Potts</td>
          <td>June Foray</td>
      </tr>
      <tr>
          <td>Ashley Johnson</td>
          <td>Jeff Bennett</td>
          <td>Michael Bell</td>
      </tr>
      <tr>
          <td>Andrea Libman</td>
          <td>Kath Soucie</td>
          <td>Darren Korb</td>
      </tr>
  </table>`
    },
    {
        stimulus: //more men (9 men, 6 women)
            `<table style = "font-size:22px;border-collapse:separate;border-spacing:1.5em">
      <tr>
          <td>Billy West</td>
          <td>Veronica Taylor</td>
          <td>Carolyn Lawrence</td>
      </tr>
      <tr>
          <td>Frank Welker</td>
          <td>Lydia Mackay</td>
          <td>Rob Paulsen</td>
      </tr>
      <tr>
          <td>Vincent Martella</td>
          <td>Scott Menville</td>
          <td>Greg Cipes</td>
      </tr>
      <tr>
          <td>Jill Talley</td>
          <td>April Stewart</td>
          <td>Ron Perlman</td>
      </tr>
      <tr>
          <td>Wallace Shawn</td>
          <td>Nolan North</td>
          <td>Stephanie Young</td>
      </tr>
  </table>`
    }
];

var avail_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> In this exercise, the Prolific user was shown lists of people's names.<p>Their goal was to <b>memorize as many names as possible</b> in a short amount of time.</p>`,
        `<p>They were only given <b>five seconds</b> to memorize each list. This is a very short amount of time, so they were told to just try their best to memorize as many names as they can.<p> They were told they would be shown <b>two</b> lists in total, with a short break in between.</p>`,
        `<p>You will now be shown the two lists of names they were shown. There will be no warning before the second list.</p>
    <p><i>Please click the "Next" button when you are ready to see the lists.</i></p>`
    ],
    show_clickable_nav: true
}

var avail_trials = {
    timeline: [
        {//fixation
            type: jsPsychHtmlKeyboardResponse,
            stimulus: '<p style = "font-size:30px">+</p>',
            choices: "NO_KEYS",
            trial_duration: 1000
        },
        {//trials
            type: jsPsychHtmlKeyboardResponse,
            stimulus: jsPsych.timelineVariable('stimulus'),
            choices: "NO_KEYS",
            trial_duration: 5000
        },
    ],
    timeline_variables: condition[0] == "Factor-Included" ? avail_included_stimuli : avail_excluded_stimuli,
    randomize_order: false
};

var more_men = null;
var avail_question = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p> After the Prolific user had seen both lists, they answered the following question:</p><p><b>Which list contained more men?</b></p><br>The Prolific user selected ` + observedList + `. <br><br> To demonstrate that you understand the Prolific user's choice, <b>please select the option that they selected</b> (regardless of your own beliefs).<br><br>`,
    choices: ["List 1", "List 2"],
    enable_button_after: observerTime,
    correct_response: observedList == "List 1" ? 0 : 1,
    on_finish: function (data) {
        rt_main_question = data.rt;
        more_men = data.response;
    }
};

var avail_openQ_response = null;
var avail_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, the Prolific user was shown two lists of names and asked to determine which list contained more men.</p><p>What do you think was their thought process behind their decision about which list contained more men? How do you think they came to their eventual decision?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        avail_openQ_response = data.response.Q0;
    }
};


var introspection_q_labels_avail1 = [`<strong>It made them more likely to think that the <u>SECOND</u> list (where all the women were famous) contained more men</strong>`, "", "<strong>It did not affect their response</strong>", "", `<strong>It made them more likely to think that the <u>FIRST</u> list (where all the men were famous) contained more men</strong>`];

var introspection_q_labels_avail2 = [`<strong>It would have made me more likely to think that the <u>SECOND</u> list (where all the women were famous) contained more men</strong>`,"","<strong>It would not have affected my response</strong>", "",`<strong>It would have made me more likely to think that <u>FIRST</u> list (where all the men were famous) contained more men</strong>`];
var label_order_randomized = function() {
    return Math.random() < 0.5 ? 'original' : 'flipped';
};
var avail_intro_response1 = null;
var avail_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In this exercise, the Prolifc user was asked to memorize two lists of names. Some of the names on these lists belonged to very famous people, while others belonged to people who are not famous.</p>
        <p>Specifically, in the <b>first list</b>, all of the <i>men</i> were very famous, while all of the women were not famous. However, in the <b>second list</b>, all of the <i>women</i> were very famous while all of the men were not famous.</p>
        <p>Do you think the <b>fame of the people in each list</b> affected their response about which list contained more men? If so, how?`
        } else {
            return `<p>In this exercise, you were asked to memorize two lists of names. None of the names on the list you saw belonged to very famous people.</p>
        <p>Now, imagine if some of the names on each list had belonged to very famous people. For example, imagine that in the <b>first list</b>, all of the <i>men</i> were very famous, while all of the women were not famous. And imagine that in the <b>second list</b>, all of the <i>women</i> were very famous while all of the men were not famous.</p>
        <p>If this were the case, do you think the <b>fame of the people in each list</b> would have affected your response about which list contained more men? If so, how?`
        }
    },
    labels: function() {

        if (condition[0] == 'Factor-Included' && label_order_randomized == 'original') {
            return introspection_q_labels_avail1;
        } else if (condition[0] == 'Factor-Included' && label_order_randomized == 'flipped') {
            return introspection_q_labels_avail1.slice().reverse();
        } else if (condition[0] == 'Factor-Excluded' && label_order_randomized == 'original') {
            return introspection_q_labels_avail2;
        } else {
            return introspection_q_labels_avail2.slice().reverse();
        }
    },
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br>",
    on_finish: function (data) {
        rt_introspection_question = data.rt;
        if (label_order_randomized == 'original') {
            avail_intro_response1 = data.response
    }
        else {
            avail_intro_response1 = 100 - data.response;
            }
        }

};

var avail_intro_response2 = null;
var avail_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you give the rating that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        avail_intro_response2 = data.response.Q0
    }
};

var avail_intro_confidence_response = null;
var avail_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        avail_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            observer_or_actor: observer_or_actor,
            factor: data.condition,
            task_name: "availability",
            condition: condition[0] == "Factor-Included" ? "Famous" : "Unfamous",
            stimulus: null,
            choice: more_men == 0 ? "List 1" : "List 2",
            flipped_scale: label_order_randomized,
            auxiliary_info1:  null,
            openq_response: avail_openQ_response,
            introspect_rating: avail_intro_response1,
            introspect_open: avail_intro_confidence_response,
            familiarity: familiarity,
            rt_main_question: rt_main_question,
            rt_introspection_question: rt_introspection_question
        }
        console.log("hello!", s1_data);
        save_data(s1_data, 'introspection');
    }
};

var familiarity = null;
var avail_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity= data.response == 0 ? "Yes" : "No"
    }
}


if (only_main_question) {
    var avail = {
        timeline: [avail_instructions, avail_trials, avail_question]    
    };
} else {
    var avail = {
        timeline: [avail_instructions, avail_trials, avail_question, avail_familiar, avail_openQ, avail_introspect1, avail_intro_confidence]
    };
}

/*var avail_excluded = {
  timeline: [avail_instructions, avail_trials, avail_question, avail_openQ, avail_introspect1, avail_introspect2, avail_familiar]
}*/
//#endregion
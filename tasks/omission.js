//#region 4. omission Principle - BETWEEN

var confidence_q = '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by whether or not Ellen told the police that Peter was at fault)?</p>';


var omission_instructions = {
    type: jsPsychInstructions,
    pages: [
        `<p> In this exercise, you will be given a hypothetical scenario and then asked to judge the morality of one of the actors in that scenario.</p>
<p><i>Please click the button below to view the scenario.</i>`,
    ],
    show_clickable_nav: true
}

var choice = null;
var omission_question = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>Peter, a resident of Ohio, is driving through a small town in South Carolina. At a 4-way stop, he gets into a small accident with a town resident named Lyle. The accident came about like this:     

<p> Traveling north, Lyle approached the 4-way stop and failed either to slow down or to stop. Meanwhile, Peter had just finished stopping and began to move east through this intersection. Peter noticed that a car, Lyle's, was crossing the intersection after having failed to stop. Peter slammed on his brakes, but too late to prevent his car from hitting Lyle's car as it passed in front of him. The accident was clearly Lyle's fault, because the accident was caused by his failure to stop. However, because the accident's cause is not clear from its effects, the police may believe that Peter failed to stop and that caused Peter to run into Lyle's car broadside.  

<p> Immediately after the accident, both men exclaimed that it was the other's fault. When the police came. Peter told them that the accident was caused by Lyle's failure to stop. Lyle told the police that the accident was caused by Peter's failure to stop.  

<p> Unknown to either man, there was an eyewitness to the accident, Ellen. Like Lyle, Ellen is a town resident. She thought to herself, "I know the accident is Lyle's fault, but I know Lyle and do not wish him to be punished. The only way that Lyle will be faulted by the police is if I testify that the accident is indeed Lyle's fault."  <b> Ellen told the police that the accident was caused by Peter's failure to stop. </b> Peter is charged with failure to stop and fined.    
            <p><i>Please rate Ellen's morality in the situation on the given scale:     
-100 (as immoral as possible to be in this situation) to 0 (not immoral at all)
<i><p>(Slide instructions: Please move the slider to one point of your choice)
</i></p>`
        } else {
            return `<p>Peter, a resident of Ohio, is driving through a small town in South Carolina. At a 4-way stop, he gets into a small accident with a town resident named Lyle. The accident came about like this:     

<p> Traveling north, Lyle approached the 4-way stop and failed either to slow down or to stop. Meanwhile, Peter had just finished stopping and began to move east through this intersection. Peter noticed that a car, Lyle's, was crossing the intersection after having failed to stop. Peter slammed on his brakes, but too late to prevent his car from hitting Lyle's car as it passed in front of him. The accident was clearly Lyle's fault, because the accident was caused by his failure to stop. However, because the accident's cause is not clear from its effects, the police may believe that Peter failed to stop and that caused Peter to run into Lyle's car broadside.  

<p> Immediately after the accident, both men exclaimed that it was the other's fault. When the police came. Peter told them that the accident was caused by Lyle's failure to stop. Lyle told the police that the accident was caused by Peter's failure to stop.  

<p> Unknown to either man, there was an eyewitness to the accident, Ellen. Like Lyle, Ellen is a town resident. She thought to herself, "I know the accident is Lyle's fault, but I know Lyle and do not wish him to be punished. The only way that Lyle will be faulted by the police is if I testify that the accident is indeed Lyle's fault."  

<p>Ellen <b> told the police nothing. </b> Peter is charged with failure to stop and fined.     
            <p><i>Please rate Ellen's morality in the situation on the given scale:     
-100 (as immoral as possible to be in this situation) to 0 (not immoral at all)
<i><p>(Slide instructions: Please move the slider to one point of your choice)
</i></p>`
        }
    },
    labels: ['-100 (as immoral as possible to be in this situation', '0 (not immoral at all)'],
    slider_width: introspection_q_slider_width,
    min: -100,
    max: 0,
    slider_start: -50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br><br>",    
    on_finish: function (data) {
        choice = data.response
    }
}

var omission_openQ_response = null;
var omission_openQ = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>In this exercise, you were asked to judge the morality of an actor in a hypothetical scenario.</p><p>Describe your thought process while judging whether the actor was moral in the situation. How did you come to your eventual judgment?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        omission_openQ_response = data.response.Q0;
    }
};

var introspection_q_labels_omission1 = [`<strong>It made me <u>LESS</u> likely to judge Ellen as immoral</strong>`, "", "<strong>It did not affect my response</strong>", "", `<strong>It made me <u>MORE</u> likely to judge Ellen as immoral</strong>`];
var introspection_q_labels_omission2 = [`<strong>It would have made me <u>LESS</u> likely to judge Ellen as immoral</strong>`, "", "<strong>It would not have affected my response</strong>", "", `<strong>It would have made me <u>MORE</u> likely to judge Ellen as immoral</strong>`];

var omission_intro_response1 = null;
var omission_introspect1 = {
    type: jsPsychHtmlSliderResponse,
    stimulus: function () {
        if (condition[0] == "Factor-Included") {
            return `<p>In the scenario you saw, Ellen lied and told the police that the accident was Peter's fault.</p>
                <p>Do you think the fact that Ellen <b>told the police Peter was at fault as opposed to remaining silent</b> affected your judgment about Ellen's morality in the situation? If so, how?`
        } else {
            return `<p>In the scenario you saw, Ellen said nothing to the police.
            <p>Now, imagine if Ellen <i>had instead told the police that the accident was Peter's fault</i> 
            <p>Do you think the fact that Ellen <b>told the police Peter was at fault as opposed to remaining silent</b> would have affected your judgment about Ellen's morality in the situation? If so, how?`
        }
    },
    labels: condition[0] == 'Factor-Included' ? introspection_q_labels_omission1 : introspection_q_labels_omission2,
    slider_width: introspection_q_slider_width,
    min: introspection_q_min,
    max: introspection_q_max,
    slider_start: 50,
    require_movement: introspection_q_require,
    prompt: "<br><br><br><br><br><br>",
    on_finish: function (data) {
        omission_intro_response1 = data.response
    }
};

var omission_intro_response2 = null;
var omission_introspect2 = {
    type: jsPsychSurveyText,
    questions: [{
        prompt: `<p>Please describe your thought process behind the answer you gave in the previous question. Why did you choose the number that you did?</p>`,
        required: required_general, rows: 5, columns: 80
    }],
    on_finish: function (data) {
        omission_intro_response2 = data.response.Q0
    }
};

var omission_intro_confidence_response = null;
var omission_intro_confidence = {
    type: jsPsychHtmlSliderResponse,
    stimulus: confidence_q,
    labels: confidence_q_labels,
    slider_width: confidence_q_slider_width,
    min: confidence_q_min,
    max: confidence_q_max,
    slider_start: 50,
    require_movement: require_movement_general,
    on_finish: function (data) {
        omission_intro_confidence_response = data.response;
        s1_data = {
            subject: data.subject,
            version: data.version,
            factor: data.condition,
            task_name: "omission principle",
            condition: condition[0] == "Factor-Included" ? "commission" : "omission",
            stimulus: null,
            choice: choice,
            auxiliary_info1: null,
            openq_response: omission_openQ_response,
            introspect_rating: omission_intro_response1,
            introspect_open: omission_intro_confidence_response,
            familiarity: familiarity,
            rt: data.rt
        }
        save_data(s1_data, 'introspection')
    }
};

var familiarity = null;
var omission_familiar = {
    type: jsPsychHtmlButtonResponse,
    stimulus: familiarity_prompt,
    choices: ["Yes", "No"],
    on_finish: function (data) {
        familiarity = data.response == 0 ? "Yes" : "No"
    }
}


var omission = {
    timeline: [omission_instructions, omission_question, omission_familiar, omission_openQ, omission_introspect1, omission_intro_confidence]
}

//#endregion
//timeline.push(omission)
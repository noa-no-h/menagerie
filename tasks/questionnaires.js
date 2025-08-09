

agreement_scale_labels = ["Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"];

frequency_scale_labels = ["Rarely/Not at all", "Sometimes", "Often", "Almost Always"];

const reverse_coded_items = ['esr1', 'esr2', 'esr4',
    'nsr1',
    'insight2', 'insight4', 'insight5', 'insight6', 'insight7',
    'decision6', 'decision7', 'decision8', 'decision9', 'decision10',
    'nfc3', 'nfc4', 'nfc5', 'nfc7', 'nfc8', 'nfc9', 'nfc12', 'nfc16', 'nfc17',
    'cam2','cam6','cam7'];


engagement_in_self_reflection_questions = [`<p>I don't often think about  my thoughts</p>`,
    `<p>I rarely spend time in self-reflection</p>`,
    `I frequently examine my feelings`,
    `I don't really think about why I behave in the way that I do`, `I frequently take time to  reflect on my thoughts`,
    `I often think about the  way I feel about things`];

need_for_self_reflection_questions = [`I am not really interested  in analyzing my behaviour`,
    `It is important for me  to evaluate the things that I do`,
    `I am very interested in  examining what I think about`,
    `It is important to me  to try to understand what my feelings mean`,
    `I have a definite need to understand the way that my mind works`,
    `It is important to me  to be able to understand how my thoughts arise`];

insight_questions = [`I am usually aware of  my thoughts`,
    `I'm often confused about the  way that I really feel about things`,
    `I usually have a very  clear idea about why I've behaved in a certain way`,
    `I'm often aware that I'm having a feeling, but I often don't quite know what it is`,
    `My behavior often puzzles me`,
    `Thinking about my thoughts makes me more confused`,
    `Often I find it difficult to make sense of the way I feel about things`,
    `I usually know why I  feel the way I do`];

decision_scale_questions = [` I prefer to gather all the necessary information before committing to a decision.`,
    `I thoroughly evaluate decision alternatives before making a final choice.`,
    `In decision making, I take time to contemplate the pros/cons or risks/benefits of a situation.`,
    `Investigating the facts is an important part of my decision making process.`,
    `I weigh a number of different factors when making decisions.`, `When making decisions, I rely mainly on my gut feelings.`, `My initial hunch about decisions is generally what I follow.`, `I make decisions based on intuition.`, `I rely on my first impressions when making decisions.`, `I weigh feelings more than analysis in making decisions.`];

need_for_cognition_questions = [`I would prefer complex to simple problems.`, `I like to have the responsibility of handling a situation that requires a lot of thinking.`, `Thinking is not my idea of fun.`, `I would rather do something that requires little thought than something that is sure to challenge my thinking abilities.`, `I try to anticipate and avoid situations where there is likely chance I will have to think in depth about something.`, `I find satisfaction in deliberating hard and for long hours.`, `I only think as hard as I have to.`, `I prefer to think about small, daily projects to long-term ones.`, `I like tasks that require little thought once I've learned them.`, `The idea of relying on thought to make my way to the top appeals to me.`, `I really enjoy a task that involves coming up with new solutions to problems.`, `Learning new ways to think doesn't excite me very much.`, `I prefer my life to be filled with puzzles that I must solve.`, `The notion of thinking abstractly is appealing to me.`, `I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought.`, `I feel relief rather than satisfaction after completing a task that required a lot of mental effort.`, `It's enough for me that something gets the job done; I don't care how or why it works.`, `I usually end up deliberating about issues even when they do not affect me personally.`];

cognitive_affective_mindfulness_questions = [`It is easy for me to concentrate on what I am doing.`,
    `I can tolerate emotional pain.`,
    `I can accept things I cannot change.`,
    `I can usually describe how I feel at the moment in considerable detail.`,
    `I am easily distracted.`,
    `It’s easy for me to keep track of my thoughts and feelings.`,
    `I try to notice my thoughts without judging them.`,
    `I am able to accept the thoughts and feelings I have.`,
    `I am able to focus on the present moment.`,
    `I am able to pay close attention to one thing for a long period of time.`];

const processed_scores = {};
function process_scores(data_list, max_value = 4) {

    const local_processed_scores = {};

    for (const question_name in data_list) {
        let raw_score = data_list[question_name];
        let final_score = raw_score;
        if (reverse_coded_items.includes(question_name)) {
            
            final_score = max_value - raw_score;
        }
        local_processed_scores[question_name] = final_score + 1;
    }
    console.log("Processed Scores:", local_processed_scores);
    return local_processed_scores;

}

engagement_in_self_reflection_data = {};
var engagement_in_self_reflection = {
    type: jsPsychSurveyLikert,
    questions: engagement_in_self_reflection_questions.map((prompt_text, index) => {
        return {
            prompt: prompt_text,
            name: `esr${index + 1}`,
            labels: agreement_scale_labels,
            required: true
        };
    }),
    randomize_question_order: true,
    on_finish: function (data) {
        engagement_in_self_reflection_data = process_scores(data.response);
    }
};

need_for_self_reflection_data = {};

var need_for_self_reflection = {
    type: jsPsychSurveyLikert,
    questions: need_for_self_reflection_questions.map((prompt_text, index) => {
        return {
            prompt: prompt_text,
            name: `nsr${index + 1}`,
            labels: agreement_scale_labels,
            required: true
        };
    }),
    randomize_question_order: true,
    on_finish: function (data) {
        need_for_self_reflection_data = process_scores(data.response);
    }
};

var insight_questionnaire_data = {};
var insight_questionnaire = {
    type: jsPsychSurveyLikert,
    questions: insight_questions.map((prompt_text, index) => {
        return {
            prompt: prompt_text,
            name: `insight${index + 1}`,
            labels: agreement_scale_labels,
            required: true
        };
    }),
    randomize_question_order: true,
    on_finish: function (data) {
        insight_questionnaire_data = process_scores(data.response);
    }
};

decision_scale_data = {};
var decision_scale = {
    type: jsPsychSurveyLikert,
    questions: decision_scale_questions.map((prompt_text, index) => {
        return {
            prompt: prompt_text,
            name: `decision${index + 1}`,
            labels: agreement_scale_labels,
            required: true
        };
    }),
    randomize_question_order: true,
    on_finish: function (data) {
        decision_scale_data = process_scores(data.response);
    }
};

need_for_cognition_data = {};
var need_for_cognition = {
    type: jsPsychSurveyLikert,
    questions: need_for_cognition_questions.map((prompt_text, index) => {
        return {
            prompt: prompt_text,
            name: `nfc${index + 1}`,
            labels: agreement_scale_labels,
            required: true
        };
    }),
    randomize_question_order: true,
    on_finish: function (data) {
        need_for_cognition_data = process_scores(data.response);
    }
};

cognitive_affective_mindfulness_scale_data = {};
var cognitive_affective_mindfulness_scale = {
    type: jsPsychSurveyLikert,
    questions: cognitive_affective_mindfulness_questions.map((prompt_text, index) => {
        return {
            prompt: prompt_text,
            name: `cam${index + 1}`,
            labels: frequency_scale_labels,
            required: true
        };
    }),
    randomize_question_order: true,
    on_finish: function(data) {
        const parsed_responses = data.response;

        cognitive_affective_mindfulness_scale_data = process_scores(parsed_responses, 3);
    }
};



questionnaires_timeline = [cognitive_affective_mindfulness_scale, engagement_in_self_reflection, need_for_self_reflection, insight_questionnaire, decision_scale, need_for_cognition, ];

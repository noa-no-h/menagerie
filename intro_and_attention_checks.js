jsPsych.plugins["intro_and_attention_checks"] = (function() {

    var plugin = {};
  
    plugin.info = {
      name: 'intro_and_attention_checks',
      parameters: {}
    };
  
    plugin.trial = function(display_element, trial) {
      // Setup
      var version = "v4_sona";
  
      // Find the assignmentId & debugging status from the URL
      var urlParams = parseURLParams(window.location.href);
      var subject_id_url = '';
      var debug = false;
      if (typeof urlParams != "undefined") {
        if (urlParams.hasOwnProperty('PROLIFIC_PID')) {
          subject_id_url = urlParams.PROLIFIC_PID[0];
        }
  
        if (urlParams.hasOwnProperty('debug')) {
          debug = urlParams.debug[0];
        }
      }
  
      // Initialize jsPsych
      var jsPsych = initJsPsych({
        show_progress_bar: false,
        on_interaction_data_update: function(data) {
          if (id != '') {
            var browser_event = {
              subject: id,
              version: version,
              browser_event: data.event,
              trial: data.trial,
              time: data.time
            }
            save_data(browser_event, 'browser_events');
          }
        }
      });
  
      // Create timeline
      var timeline = [];
      var condition = jsPsych.randomization.sampleWithoutReplacement(["Factor-Included", "Factor-Excluded"], 1);
  
      // Variables & stimuli
      var images = [
        'img/redbox.png', 'img/bluebox.png',
        'img/12.png', 'img/13.png', 'img/14.png', 'img/21.png', 'img/23.png', 'img/24.png', 'img/26.png', 'img/31.png', 'img/32.png', 'img/34.png', 'img/36.png', 'img/37.png', 'img/41.png', 'img/42.png', 'img/43.png', 'img/46.png', 'img/47.png', 'img/48.png',
        'img/62.png', 'img/63.png', 'img/64.png', 'img/67.png', 'img/68.png', 'img/69.png', 'img/73.png', 'img/74.png', 'img/76.png', 'img/78.png', 'img/79.png', 'img/84.png', 'img/86.png', 'img/87.png', 'img/89.png', 'img/96.png', 'img/97.png', 'img/98.png',
        'img/ned1.png', 'img/ned2.png',
        'img/oneball.png', 'img/nineballs.png',
      ];
  
      var introspection_q_slider_width = 750;
      var introspection_q_min = 0;
      var introspection_q_max = 100;
      var introspection_q_require = false;
      
      var confidence_q = '<p>How confident are you that you gave the correct answer to the previous question (i.e., that you correctly reported the way you were influenced by the factor described)?</p>';
      var confidence_q_labels = [`Not at all confident`, "", "", "Moderately confident", "", "", `Very confident`];
      var confidence_q_slider_width = 750;
      var confidence_q_min = 0;
      var confidence_q_max = 100;
  
      var require_movement_general = false;
      var required_general = debug ? false : true;
  
      // Preload
      var preload = {
        type: jsPsychPreload,
        images: images
      };
      timeline.push(preload);
  
      // Introduction
      var check_consent = function(elem) {
        if (document.getElementById('consent_checkbox').checked && document.getElementById('age_checkbox').checked) {
          return true;
        }
        else {
          alert("In order to participate, you must select 'Yes' for both checkboxes. Otherwise, please exit the study.");
          return false;
        }
        return false;
      };
  
      var consent_block = {
        type: jsPsychExternalHtml,
        url: "consent-form.html",
        cont_btn: "start",
        check_fn: check_consent,
        force_refresh: true
      };
      timeline.push(consent_block);
  
      var start_time = null;
      var welcome = {
        type: jsPsychInstructions,
        pages: [
          `<p><b>Welcome to the experiment.</b></p> <p>Click next to begin.</p>`,
          `<p>You must be 18 years or older to complete this study. If you under 18, please exit now.`,
          `<p>This study will take around 60 minutes to complete. You must complete the experiment in one sitting.
          <b>So, before you start, please make sure that you have one hour to devote entirely
          to this study.</b> We appreciate your participation.</p>`,
          `<p>On the next page, you'll find a consent form. Please read it and select "Yes" if you wish to participate in the study.`,
        ],
        show_clickable_nav: true,
        on_finish: function() {
          start_time = Date.now();
        }
      };
      timeline.push(welcome);
  
      var MTurk_id = {
        type: jsPsychSurveyText,
        questions: [{ prompt: "Your response", name: "worker_id" }],
        preamble: [`Please enter your Amazon Mechanical Turk Worker ID below.<br>(When you're done, press 'Submit Answer'; don't hit Enter.)<br><br>If
        you do not enter your ID accurately, we will not be able to pay you.`],
        button_label: "Submit Answer"
      };
  
      var id = getRandomInt(1, 9999999);
      jsPsych.data.addProperties({
        subject: id,
        version: version,
        condition: condition[0]
      });
  
      var MTurk_id_loop = {
        timeline: [MTurk_id],
        loop_function: function(data) {
          id = data.last(1).values()[0].response.worker_id;
          if (id) {
            jsPsych.data.addProperties({
              subject: id,
              version: version,
              condition: condition[0]
            });
            start_time = Date.now();
            return false;
          } else {
            alert("You must enter your ID to continue.");
            return true;
          }
        }
      };
      timeline.push(MTurk_id_loop);
  
      // Attention checks
      var attnCheck1 = {
        type: jsPsychSurveyText,
        questions: [{ prompt: "Your response" }],
        preamble: [`If you are certain you would like to participate, please type this <i>exact</i>
        sentence into the box below: "I will complete this study with my full attention"
        and then continue.`]
      };
  
      var attnCheck1_loop = {
        timeline: [attnCheck1],
        loop_function: function(data) {
          var attnResponse1 = data.last(1).values()[0].response.Q0;
          if (attnResponse1 == "I will complete this study with my full attention" || attnResponse1 == '"I will complete this study with my full attention"') {
            return false;
          } else {
            alert("Please read the instructions and try again");
            return true;
          }
        }
      };
      timeline.push(attnCheck1_loop);
  
      var attnCheck2 = {
        type: jsPsychSurveyText,
        questions: [{ prompt: "Your response", required: false }],
        preamble: [`
        <p>Please read the bolded statement below and then type it in the box in reverse order. Please include any capitalizations in the words that have capital letters. Do not include any punctuation (e.g., periods, quotation marks, etc.).</p>
        <p>For example, if the statement said "fun are trucks Red," you would type the following phrase: Red trucks are fun</p>
        <p><b>cheese green of made is moon The<b></p>
        `],
        button_label: "Submit Answer",
        on_finish: function(data) {
          s1_data = {
            subject: data.subject,
            version: data.version,
            task_name: "attention check 2",
            choice: data.response.Q0,
            auxiliary_info1: data.response.Q0.replace(/\s+/g, '').toLowerCase() == "themoonismadeofgreencheese" ? "Success" : "Failure",
            rt: data.rt
          };
          save_data(s1_data, 'introspection');
        }
      };
      timeline.push(attnCheck2);
  
      var movie = '';
      var attnCheck3 = {
        type: jsPsychSurveyText,
        questions: [
          { prompt: "My Blue Heaven", correct_response: "3", required: false },
          { prompt: "Up", correct_response: "5", required: false },
          { prompt: "Along Came Polly", correct_response: "1", required: false },
          { prompt: "The Tale of Desperaux", correct_response: "4", required: false },
          { prompt: "Jaws", correct_response: "2", required: false },
        ],
        preamble: `<p>Please order the following movie titles alphabetically (by the first letter of the movie titles) by numbering them from 1 to 5. 1 should be the movie title that appears earliest in the alphabet, while 5 should be the movie title that appears the latest in the alphabet.</p>
        <p>Please only type in one number for each movie title. Do not include any punctuation, letters, symbols, or any numbers except for 1, 2, 3, 4, and 5.</p>`,
        button_label: "Submit Answer",
        on_finish: function(data) {
          movie = data.response.Q0 + "," + data.response.Q1 + "," + data.response.Q2 + "," + data.response.Q3 + "," + data.response.Q4;
          s1_data = {
            subject: data.subject,
            version: data.version,
            task_name: "attention check 3",
            choice: movie,
            auxiliary_info1: movie == "3,5,1,4,2" ? "Correct" : "Incorrect",
            rt: data.rt
          };
          save_data(s1_data, 'introspection');
        }
      };
      timeline.push(attnCheck3);
  
      // Start page
      var begin = {
        type: jsPsychInstructions,
        pages: [
          `<p>Thank you for beginning the study! In the first phase of this study, you will be asked to complete a variety of short judgment/decision-making tasks. Then, in the second phase, you will be asked to answer a series of short questionnaires.</p>`,
          `<p>While you're completing the decision-making tasks, please <b>pay attention to how you're making your judgments/decisions</b>.</p>`,
          `<p>The first phase will take around 25-35 minutes to complete, while the second phase will take around 15-25 minutes to complete, for a total of 50-60 minutes.</p>
          <p>We will now begin with the first phase of the study. Please click the button below to continue!</p>
        `],
        show_clickable_nav: true
      };
      timeline.push(begin);
  
      // Initialize and start the experiment
      jsPsych.init({
        timeline: timeline,
        on_finish: function() {
          jsPsych.data.displayData();
        }
      });
    };
  
    function parseURLParams(url) {
      var queryStart = url.indexOf("?") + 1,
        queryEnd = url.indexOf("#") + 1 || url.length + 1,
        query = url.slice(queryStart, queryEnd - 1),
        pairs = query.replace(/\+/g, " ").split("&"),
        parms = {},
        i, n, v, nv;
  
      if (query === url || query === "") return;
  
      for (i = 0; i < pairs.length; i++) {
        nv = pairs[i].split("=", 2);
        n = decodeURIComponent(nv[0]);
        v = decodeURIComponent(nv[1]);
  
        if (!parms.hasOwnProperty(n)) parms[n] = [];
        parms[n].push(nv.length === 2 ? v : null);
      }
      return parms;
    }
  
    function save_data(data, filename) {
      var xhr = new XMLHttpRequest();
      xhr.open('POST', 'save_data.php');
      xhr.setRequestHeader('Content-Type', 'application/json');
      xhr.send(JSON.stringify({filename: filename, filedata: data}));
    }
  
    function getRandomInt(min, max) {
      min = Math.ceil(min);
      max = Math.floor(max);
      return Math.floor(Math.random() * (max - min + 1)) + min;
    }
  
    return plugin;
  })();
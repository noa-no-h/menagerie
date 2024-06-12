var _minimalPlugin_ = (function (jspsych) {
    "use strict";
  
    const info = {
      name: "{_minimalPlugin_}",
      parameters: {
        parameter_name: {
          type: jspsych.ParameterType.INT,
          default: undefined,
        },
        parameter_name2: {
          type: jspsych.ParameterType.IMAGE,
          default: undefined,
        },
      },
    };
  
    class MinimalPlugin {
      constructor(jsPsych) {
        this.jsPsych = jsPsych;
      }
      trial(display_element, trial) {
        // data saving
        var trial_data = {
          parameter_name: "parameter value",
        };
        console.log(parameter_name);

        
        // end trial
        this.jsPsych.finishTrial(trial_data);
      }
    }
    MinimalPlugin.info = info;
  
    return MinimalPlugin;
  })(jsPsychModule);
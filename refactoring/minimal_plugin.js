var intro_and_attention_checks = (function(jsPsych) {
    console.log(jsPsych.ParameterType);
  
    const info = {
      name: "intro_and_attention_checks",
      description:"",
      parameters: {
        version: {
          type: jsPsych.ParameterType.STRING,
          pretty_name: "Version",
          default: "v4_sona",
        }
      },
    };
    
    jsPsych.plugins["intro_and_attention_checks"] = (function () {
      const plugin = {};
    
      plugin.info = info;
    
      plugin.trial = function (display_element, trial) {};
    
  
    return plugin;
    })();
  })(jsPsych);
  
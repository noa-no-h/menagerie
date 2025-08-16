/*
 * jsPsych plugin for recalling a sequence of digits using a numpad.
 *
 */


var jsPsychDigitSpanRecall = (function (jspsych) {
  'use strict';

  const info = {
    name: 'digit-span-recall',
    description: 'A jsPsych plugin for a digit span recall task.',
    parameters: {
      /** How long to show the trial in milliseconds. If null, the trial will wait for the user to click 'Continue'. */
      trial_duration: {
        type: jspsych.ParameterType.INT,
        pretty_name: 'Trial duration',
        default: null
      },
      /** The width and height of each cell of the numpad in pixels. */
      size_cells: {
        type: jspsych.ParameterType.INT,
        pretty_name: 'Size of cells',
        default: 80
      },
      /** The correct sequence of digits for the trial. */
      correct_order: {
        type: jspsych.ParameterType.INT,
        pretty_name: 'Correct order',
        default: undefined,
        array: true
      }
    },
    data: {
      /** The response time in milliseconds, measured from when the trial began to when the user clicked 'Continue'. */
      rt: {
        type: jspsych.ParameterType.INT
      },
      /** An array containing the sequence of digits recalled by the participant. */
      recall: {
        type: jspsych.ParameterType.INT,
        array: true
      },
      /** An array containing the correct sequence of digits. */
      stimuli: {
        type: jspsych.ParameterType.INT,
        array: true
      },
      /** An integer (1 or 0) indicating whether the participant's response was correct. */
      accuracy: {
        type: jspsych.ParameterType.INT
      }
    }
  };

  class DigitSpanRecallPlugin {
    constructor(jsPsych) {
      this.jsPsych = jsPsych;
    }

    static {
      this.info = info;
    }

    trial(display_element, trial) {

      // --- State variables ---
      let recalledGrid = [];
      let startTime = -1;

      // --- HTML and CSS for the display ---
      let html = `<style>
        .jspsych-digit-span-recall-container {
          display: flex;
          flex-direction: column;
          align-items: center;
        }
        .jspsych-digit-span-recall-display {
          display: flex;
          align-items: center;
          justify-content: center;
          border: 1px solid #aaa;
          border-radius: 5px;
          width: 300px;
          height: 60px;
          font-size: 28px;
          text-align: center;
          padding: 10px;
          margin-bottom: 25px;
          letter-spacing: 3px;
        }
        .jspsych-digit-span-recall-numpad {
          display: grid;
          grid-template-columns: repeat(3, ${trial.size_cells}px);
          grid-gap: 8px;
        }
        .jspsych-digit-span-recall-numpad button {
          width: ${trial.size_cells}px;
          height: ${trial.size_cells}px;
          font-size: 24px;
        }
        .jspsych-digit-span-recall-numpad .zero-button {
          grid-column: 2; /* Center the '0' button on the last row */
        }
        .jspsych-digit-span-recall-controls {
          margin-top: 20px;
        }
        .jspsych-digit-span-recall-controls button {
            margin: 0 10px;
        }
      </style>`;

      html += `<div class="jspsych-digit-span-recall-container">
        <div id="recall-space" class="jspsych-digit-span-recall-display">&nbsp;</div>
        <div id="numpad" class="jspsych-digit-span-recall-numpad"></div>
        <div class="jspsych-digit-span-recall-controls">
          <button id="backspace-btn" class="jspsych-btn">Backspace</button>
          <button id="continue-btn" class="jspsych-btn">Continue</button>
        </div>
      </div>`;

      display_element.innerHTML = html;

      const numpadElement = display_element.querySelector("#numpad");
      const recallSpaceElement = display_element.querySelector("#recall-space");

      // --- Numpad Button Generation ---
      const buttonLabels = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"];
      for (const label of buttonLabels) {
        const button = document.createElement('button');
        button.textContent = label;
        button.dataset.digit = label;
        button.classList.add('jspsych-btn');
        if (label === '0') {
          button.classList.add('zero-button');
        }
        numpadElement.appendChild(button);
      }

      // --- Function to end the trial ---
      const end_trial = () => {
        // kill any remaining setTimeout handlers
        this.jsPsych.pluginAPI.clearAllTimeouts();

        // kill all event listeners
        numpadElement.removeEventListener('click', handleNumpadClick);
        display_element.querySelector('#backspace-btn').removeEventListener('click', handleBackspaceClick);
        display_element.querySelector('#continue-btn').removeEventListener('click', handleContinueClick);

        // Calculate accuracy
        let accuracy = 0;
        if (trial.correct_order && trial.correct_order.length === recalledGrid.length) {
          accuracy = 1; // Assume correct until a mismatch is found
          for (let i = 0; i < recalledGrid.length; i++) {
if (String(recalledGrid[i]) !== String(trial.correct_order[i])) {
              accuracy = 0;
              break;
            }
          }
        }

        // gather the data to store for the trial
        const trial_data = {
          rt: Math.round(performance.now() - startTime),
          recall: recalledGrid,
          stimuli: trial.correct_order,
          accuracy: accuracy
        };

        // clear the display
        display_element.innerHTML = '';

        // move on to the next trial
        this.jsPsych.finishTrial(trial_data);
      }

      // --- Event Handler Functions ---
      const handleNumpadClick = (e) => {
        if (e.target.tagName === 'BUTTON' && e.target.dataset.digit) {
          const digit = parseInt(e.target.dataset.digit);
          recalledGrid.push(digit);
          recallSpaceElement.textContent = recalledGrid.join(" ");
        }
      };

      const handleBackspaceClick = () => {
        recalledGrid.pop();
        const newText = recalledGrid.join(" ");
        recallSpaceElement.textContent = newText === "" ? String.fromCharCode(160) : newText; // Use &nbsp; to maintain height
      };
      
      const handleContinueClick = () => {
        end_trial();
      };

      // --- Add Event Listeners ---
      numpadElement.addEventListener('click', handleNumpadClick);
      display_element.querySelector('#backspace-btn').addEventListener('click', handleBackspaceClick);
      display_element.querySelector('#continue-btn').addEventListener('click', handleContinueClick);

      // Start the trial timer
      startTime = performance.now();

      // --- Handle trial duration ---
      if (trial.trial_duration !== null) {
        this.jsPsych.pluginAPI.setTimeout(() => {
          end_trial();
        }, trial.trial_duration);
      }
    }

    simulate(trial, simulation_mode, simulation_options, load_callback) {
      if (simulation_mode == "data-only") {
        load_callback();
        this.simulate_data_only(trial, simulation_options);
      }
      if (simulation_mode == "visual") {
        this.simulate_visual(trial, simulation_options, load_callback);
      }
    }

    create_simulation_data(trial, simulation_options) {
      // Determine recalled sequence
      const recalled_sequence = [];
      const correct_sequence = trial.correct_order;
      const should_be_correct = this.jsPsych.randomization.sampleWithReplacement([true, false], 1, [0.8, 0.2])[0];
      
      if (should_be_correct) {
          recalled_sequence.push(...correct_sequence);
      } else {
          const random_length = this.jsPsych.randomization.randomInt(correct_sequence.length - 1, correct_sequence.length + 1);
          for(let i=0; i < random_length; i++){
              recalled_sequence.push(this.jsPsych.randomization.randomInt(0, 9));
          }
      }

      const default_data = {
        stimuli: correct_sequence,
        recall: recalled_sequence,
        rt: this.jsPsych.randomization.sampleExGaussian(1500 + correct_sequence.length * 250, 400, 1 / 200, true),
        accuracy: should_be_correct ? 1 : 0
      };

      const data = this.jsPsych.pluginAPI.mergeSimulationData(default_data, simulation_options);
      this.jsPsych.pluginAPI.ensureSimulationDataConsistency(trial, data);
      return data;
    }

    simulate_data_only(trial, simulation_options) {
      const data = this.create_simulation_data(trial, simulation_options);
      this.jsPsych.finishTrial(data);
    }

    simulate_visual(trial, simulation_options, load_callback) {
      const data = this.create_simulation_data(trial, simulation_options);
      const display_element = this.jsPsych.getDisplayElement();

      this.trial(display_element, trial);
      load_callback();

      let click_time = 0;
      // Simulate clicks on numpad
      for(const digit of data.recall){
        const delay = this.jsPsych.randomization.randomInt(200, 500);
        click_time += delay;
        this.jsPsych.pluginAPI.clickTarget(
            display_element.querySelector(`button[data-digit="${digit}"]`),
            click_time
        );
      }

      // Simulate click on continue button at the end
      this.jsPsych.pluginAPI.clickTarget(
        display_element.querySelector('#continue-btn'),
        data.rt
      );
    }
  }

  return DigitSpanRecallPlugin;

})(jsPsychModule);
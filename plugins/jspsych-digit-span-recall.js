/*
 * jsPsych plugin for recalling a sequence of digits using a numpad.
 * Updated for jsPsych 7.3.4
 */


jsPsych.plugins["digit-span-recall"] = (function() {

  class DigitSpanRecallPlugin {

    constructor(jsPsych) {
      this.jsPsych = jsPsych;
    }

    trial(display_element, trial) {
      // Return a promise to handle the trial asynchronous nature
      return new Promise((resolve, reject) => {

        // --- State Variables ---
        let recalledGrid = [];
        let displayString = "&nbsp;"; // Use &nbsp; for initial empty space

        // --- HTML Generation ---
        const paper_size = [(3 * trial.size_cells), (4 * trial.size_cells) + 80];
        let html = `<style>
          .jspsych-digit-span-recall-button {
            border: 1px solid #555;
            display: flex;
            justify-content: center;
            align-items: center;
            font-size: 1.5em;
            cursor: pointer;
            user-select: none; /* Prevent text selection */
            background-color: #f0f0f0;
          }
          .jspsych-digit-span-recall-button:hover {
            background-color: #ddd;
          }
          .recall-space {
            border: 1px solid #ccc;
            background-color: #fff;
            line-height: 64px;
            font-size: 2em;
            text-align: center;
            letter-spacing: 0.1em;
          }
        </style>`;

        // Main wrapper
        html += `<div id="jspsych-digit-span-recall-wrapper" style="position: relative; width:${paper_size[0]}px; height:${paper_size[1]}px;">`;

        // Display area for recalled digits
        html += `<div class="recall-space" style="position: absolute; top: 0px; left: 50%; transform: translateX(-50%); width: 90%; height: 64px;" id="recall_space">${displayString}</div>`;

        // --- Numpad Generation ---
        const buttons = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"];
        let matrix = [];
        for (let i = 0; i < 3; i++) {
          for (let h = 0; h < 3; h++) {
            matrix.push([i, h]); // 1-9 grid positions
          }
        }
        matrix.push([3, 1]); // '0' button position

        html += `<div id="jspsych-digit-span-recall-btngroup" style="position: relative; top: 80px; width: ${paper_size[0]}px; height: ${paper_size[1] - 140}px;">`;
        for (let i = 0; i < matrix.length; i++) {
          const button_style = `position: absolute; top:${matrix[i][0] * trial.size_cells}px; left:${matrix[i][1] * trial.size_cells}px; width:${trial.size_cells - 6}px; height:${trial.size_cells - 6}px;`;
          html += `<div class="jspsych-digit-span-recall-button" style="${button_style}" data-choice="${buttons[i]}">${buttons[i]}</div>`;
        }
        html += `</div>`; // Close btngroup

        // --- Control Buttons ---
        html += `<div id="jspsych-digit-span-recall-controls" style="position: absolute; bottom: 0; left: 50%; transform: translateX(-50%);">
                   <button class="jspsych-btn" style="margin-right: 10px;" id="jspsych-digit-span-recall-backspace">Backspace</button>
                   <button class="jspsych-btn" id="jspsych-digit-span-recall-continue">Continue</button>
                 </div>`;

        html += `</div>`; // Close wrapper
        display_element.innerHTML = html;

        const startTime = performance.now();
        const recallSpace = display_element.querySelector('#recall_space');

        // --- Helper Functions ---
        const updateDisplay = () => {
          // Join the array and add spaces, ensuring an empty space for a blank display
          recallSpace.innerHTML = recalledGrid.length > 0 ? recalledGrid.join(" ") : "&nbsp;";
        };

        // --- End Trial Function ---
        const end_trial = () => {
          // kill any remaining setTimeout handlers
          this.jsPsych.pluginAPI.clearAllTimeouts();

          // Calculate accuracy
          let accuracy = 0;
          // Ensure correct_order is defined before comparing
          if (trial.correct_order) {
             // Use JSON.stringify for a simple and robust array comparison
            if (JSON.stringify(trial.correct_order) === JSON.stringify(recalledGrid.map(Number))) {
              accuracy = 1;
            }
          } else {
            accuracy = null; // Set to null if no correct answer was provided
          }

          const endTime = performance.now();
          const rt = Math.round(endTime - startTime);

          // Gather the data to store for the trial
          const trial_data = {
            rt: rt,
            recalled_sequence: recalledGrid,
            correct_sequence: trial.correct_order,
            accuracy: accuracy
          };

          // Clear the display
          display_element.innerHTML = '';
          // Resolve the promise to end the trial
          resolve(trial_data);
        };

        // --- Event Listeners ---
        display_element.querySelectorAll('.jspsych-digit-span-recall-button').forEach(button => {
          button.addEventListener('click', (e) => {
            const choice = e.currentTarget.getAttribute('data-choice');
            recalledGrid.push(choice);
            updateDisplay();
          });
        });

        display_element.querySelector('#jspsych-digit-span-recall-backspace').addEventListener('click', () => {
          recalledGrid.pop();
          updateDisplay();
        });

        display_element.querySelector('#jspsych-digit-span-recall-continue').addEventListener('click', end_trial);

        // --- Optional Trial Duration ---
        if (trial.trial_duration !== null) {
          this.jsPsych.pluginAPI.setTimeout(end_trial, trial.trial_duration);
        }
      }); // end Promise
    } // end trial
  } // end class

  DigitSpanRecallPlugin.info = {
    name: 'digit-span-recall',
    description: 'A jsPsych plugin for recalling a sequence of digits using a numpad.',
    parameters: {
      trial_duration: {
        type: jsPsych.ParameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show the trial before it ends automatically.'
      },
      size_cells: {
        type: jsPsych.ParameterType.INT,
        pretty_name: 'Size of cells',
        default: 80,
        description: 'The height and width of each cell in the numpad grid.'
      },
      correct_order: {
        type: jsPsych.ParameterType.INT, // This will be an array of numbers
        pretty_name: 'Correct order',
        array: true,
        default: undefined,
        description: 'The correct sequence of digits to be recalled (e.g., [1, 5, 2]).'
      }
    }
  };

  return DigitSpanRecallPlugin;
})();
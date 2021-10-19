/* ************************************ */
/* Define helper functions */
/* ************************************ */

const id = localStorage.getItem('prolific');
const lastChar = id[id.length - 1];
const isLeftGroup = lastChar >= 0 && lastChar <= 7;

var getInstructFeedback = function() {
	return '<div class = centerbox><p class = center-block-text>' + feedback_instruct_text +
		'</p></div>'
}

function assessPerformance() {
	/* Function to calculate the "credit_var", which is a boolean used to
	credit individual experiments in expfactory. */
	var experiment_data = jsPsych.data.getTrialsOfType('poldrack-single-stim')
	var missed_count = 0
	var trial_count = 0
	var rt_array = []
	var rt = 0
		//record choices participants made
	var choice_counts = {}
	choice_counts[-1] = 0
	for (var k = 0; k < choices.length; k++) {
		choice_counts[choices[k]] = 0
	}
	for (var i = 0; i < experiment_data.length; i++) {
		if (experiment_data[i].possible_responses != 'none') {
			trial_count += 1
			rt = experiment_data[i].rt
			key = experiment_data[i].key_press
			choice_counts[key] += 1
			if (rt == -1) {
				missed_count += 1
			} else {
				rt_array.push(rt)
			}
		}
	}
	//calculate average rt
	var avg_rt = -1
	if (rt_array.length !== 0) {
		avg_rt = math.median(rt_array)
	}
		//calculate whether response distribution is okay
	var responses_ok = true
	Object.keys(choice_counts).forEach(function(key, index) {
		if (choice_counts[key] > trial_count * 0.85) {
			responses_ok = false
		}
	})
	var missed_percent = missed_count/trial_count
	credit_var = (missed_percent < 0.4 && avg_rt > 200 && responses_ok)
	if (credit_var === true) {
	  performance_var = total_score
	} else {
	  performance_var = 0
	}
	jsPsych.data.addDataToLastTrial({"credit_var": credit_var, "performance_var": performance_var})
}

function evalAttentionChecks() {
	var check_percent = 1
	if (run_attention_checks) {
		var attention_check_trials = jsPsych.data.getTrialsOfType('attention-check')
		var checks_passed = 0
		for (var i = 0; i < attention_check_trials.length; i++) {
			if (attention_check_trials[i].correct === true) {
				checks_passed += 1
			}
		}
		check_percent = checks_passed / attention_check_trials.length
	}
	return check_percent
}

//Polar method for generating random samples from a norma distribution.
//Source: http://blog.yjl.im/2010/09/simulating-normal-random-variable-using.html
function normal_random(mean, variance) {
	if (mean === undefined)
		mean = 0.0;
	if (variance === undefined)
		variance = 1.0;
	var V1, V2, S;
	do {
		var U1 = Math.random();
		var U2 = Math.random();
		V1 = 2 * U1 - 1;
		V2 = 2 * U2 - 1;
		S = V1 * V1 + V2 * V2;
	} while (S > 1);

	X = Math.sqrt(-2 * Math.log(S) / S) * V1;
	//Y = Math.sqrt(-2 * Math.log(S) / S) * V2;
	X = mean + Math.sqrt(variance) * X;
	//Y = mean + Math.sqrt(variance) * Y ;
	return X;
}

var get_condition = function() {
	return condition
}

var get_current_trial = function() {
	return current_trial
}

var initialize_FB_matrix = function() {
	return [Math.random() * 0.5 + 0.25, Math.random() * 0.5 + 0.25, Math.random() * 0.5 + 0.25, Math.random() * 0.5 + 0.25]
}

//Change phase from practice to test
var change_phase = function() {
	if (curr_images == practice_images) {
		curr_images = test_images
		curr_colors = test_colors
		curr_fs_stims = test_fs_stims
		curr_ss_stims = test_ss_stim
		terminal_state_img =  terminal_state_img_test
		exp_stage = 'test'
	} else {
		curr_images = practice_images
		curr_colors = practice_colors
		curr_fs_stims = practice_fs_stims
		curr_ss_stim = practice_ss_stim
		terminal_state_img =  terminal_state_img_practice
		exp_stage = 'practice'
	}
	total_score = 0
	current_trial = -1 //reset count
}


/*
Generate first stage stims. Takes in an array of images and colors (which change between practice anad test)
*/


/* yoav : added 4 stimulus to cover all vartion of first stage */
var get_fs_stim_right = function(images, colors) {
	var fs_stim = [{
		stimulus:
		 "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		 "<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-fs' src= '" + images[0] + "'></img></div>"+
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[2] +"'> </img></div>" +
				"<img class = 'decision-right-fs' src= '" + images[1] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[0] +"'> </img></div>",
		stim_order: [0, 0]
	/*}, {
		stimulus:
			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-fs' src= '" + images[0] + "'></img>"+
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[1] +"'> </img></div>" +
			"<img class = 'decision-right-fs' src= '" + images[1] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[2] +"'> </img></div>",
		stim_order: [1, 1]


	}, {
		stimulus:
			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
		  "<img class = 'decision-left-fs' src= '" + images[2] + "'></img></div>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[1] +"'> </img></div>" +
			"<img class = 'decision-right-fs' src= '" + images[3] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[2] +"'> </img></div>",
		stim_order: [1, 0]*/
	},{
		stimulus:
			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
      "<img class = 'decision-left-fs' src= '" + images[2] + "'></img>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[2] +"'> </img></div>" +
		"<img class = 'decision-right-fs' src= '" + images[3] + "'></img>" +
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[0] +"'> </img></div>",
		stim_order: [0, 1]
	}

]
	return fs_stim
}

var get_fs_stim_left = function(images, colors) {
	var fs_stim = [{
		stimulus:
		 "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		 "<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-fs' src= '" + images[0] + "'></img></div>"+
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[1] +"'> </img></div>" +
				"<img class = 'decision-right-fs' src= '" + images[1] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[2] +"'> </img></div>",
		stim_order: [0, 0]
	/*}, {
		stimulus:
			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-fs' src= '" + images[0] + "'></img>"+
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[1] +"'> </img></div>" +
			"<img class = 'decision-right-fs' src= '" + images[1] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[2] +"'> </img></div>",
		stim_order: [1, 1]


	}, {
		stimulus:
			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
		  "<img class = 'decision-left-fs' src= '" + images[2] + "'></img></div>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[1] +"'> </img></div>" +
			"<img class = 'decision-right-fs' src= '" + images[3] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[2] +"'> </img></div>",
		stim_order: [1, 0]*/
	},{
		stimulus:
			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
      "<img class = 'decision-left-fs' src= '" + images[2] + "'></img>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[1] +"'> </img></div>" +
		"<img class = 'decision-right-fs' src= '" + images[3] + "'></img>" +
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[2] +"'> </img></div>",
		stim_order: [0, 1]
	}

]
	return fs_stim
}


var get_fs_stim_new = isLeftGroup ? get_fs_stim_left : get_fs_stim_right;

/* yoav : added 4 stimulus to cover all vartion of first stage */
/*var get_fs_stim = function(images, colors) {
	var fs_stim = [{
		stimulus:
		 "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		 "<div class = centerbox><div class = fixation>+</div></div>"+
			"<div class = decision-left style='background:" + colors[0] + "; '>" +
			"<img class = 'decision-stim' src= '" + images[0] + "'></img>" +
			"<img class = 'strategy_stim' src= '" + strategy_stim[2] +"'> </img></div>" +
			"<div class = decision-right style='background:" + colors[0] + "; '>" +
			"<img class = 'decision-stim' src= '" + images[1] + "'></img>" +
			"<img class = 'strategy_stim' src= '" + strategy_stim[0] +"'> </img></div>",
		stim_order: [0, 0]
	}, {
		stimulus:
			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<div class = decision-left style='background:" + colors[0] + "; '>" +
			"<img class = 'decision-stim' src= '" + images[0] + "'></img>" +
			"<img class = 'strategy_stim' src= '" + strategy_stim[1] +"'> </img></div>" +
			"<div class = decision-right style='background:" + colors[0] + "; '>" +
			"<img class = 'decision-stim' src= '" + images[1] + "'></img>" +
			"<img class = 'strategy_stim' src= '" + strategy_stim[2] +"'> </img></div>",
		stim_order: [1, 1]

	}, {
		stimulus:
			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<div class = decision-left style='background:" + colors[0] + "; '>" +
			"<img class = 'decision-stim' src= '" + images[6] + "'></img>" +
			"<img class = 'strategy_stim' src= '" + strategy_stim[1] +"'> </img></div>" +
			"<div class = decision-right style='background:" + colors[0] + "; '>" +
			"<img class = 'decision-stim' src= '" + images[7] + "'></img>" +
			"<img class = 'strategy_stim' src= '" + strategy_stim[2] +"'> </img></div>",
		stim_order: [1, 0]
	},{
		stimulus:
			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<div class = decision-left style='background:" + colors[0] + "; '>" +
			"<img class = 'decision-stim' src= '" + images[6] + "'></img>" +
			"<img class = 'strategy_stim' src= '" + strategy_stim[2] +"'> </img></div>" +
			"<div class = decision-right style='background:" + colors[0] + "; '>" +
			"<img class = 'decision-stim' src= '" + images[7] + "'></img>" +
			"<img class = 'strategy_stim' src= '" + strategy_stim[0] +"'> </img></div>",
		stim_order: [0, 1]
	}

]
	return fs_stim
}
*/
/*
Generate second stage stims. Takes in an array of images and colors (which change between practice and test)
*/

var get_ss_stim_right = function(images, colors) {
	var ss_stim_array = [
		["<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[6] + "'></img></div>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[2] +"'> </img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[7] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[0] +"'> </img></div>",

			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[7] + "'></img></div>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[2] +"'> </img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[6] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[0] +"'> </img></div>"
		],
		["<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[4] + "'></img></div>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[2] +"'> </img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[5] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[0] +"'> </img></div>",

			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[5] + "'></img></div>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[2] +"'> </img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[4] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[0] +"'> </img></div>"
		]
	]

	var ss_stim = {
		stimulus: [ss_stim_array[0][0], ss_stim_array[0][1], ss_stim_array[1][0], ss_stim_array[1][1]],
		stim_order: [[2, 3], [3, 2], [4, 5], [5, 4]]
	}
	return ss_stim
}

var get_ss_stim_left = function(images, colors) {
	var ss_stim_array = [
		["<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[6] + "'></img></div>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[1] +"'> </img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[7] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[2] +"'> </img></div>",

			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[7] + "'></img></div>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[1] +"'> </img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[6] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[2] +"'> </img></div>"
		],
		["<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[4] + "'></img></div>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[1] +"'> </img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[5] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[2] +"'> </img></div>",

			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[5] + "'></img></div>" +
			"<img class = 'strategy_stim_left' src= '" + strategy_stim[1] +"'> </img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[4] + "'></img></div>"+
			"<img class = 'strategy_stim_right' src= '" + strategy_stim[2] +"'> </img></div>"
		]
	]

	var ss_stim = {
		stimulus: [ss_stim_array[0][0], ss_stim_array[0][1], ss_stim_array[1][0], ss_stim_array[1][1]],
		stim_order: [[2, 3], [3, 2], [4, 5], [5, 4]]
	}
	return ss_stim
}


var get_ss_stim_new = isLeftGroup ? get_ss_stim_left : get_ss_stim_right;


/* yoav : 4 possible second stage choices*/
/*var get_ss_stim = function(images, colors) {
	var ss_stim_array = [
		["<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[2] + "'></img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[3] + "'></img></div>",

			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[3] + "'></img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[2] + "'></img></div>"
		],
		["<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[4] + "'></img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[5] + "'></img></div>",

			"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-ss' src= '" + images[5] + "'></img></div>" +
			"<img class = 'decision-right-ss' src= '" + images[4] + "'></img></div>"
		]
	]

	var ss_stim = {
		stimulus: [ss_stim_array[0][0], ss_stim_array[0][1], ss_stim_array[1][0], ss_stim_array[1][1]],
		stim_order: [[2, 3], [3, 2], [4, 5], [5, 4]]
	}
	return ss_stim
}
*/

/*
The following methods all support the user-dependent presentation of stimulus including animations, multiple stages
and FB. The "get_selected" functions also append data to the preceeding trials
*/

/* Selects the next first stage from a predefined, randomized list of first stages and increases the trial count*/
var choose_first_stage = function() {
	current_trial = current_trial + 1
	stim_ids = curr_fs_stims.stim_order[current_trial]
	return [curr_fs_stims.stimulus[current_trial],stim_ids]
}

/*
After a stimulus is selected, an animation proceeds whereby the selected stimulus moves to the top of the screen while
the other stimulus fades. This function accomplishes this by creating a new html object to display composed of the old stim
with appropriate handles to start the relevant animations.
Also updates the global variables choice, first_selected and first_notselected, which are used in the next function
*/


 var get_first_selected_right = function() {
 	var first_stage_trial = jsPsych.data.getLastTrialData()
 	var i = first_stage_trial.key_press;
 	var choice = choices.indexOf(first_stage_trial.key_press)
 	//console.log("stim_ids_fs = " + stim_ids)
	//console.log("choice1 = " + choice)
	//console.log("i = " + i)
 	if (i == 75 ){
		//console.log("choice1 = " + choice)
 		if (stim_ids[1] == 0 ){
			first_selected = 1
			//console.log("first_selected = " + first_selected)
				var first_notselected = first_selected-1
				jsPsych.data.addDataToLastTrial({
					stim_selected: first_selected
				})
 			return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
 			"<div class = centerbox><div class = fixation>+</div></div>"+
       "<img class = 'decision-left-fs' src= '" + curr_images[0] + "'></img></div>" +
       "<img class = 'decision-right-fs-yes' src= '" + curr_images[1] + "'></img></div>"
 			}else {
				first_selected = 3
				//console.log("first_selected = " + first_selected)
					var first_notselected = first_selected-1
					jsPsych.data.addDataToLastTrial({
						stim_selected: first_selected
					})
 				return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
 				"<div class = centerbox><div class = fixation>+</div></div>"+
         "<img class = 'decision-right-fs-yes' src= '" + curr_images[3] + "'></img></div>" +
         "<img class = 'decision-left-fs' src= '" + curr_images[2] + "'></img></div>"
				 }
	}else if(i == -1){
		if (stim_ids[1] == 0 ){
			first_selected = 0
			//console.log("first_selected = " + first_selected)
				var first_notselected = first_selected+1
				jsPsych.data.addDataToLastTrial({
					stim_selected: first_selected
				})
			return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			 "<img class = 'decision-right-fs' src= '" + curr_images[1] + "'></img></div>" +
			 "<img class = 'decision-left-fs-yes' src= '" + curr_images[0] + "'></img></div>"
			}else {
				first_selected = 2
				//console.log("first_selected = " + first_selected)
					var first_notselected = first_selected+1
					jsPsych.data.addDataToLastTrial({
						stim_selected: first_selected
					})
				return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
				"<div class = centerbox><div class = fixation>+</div></div>"+
				 "<img class = 'decision-left-fs-yes' src= '" + curr_images[2] + "'></img></div>" +
				 "<img class = 'decision-right-fs' src= '" + curr_images[3] + "'></img></div>"
				 }
 		}else{
 		first_selected = -1
		//console.log("first_selected = " + first_selected)
 		jsPsych.data.addDataToLastTrial({
 			stim_selected: first_selected
 		})
 	}
 }

 var get_first_selected_left = function() {
  var first_stage_trial = jsPsych.data.getLastTrialData()
  var i = first_stage_trial.key_press;
  var choice = choices.indexOf(first_stage_trial.key_press)
  //console.log("stim_ids1 = " + stim_ids)
  //console.log("choice1 = " + choice)
  //console.log("i = " + i)
  if (i == 83 ){
 	 //console.log("choice1 = " + choice)
 	 if (stim_ids[1] == 0 ){
 		 first_selected = 0
 		 //console.log("first_selected = " + first_selected)
 			 var first_notselected = first_selected+1
 			 jsPsych.data.addDataToLastTrial({
 				 stim_selected: first_selected
 			 })
 		 return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
 		 "<div class = centerbox><div class = fixation>+</div></div>"+
 			"<img class = 'decision-left-fs-yes' src= '" + curr_images[0] + "'></img></div>" +
 			"<img class = 'decision-right-fs' src= '" + curr_images[1] + "'></img></div>"
 		 }else {
 			 first_selected = 2
 			 //console.log("first_selected = " + first_selected)
 				 var first_notselected = first_selected+1
 				 jsPsych.data.addDataToLastTrial({
 					 stim_selected: first_selected
 				 })
 			 return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
 			 "<div class = centerbox><div class = fixation>+</div></div>"+
 				"<img class = 'decision-right-fs' src= '" + curr_images[3] + "'></img></div>" +
 				"<img class = 'decision-left-fs-yes' src= '" + curr_images[2] + "'></img></div>"
 				}
  }else if(i == -1){
 	 if (stim_ids[1] == 0 ){
 		 first_selected = 1
 		 //console.log("first_selected = " + first_selected)
 			 var first_notselected = first_selected-1
 			 jsPsych.data.addDataToLastTrial({
 				 stim_selected: first_selected
 			 })
 		 return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
 		 "<div class = centerbox><div class = fixation>+</div></div>"+
 			"<img class = 'decision-right-fs-yes' src= '" + curr_images[1] + "'></img></div>" +
 			"<img class = 'decision-left-fs' src= '" + curr_images[0] + "'></img></div>"
 		 }else {
 			 first_selected = 3
 			 //console.log("first_selected = " + first_selected)
 				 var first_notselected = first_selected-1
 				 jsPsych.data.addDataToLastTrial({
 					 stim_selected: first_selected
 				 })
 			 return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
 			 "<div class = centerbox><div class = fixation>+</div></div>"+
 				"<img class = 'decision-left-fs' src= '" + curr_images[2] + "'></img></div>" +
 				"<img class = 'decision-right-fs-yes' src= '" + curr_images[3] + "'></img></div>"
 				}
 	 }else{
 	 first_selected = -1
 	 //console.log("first_selected = " + first_selected)
 	 jsPsych.data.addDataToLastTrial({
 		 stim_selected: first_selected
 	 })
  }
 }

 var get_first_selected_new = isLeftGroup ? get_first_selected_left : get_first_selected_right;


/*var get_first_selected = function() {
	var first_stage_trial = jsPsych.data.getLastTrialData()
	var i = first_stage_trial.key_press;
	var choice = choices.indexOf(first_stage_trial.key_press)
	// //console.log("choice first = " + stim_ids)
	if (i == 37 && stim_ids[0] == 1 || i == 39 && stim_ids[0] == 0){
		var sum = stim_ids[0] + stim_ids[1]
		first_selected = stim_ids[choice]
		var first_notselected = stim_ids[1 - choice]
		jsPsych.data.addDataToLastTrial({
			stim_selected: first_selected
		})
		if ( sum == 2 || sum == 0 ){
			if (stim_ids[0] == 1){
			return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			"<div class = 'selected " + stim_side[0] + "' style='background:" + curr_colors[0] +
				"; '>" +
				"<img class = 'decision-left-fs-yes' src= '" + curr_images[0] + "'></div>" +
				"<div class = '" + stim_side[1] + " fade' style='background:" + curr_colors[0] +
				"; '>" +
				"<img class = 'decision-stim  *2fade' src= '" + curr_images[1] + "'></div>"
			}else {
				return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
				"<div class = centerbox><div class = fixation>+</div></div>"+
				"<div class = 'selected " + stim_side[1] + "' style='background:" + curr_colors[0] +
					"; '>" +
					"<img class = 'decision-right-fs-yes' src= '" + curr_images[1] + "'></div>" +
					"<div class = '" + stim_side[0] + " fade' style='background:" + curr_colors[0] +
					"; '>" +
					"<img class = 'decision-stim  *2fade' src= '" + curr_images[0] + "'></div>"
			}
		}else {
			if (stim_ids[0] == 1){
				return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
				"<div class = centerbox><div class = fixation>+</div></div>"+
				"<div class = 'selected " + stim_side[0] + "' style='background:" + curr_colors[0] +
					"; '>" +
					"<img class = 'decision-left-fs-yes' src= '" + curr_images[6] + "'></div>" +
					"<div class = '" + stim_side[1] + " fade' style='background:" + curr_colors[0] +
					"; '>" +
					"<img class = 'decision-stim  *2fade' src= '" + curr_images[7] + "'></div>"
			}else {
				return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
				"<div class = centerbox><div class = fixation>+</div></div>"+
				"<div class = 'selected " + stim_side[1] + "' style='background:" + curr_colors[0] +
					"; '>" +
					"<img class = 'decision-right-fs-yes' src= '" + curr_images[7] + "'></div>" +
					"<div class = '" + stim_side[0] + " fade' style='background:" + curr_colors[0] +
					"; '>" +
					"<img class = 'decision-stim  *2fade' src= '" + curr_images[6] + "'></div>"
			}

		}
		}else if(i == -1){
					if(stim_ids[0] == 0 && stim_ids[1] == 0){
						first_selected = 0
						var first_notselected = 1
						return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
						"<div class = centerbox><div class = fixation>+</div></div>"+
						"<div class = 'selected " + stim_side[0] + "' style='background:" + curr_colors[0] +
							"; '>" +
							"<img class = 'decision-left-fs-yes' src= '" + curr_images[first_selected] + "'></div>" +
							"<div class = '" + stim_side[1] + " fade' style='background:" + curr_colors[0] +
							"; '>" +
							"<img class = 'decision-stim  fade' src= '" + curr_images[first_notselected] + "'></div>"
					}else if (stim_ids[0] == 1 && stim_ids[1] == 1) {
						first_selected = 1
						var first_notselected = 0
						return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
						"<div class = centerbox><div class = fixation>+</div></div>"+
						 "<div class = 'selected " + stim_side[1] + "' style='background:" + curr_colors[0] +
							"; '>" +
							"<img class = 'decision-right-fs-yes' src= '" + curr_images[first_selected] + "'></div>" +
							"<div class = '" + stim_side[0] + " fade' style='background:" + curr_colors[0] +
							"; '>" +
							"<img class = 'decision-stim  fade' src= '" + curr_images[first_notselected] + "'></div>"
					}else if (stim_ids[0] == 1 && stim_ids[1] == 0){
						first_selected = 1
						var first_notselected = 0
						return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
						"<div class = centerbox><div class = fixation>+</div></div>"+
						 "<div class = 'selected " + stim_side[1] + "' style='background:" + curr_colors[0] +
							"; '>" +
							"<img class = 'decision-right-fs-yes' src= '" + curr_images[7] + "'></div>" +
							"<div class = '" + stim_side[0] + " fade' style='background:" + curr_colors[0] +
							"; '>" +
							"<img class = 'decision-stim  fade' src= '" + curr_images[6] + "'></div>"
					}else{
						first_selected = 0
						var first_notselected = 1
						return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
						"<div class = centerbox><div class = fixation>+</div></div>"+
						 "<div class = 'selected " + stim_side[0] + "' style='background:" + curr_colors[0] +
							"; '>" +
							"<img class = 'decision-left-fs-yes' src= '" + curr_images[6] + "'></div>" +
							"<div class = '" + stim_side[1] + " fade' style='background:" + curr_colors[0] +
							"; '>" +
							"<img class = 'decision-stim  fade' src= '" + curr_images[7] + "'></div>"
						}
		}else{
		first_selected = -1
		jsPsych.data.addDataToLastTrial({
			stim_selected: first_selected
		})
	}
}
*/


/*
The second stage is diterministacly chosen based on the first stage choice. Each of the first stage stimulus is associated
with one of two second stages.
This function checks to see if there was any first stage response. If not, set the global variable FB_on to off and display a reminder
If a choice was taken select a second stage. Randomly select a presentation order for the two stimulus associated with the second stage
*/
var choose_second_stage = function() {
	if (first_selected == -1) {
		FB_on = 0;
		return "<div class = centerbox><div class = center-text>" +
			"Wrong key</div></div>"
	} else if (first_selected == 1 || first_selected == 2){
		FB_on = 1;
		stage = 0 //orange
	}else{
		FB_on = 1;
		stage = 1 //purple
	/*	transition = 'frequent'
		if (Math.random() < 0) {
			stage = 1 - stage
			transition = 'infrequent'*/
		}
	var stage_index = stage * 2
	var stim_index = stage_index + Math.round(Math.random())
	stim_ids = curr_ss_stims.stim_order[stim_index]
	//console.log("stage = " + stage)
	//console.log("stim_index = " + stim_index)
	//console.log("stim_ids = " + stim_ids)

	return "<div class = 'decision-top faded' style='background:" + curr_colors[0] + "; '>" +
	//*S**Y*	"<img class = 'decision-stim' src= '" + curr_images[first_selected] + "'></div>" +
		curr_ss_stims.stimulus[stim_index]
		 //*S* "<div class = 'decision-top faded' style='background:" + curr_colors[0] + "; '>" +
			//*S*	"<img class = 'decision-stim' src= '" + curr_images[first_selected] + "'></div>" +
			// curr_ss_stims.stimulus[stim_index]
	}


/*
Animates second stage choice, similarly to get_first_selected
*/
var get_second_selected_right = function() {
	var second_stage_trial = jsPsych.data.getLastTrialData()
	var j = second_stage_trial.key_press;
	var choice = choices.indexOf(second_stage_trial.key_press)
	//console.log("stim_ids = " + stim_ids)

	//console.log("choice1 = " + choice)
	//console.log("j = " + j)
if (j == 75 ){
	//console.log("j = " + j)
	if (stim_ids[1] == 2 ){
		second_selected = 2
		//console.log("second_selected = " + second_selected)
			var second_notselected = second_selected+1
			jsPsych.data.addDataToLastTrial({
				stim_selected: second_selected
			})
		return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		"<div class = centerbox><div class = fixation>+</div></div>"+
		 "<img class = 'decision-left-ss' src= '" + curr_images[7] + "'></img></div>" +
		 "<img class = 'decision-right-ss-yes' src= '" + curr_images[6] + "'></img></div>"
	 }else if (stim_ids[1] == 3 ){
			second_selected = 3
			//console.log("second_selected = " + second_selected)
				var second_notselected = second_selected-1
				jsPsych.data.addDataToLastTrial({
					stim_selected: second_selected
				})
			return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			 "<img class = 'decision-right-ss-yes' src= '" + curr_images[7] + "'></img></div>" +
			 "<img class = 'decision-left-ss' src= '" + curr_images[6] + "'></img></div>"
		 }else if (stim_ids[1] == 4 ){
					second_selected = 4
					//console.log("second_selected = " + second_selected)
						var second_notselected = second_selected+1
						jsPsych.data.addDataToLastTrial({
							stim_selected: second_selected
						})
					return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
					"<div class = centerbox><div class = fixation>+</div></div>"+
					 "<img class = 'decision-right-ss-yes' src= '" + curr_images[4] + "'></img></div>" +
					 "<img class = 'decision-left-ss' src= '" + curr_images[5] + "'></img></div>"
				 }else if (stim_ids[1] == 5 ){
				 		 second_selected = 5
				 		 //console.log("second_selected = " + second_selected)
				 			 var second_notselected = second_selected-1
				 			 jsPsych.data.addDataToLastTrial({
				 				 stim_selected: second_selected
				 			 })
				 		 return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
				 		 "<div class = centerbox><div class = fixation>+</div></div>"+
				 			"<img class = 'decision-right-ss-yes' src= '" + curr_images[5] + "'></img></div>" +
				 			"<img class = 'decision-left-ss' src= '" + curr_images[4] + "'></img></div>"
}
}else if(j == -1){
	//console.log("j = " + j)
	if (stim_ids[1] == 2 ){
		second_selected = 3
		//console.log("second_selected = " + second_selected)
			var second_notselected = second_selected-1
			jsPsych.data.addDataToLastTrial({
				stim_selected: second_selected
			})
		return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		"<div class = centerbox><div class = fixation>+</div></div>"+
		 "<img class = 'decision-right-ss' src= '" + curr_images[6] + "'></img></div>" +
		 "<img class = 'decision-left-ss-yes' src= '" + curr_images[7] + "'></img></div>"
	 }else if (stim_ids[1] == 3 ){
			second_selected = 2
			//console.log("second_selected = " + second_selected)
				var second_notselected = second_selected+1
				jsPsych.data.addDataToLastTrial({
					stim_selected: second_selected
				})
			return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			 "<img class = 'decision-right-ss' src= '" + curr_images[7] + "'></img></div>" +
			 "<img class = 'decision-left-ss-yes' src= '" + curr_images[6] + "'></img></div>"
		 }else if (stim_ids[1] == 4 ){
					second_selected = 5
					//console.log("second_selected = " + second_selected)
						var second_notselected = second_selected-1
						jsPsych.data.addDataToLastTrial({
							stim_selected: second_selected
						})
					return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
					"<div class = centerbox><div class = fixation>+</div></div>"+
					 "<img class = 'decision-right-ss' src= '" + curr_images[4] + "'></img></div>" +
					 "<img class = 'decision-left-ss-yes' src= '" + curr_images[5] + "'></img></div>"
				 }else if (stim_ids[1] == 5 ){
				 		 second_selected = 4
				 		 //console.log("second_selected = " + second_selected)
				 			 var second_notselected = second_selected+1
				 			 jsPsych.data.addDataToLastTrial({
				 				 stim_selected: second_selected
				 			 })
				 		 return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
				 		 "<div class = centerbox><div class = fixation>+</div></div>"+
				 			"<img class = 'decision-right-ss' src= '" + curr_images[5] + "'></img></div>" +
				 			"<img class = 'decision-left-ss-yes' src= '" + curr_images[4] + "'></img></div>"
}
	} else {
		second_selected = -1
		jsPsych.data.addDataToLastTrial({
			stim_selected: second_selected
		})
	}

}

var get_second_selected_left = function() {
	var second_stage_trial = jsPsych.data.getLastTrialData()
	var j = second_stage_trial.key_press;
	var choice = choices.indexOf(second_stage_trial.key_press)
	/*console.log("stim_ids1 = " + stim_ids)
	console.log("choice1 = " + choice)
	console.log("j = " + j)*/
if (j == 83 ){
	//console.log("j = " + j)
	if (stim_ids[1] == 2 ){
		second_selected = 3
		//console.log("second_selected = " + second_selected)
			var second_notselected = second_selected-1
			jsPsych.data.addDataToLastTrial({
				stim_selected: second_selected
			})
		return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		"<div class = centerbox><div class = fixation>+</div></div>"+
		 "<img class = 'decision-left-ss-yes' src= '" + curr_images[7] + "'></img></div>" +
		 "<img class = 'decision-right-ss' src= '" + curr_images[6] + "'></img></div>"
	 }else if (stim_ids[1] == 3 ){
			second_selected = 2
			//console.log("second_selected = " + second_selected)
				var second_notselected = second_selected+1
				jsPsych.data.addDataToLastTrial({
					stim_selected: second_selected
				})
			return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			 "<img class = 'decision-right-ss' src= '" + curr_images[7] + "'></img></div>" +
			 "<img class = 'decision-left-ss-yes' src= '" + curr_images[6] + "'></img></div>"
		 }else if (stim_ids[1] == 4 ){
					second_selected = 5
					//console.log("second_selected = " + second_selected)
						var second_notselected = second_selected-1
						jsPsych.data.addDataToLastTrial({
							stim_selected: second_selected
						})
					return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
					"<div class = centerbox><div class = fixation>+</div></div>"+
					 "<img class = 'decision-right-ss' src= '" + curr_images[4] + "'></img></div>" +
					 "<img class = 'decision-left-ss-yes' src= '" + curr_images[5] + "'></img></div>"
				 }else if (stim_ids[1] == 5 ){
				 		 second_selected = 4
				 		 //console.log("second_selected = " + second_selected)
				 			 var second_notselected = second_selected+1
				 			 jsPsych.data.addDataToLastTrial({
				 				 stim_selected: second_selected
				 			 })
				 		 return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
				 		 "<div class = centerbox><div class = fixation>+</div></div>"+
				 			"<img class = 'decision-right-ss' src= '" + curr_images[5] + "'></img></div>" +
				 			"<img class = 'decision-left-ss-yes' src= '" + curr_images[4] + "'></img></div>"
}
}else if(j == -1){
	//console.log("j = " + j)
	if (stim_ids[1] == 2 ){
		second_selected = 2
		//console.log("second_selected = " + second_selected)
			var second_notselected = second_selected+1
			jsPsych.data.addDataToLastTrial({
				stim_selected: second_selected
			})
		return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
		"<div class = centerbox><div class = fixation>+</div></div>"+
		 "<img class = 'decision-right-ss-yes' src= '" + curr_images[6] + "'></img></div>" +
		 "<img class = 'decision-left-ss' src= '" + curr_images[7] + "'></img></div>"
	 }else if (stim_ids[1] == 3 ){
			second_selected = 3
			//console.log("second_selected = " + second_selected)
				var second_notselected = second_selected-1
				jsPsych.data.addDataToLastTrial({
					stim_selected: second_selected
				})
			return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
			 "<img class = 'decision-right-ss-yes' src= '" + curr_images[7] + "'></img></div>" +
			 "<img class = 'decision-left-ss' src= '" + curr_images[6] + "'></img></div>"
		 }else if (stim_ids[1] == 4 ){
					second_selected = 4
					//console.log("second_selected = " + second_selected)
						var second_notselected = second_selected+1
						jsPsych.data.addDataToLastTrial({
							stim_selected: second_selected
						})
					return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
					"<div class = centerbox><div class = fixation>+</div></div>"+
					 "<img class = 'decision-right-ss-yes' src= '" + curr_images[4] + "'></img></div>" +
					 "<img class = 'decision-left-ss' src= '" + curr_images[5] + "'></img></div>"
				 }else if (stim_ids[1] == 5 ){
				 		 second_selected = 5
				 		 //console.log("second_selected = " + second_selected)
				 			 var second_notselected = second_selected-1
				 			 jsPsych.data.addDataToLastTrial({
				 				 stim_selected: second_selected
				 			 })
				 		 return "<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
				 		 "<div class = centerbox><div class = fixation>+</div></div>"+
				 			"<img class = 'decision-right-ss-yes' src= '" + curr_images[5] + "'></img></div>" +
				 			"<img class = 'decision-left-ss' src= '" + curr_images[4] + "'></img></div>"
}
	} else {
		second_selected = -1
		jsPsych.data.addDataToLastTrial({
			stim_selected: second_selected
		})
	}

}

var get_second_selected_new = isLeftGroup ? get_second_selected_left : get_second_selected_right;

/*var get_second_selected = function() {
	var second_stage_trial = jsPsych.data.getLastTrialData()
	var choice = choices.indexOf(second_stage_trial.key_press)
	 console.log("second selected");
	console.log("choice "  + choice )
	if (stim_ids[0] == 2 || stim_ids[0] == 3 ){
		var background = background_Image_stage_1
	}else {
		var background = background_Image_stage_1
	}
	if (choice != -1) {
		second_selected = stim_ids[choice]
		var second_notselected = stim_ids[1 - choice]
		jsPsych.data.addDataToLastTrial({
			stim_selected: second_selected
		})
		if (choice == 0){
		return "<img class = 'background_images' src= '" + background +"'> </img></div>"+
		"<div class = centerbox><div class = fixation>+</div></div>"+
			"<img class = 'decision-left-sss_yes' src= '" + curr_images[second_selected] + "'></img></div>" +
			"<img class = 'decision-right-sss_not' src= '" + curr_images[second_notselected] + "'></img></div>"
		}else {
			return "<img class = 'background_images' src= '" + background +"'> </img></div>"+
			"<div class = centerbox><div class = fixation>+</div></div>"+
				"<img class = 'decision-left-sss_not' src= '" + curr_images[second_notselected] + "'></img></div>" +
				"<img class = 'decision-right-sss_yes' src= '" + curr_images[second_selected] + "'></img></div>"
		}
	} else {
		second_selected = -1
		jsPsych.data.addDataToLastTrial({
			stim_selected: second_selected
		})
	}

}
*/

/*
After each trial the FB_matrix is updated such that each of the 4 reward probabilities changes by a random amount
parametrized a Gaussian. Reward probabilities are bound by 25% and 75%
*/
/*var update_FB = function() {
	for (i = 0; i < FB_matrix.length; i++) {
		var curr_value = FB_matrix[i]
		var step = normal_random(0, 0.025 * 0.025)
		if (curr_value + step < 0.75 && curr_value + step > 0.25) {
			FB_matrix[i] = curr_value + step
		} else {
			FB_matrix[i] = curr_value - step
		}
	}
}*/

/*
If no choice was taken during the second stage display a reminder.
Otherwise, check the FB_matrix which determines the reward probabilities for each stimulus (there are 4 final stimulus associated with rewards:
2 for each of the 2 second stages). Flip a coin using the relevant probability and give FB.
After FB, the FB_atrix is updated.
*/

var get_feedback_new = function() {
	/* console.log("second_selected = " + second_selected)
	  console.log("second_selected = " + (second_selected-2))
	 console.log("current_trial = " + current_trial)
 try {	 console.log("prob = " + FB_matrix[second_selected - 2][current_trial])
  }catch{}
	  console.log(FB_matrix[second_selected - 2])
	  console.log(FB_matrix)*/

	if (second_selected == -1) {
		return "<div class = centerbox><div class = center-text>" +
		"Wrong key</div></div>"
	} else if (Math.random() < FB_matrix[second_selected - 2][current_trial]) {
		var fb_img = 0
	//	var bg_img = background_Image_stage_1
		// update_FB();
		FB = 1
		 //console.log("+1")
		total_score += 1
		 //console.log("total_score= " + total_score)
		if (second_selected == 2){
				fb_img = terminal_state_img[2]
		}else if (second_selected == 3){
				fb_img = terminal_state_img[3]
		}else if (second_selected == 4){
				fb_img = terminal_state_img[0]
		}else if (second_selected == 5){
				fb_img = terminal_state_img[1]
		}
		return 	"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
						"<img  class = 'FB-stim-reward' src ='" + fb_img + "'></div>"

		//return "<div><img  class = decision-fb src = 'images/gold_coin.png'></img></div>"

	   //*S*	"<div class = 'decision-top faded' style='background:" + curr_colors[stage + 1] + "; '>" +
		//*S*	"<img class = 'decision-stim' src= '" + curr_images[second_selected] + "'></div>" +

	} else {
	//	update_FB();
		FB = 0
		//console.log("-1")
		total_score -= 1
		//console.log("total_score= " + total_score)
		if (second_selected == 2){
				fb_img = terminal_state_img[6]
		}else if (second_selected == 3){
				fb_img = terminal_state_img[7]
		}else if (second_selected == 4){
				fb_img = terminal_state_img[4]
		}else if (second_selected == 5){
				fb_img = terminal_state_img[5]
		}
		return 	"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
						"<img  class = 'FB-stim' src ='" + fb_img + "'></div>"

		// return "<div style = text-align:center><p class = decision-fb style = 'color:red;font-size:120px'>0!</p></div>"
		  //*S*"<div class = 'decision-top faded' style='background:" + curr_colors[stage + 1] + "; '>" +
			//*S* "<img class = 'decision-stim' src= '" + curr_images[second_selected] + "'></div>" +
	}
}



var get_feedback = function() {
	/* console.log("second_selected = " + second_selected)
	  console.log("second_selected = " + (second_selected-2))
	 console.log("current_trial = " + current_trial)
 try {	 console.log("prob = " + FB_matrix[second_selected - 2][current_trial])
  }catch{}
	  console.log(FB_matrix[second_selected - 2])
	  console.log(FB_matrix)*/

	if (second_selected == -1) {
		return "<div class = centerbox><div class = center-text>" +
		"Wrong key</div></div>"
	} else if (Math.random() < FB_matrix[second_selected - 2][current_trial]) {
		var fb_img = 0
		var bg_imf = 0
		// update_FB();
		FB = 1
		// console.log("1")
		total_score += 1
		 //console.log("total_score= " + total_score)
		if (stim_ids[0] == 2 || stim_ids[0] == 3){
				fb_img = terminal_state_img[0]
				bg_img = background_Image_stage_1
		}else{
				fb_img = terminal_state_img[2]
				bg_img = background_Image_stage_1
		}
		return 	"<img class = 'background_images' src= '" + bg_img + "'> </img></div>" +
						"<img  class = 'FB-stim' src ='" + fb_img + "'></div>"

		//return "<div><img  class = decision-fb src = 'images/gold_coin.png'></img></div>"

	   //*S*	"<div class = 'decision-top faded' style='background:" + curr_colors[stage + 1] + "; '>" +
		//*S*	"<img class = 'decision-stim' src= '" + curr_images[second_selected] + "'></div>" +

	} else {
	//	update_FB();
		FB = 0
		total_score -= 1
		//console.log("total_score= " + total_score)
		if (stim_ids[0] == 2 ||stim_ids[0] == 3){
			 	fb_img = terminal_state_img[1]
				bg_img = background_Image_stage_1
		}else{
				fb_img = terminal_state_img[3]
				bg_img = background_Image_stage_1
		}

		return 	"<img class = 'background_images' src= '" + bg_img + "'> </img></div>" +
						"<img  class = 'FB-stim' src ='" + fb_img + "'> </div>"

		// return "<div style = text-align:center><p class = decision-fb style = 'color:red;font-size:120px'>0!</p></div>"
		  //*S*"<div class = 'decision-top faded' style='background:" + curr_colors[stage + 1] + "; '>" +
			//*S* "<img class = 'decision-stim' src= '" + curr_images[second_selected] + "'></div>" +
	}
}

var update_FB_data = function() {
	jsPsych.data.addDataToLastTrial({
		feedback: FB,
		trial_num: current_trial,
		FB_probs: FB_matrix.slice(0)
	})
	return ""
}


/* ************************************ */
/* Define experimental variables */
/* ************************************ */
// generic task variables
var run_attention_checks = false
var attention_check_thresh = 0.62
var sumInstructTime = 0 //ms
var instructTimeThresh = 0 ///in seconds
var credit_var = true
var performance_var = 0

// task specific variables
var total_score = 0 //track performance
var practice_trials_num = 20
var test_trials_num = 252
var stim_ids = [] //Tracks the ids of the last chosen stims.
var current_trial = -1
var first_selected = -1 //Tracks the ID of the selected fs stimulus
var second_selected = -1 //Tracks the ID of the selected fs stimulus
var FB_on = 1 //tracks whether FB should be displayed on this trial
var FB = -1 //tracks FB value
var stage = 0 //stage is used to track which second stage was shown, 0 or 1
var transition = ''
// var FB_matrix = initialize_FB_matrix() //tracks the reward probabilities for the four final stimulus
var exp_stage = 'practice'

//*S* FB_matrix Rndwlk version

var FB_matrix1 = [];
	FB_matrix1[0] = [0.6030469,0.6340575,0.6800188,0.6811093,0.6961447,0.7061541,0.6959717
	,0.7429852,0.7318529,0.7187331,0.7186522,0.7469616,0.8000000,0.7963728
	,0.8000000,0.7636981,0.7676027,0.7554596,0.7830112,0.8000000,0.7780572
	,0.7401294,0.7017912,0.6801827,0.6907607,0.7224145,0.7085174,0.7211862
	,0.7099190,0.7263356,0.7017973,0.7277780,0.7155975,0.7089450,0.6826835
	,0.6870117,0.6799595,0.6659270,0.7288866,0.7307078,0.7236430,0.7691793
	,0.7434590,0.7180406,0.7716028,0.7880193,0.7727901,0.7184666,0.7143420
	,0.6879772,0.6643028,0.6753365,0.6575539,0.6304078,0.6504997,0.6099120
	,0.5781999,0.5498800,0.5747225,0.5799968,0.5826564,0.5448545,0.5790387
	,0.5886893,0.5974002,0.6064797,0.6371682,0.6353878,0.6967238,0.7075852
	,0.7521318,0.7511497,0.7392637,0.7296798,0.7110563,0.7201802,0.7196773
	,0.7077950,0.7171622,0.6878151,0.6885870,0.6850119,0.7204229,0.7503918
	,0.7308885,0.7762287,0.8000000,0.8000000,0.8000000,0.8000000,0.8000000
	,0.8000000,0.8000000,0.7769687,0.7849102,0.7721738,0.7487894,0.7485545
	,0.7291696,0.7207025,0.7531966,0.7211532,0.7015023,0.7242004,0.6838234
	,0.6943901,0.6652711,0.6785139,0.6537705,0.6824533,0.7125198,0.7082421
	,0.7175168,0.7154520,0.6927694,0.7030912,0.7212108,0.7214708,0.7878718
	,0.7532171,0.6942272,0.6573043,0.6782852,0.6384292,0.6559797,0.7019024
	,0.6993338,0.7291960,0.7339224,0.7074303,0.7078750,0.7033249,0.7562657
	,0.7421337,0.7429321,0.7125922,0.7328396,0.7561935,0.7609605,0.7670338
	,0.7680075,0.7852891,0.7953185,0.7872962,0.7746545,0.7923850,0.7469222
	,0.7242884,0.6922402,0.6504438,0.6295227,0.6563532,0.6854369,0.6165695
	,0.5877980,0.5587811,0.5521680,0.5500850,0.5638867,0.5505846,0.5394165
	,0.5211106,0.5094890,0.5380630,0.5122375,0.4926157,0.4778834,0.4617732
	,0.4572346,0.4661070,0.4641508,0.4249968,0.4091944,0.3863114,0.3522833
	,0.3200631,0.3659204,0.3934462,0.3778989,0.4210678,0.4114344,0.3976804
	,0.3952923,0.4029236,0.3937856,0.3927285,0.4178309,0.3867017,0.3702394
	,0.3458279,0.3621667,0.3683214,0.3647945,0.3769352,0.3694809,0.3530016
	,0.3577540,0.3800784,0.4110034,0.3950293,0.3891510,0.4026484,0.4385318
	,0.4226087,0.4640847,0.4711546,0.4846376,0.4604088,0.4587025,0.4412308
	,0.4005495,0.4085503,0.3725671,0.3788289,0.4032072,0.3812726,0.3655761
	,0.3666809,0.3709775,0.4123128,0.3984225,0.4355939,0.4399011,0.4475487
	,0.3982890,0.3598750,0.3820394,0.3654996,0.3970609,0.4218215,0.4007676
	,0.4035861,0.3943323,0.4424521,0.4781205,0.4498318,0.4713568,0.5119905
	,0.5102580,0.5102424,0.5244195,0.5826288,0.5771750,0.5703648,0.6005543
	,0.5679627,0.5649677,0.5726870,0.5642461,0.5652227,0.5616803,0.5645683
	,0.5298117,0.5164796,0.4900147,0.4859009,0.4647708,0.4781572,0.4869686
	,0.4870585,0.4987289,0.5484822,0.5466218,0.5530684,0.5393077,0.5030316
	,0.5210511,0.4816930,0.5346753,0.5657698,0.5740266,0.5603983,0.6064441
	,0.5598943,0.5669779,0.5900629,0.5682400,0.5293508,0.5537788,0.5598105
	,0.5858878,0.6203309,0.6490873,0.6566718,0.6802193,0.6634929,0.6537906
	,0.6216524,0.6476350,0.6436044,0.6783327,0.6885290,0.7032067,0.7019980
	,0.6482667,0.6448427,0.6487436,0.6576984,0.6598191,0.6560299,0.6671876
	,0.7126151,0.7068430,0.7141879,0.7143230,0.7074806,0.7142709,0.7358037
	,0.7187586,0.7306632,0.6977069,0.7457873,0.7638021,0.7805403,0.7591762
	,0.7727686,0.7615263,0.7830312,0.7964935,0.7742526,0.7831798,0.8000000
	,0.7857411,0.7627617,0.7707755,0.7743786,0.7737380,0.7901590,0.8000000
	,0.8000000,0.7838159,0.7950069,0.7793723,0.7832790,0.8000000,0.8000000
	,0.7699792,0.7979845,0.7758324,0.7659155,0.7843866,0.8000000,0.8000000
	,0.7899025,0.7938885,0.8000000,0.8000000,0.7995794,0.7903184,0.7568883
	,0.7209270,0.7200504,0.7146226,0.7445271,0.7929319,0.7864982,0.7657554
	,0.7592244,0.7468335,0.7052685,0.7087506,0.7205411,0.7157126,0.7148104
	,0.7255274,0.7310068,0.7284023,0.6920834,0.6956309,0.7158034,0.7190364
	,0.6963428,0.6296463,0.6434573,0.6294150,0.5984925,0.6151903,0.6185288
	,0.6570558,0.6577564,0.6723084,0.6765589,0.6578021,0.6709603,0.6423009
	,0.6236455,0.5545996,0.5547619,0.5684187,0.5379467,0.5428602,0.5314343
	,0.5328927,0.5432863,0.5425215,0.5136805,0.5092575,0.5240755,0.5395786
	,0.5247422,0.5214644,0.4782228,0.4738405,0.4932639,0.4671159,0.4160854
	,0.3797269,0.3890397,0.3789531,0.4071316,0.4360256,0.4154958,0.4008688
	,0.4104671,0.3544015,0.3616295,0.3902526,0.3547635,0.3608809,0.3780219
	,0.3835501,0.4290559,0.4142197,0.3723940,0.3795812,0.3802650,0.3797703
	,0.3778364,0.3555065,0.3671108,0.3870552,0.4010084,0.4281066,0.4312145
	,0.4737583,0.4751877,0.4842445,0.4901915,0.5100713,0.5309771,0.5277524
	,0.4916731,0.4978072,0.4991440,0.4780738,0.4919401,0.4670812,0.4088519
	,0.4076700,0.4342756,0.4636356,0.4591382,0.4499192,0.4523148,0.4395195
	,0.4536662,0.4492447,0.4746377,0.5083769,0.5181176,0.4972136,0.4888360
	,0.4795897,0.4707966,0.4682206,0.4568929,0.4610408,0.4958293,0.5319405
	,0.5229772,0.5079401,0.4758231,0.4524616,0.4825635,0.4952451,0.4743304
	,0.5073738,0.5074268,0.4866268,0.4568943,0.4739607,0.4869208,0.5152346
	,0.4437620,0.4455432,0.4457691,0.4246769,0.4291080,0.4425431,0.4650182
	,0.4597672,0.4201320,0.4026626,0.4080252,0.3687208,0.3493708,0.3321777
	,0.3231717,0.3208328,0.2841057,0.2819773,0.2387802,0.2000000,0.2000000
	,0.2000000,0.2000000,0.2000000,0.2000000,0.2000000,0.2449247,0.2621222
	,0.2763061,0.3420149,0.4131194,0.4102980,0.4131916,0.4255962,0.4325284
	,0.4051568,0.4352190,0.3975347,0.4227403,0.4388245,0.4927968,0.5043618
	,0.5075094,0.5138193,0.5371729,0.5757443,0.5607230,0.5486183,0.4929384
	,0.4361330,0.4223764,0.3927308,0.4067315,0.4158999,0.4252345,0.4320348
	,0.4899469,0.5225050,0.4934340,0.5180970,0.4906985,0.4758752,0.4871314
	,0.5312396,0.5252205,0.5322089,0.5541964,0.5401392,0.5079153,0.5183835
	,0.5166770,0.4673623,0.4715448,0.4884892,0.4748990,0.4476299,0.4358722
	,0.4711266,0.5181454,0.4638828,0.4145035,0.4086761,0.3769698,0.4168491
	,0.3995621,0.3994567,0.3818555,0.3566065,0.3210223,0.3277504,0.3347357
	,0.3096230,0.2494944,0.2752111,0.2812650,0.2880133,0.2547461,0.2577831
	,0.2655543,0.3082606,0.2729099,0.2864546,0.3400130,0.2939717,0.2772311
	,0.2401896,0.2469673,0.2760278,0.3149003,0.2937609,0.3014855,0.3430952
	,0.3317969,0.3274767,0.3273904,0.2719462,0.2999646,0.2899044,0.2957682
	,0.3148225,0.3270416,0.3366530,0.3362095,0.3519136,0.3528588,0.3714007
	,0.3542342,0.3746892,0.3830057,0.4614028,0.4926285,0.5287413,0.4661413
	,0.4122825,0.4094138,0.3878824,0.3630945,0.3859890,0.3870051,0.4018734
	,0.4035850,0.4074792,0.4115561,0.3887392,0.3937447,0.3635494,0.3809561
	,0.3901723,0.4062473,0.4217663,0.4481528,0.4352627,0.4436658,0.4591507
	,0.4460224,0.4302818,0.4121313,0.4051081,0.4112737,0.4071016,0.4105361
	,0.3844476,0.3941982,0.4614024,0.4991462,0.4690294,0.4398135,0.4676629
	,0.4781111,0.4583126,0.4873321,0.4745802,0.5034698,0.4673128,0.4659498
	,0.4535076,0.3750969,0.3955898,0.3952339,0.3918407,0.4198282,0.4253686
	,0.4651107,0.5236992,0.5125279,0.5220159,0.5226915,0.5358942,0.5123398
	,0.5337614,0.5395950,0.5559245,0.5363898,0.5665369,0.6185322,0.6413660
	,0.6688709,0.6667232,0.6295344,0.6418631,0.6104991,0.6077750,0.5964772
	,0.6097810,0.6121514,0.5663109,0.5935986,0.5604419,0.5704700,0.5389148
	,0.4891560,0.4941551,0.4777012,0.4999693,0.4928038,0.4728614,0.4931551
	,0.5023519,0.4847507,0.5045297,0.4681350,0.4150245,0.3871865,0.4051983
	,0.4206542,0.4242496,0.3710008,0.3801972,0.3640533,0.3626515,0.3686213
	,0.3610023,0.3511952,0.3681898,0.3426478,0.3951637,0.3993360,0.3728225
	,0.3807452,0.3409846,0.3691345,0.3724542,0.3450199,0.3638699,0.3774255
	,0.3996511,0.4148192,0.4385101,0.4925334,0.5120324,0.5002565,0.5203925
	,0.4791517,0.4823381,0.4566553,0.4130824,0.4516081,0.4656073,0.4941690
	,0.5155517,0.5068433,0.4923941,0.4814121,0.5277589,0.5263973,0.5346948
	,0.5229837,0.5117594,0.5280914,0.5506838,0.5792088,0.5635740,0.5644362
	,0.5809358,0.6119965,0.5732389,0.5744329,0.6112362,0.5810814,0.5765827
	,0.5917304,0.5877516,0.5868656,0.6032156,0.6295300,0.6062283,0.6329054
	,0.6756078,0.6529967,0.6688667,0.6760310,0.6593934,0.6835539,0.6732955
	,0.6408130,0.6245613,0.6617184,0.7006874,0.6954005,0.7169194,0.7276890
	,0.7124657,0.6937119,0.7132574,0.7175151,0.7374374,0.6806536,0.6292985
	,0.6519937,0.6347332,0.6477382,0.6293217,0.6096423,0.6347078,0.6400194
	,0.6007239,0.6204448,0.6248804,0.6081984,0.5796915,0.5982369,0.5938009
	,0.6393520,0.6078676,0.5945788,0.5828047,0.5879304,0.5487439,0.5466403
	,0.5166143,0.5959482,0.6253057,0.6251259,0.5996078,0.5777931,0.5943473
	,0.5861946,0.5761836,0.6338423,0.6210723,0.6180332,0.6393480,0.5887565
	,0.6377954,0.6548214,0.6427114,0.6009161,0.6185744,0.6695215,0.6683355
	,0.6452573,0.5833309,0.5793243,0.6020430,0.6082401,0.6004815,0.5682867
	,0.5583667,0.5558955,0.5517333,0.5703526,0.5454106,0.5399706,0.5470319
	,0.5168974,0.5174503,0.5347812,0.5369852,0.5422179,0.5118822,0.4921483
	,0.5318369,0.4997766,0.4701401,0.4845768,0.4787737,0.4385020,0.4205838
	,0.3874563,0.3877289,0.3643715,0.3496778,0.3539614,0.3050362,0.3009444
	,0.2768285,0.2688291,0.2781685,0.2819621,0.2729789,0.3066509,0.2523713
	,0.2562888,0.3041062,0.3321943,0.2918876,0.3390311,0.3258945,0.3390627
	,0.3255537,0.3211422,0.3599334,0.3617782,0.3884084,0.4342485,0.4567677
	,0.4734630,0.4283309,0.4396863,0.4355196,0.4415061,0.4813137,0.4612586
	,0.4612385,0.4482962,0.4475342,0.4577646,0.4828377,0.5314959,0.5763702
	,0.5755668,0.5813282,0.5394374,0.5379922,0.5535083,0.5571914,0.5854053
	,0.5883346,0.5636016,0.5539500,0.5650132,0.5366127,0.5321532,0.5107034
	,0.5475683,0.5217959,0.4960035,0.5109130,0.5466659,0.5393121,0.5280207
	,0.4758744,0.4481659,0.4091691,0.4405916,0.4074067,0.4078477,0.4316715
	,0.4543463,0.4446475,0.4355632,0.4348114,0.4029807,0.3818708,0.4416302
	,0.4417296,0.4574880,0.4569616,0.4184682,0.4357853,0.3981934,0.4240734
	,0.3939916,0.3980502,0.3824554,0.4627833,0.4274445,0.4090374,0.4268217
	,0.4086987,0.3455261,0.3270066,0.3209647,0.3414739,0.3053481,0.2705716
	,0.2678843,0.2920721,0.2733469,0.3196192,0.3483625,0.3440595,0.3844619
	,0.3964599,0.3856161,0.3697749,0.3469533,0.3401540,0.2547506,0.2831229
	,0.3029712,0.3075460,0.3169798,0.3106951,0.3169019,0.3061075,0.3106125
	,0.2862985,0.2679283,0.2841206,0.3147856,0.2926715,0.2759874,0.2651469
	,0.3053058,0.3223947,0.3013492,0.3528692,0.3402529,0.3507029,0.3338409
	,0.3074246,0.2670437,0.2571091,0.2546759,0.2124865,0.2000000];

	FB_matrix1[1] = [0.3442198,0.3340234,0.3327027,0.3455348,0.3802842,0.3861670,0.3701327
	,0.3658172,0.3568191,0.3352908,0.3063046,0.2865610,0.2715607,0.2764143
	,0.3058561,0.2869535,0.3120974,0.3151766,0.3444998,0.3086873,0.2878920
	,0.2946128,0.2907544,0.2600783,0.2852903,0.2809752,0.2520196,0.2779951
	,0.2970468,0.2692975,0.2727663,0.2918218,0.3187624,0.2473905,0.2767178
	,0.2879378,0.3370201,0.3306566,0.3802129,0.3602474,0.3463669,0.3606268
	,0.3623437,0.3471383,0.3686864,0.3486585,0.3501418,0.3806813,0.3657619
	,0.4007836,0.4199924,0.4416488,0.4584806,0.4784791,0.4915801,0.4851725
	,0.5126764,0.5171627,0.5493345,0.5539577,0.5459113,0.5190503,0.5138296
	,0.5129728,0.5067886,0.5381197,0.5116876,0.4812901,0.4396586,0.4299833
	,0.4432136,0.4654048,0.4764849,0.4756466,0.4526142,0.4976376,0.5155977
	,0.5453248,0.5458380,0.5676103,0.5628491,0.5827539,0.5297262,0.5270097
	,0.5457808,0.5827900,0.5774289,0.6032056,0.6067180,0.5656227,0.5510663
	,0.5711649,0.5326743,0.5186788,0.5082897,0.5110291,0.4773799,0.5255665
	,0.5060966,0.5207229,0.5203451,0.5469765,0.5184408,0.5392334,0.5038533
	,0.5470979,0.5143895,0.5011960,0.5226962,0.5039385,0.5179177,0.5321415
	,0.5012155,0.4575785,0.3939778,0.3825049,0.3196566,0.3301440,0.3556741
	,0.3172770,0.3588511,0.4103614,0.4171459,0.4243370,0.4383874,0.3860340
	,0.3716999,0.3363648,0.2813305,0.2655353,0.3001068,0.3259080,0.2769494
	,0.3292448,0.3306038,0.3364893,0.3683037,0.3275482,0.3548322,0.3319966
	,0.3019582,0.3297482,0.3420393,0.3460975,0.3370220,0.3099523,0.2896268
	,0.2839943,0.2778810,0.2568358,0.2543509,0.2450374,0.2575521,0.3034955
	,0.2525538,0.2827668,0.3379238,0.3529317,0.3910820,0.3914956,0.4434300
	,0.4303701,0.4677804,0.4702506,0.5022478,0.5240762,0.4971677,0.4717096
	,0.5039364,0.5432843,0.5131574,0.4894094,0.4831504,0.4697534,0.4578356
	,0.4289157,0.4102236,0.3703785,0.3918719,0.3992249,0.3925409,0.4019363
	,0.4250490,0.4004574,0.3846007,0.3760110,0.3717637,0.3519037,0.3649949
	,0.3620816,0.3946444,0.3859697,0.3881760,0.3805629,0.4003124,0.3802867
	,0.3908352,0.3722725,0.3468315,0.3675040,0.3508363,0.4083598,0.4281959
	,0.4271477,0.4590347,0.4552455,0.4090434,0.4162599,0.4167985,0.3905694
	,0.4339216,0.4650452,0.4399398,0.4379262,0.4013044,0.4106990,0.4302986
	,0.3718831,0.3764317,0.3688253,0.3634982,0.3791679,0.3994622,0.3777632
	,0.3724251,0.3520685,0.3266572,0.2960185,0.2606368,0.2639534,0.2634164
	,0.2375439,0.2664626,0.2773618,0.3198797,0.3038661,0.2953690,0.2978204
	,0.3702295,0.3905457,0.3903121,0.3772850,0.3766211,0.4278559,0.4247251
	,0.4173121,0.4339071,0.4208694,0.4235120,0.3967979,0.3875875,0.4070193
	,0.4060842,0.3834087,0.3929368,0.3747812,0.3731893,0.3670595,0.3517420
	,0.3659343,0.3250779,0.3517311,0.3741093,0.3965686,0.3944253,0.3869561
	,0.4079472,0.3691395,0.3494927,0.3688544,0.3854848,0.3862469,0.4070734
	,0.3839897,0.4090918,0.4184447,0.4311470,0.4733765,0.5074098,0.5012000
	,0.4814235,0.4787160,0.5097549,0.5048700,0.4375180,0.4339540,0.4007951
	,0.3820342,0.3773760,0.3995810,0.4434240,0.5059051,0.5020424,0.5280267
	,0.5301615,0.5461594,0.5571780,0.5867020,0.6002622,0.6009167,0.5843301
	,0.5921734,0.6195610,0.6036920,0.5863663,0.6011506,0.6043275,0.6005526
	,0.6084452,0.5728783,0.5841401,0.6070789,0.5652639,0.5646594,0.5504347
	,0.5566177,0.5574798,0.6222259,0.6384716,0.6365517,0.6195838,0.6243111
	,0.6182311,0.5774104,0.5879160,0.6629384,0.6774576,0.6672006,0.6303399
	,0.6179342,0.6182393,0.6059312,0.6386963,0.6618159,0.6435418,0.6493121
	,0.6469418,0.6814056,0.6804449,0.6780661,0.6745880,0.6555908,0.7080271
	,0.7038212,0.7264850,0.7079905,0.7226177,0.7458587,0.7729886,0.7806829
	,0.7969962,0.8000000,0.8000000,0.8000000,0.8000000,0.8000000,0.8000000
	,0.8000000,0.7999799,0.8000000,0.8000000,0.7883056,0.7909646,0.7928877
	,0.8000000,0.7736038,0.7456671,0.7576285,0.7408537,0.7234604,0.7558498
	,0.7725547,0.7809183,0.8000000,0.8000000,0.7561643,0.7424624,0.7136600
	,0.7457102,0.7330630,0.7372419,0.7147755,0.7343083,0.7230029,0.7548992
	,0.7257958,0.6808510,0.7060089,0.7149340,0.7278126,0.7468853,0.7171280
	,0.7247618,0.7390135,0.7214293,0.7086096,0.7017023,0.7105530,0.7098807
	,0.6822656,0.6713146,0.6729737,0.7032107,0.6937017,0.6928422,0.7006002
	,0.6909675,0.6780906,0.7080878,0.6961255,0.7134122,0.7287310,0.7208724
	,0.6706240,0.6615174,0.6673393,0.6730587,0.7146829,0.7284414,0.7336293
	,0.7651668,0.7530105,0.7405801,0.7350279,0.7071809,0.7534448,0.7581855
	,0.7925443,0.7579208,0.7811258,0.7627962,0.7707469,0.7541374,0.7079547
	,0.6968397,0.7237010,0.7131916,0.7002084,0.7357998,0.7266216,0.7178934
	,0.7465136,0.7919050,0.8000000,0.8000000,0.7664576,0.7467976,0.7796029
	,0.7101267,0.6946649,0.7057424,0.6767992,0.6285513,0.6342783,0.6162477
	,0.6002831,0.5893561,0.5606682,0.5825323,0.6090500,0.6133894,0.6271193
	,0.6274979,0.6649038,0.6685988,0.6787316,0.6633412,0.6521523,0.6729275
	,0.6813263,0.7141777,0.7367559,0.7226787,0.7522743,0.6827326,0.7165237
	,0.7331711,0.7458800,0.7290826,0.7619809,0.8000000,0.8000000,0.8000000
	,0.8000000,0.7722989,0.7377525,0.7101397,0.7480826,0.7588847,0.7256522
	,0.7447037,0.7258783,0.7309174,0.7613223,0.7684294,0.7624058,0.7562011
	,0.7411200,0.7691449,0.7911839,0.8000000,0.7726857,0.8000000,0.7886868
	,0.8000000,0.7967447,0.7570324,0.7834660,0.7685911,0.7924935,0.7973317
	,0.7721570,0.7724719,0.7980537,0.7956969,0.8000000,0.7725176,0.7732415
	,0.7715358,0.7805771,0.7933995,0.7490290,0.7267081,0.7335118,0.7271633
	,0.7256815,0.7560992,0.8000000,0.7726164,0.7460307,0.7326137,0.7863034
	,0.7931804,0.8000000,0.7422843,0.7321047,0.7345984,0.7328768,0.7247203
	,0.7140380,0.7019472,0.7012770,0.6954898,0.7024334,0.7099975,0.6992703
	,0.7183083,0.6979131,0.6964399,0.6958513,0.6932373,0.7503616,0.7505321
	,0.7719559,0.8000000,0.8000000,0.7613855,0.7757674,0.7670727,0.7348525
	,0.7005832,0.6967524,0.7414391,0.6978338,0.7135982,0.6907731,0.6896196
	,0.6805762,0.6634437,0.6996566,0.6882440,0.6658322,0.6775436,0.6898511
	,0.6440855,0.6342447,0.6175236,0.6021703,0.5604761,0.5780441,0.5985023
	,0.6030947,0.5934831,0.5701494,0.5511613,0.5817813,0.5160041,0.5453941
	,0.5484431,0.5853498,0.6007215,0.5931208,0.5758980,0.5836393,0.5661687
	,0.6023468,0.5848987,0.5950081,0.6064660,0.5958558,0.6314681,0.6243028
	,0.5726141,0.5691486,0.6053347,0.6171063,0.6436007,0.6560855,0.6634690
	,0.6484652,0.6147073,0.6367216,0.6441656,0.6346779,0.6079503,0.6025773
	,0.6089897,0.5787538,0.6059279,0.6283548,0.6331341,0.6142626,0.6077591
	,0.6246983,0.6113448,0.6307591,0.6261207,0.6385355,0.6688429,0.6982762
	,0.6725169,0.6922693,0.6838726,0.6728939,0.7167105,0.7078889,0.7743422
	,0.7780134,0.7855654,0.8000000,0.8000000,0.7943864,0.8000000,0.7764006
	,0.7817362,0.7797207,0.8000000,0.8000000,0.8000000,0.8000000,0.7525208
	,0.7128543,0.6942207,0.7177129,0.7312002,0.7344293,0.7279041,0.7054607
	,0.7122089,0.7033314,0.7343117,0.7310310,0.7224530,0.7258564,0.7175684
	,0.6782806,0.6888890,0.6729515,0.6990549,0.6647791,0.6470727,0.6226780
	,0.6204897,0.6429497,0.6182031,0.6080647,0.5899625,0.5755353,0.5790492
	,0.5876073,0.5637187,0.6194163,0.6141719,0.6228513,0.6299639,0.6538107
	,0.6311717,0.6260807,0.6102388,0.6288829,0.6434122,0.6070058,0.6426303
	,0.6358638,0.6374097,0.6498306,0.6581518,0.6407534,0.5929547,0.5790267
	,0.6002181,0.6189889,0.6300200,0.5896928,0.5833896,0.5583892,0.5690030
	,0.6061704,0.6269479,0.5835758,0.6010119,0.5856935,0.6086481,0.6076399
	,0.6496246,0.6352387,0.6282161,0.6740317,0.6659616,0.6797052,0.6801363
	,0.6842486,0.7035514,0.7558177,0.7819050,0.7865537,0.7938050,0.7601983
	,0.7564934,0.7839667,0.7600863,0.7632799,0.7754721,0.7696952,0.7477066
	,0.7740529,0.7330200,0.7544425,0.7430918,0.7757746,0.7645412,0.7510008
	,0.7759994,0.7945658,0.8000000,0.7907524,0.7768207,0.7940481,0.8000000
	,0.7995993,0.7848513,0.7037198,0.7298924,0.7389580,0.7318966,0.7078749
	,0.7079088,0.7333195,0.7242198,0.7521049,0.7512019,0.7701681,0.7763000
	,0.8000000,0.7511428,0.7387027,0.7320722,0.7276617,0.7313672,0.7422932
	,0.7724585,0.7799519,0.7778130,0.7537326,0.8000000,0.7955916,0.7632442
	,0.7449044,0.7235985,0.7157699,0.7243648,0.7520902,0.8000000,0.8000000
	,0.7788664,0.7877905,0.7841467,0.7746897,0.7381396,0.7482404,0.7539550
	,0.7000457,0.6838281,0.6966760,0.6865291,0.7034475,0.6712457,0.6452676
	,0.6931114,0.6642477,0.6047202,0.6177916,0.5753501,0.5657315,0.5621175
	,0.5523821,0.5359241,0.5311083,0.5404807,0.5513841,0.5687250,0.5277861
	,0.5647517,0.5655048,0.5598115,0.5710410,0.5838068,0.5947475,0.6169552
	,0.6065019,0.6148897,0.6393264,0.6328701,0.6534595,0.6683961,0.6740730
	,0.7041935,0.7410636,0.7304547,0.7699609,0.7885055,0.7707120,0.7387877
	,0.7302464,0.7251820,0.7607149,0.8000000,0.7907913,0.7775518,0.7195197
	,0.7181262,0.7683397,0.8000000,0.8000000,0.8000000,0.8000000,0.8000000
	,0.8000000,0.7788033,0.7757785,0.8000000,0.7845730,0.8000000,0.7931795
	,0.7930755,0.7830922,0.7493083,0.7734251,0.7731574,0.7397441,0.7531389
	,0.7359429,0.7134701,0.7314303,0.7446735,0.7136065,0.6895189,0.7125725
	,0.6746083,0.6701833,0.6467362,0.6328279,0.6095483,0.5815875,0.5954247
	,0.5995502,0.5882813,0.6187062,0.5961184,0.5677608,0.5359799,0.5608594
	,0.5207660,0.5102712,0.5112437,0.5187446,0.4854221,0.4702423,0.4749293
	,0.4659548,0.4613732,0.4269202,0.3875811,0.4408182,0.4600126,0.4294455
	,0.4424713,0.4780907,0.5064911,0.5249876,0.4905161,0.4357991,0.4277924
	,0.4120048,0.3883755,0.3745270,0.3450742,0.3537332,0.3520550,0.3259480
	,0.3000864,0.3075950,0.2498490,0.2621614,0.2440765,0.2926080,0.2864440
	,0.2567929,0.2213520,0.2237951,0.2000000,0.2000000,0.2000000,0.2000000
	,0.2148414,0.2234677,0.2000000,0.2000000,0.2264828,0.2491618,0.2687543
	,0.2491160,0.2204955,0.2309614,0.2310816,0.2895083,0.2623632,0.2781455
	,0.2902923,0.3297585,0.2954491,0.2714650,0.2471068,0.2372507,0.2795190
	,0.2763614,0.2232469,0.2244084,0.2341864,0.2200532,0.2414784,0.2000000
	,0.2000000,0.2149594,0.2043310,0.2134235,0.2000000,0.2070188,0.2071181
	,0.2178217,0.2000000,0.2000000,0.2000000,0.2000000,0.2131923,0.2000000
	,0.2325933,0.2145468,0.2102925,0.2011537,0.2000000,0.2000000,0.2000000
	,0.2000000,0.2282381,0.2112183,0.2334598,0.2838617,0.3160794,0.3176113
	,0.3276953,0.3125008,0.2642037,0.2517580,0.2378093,0.2657709,0.3129812
	,0.4025286,0.4407576,0.4209231,0.3849606,0.4592534,0.4242348,0.4071564
	,0.4231040,0.4139300,0.3661564,0.3746036,0.3791919,0.4192339];

	FB_matrix1[2] =	[0.4191846,0.4837978,0.4538876,0.4447230,0.4829738,0.4690658,0.4703774
	,0.4766901,0.4813980,0.4450169,0.4344249,0.4552269,0.4410684,0.4926503
	,0.5097746,0.4980754,0.4834097,0.5093161,0.5451685,0.5526403,0.5056880
	,0.4980453,0.4885703,0.4629185,0.5158396,0.4656236,0.4997898,0.5122041
	,0.4663080,0.4773360,0.4724523,0.4646198,0.4527151,0.4267201,0.4704202
	,0.5164488,0.5312920,0.5010951,0.5087342,0.5016573,0.4696971,0.4781522
	,0.4706787,0.4406662,0.4261310,0.4199333,0.4109880,0.3726346,0.3977935
	,0.4038553,0.3755172,0.3974936,0.3892183,0.3643456,0.3218912,0.3381754
	,0.3346765,0.3655186,0.3720537,0.3573307,0.3748870,0.3778702,0.3608839
	,0.3428423,0.3541212,0.2702693,0.2836891,0.2873456,0.2735096,0.2526175
	,0.2123193,0.2222103,0.2147373,0.2116712,0.2307345,0.2316824,0.2845272
	,0.3036792,0.2951565,0.3045697,0.2915602,0.2983851,0.3298273,0.3636776
	,0.3625237,0.3643159,0.3805559,0.4098097,0.3843984,0.3685425,0.3762417
	,0.3454392,0.3450273,0.4015680,0.3993194,0.3806927,0.3859952,0.3083676
	,0.3099713,0.2833906,0.2886928,0.2829741,0.2901103,0.2732318,0.3124133
	,0.2935851,0.3039941,0.3344642,0.3010489,0.2790355,0.2268626,0.2179643
	,0.2014910,0.2032132,0.2067225,0.2000000,0.2069127,0.2202840,0.2580958
	,0.2329326,0.2193211,0.2093899,0.2331028,0.2616294,0.2407604,0.2613230
	,0.2486029,0.2325614,0.2239179,0.2000305,0.2000000,0.2154627,0.2041893
	,0.2000000,0.2370912,0.2410991,0.2329849,0.2196981,0.2638798,0.2467821
	,0.2611474,0.2417215,0.2405136,0.2349165,0.2243606,0.2000000,0.2000000
	,0.2000000,0.2313394,0.2223961,0.2246224,0.2204732,0.2000000,0.2109819
	,0.2451268,0.2393975,0.2430315,0.2294413,0.2276994,0.2145005,0.2300422
	,0.2224696,0.2573651,0.2955954,0.3282475,0.3107596,0.3211850,0.3111794
	,0.3408966,0.3164138,0.3308772,0.3336275,0.3506148,0.3637668,0.3152397
	,0.2993681,0.2737498,0.2834076,0.2444666,0.3327666,0.3475586,0.3464853
	,0.3849286,0.4223799,0.4629829,0.4570241,0.4830622,0.4876511,0.4698549
	,0.4430799,0.4963925,0.4787772,0.4671977,0.4342438,0.4199505,0.4062382
	,0.4016363,0.4237504,0.4550849,0.4983612,0.5203918,0.5261720,0.5478310
	,0.5083476,0.5357536,0.5056088,0.4695358,0.4960359,0.4253359,0.4350220
	,0.4097604,0.3749156,0.3471238,0.3719857,0.3738621,0.3702649,0.4062069
	,0.3913526,0.4194027,0.3948287,0.4074499,0.3861498,0.3783899,0.3582184
	,0.3104724,0.3130402,0.3319572,0.3483902,0.3390156,0.3478721,0.3823620
	,0.3878292,0.3762822,0.3819742,0.4221744,0.4088014,0.4053000,0.4359550
	,0.4754647,0.4843993,0.4830215,0.4675407,0.4621255,0.4262993,0.4013459
	,0.4108737,0.3740060,0.3743162,0.3699242,0.3619231,0.3602730,0.3370395
	,0.3311713,0.3569456,0.3598862,0.3556912,0.3883483,0.3812861,0.3992248
	,0.4113805,0.4156902,0.4908213,0.4980758,0.4706559,0.5068311,0.4600374
	,0.4694101,0.4720440,0.4572446,0.4451099,0.4517472,0.4798074,0.5045857
	,0.5031791,0.4861286,0.5037972,0.4889749,0.4466473,0.4492714,0.4543600
	,0.4386973,0.4015502,0.3895317,0.3632671,0.3632813,0.3726395,0.3989931
	,0.4063020,0.4597887,0.4028015,0.4217746,0.3933427,0.4113484,0.4212507
	,0.4262586,0.4260288,0.4193968,0.4225182,0.4432970,0.4097312,0.3852358
	,0.4200462,0.4380124,0.4482545,0.4533131,0.3796595,0.3805073,0.4330795
	,0.4136787,0.3776310,0.3651447,0.3428474,0.3731897,0.3887619,0.3789733
	,0.3787293,0.3691887,0.3992186,0.3772772,0.3588173,0.3727209,0.4141090
	,0.3955273,0.3945516,0.4037202,0.4085999,0.4163046,0.4523067,0.4172080
	,0.3748607,0.3742061,0.3767275,0.3868933,0.3705458,0.4279957,0.4579089
	,0.4734079,0.4301690,0.4222925,0.4488135,0.4305778,0.4166668,0.4126220
	,0.3614696,0.3744481,0.3728764,0.3878846,0.4186135,0.4065459,0.4109713
	,0.4170761,0.4126800,0.4164538,0.3557661,0.3308396,0.3143156,0.2983277
	,0.2749309,0.2982389,0.2841864,0.2859686,0.2925045,0.3100506,0.3198900
	,0.2952968,0.2795630,0.2948850,0.2502900,0.2257773,0.2550189,0.2655670
	,0.2597891,0.2330587,0.2035889,0.2000000,0.2130485,0.2305645,0.2602531
	,0.2782474,0.2806552,0.2922301,0.2506544,0.2213672,0.2092104,0.2084887
	,0.2151564,0.2000000,0.2147108,0.2062161,0.2000000,0.2302113,0.2422007
	,0.2857751,0.2846647,0.2984180,0.2817206,0.2692260,0.3068123,0.3566071
	,0.3903765,0.3829456,0.3654294,0.4135928,0.4007702,0.4030623,0.3437919
	,0.3587721,0.3638885,0.3649893,0.3547480,0.3258769,0.3360191,0.3203870
	,0.3544843,0.4033126,0.3844898,0.3938011,0.4206674,0.3622921,0.3396353
	,0.3513178,0.3646221,0.3337632,0.3684627,0.3815986,0.3448751,0.3409695
	,0.3966917,0.4108213,0.4063784,0.4094325,0.3852668,0.3692780,0.3801089
	,0.3967721,0.3927068,0.4011836,0.4501492,0.3913527,0.3676050,0.3562555
	,0.3329159,0.3640177,0.3679324,0.3823323,0.3881313,0.4165830,0.4057938
	,0.3970997,0.3789061,0.3619307,0.3511884,0.3441079,0.3617027,0.3935091
	,0.3969319,0.3967759,0.4230092,0.4293200,0.4565081,0.4380355,0.4590598
	,0.4822984,0.4334195,0.4641163,0.4550911,0.4811526,0.4734525,0.5118685
	,0.4603894,0.4675081,0.4762839,0.4504248,0.4543345,0.4555387,0.4441897
	,0.4410238,0.4513675,0.4309503,0.4033140,0.4032195,0.4005700,0.3703995
	,0.4005870,0.3904929,0.4048425,0.3849401,0.3970996,0.3507764,0.3576186
	,0.3547076,0.4058203,0.3994040,0.3720288,0.3780943,0.3865706,0.3407845
	,0.3529642,0.3516170,0.3533108,0.3573268,0.3684499,0.3432096,0.3706120
	,0.3250448,0.3453497,0.3469674,0.2828348,0.2952436,0.2649677,0.2643300
	,0.2642682,0.2679595,0.2757205,0.2559709,0.2234976,0.2194411,0.2000000
	,0.2000000,0.2207951,0.2230163,0.2273550,0.2482232,0.2366486,0.2377538
	,0.2961213,0.3184967,0.3150288,0.3180729,0.3271413,0.3492436,0.3805028
	,0.4076655,0.3333721,0.3072230,0.2880344,0.2576502,0.2263409,0.2084036
	,0.2047326,0.2123552,0.2062660,0.2000000,0.2082132,0.2000000,0.2000000
	,0.2152277,0.2000000,0.2140396,0.2376859,0.2133517,0.2327604,0.2939844
	,0.3333614,0.3504065,0.3512907,0.3529123,0.3273446,0.3503713,0.3750694
	,0.3948088,0.4168274,0.4056806,0.3946386,0.4144830,0.3986768,0.3896134
	,0.3734442,0.4193014,0.4044212,0.4489513,0.5339731,0.5667934,0.5789762
	,0.5348182,0.5157480,0.5148415,0.5433684,0.5326017,0.5263225,0.4878035
	,0.4529000,0.5043943,0.5039027,0.5026642,0.4814850,0.5027189,0.4816683
	,0.5063668,0.5372190,0.5338814,0.5512426,0.5631122,0.5526163,0.5111844
	,0.5262257,0.5352507,0.5356396,0.5384619,0.5312986,0.5532493,0.5146085
	,0.5264384,0.5146238,0.4862327,0.4395864,0.4357928,0.4541251,0.4538156
	,0.4252838,0.3897354,0.4129226,0.4391500,0.4736689,0.4490386,0.4395279
	,0.3991927,0.4081390,0.3924426,0.3878033,0.3877176,0.3683100,0.3332115
	,0.3612164,0.3406301,0.3811499,0.3662985,0.3752276,0.3449936,0.3312562
	,0.3329943,0.3591434,0.3614940,0.3668948,0.3890898,0.3660531,0.3670254
	,0.3579261,0.3747692,0.3506009,0.3562945,0.3802595,0.3332009,0.3595241
	,0.3701228,0.3847331,0.3891959,0.4198635,0.4090525,0.4003516,0.3889587
	,0.4196022,0.4430696,0.4369078,0.4038370,0.4212393,0.4008266,0.4171659
	,0.4566988,0.4583847,0.4348134,0.4275350,0.4240878,0.4284927,0.4225591
	,0.4552527,0.4615509,0.4963156,0.4704545,0.4688930,0.4573146,0.4717802
	,0.5049659,0.4994052,0.4802803,0.4799692,0.4990298,0.4939954,0.5280624
	,0.5105497,0.5318059,0.5551243,0.5175113,0.5128920,0.5473390,0.5505485
	,0.5043277,0.5073816,0.4877517,0.5109188,0.4995238,0.4789708,0.4491155
	,0.4552734,0.4725114,0.4653080,0.4668264,0.4948124,0.4754921,0.4857066
	,0.4557521,0.4927348,0.4962306,0.4776273,0.4823412,0.4668852,0.5347966
	,0.4888830,0.4755572,0.4589471,0.4213880,0.4113833,0.3937740,0.3593118
	,0.3440872,0.3850474,0.3747311,0.4292403,0.3962751,0.3740324,0.3754491
	,0.3671731,0.3796760,0.4053028,0.4059814,0.4029929,0.3781760,0.3314745
	,0.3499317,0.3438579,0.3429124,0.3503184,0.3134474,0.3623848,0.3756246
	,0.4013103,0.4261762,0.4550363,0.4780742,0.4968667,0.5127902,0.5667757
	,0.5560358,0.5599556,0.5457050,0.5660316,0.5723695,0.5861465,0.6175715
	,0.6154747,0.6088780,0.6193619,0.6522663,0.6327355,0.6124047,0.6018534
	,0.6149192,0.6084686,0.6236414,0.6136780,0.6152097,0.6209321,0.6427033
	,0.6200880,0.6185524,0.5869279,0.5867760,0.5957065,0.6034969,0.6016825
	,0.5961580,0.6220319,0.6640458,0.7177657,0.7136456,0.7248408,0.7208985
	,0.7229297,0.7540777,0.7708947,0.7727030,0.7869712,0.7765193,0.7607724
	,0.7926547,0.7680723,0.7252822,0.7089182,0.7126482,0.6989864,0.6947708
	,0.6867259,0.6884180,0.6749150,0.6625014,0.6747849,0.6469333,0.6720698
	,0.6870049,0.6962361,0.6942654,0.6835591,0.6639002,0.6244608,0.6407262
	,0.6238317,0.6233045,0.6735777,0.6334440,0.6417941,0.6419738,0.6307861
	,0.6277063,0.6138387,0.6097867,0.6299191,0.6257269,0.6490171,0.6772175
	,0.6831611,0.6474174,0.6270156,0.5922803,0.5812659,0.5251918,0.5643735
	,0.5378678,0.5476758,0.5462472,0.6137979,0.6253784,0.6093254,0.5746140
	,0.5555959,0.5446166,0.5098692,0.5498679,0.5408029,0.5287257,0.5176601
	,0.5490051,0.5797045,0.5758886,0.5475144,0.5333510,0.5593540,0.5428305
	,0.5318261,0.5232214,0.5182910,0.4929802,0.4929693,0.4752694,0.5192724
	,0.5077111,0.5250315,0.5127259,0.4742918,0.4765173,0.5047107,0.5038269
	,0.4584126,0.4317829,0.4258636,0.4373139,0.4457603,0.4216809,0.4153204
	,0.4299520,0.4038486,0.3723008,0.3992570,0.3852729,0.3825466,0.4116830
	,0.3896392,0.3958218,0.4077928,0.3708147,0.3614458,0.3173588,0.3610284
	,0.3152078,0.3504099,0.3396912,0.3255554,0.2976685,0.3041475,0.3055283
	,0.3306015,0.3708251,0.3923083,0.3952105,0.3945216,0.4036226,0.3510553
	,0.3234812,0.3061375,0.3124070,0.3570716,0.3712463,0.3821763,0.3547405
	,0.3803842,0.3701144,0.3305001,0.3595452,0.3540574,0.3588887,0.3771394
	,0.3637598,0.3683321,0.3855851,0.3957062,0.4103269,0.3975699,0.3604928
	,0.3250984,0.3477809,0.3269878,0.3216074,0.2921393,0.3279753,0.3506297
	,0.3077225,0.2794371,0.2395771,0.2011381,0.2000000,0.2000000,0.2220685
	,0.2063433,0.2000000,0.2630504,0.2649927,0.3074845,0.3430216,0.3498673
	,0.3067653,0.3043615,0.2677235,0.2763317,0.2947632,0.2959085,0.3221200
	,0.3126151,0.3144622,0.3317337,0.2962048,0.2502308,0.2458366,0.2343610
	,0.2632791,0.2869680,0.3080780,0.3166357,0.3299581,0.3451409,0.3791878
	,0.3361747,0.3143610,0.3478060,0.3549181,0.3952940,0.3830087,0.3884806
	,0.3966207,0.3759362,0.3705448,0.4059238,0.3980828,0.4090989,0.4021774
	,0.3826359,0.3464629,0.3365837,0.3375902,0.3377587,0.3041539,0.2538402
	,0.2635236,0.2289797,0.2483044,0.2000000,0.2000000,0.2000000,0.2060638
	,0.2064626,0.2000000,0.2015175,0.2000000,0.2405717,0.2199498,0.2353825
	,0.2423687,0.2604078,0.2341267,0.2564792,0.2914024,0.2814275];

	FB_matrix1[3] = [0.6700551,0.6772635,0.6590096,0.6574792,0.6580501,0.6066152,0.5777221
	,0.5856813,0.6038298,0.6159238,0.5910694,0.5949191,0.6253701,0.6395690
	,0.6288624,0.6206151,0.6188085,0.6443423,0.6421750,0.6457935,0.6625932
	,0.6670194,0.6451251,0.6450982,0.6567860,0.6575728,0.6273536,0.6449395
	,0.6881309,0.7124408,0.6467350,0.6476505,0.6769757,0.6663022,0.6576330
	,0.6335500,0.5719868,0.5663279,0.5564954,0.5571602,0.5555496,0.5861793
	,0.6310136,0.6362960,0.6234761,0.6580132,0.6219486,0.5916512,0.5787742
	,0.5954469,0.5750869,0.5862959,0.6142394,0.6173160,0.6179492,0.7036326
	,0.6890120,0.7119556,0.7423169,0.7725073,0.8000000,0.7894357,0.8000000
	,0.8000000,0.7934579,0.7559959,0.7655633,0.7990149,0.8000000,0.7713253
	,0.7961206,0.7867441,0.7910536,0.7377914,0.7313431,0.7240317,0.7337687
	,0.7143157,0.7604630,0.7281542,0.7335116,0.7482423,0.7767002,0.8000000
	,0.8000000,0.7777034,0.7838826,0.8000000,0.7726452,0.7528934,0.7548263
	,0.7443510,0.7600428,0.7851646,0.7747253,0.7597745,0.7389300,0.7492102
	,0.7128490,0.7112402,0.7518594,0.7307639,0.7047241,0.7044863,0.7245970
	,0.6906722,0.6673317,0.6781939,0.6432074,0.6497555,0.6517377,0.6361774
	,0.5956686,0.6101307,0.6222302,0.5939239,0.6059770,0.6090301,0.6180701
	,0.5261311,0.5514016,0.5555868,0.5830846,0.5965222,0.5742512,0.5720644
	,0.5806179,0.5854438,0.6012277,0.6214551,0.6088201,0.6370767,0.6276858
	,0.6417522,0.6240867,0.6121706,0.6233503,0.6192361,0.6330627,0.6383466
	,0.6059206,0.5806348,0.5738463,0.6072055,0.5943385,0.6003382,0.6066171
	,0.6139107,0.6240660,0.6711620,0.7110651,0.7218322,0.7074368,0.6747813
	,0.6926509,0.7208432,0.7057168,0.7268378,0.7404987,0.7438514,0.7219344
	,0.7173851,0.6887944,0.6923488,0.6274829,0.5910909,0.5657538,0.5592786
	,0.5732752,0.5935875,0.5925384,0.6139533,0.6145996,0.6036565,0.5958461
	,0.5639349,0.5771974,0.6387522,0.5964220,0.5687517,0.5854808,0.6048537
	,0.6028262,0.5894067,0.5899417,0.6557857,0.7264469,0.6670787,0.6855319
	,0.6791569,0.6776240,0.6873506,0.6751781,0.6293180,0.5973146,0.5847547
	,0.5598477,0.5081022,0.4967000,0.4826454,0.5112483,0.4998879,0.4885300
	,0.4871491,0.4851854,0.4270669,0.4076122,0.4477853,0.5078258,0.5502944
	,0.5549874,0.5722922,0.5429385,0.5497409,0.5831211,0.6040968,0.5608024
	,0.5070508,0.5660282,0.5737838,0.6457066,0.6828707,0.7036370,0.6978249
	,0.6943055,0.7215147,0.6786551,0.6868034,0.7316948,0.7045679,0.7069792
	,0.7143358,0.7153747,0.6985840,0.7407284,0.7765438,0.7616594,0.7666895
	,0.7688351,0.7524162,0.7165915,0.7168768,0.7213448,0.7242944,0.6892344
	,0.7364554,0.7188481,0.7498378,0.7155912,0.7342694,0.7856357,0.7693747
	,0.7999181,0.7705138,0.7474810,0.7595706,0.7558565,0.7411011,0.7511723
	,0.7517396,0.7243236,0.7468059,0.7292964,0.7318973,0.7221071,0.7291344
	,0.7015364,0.6915929,0.6664334,0.6772399,0.6510284,0.6314220,0.5979987
	,0.6132438,0.5560583,0.5562466,0.5255830,0.5200233,0.4862721,0.5367413
	,0.5534755,0.5223796,0.5343304,0.5248009,0.5144093,0.4536355,0.4107472
	,0.4364323,0.4339796,0.4620637,0.4636073,0.4484014,0.4328072,0.4521450
	,0.4832662,0.4865948,0.4582951,0.5064032,0.4740363,0.4682906,0.4536111
	,0.4354005,0.4311594,0.4489911,0.4545427,0.4429109,0.4551072,0.4788274
	,0.5270007,0.5099876,0.5125328,0.4935163,0.4951447,0.5060406,0.5237252
	,0.5389509,0.5378611,0.5941850,0.5894254,0.5718635,0.5879722,0.5741959
	,0.5706623,0.5575027,0.5787836,0.5958633,0.5965374,0.6473103,0.6488151
	,0.6902187,0.6805996,0.6650221,0.6783560,0.7026132,0.6703013,0.6708449
	,0.6890782,0.6738667,0.7097067,0.7594556,0.8000000,0.8000000,0.7922313
	,0.8000000,0.8000000,0.7952396,0.7761330,0.7528350,0.8000000,0.8000000
	,0.7649710,0.7953590,0.7514423,0.7598431,0.7849829,0.8000000,0.8000000
	,0.7990351,0.8000000,0.7892324,0.7936714,0.7828773,0.7807350,0.7824751
	,0.8000000,0.8000000,0.8000000,0.7653392,0.7309394,0.7217308,0.7017224
	,0.7279553,0.7084185,0.6822601,0.6876287,0.7076620,0.7298630,0.7297868
	,0.6969411,0.6869392,0.7541678,0.7497730,0.7387456,0.7533186,0.7414466
	,0.7329872,0.7260323,0.7482128,0.7876664,0.8000000,0.7984946,0.7962533
	,0.7588728,0.7206637,0.7097546,0.6670354,0.6186331,0.5774847,0.6183951
	,0.6065693,0.6239707,0.6508275,0.6526711,0.6959541,0.7016874,0.6693749
	,0.6951436,0.6930785,0.6994102,0.6968762,0.7103447,0.6966270,0.7048117
	,0.6839780,0.7116096,0.7113728,0.6803570,0.6867019,0.6843111,0.6536806
	,0.6643235,0.6012537,0.5851999,0.5546921,0.5161761,0.5126873,0.4936481
	,0.4400443,0.4625874,0.4485767,0.4230597,0.4089382,0.4033851,0.3662870
	,0.3875474,0.4014367,0.3943181,0.3757216,0.4176377,0.4147143,0.4052660
	,0.3924044,0.3674833,0.3702723,0.3835990,0.3674343,0.3388048,0.3773181
	,0.3634715,0.3354585,0.3220856,0.3404031,0.3085002,0.3446487,0.3312102
	,0.2808122,0.3357114,0.3461786,0.3584144,0.4332407,0.4777683,0.4853354
	,0.4768458,0.4912509,0.4582864,0.4549660,0.4725502,0.4460474,0.4283269
	,0.4356542,0.4403033,0.4478074,0.4872071,0.4707101,0.4822267,0.4745740
	,0.5013123,0.4574449,0.3974843,0.3765784,0.3771437,0.3873352,0.4000251
	,0.3853280,0.3782209,0.3863791,0.4120494,0.3905647,0.3560554,0.3516275
	,0.3194075,0.3085371,0.3267964,0.3415301,0.3300477,0.3325329,0.3211671
	,0.3062208,0.3076186,0.3390883,0.3445664,0.3160649,0.3140733,0.2983565
	,0.2659985,0.2591981,0.2613980,0.2389171,0.2586046,0.3029592,0.3123297
	,0.3035088,0.2793803,0.3203195,0.3235956,0.2953878,0.2624788,0.2311880
	,0.2199774,0.2000000,0.2000000,0.2000000,0.2000000,0.2000000,0.2392064
	,0.2555134,0.2317748,0.2484832,0.2465825,0.2094595,0.2000000,0.2000000
	,0.2490428,0.2960638,0.2937465,0.2681658,0.3004101,0.3120561,0.3614905
	,0.3959851,0.4411916,0.4137656,0.4219014,0.4831030,0.4445959,0.5013227
	,0.4962059,0.4641680,0.5110794,0.4644142,0.4118512,0.4008749,0.4142897
	,0.4762321,0.4783598,0.4410484,0.4675649,0.4628803,0.4548536,0.4588824
	,0.4669805,0.4841369,0.4933520,0.4799267,0.4884823,0.5349123,0.5280133
	,0.5493124,0.5326127,0.5048164,0.5584496,0.5470087,0.5414498,0.5824316
	,0.5476465,0.5461618,0.5467809,0.5623874,0.5551313,0.5737549,0.5921933
	,0.5673048,0.5714689,0.5939443,0.6376914,0.6462078,0.6756644,0.6884310
	,0.6338734,0.6779872,0.6635608,0.6398201,0.6671739,0.6416251,0.6158214
	,0.6410092,0.6694212,0.6864919,0.6968071,0.6872340,0.7086504,0.7200923
	,0.6992567,0.6979217,0.7016800,0.7089758,0.7032732,0.7210921,0.7009531
	,0.7030475,0.7175835,0.7512100,0.7699249,0.7462120,0.7138298,0.7461313
	,0.7414436,0.7757363,0.7516873,0.7505083,0.7290857,0.7462239,0.7452414
	,0.7643254,0.7331825,0.7212544,0.6952151,0.6571502,0.6040256,0.5736638
	,0.5874778,0.6174869,0.6173060,0.5953826,0.6027048,0.5577768,0.5927980
	,0.5984885,0.6033969,0.6164585,0.6055507,0.6319919,0.6295999,0.6253094
	,0.5707517,0.5579488,0.4860888,0.4525117,0.4400397,0.4227914,0.4269773
	,0.4287570,0.4275882,0.4406533,0.4088788,0.4314349,0.4003258,0.4341047
	,0.5027883,0.4904738,0.4498152,0.4476709,0.4422417,0.4236144,0.4264933
	,0.4445541,0.3985034,0.3220545,0.3397860,0.3413972,0.3237446,0.3241543
	,0.3363137,0.3186065,0.3195852,0.3063721,0.2951625,0.3105672,0.3473276
	,0.3739839,0.3553478,0.3768626,0.3544212,0.3484310,0.3545485,0.3818655
	,0.4000908,0.3891620,0.4475758,0.4404447,0.4075018,0.4031007,0.4366197
	,0.3972161,0.4000326,0.3846400,0.4489519,0.4521249,0.4645296,0.4537475
	,0.4389090,0.4673451,0.4618340,0.4702667,0.4653870,0.3988339,0.4235680
	,0.4132376,0.4011248,0.3911892,0.4045060,0.4452531,0.4580485,0.4602425
	,0.4745879,0.4706880,0.4962972,0.5580233,0.5789029,0.5515023,0.5420469
	,0.5193402,0.4881853,0.4419159,0.4313074,0.4520769,0.4469154,0.4366224
	,0.4283808,0.4492582,0.3940233,0.4487388,0.4463088,0.4242874,0.4268918
	,0.4433764,0.4601782,0.4332190,0.4632100,0.5017584,0.5124600,0.5101268
	,0.5338480,0.5462651,0.5625290,0.5335771,0.5230860,0.5452290,0.5403904
	,0.5406154,0.4928459,0.4588670,0.4644554,0.4758694,0.5051310,0.5007445
	,0.4923940,0.4911663,0.5636915,0.6180278,0.5981104,0.6018541,0.6102799
	,0.6516081,0.6463896,0.6504135,0.7055488,0.6897563,0.6486883,0.6623137
	,0.6655988,0.6835687,0.6630179,0.6572268,0.6670993,0.6905301,0.7091844
	,0.6945453,0.7046701,0.6752245,0.6647563,0.6763016,0.6784630,0.7062675
	,0.7086124,0.7301836,0.7047642,0.6822257,0.6916873,0.7021170,0.7009769
	,0.6682268,0.6631634,0.6885916,0.6926818,0.6959012,0.6867698,0.6803888
	,0.6711276,0.6605447,0.6504205,0.6407674,0.6878251,0.7116591,0.6937008
	,0.6968022,0.6959182,0.6912941,0.7185150,0.6998846,0.6589748,0.6270096
	,0.7129003,0.7094230,0.7338829,0.7273945,0.6882497,0.6712259,0.6157338
	,0.6734234,0.6597404,0.6623719,0.6434522,0.6370580,0.5973499,0.5988623
	,0.5888006,0.6139539,0.5835918,0.5458037,0.5184428,0.4952150,0.5192458
	,0.5152308,0.4852397,0.5159749,0.5355743,0.5567615,0.5654259,0.5508563
	,0.4858159,0.4707679,0.4398558,0.4251392,0.4094751,0.4209550,0.4357552
	,0.4241245,0.4372855,0.4295686,0.3762281,0.3845102,0.3484419,0.3284020
	,0.3545764,0.3250899,0.3384502,0.3044966,0.3042716,0.2969352,0.3151480
	,0.3261822,0.3338017,0.3057786,0.3652108,0.4088093,0.3854057,0.3741723
	,0.3998458,0.4138625,0.4491475,0.4448469,0.4144029,0.4249637,0.4216573
	,0.3913436,0.3518718,0.3355652,0.3096393,0.3323978,0.2629483,0.2817528
	,0.2735674,0.2721543,0.2707218,0.2882830,0.2829928,0.2724271,0.2723735
	,0.3090675,0.2988770,0.3018775,0.2835955,0.3517095,0.3470106,0.3683531
	,0.3242109,0.2938952,0.2817154,0.2642793,0.2611463,0.2771407,0.2437383
	,0.2375118,0.2622747,0.2647178,0.3011475,0.2729073,0.2744574,0.2763697
	,0.2985141,0.3216371,0.3106508,0.3538173,0.3301575,0.3010599,0.2583870
	,0.2251566,0.2751868,0.2704306,0.3031513,0.3032360,0.2861057,0.2835814
	,0.2797444,0.2637903,0.2529620,0.2635239,0.2671385,0.2450239,0.2199332
	,0.2327113,0.2720223,0.3128964,0.3363441,0.3181453,0.3408584,0.3559775
	,0.3404515,0.3723021,0.3477245,0.3296489,0.3407860,0.3264256,0.3418024
	,0.3275367,0.3022457,0.2741760,0.2896025,0.2986334,0.2756776,0.2917165
	,0.2619290,0.2390367,0.2120774,0.2442769,0.2650921,0.2993681,0.2982663
	,0.3077265,0.2617599,0.2665871,0.2886663,0.2835237,0.2923738,0.2605449
	,0.2997978,0.3011428,0.3026857,0.3308477,0.3277342,0.3000871,0.2729207
	,0.2991542,0.3037920,0.2767562,0.2678217,0.2252819,0.2128638,0.2101101
	,0.2153417,0.2428547,0.2330288,0.2425897,0.2524068,0.2053539,0.2120120
	,0.2000000,0.2261948,0.2155050,0.2107567,0.2000000,0.2000000,0.2000000
	,0.2009506,0.2009397,0.2000000,0.2000000,0.2283085,0.2232115];

var	FB_matrix2 = [];
		FB_matrix2[0] = [0.25      , 0.25      , 0.29653201, 0.27076179, 0.29888813,
        0.28412809, 0.29432658, 0.3072721 , 0.29560333, 0.3072023 ,
        0.28907754, 0.28417372, 0.25953949, 0.28229099, 0.29133357,
        0.28202734, 0.29670496, 0.28977379, 0.30849592, 0.31526801,
        0.38909483, 0.4260348 , 0.44457387, 0.42247896, 0.4626376 ,
        0.43393416, 0.39378826, 0.42133862, 0.40597954, 0.39863518,
        0.4408057 , 0.44468558, 0.46470837, 0.46996881, 0.46396124,
        0.4733184 , 0.49485794, 0.51741877, 0.506317  , 0.54993992,
        0.56932012, 0.5911291 , 0.56140896, 0.59119331, 0.60978899,
        0.56702032, 0.5685424 , 0.58924862, 0.59505489, 0.61360677,
        0.6341522 , 0.60382897, 0.58491946, 0.63254496, 0.6592696 ,
        0.64748938, 0.62387202, 0.61793518, 0.61768686, 0.58700643,
        0.55631618, 0.56493872, 0.59584311, 0.57625231, 0.56924477,
        0.49748586, 0.53618058, 0.55791015, 0.58463809, 0.59454323,
        0.57854233, 0.54013773, 0.55860499, 0.54873916, 0.56754808,
        0.55707299, 0.54799818, 0.57268522, 0.57831684, 0.56590706,
        0.56589777, 0.56965327, 0.57782928, 0.60006409, 0.65047214,
        0.6364489 , 0.62330925, 0.60604435, 0.58084271, 0.58270879,
        0.59816477, 0.60044994, 0.58356501, 0.58006644, 0.56685317,
        0.54993071, 0.55534887, 0.59210925, 0.62496258, 0.65010374,
        0.63678605, 0.64692032, 0.66446859, 0.61314971, 0.60447043,
        0.63267655, 0.58509533, 0.57646801, 0.58314691, 0.56658277,
        0.60032151, 0.54155898, 0.54318705, 0.55848724, 0.54943992,
        0.52841887, 0.55360026, 0.53215302, 0.55368953, 0.61342992,
        0.62919074, 0.62445923, 0.64227641, 0.64386443, 0.61762331,
        0.62621401, 0.58959499, 0.62548665, 0.61789333, 0.62024515,
        0.60053234, 0.56438362, 0.55057144, 0.53651071, 0.54235848,
        0.54260781, 0.53029942, 0.51985796, 0.52654886, 0.55037448,
        0.54586584, 0.56093255, 0.55091834, 0.5487491 , 0.54553658,
        0.46480801, 0.43665417, 0.39663153, 0.39756167, 0.43620843,
        0.25      , 0.25      , 0.29653201, 0.27076179, 0.29888813,
        0.28412809, 0.29432658, 0.3072721 , 0.29560333, 0.3072023 ,
        0.28907754, 0.28417372, 0.25953949, 0.28229099, 0.29133357,
        0.28202734, 0.29670496, 0.28977379, 0.30849592, 0.31526801,
        0.38909483, 0.4260348 , 0.44457387, 0.42247896, 0.4626376 ,
        0.43393416, 0.39378826, 0.42133862, 0.40597954, 0.39863518,
        0.4408057 , 0.44468558, 0.46470837, 0.46996881, 0.46396124,
        0.4733184 , 0.49485794, 0.51741877, 0.506317  , 0.54993992,
        0.56932012, 0.5911291 , 0.56140896, 0.59119331, 0.60978899,
        0.56702032, 0.5685424 , 0.58924862, 0.59505489, 0.61360677,
        0.6341522 , 0.60382897, 0.58491946, 0.63254496, 0.6592696 ,
        0.64748938, 0.62387202, 0.61793518, 0.61768686, 0.58700643,
        0.55631618, 0.56493872, 0.59584311, 0.57625231, 0.56924477,
        0.49748586, 0.53618058, 0.55791015, 0.58463809, 0.59454323,
        0.57854233, 0.54013773, 0.55860499, 0.54873916, 0.56754808,
        0.55707299, 0.54799818, 0.57268522, 0.57831684, 0.56590706,
        0.56589777, 0.56965327, 0.57782928, 0.60006409, 0.65047214,
        0.6364489 , 0.62330925, 0.60604435, 0.58084271, 0.58270879,
        0.59816477, 0.60044994, 0.58356501, 0.58006644, 0.56685317,
        0.54993071, 0.55534887, 0.59210925, 0.62496258, 0.65010374,
        0.63678605, 0.64692032, 0.66446859, 0.61314971, 0.60447043,
        0.63267655, 0.58509533, 0.57646801, 0.58314691, 0.56658277,
        0.60032151, 0.54155898, 0.54318705, 0.55848724, 0.54943992,
        0.52841887, 0.55360026, 0.53215302, 0.55368953, 0.61342992,
        0.62919074, 0.62445923, 0.64227641, 0.64386443, 0.61762331,
        0.62621401, 0.58959499, 0.62548665, 0.61789333, 0.62024515,
        0.60053234, 0.56438362, 0.55057144, 0.53651071, 0.54235848,
        0.54260781, 0.53029942, 0.51985796, 0.52654886, 0.55037448,
        0.54586584, 0.56093255, 0.55091834, 0.5487491 , 0.54553658,
        0.46480801, 0.43665417, 0.39663153, 0.39756167, 0.43620843]
	FB_matrix2[1] =  [0.66      , 0.67904224, 0.6865016 , 0.66176208, 0.65776337,
        0.68406253, 0.66325859, 0.66777431, 0.68512092, 0.68814263,
        0.71762094, 0.73192014, 0.69929896, 0.71994192, 0.72419868,
        0.71983205, 0.70603651, 0.75      , 0.75      , 0.73279781,
        0.75      , 0.72852555, 0.71602029, 0.71674728, 0.71232021,
        0.71328571, 0.71993105, 0.73089371, 0.69889009, 0.67625524,
        0.70738111, 0.71798967, 0.7083456 , 0.71146363, 0.68279303,
        0.69784356, 0.68792201, 0.68170684, 0.68250988, 0.66391005,
        0.67598681, 0.68915058, 0.65098302, 0.67636701, 0.67919132,
        0.7024188 , 0.68535917, 0.6975601 , 0.65409894, 0.66274217,
        0.63182945, 0.64120404, 0.68853287, 0.68283953, 0.68465153,
        0.66881108, 0.68833049, 0.68413461, 0.67686983, 0.64244577,
        0.6599678 , 0.61177894, 0.65375721, 0.62903522, 0.61601974,
        0.63257741, 0.59333604, 0.57841725, 0.60031267, 0.57904955,
        0.61846007, 0.6490019 , 0.64984706, 0.66070046, 0.66494399,
        0.6701445 , 0.70001279, 0.69402977, 0.71723263, 0.73351544,
        0.71916799, 0.74579943, 0.70845006, 0.75      , 0.75      ,
        0.75      , 0.73850616, 0.75      , 0.72528673, 0.70515428,
        0.71528992, 0.70726893, 0.70061906, 0.6826053 , 0.65958503,
        0.64905027, 0.65866324, 0.67911254, 0.68803827, 0.63731643,
        0.59957992, 0.59253182, 0.5714805 , 0.56668606, 0.6293194 ,
        0.65390618, 0.69057396, 0.63193127, 0.61427448, 0.59095297,
        0.59857043, 0.59127916, 0.60100485, 0.56678431, 0.5063182 ,
        0.54733875, 0.57084358, 0.6012883 , 0.57959987, 0.58393692,
        0.59217529, 0.59226945, 0.60222565, 0.60881098, 0.60024137,
        0.57072137, 0.56675062, 0.54030745, 0.56743097, 0.54859511,
        0.56208968, 0.56576367, 0.58638037, 0.54211301, 0.49177819,
        0.45769158, 0.43211342, 0.42940443, 0.41586455, 0.44500169,
        0.44730139, 0.41295299, 0.46236718, 0.42526029, 0.39300105,
        0.39411948, 0.42464317, 0.43668793, 0.43989258, 0.46190148,
        0.66      , 0.67904224, 0.6865016 , 0.66176208, 0.65776337,
        0.68406253, 0.66325859, 0.66777431, 0.68512092, 0.68814263,
        0.71762094, 0.73192014, 0.69929896, 0.71994192, 0.72419868,
        0.71983205, 0.70603651, 0.75      , 0.75      , 0.73279781,
        0.75      , 0.72852555, 0.71602029, 0.71674728, 0.71232021,
        0.71328571, 0.71993105, 0.73089371, 0.69889009, 0.67625524,
        0.70738111, 0.71798967, 0.7083456 , 0.71146363, 0.68279303,
        0.69784356, 0.68792201, 0.68170684, 0.68250988, 0.66391005,
        0.67598681, 0.68915058, 0.65098302, 0.67636701, 0.67919132,
        0.7024188 , 0.68535917, 0.6975601 , 0.65409894, 0.66274217,
        0.63182945, 0.64120404, 0.68853287, 0.68283953, 0.68465153,
        0.66881108, 0.68833049, 0.68413461, 0.67686983, 0.64244577,
        0.6599678 , 0.61177894, 0.65375721, 0.62903522, 0.61601974,
        0.63257741, 0.59333604, 0.57841725, 0.60031267, 0.57904955,
        0.61846007, 0.6490019 , 0.64984706, 0.66070046, 0.66494399,
        0.6701445 , 0.70001279, 0.69402977, 0.71723263, 0.73351544,
        0.71916799, 0.74579943, 0.70845006, 0.75      , 0.75      ,
        0.75      , 0.73850616, 0.75      , 0.72528673, 0.70515428,
        0.71528992, 0.70726893, 0.70061906, 0.6826053 , 0.65958503,
        0.64905027, 0.65866324, 0.67911254, 0.68803827, 0.63731643,
        0.59957992, 0.59253182, 0.5714805 , 0.56668606, 0.6293194 ,
        0.65390618, 0.69057396, 0.63193127, 0.61427448, 0.59095297,
        0.59857043, 0.59127916, 0.60100485, 0.56678431, 0.5063182 ,
        0.54733875, 0.57084358, 0.6012883 , 0.57959987, 0.58393692,
        0.59217529, 0.59226945, 0.60222565, 0.60881098, 0.60024137,
        0.57072137, 0.56675062, 0.54030745, 0.56743097, 0.54859511,
        0.56208968, 0.56576367, 0.58638037, 0.54211301, 0.49177819,
        0.45769158, 0.43211342, 0.42940443, 0.41586455, 0.44500169,
        0.44730139, 0.41295299, 0.46236718, 0.42526029, 0.39300105,
        0.39411948, 0.42464317, 0.43668793, 0.43989258, 0.46190148]
		FB_matrix2[2] = [0.25      , 0.25      , 0.25      , 0.25188833, 0.25      ,
        0.25727115, 0.29014945, 0.29474925, 0.2772231 , 0.25      ,
        0.2599675 , 0.25      , 0.25      , 0.25077131, 0.25      ,
        0.25      , 0.26339086, 0.31900786, 0.2942447 , 0.30324699,
        0.30227632, 0.34316062, 0.32912057, 0.32286589, 0.34484398,
        0.31075089, 0.30603059, 0.33582741, 0.33589548, 0.34688621,
        0.34433675, 0.36003711, 0.341323  , 0.32968534, 0.2977107 ,
        0.2555033 , 0.25      , 0.25      , 0.25886913, 0.25921928,
        0.25      , 0.25      , 0.25      , 0.25      , 0.25      ,
        0.27503791, 0.25      , 0.25      , 0.2940635 , 0.29526366,
        0.29343237, 0.28321222, 0.30637056, 0.2876127 , 0.29622837,
        0.34059659, 0.35794747, 0.42903068, 0.41803865, 0.43391942,
        0.41721643, 0.43553081, 0.42342208, 0.42857669, 0.42875456,
        0.44810461, 0.44011263, 0.38791142, 0.41021495, 0.41557201,
        0.43986132, 0.42179304, 0.41934617, 0.46630278, 0.4740757 ,
        0.44664988, 0.42774752, 0.44858698, 0.43861428, 0.47261554,
        0.47608982, 0.50757694, 0.53674002, 0.55887402, 0.55954806,
        0.60021088, 0.66010064, 0.65595691, 0.62125501, 0.67739913,
        0.69675321, 0.6828989 , 0.69129564, 0.7021905 , 0.67115508,
        0.70165271, 0.68863755, 0.70594543, 0.67763873, 0.66584752,
        0.65368854, 0.64368556, 0.6390816 , 0.62488955, 0.6415483 ,
        0.65196482, 0.62192137, 0.57804677, 0.57874477, 0.64022895,
        0.63980853, 0.65016179, 0.64912216, 0.67252908, 0.68172524,
        0.6933682 , 0.68983028, 0.65333221, 0.68655056, 0.70929785,
        0.71700756, 0.73043253, 0.74935088, 0.75      , 0.75      ,
        0.70920975, 0.68795246, 0.68606165, 0.68879358, 0.686294  ,
        0.65971481, 0.68912501, 0.68913348, 0.67115165, 0.6422476 ,
        0.65768392, 0.63699663, 0.66753016, 0.70939099, 0.72356223,
        0.72292383, 0.68334116, 0.64551018, 0.65908022, 0.69824924,
        0.68651221, 0.69519769, 0.74525197, 0.74187706, 0.72259072,
        0.25      , 0.25      , 0.25      , 0.25188833, 0.25      ,
        0.25727115, 0.29014945, 0.29474925, 0.2772231 , 0.25      ,
        0.2599675 , 0.25      , 0.25      , 0.25077131, 0.25      ,
        0.25      , 0.26339086, 0.31900786, 0.2942447 , 0.30324699,
        0.30227632, 0.34316062, 0.32912057, 0.32286589, 0.34484398,
        0.31075089, 0.30603059, 0.33582741, 0.33589548, 0.34688621,
        0.34433675, 0.36003711, 0.341323  , 0.32968534, 0.2977107 ,
        0.2555033 , 0.25      , 0.25      , 0.25886913, 0.25921928,
        0.25      , 0.25      , 0.25      , 0.25      , 0.25      ,
        0.27503791, 0.25      , 0.25      , 0.2940635 , 0.29526366,
        0.29343237, 0.28321222, 0.30637056, 0.2876127 , 0.29622837,
        0.34059659, 0.35794747, 0.42903068, 0.41803865, 0.43391942,
        0.41721643, 0.43553081, 0.42342208, 0.42857669, 0.42875456,
        0.44810461, 0.44011263, 0.38791142, 0.41021495, 0.41557201,
        0.43986132, 0.42179304, 0.41934617, 0.46630278, 0.4740757 ,
        0.44664988, 0.42774752, 0.44858698, 0.43861428, 0.47261554,
        0.47608982, 0.50757694, 0.53674002, 0.55887402, 0.55954806,
        0.60021088, 0.66010064, 0.65595691, 0.62125501, 0.67739913,
        0.69675321, 0.6828989 , 0.69129564, 0.7021905 , 0.67115508,
        0.70165271, 0.68863755, 0.70594543, 0.67763873, 0.66584752,
        0.65368854, 0.64368556, 0.6390816 , 0.62488955, 0.6415483 ,
        0.65196482, 0.62192137, 0.57804677, 0.57874477, 0.64022895,
        0.63980853, 0.65016179, 0.64912216, 0.67252908, 0.68172524,
        0.6933682 , 0.68983028, 0.65333221, 0.68655056, 0.70929785,
        0.71700756, 0.73043253, 0.74935088, 0.75      , 0.75      ,
        0.70920975, 0.68795246, 0.68606165, 0.68879358, 0.686294  ,
        0.65971481, 0.68912501, 0.68913348, 0.67115165, 0.6422476 ,
        0.65768392, 0.63699663, 0.66753016, 0.70939099, 0.72356223,
        0.72292383, 0.68334116, 0.64551018, 0.65908022, 0.69824924,
        0.68651221, 0.69519769, 0.74525197, 0.74187706, 0.72259072]
		FB_matrix2[3] = [0.68      , 0.68816205, 0.66203033, 0.69590268, 0.69072642,
        0.67013795, 0.69427731, 0.64344347, 0.60500363, 0.62341362,
        0.62780727, 0.64439041, 0.63848694, 0.59521544, 0.59651779,
        0.60220021, 0.58517908, 0.57503061, 0.58284646, 0.56281016,
        0.55332787, 0.55313736, 0.5539741 , 0.56812893, 0.56814546,
        0.59595104, 0.58994929, 0.59254596, 0.56482352, 0.54348757,
        0.53820228, 0.5160485 , 0.52434118, 0.54900963, 0.49917149,
        0.47506879, 0.52182693, 0.48735981, 0.4727774 , 0.45771447,
        0.49406163, 0.51293603, 0.49436279, 0.53778672, 0.54034622,
        0.53727333, 0.55640977, 0.56779492, 0.63344723, 0.60527937,
        0.57951292, 0.57080741, 0.55517102, 0.5493658 , 0.58557349,
        0.61863286, 0.64970968, 0.68645481, 0.68577119, 0.65017654,
        0.67145584, 0.69341181, 0.69064436, 0.66400245, 0.68340523,
        0.67212603, 0.65635003, 0.63831157, 0.61597733, 0.68287028,
        0.66127869, 0.67263249, 0.67734152, 0.71624722, 0.72875372,
        0.71737757, 0.69466567, 0.69838477, 0.72717158, 0.75      ,
        0.75      , 0.714983  , 0.68570691, 0.66553469, 0.65133699,
        0.66418917, 0.63395347, 0.66473839, 0.62147034, 0.56905453,
        0.56577609, 0.57451475, 0.5948663 , 0.59139161, 0.57136086,
        0.54795735, 0.51300811, 0.49700221, 0.47568939, 0.50254285,
        0.51829833, 0.57584122, 0.5847985 , 0.58656003, 0.59203096,
        0.58245103, 0.53708341, 0.49051456, 0.49592454, 0.51301751,
        0.50506558, 0.47368124, 0.45176924, 0.47169449, 0.50938673,
        0.46338109, 0.42432883, 0.40527288, 0.40665468, 0.4134301 ,
        0.43223874, 0.43558391, 0.44834179, 0.46638791, 0.4593474 ,
        0.49207167, 0.46446579, 0.477035  , 0.52196585, 0.49351522,
        0.44478144, 0.43089997, 0.40152154, 0.42518677, 0.39856933,
        0.40785366, 0.38783216, 0.41281833, 0.42104078, 0.39166313,
        0.39134366, 0.40381981, 0.37143155, 0.355438  , 0.36831722,
        0.38178385, 0.39267902, 0.42702334, 0.44969511, 0.47476515,
        0.68      , 0.68816205, 0.66203033, 0.69590268, 0.69072642,
        0.67013795, 0.69427731, 0.64344347, 0.60500363, 0.62341362,
        0.62780727, 0.64439041, 0.63848694, 0.59521544, 0.59651779,
        0.60220021, 0.58517908, 0.57503061, 0.58284646, 0.56281016,
        0.55332787, 0.55313736, 0.5539741 , 0.56812893, 0.56814546,
        0.59595104, 0.58994929, 0.59254596, 0.56482352, 0.54348757,
        0.53820228, 0.5160485 , 0.52434118, 0.54900963, 0.49917149,
        0.47506879, 0.52182693, 0.48735981, 0.4727774 , 0.45771447,
        0.49406163, 0.51293603, 0.49436279, 0.53778672, 0.54034622,
        0.53727333, 0.55640977, 0.56779492, 0.63344723, 0.60527937,
        0.57951292, 0.57080741, 0.55517102, 0.5493658 , 0.58557349,
        0.61863286, 0.64970968, 0.68645481, 0.68577119, 0.65017654,
        0.67145584, 0.69341181, 0.69064436, 0.66400245, 0.68340523,
        0.67212603, 0.65635003, 0.63831157, 0.61597733, 0.68287028,
        0.66127869, 0.67263249, 0.67734152, 0.71624722, 0.72875372,
        0.71737757, 0.69466567, 0.69838477, 0.72717158, 0.75      ,
        0.75      , 0.714983  , 0.68570691, 0.66553469, 0.65133699,
        0.66418917, 0.63395347, 0.66473839, 0.62147034, 0.56905453,
        0.56577609, 0.57451475, 0.5948663 , 0.59139161, 0.57136086,
        0.54795735, 0.51300811, 0.49700221, 0.47568939, 0.50254285,
        0.51829833, 0.57584122, 0.5847985 , 0.58656003, 0.59203096,
        0.58245103, 0.53708341, 0.49051456, 0.49592454, 0.51301751,
        0.50506558, 0.47368124, 0.45176924, 0.47169449, 0.50938673,
        0.46338109, 0.42432883, 0.40527288, 0.40665468, 0.4134301 ,
        0.43223874, 0.43558391, 0.44834179, 0.46638791, 0.4593474 ,
        0.49207167, 0.46446579, 0.477035  , 0.52196585, 0.49351522,
        0.44478144, 0.43089997, 0.40152154, 0.42518677, 0.39856933,
        0.40785366, 0.38783216, 0.41281833, 0.42104078, 0.39166313,
        0.39134366, 0.40381981, 0.37143155, 0.355438  , 0.36831722,
        0.38178385, 0.39267902, 0.42702334, 0.44969511, 0.47476515]


var FB_matrix = FB_matrix2 //assign rnwlk version 1 or 2
if (Math.random() <= 0.5 ){
	FB_matrix = FB_matrix1;
}


// Actions go or no-go
var choices_1 = []
// Actions for left and right
var choices = [83, 75]
var stim_side = ['decision-left', 'decision-right']
var stim_move = ['selected-left', 'selected-right']

// Set up colors
// var test_colors = jsPsych.randomization.shuffle(['#98bf21', '#FF9966', '#C2C2FF'])
// var practice_colors = jsPsych.randomization.shuffle(['#F1B8D4', '#CCFF99', '#E0C2FF'])
var test_colors = jsPsych.randomization.shuffle(['#98bf2100', '#FF996600', '#C2C2FF00'])
var practice_colors = jsPsych.randomization.shuffle(['#98bf2100', '#FF996600', '#C2C2FF00'])
//*S* var test_colors = jsPsych.randomization.shuffle(['#98bf21', '#FF9966', '#C2C2FF'])
//*S* var practice_colors = jsPsych.randomization.shuffle(['#00F1B8D4', '#00CCFF99', '#00E0C2FF'])
var curr_colors = practice_colors

//The first two stims are first-stage stims.
//The next four are second-stage
// var test_images = jsPsych.randomization.repeat(
// 	["images/11.png",
// 		"images/12.png",
// 		"images/13.png",
// 		"images/14.png",
// 		"images/15.png",
// 		"images/16.png",
// 	], 1)
var test_images =

		[
			"stim/f_p_l.png",
			"stim/f_o_r.png",
			"stim/f_o_l.png",
			"stim/f_p_r.png",
			"stim/o_pur1.png",
			"stim/o_pur2.png",
			"stim/o_o1.png",
			"stim/o_o2.png"
		]



var practice_images =
	[
		"stim/f_y_l.png",
		"stim/f_b_r.png",
		"stim/f_b_l.png",
		"stim/f_y_r.png",
		"stim/o_y1.png",
		"stim/o_y2.png",
		"stim/o_b1.png",
		"stim/o_b2.png"
	]

	var glow_images =
		[
			"stim/blue_boat_left_glow.png",
			"stim/orange_boat_right_glow.png",
			"stim/green_shell_1_glow.png",
			"stim/green_shell_2_glow.png",
			"stim/purpel_shell_1_glow.png",
			"stim/purpel_shell_2_glow.png",
			"stim/orange_boat_left_glow.png",
			"stim/blue_boat_right_glow.png"
		]

	//*S* set background Image
var background_Image_stage_1 = "stim/background_1.png"
//var background_Image_stage_2_green = "stim/green_background.png"
//var background_Image_stage_2_purpel = "stim/purple_background.png"

var terminal_state_img_test = ["stim/o_pur1_r.png",
															"stim/o_pur2_r.png",
															"stim/o_o1_r.png",
															"stim/o_o2_r.png",
															"stim/o_pur1_p.png",
															"stim/o_pur2_p.png",
															"stim/o_o1_p.png",
															"stim/o_o2_p.png"
											]

var terminal_state_img_practice = ["stim/o_y1_r.png",
																	"stim/o_y2_r.png",
																	"stim/o_b1_r.png",
																	"stim/o_b2_r.png",
																	"stim/o_y1_p.png",
																	"stim/o_y2_p.png",
																	"stim/o_b1_p.png",
																	"stim/o_b2_p.png"
											]


var strategy_stim = ["symbol/right_arrow_black.png","symbol/left_arrow_black.png","symbol/no_go_black.png"]

//Preload images
jsPsych.pluginAPI.preloadImages(practice_images)
jsPsych.pluginAPI.preloadImages(test_images)
jsPsych.pluginAPI.preloadImages(background_Image_stage_1)
//jsPsych.pluginAPI.preloadImages(background_Image_stage_2_green)
//jsPsych.pluginAPI.preloadImages(background_Image_stage_2_purpel)
jsPsych.pluginAPI.preloadImages(terminal_state_img_test)
jsPsych.pluginAPI.preloadImages(terminal_state_img_practice)

jsPsych.pluginAPI.preloadImages(glow_images)


var curr_images =  practice_images
var terminal_state_img =  terminal_state_img_practice


var test_fs_stim = get_fs_stim_new(test_images, test_colors)
var practice_fs_stim = get_fs_stim_new(practice_images, practice_colors)

var test_ss_stim = get_ss_stim_new(test_images, test_colors)
var practice_ss_stim = get_ss_stim_new(practice_images, practice_colors)

var test_fs_stims = jsPsych.randomization.repeat(test_fs_stim, test_trials_num, true);
var practice_fs_stims = jsPsych.randomization.repeat(practice_fs_stim, practice_trials_num, true);


var curr_fs_stims = practice_fs_stims
var curr_ss_stims = practice_ss_stim


/* ************************************ */
/* Set up jsPsych blocks */
/* ************************************ */
// Set up attention check node
var attention_check_block = {
	type: 'attention-check',
	data: {
		trial_id: 'attention_check'
	},
	timing_response: 180000,
	response_ends_trial: true,
	timing_post_trial: 200
}

var attention_node = {
	timeline: [attention_check_block],
	conditional_function: function() {
		return run_attention_checks
	}
}

//Set up post task questionnaire
var post_task_block = {
   type: 'survey-text',
   data: {
       trial_id: "post task questions"
   },
   questions: ['<p class = center-block-text style = "font-size: 20px">What is your age?</p>',
	 						'<p class = center-block-text style = "font-size: 20px">What is your gender? (female/ male/ other)</p>',
		 					'<p class = center-block-text style = "font-size: 20px">How was the difficulty level of the game?</p>',
	 						'<p class = center-block-text style = "font-size: 20px">What made it difficult for you?</p>',
	 						'<p class = center-block-text style = "font-size: 20px">How well do you feel you understood the rules of the game?</p>',
	 						'<p class = center-block-text style = "font-size: 20px">Was there anything you didn’t understand? or something specific that was confusing?</p>',
              '<p class = center-block-text style = "font-size: 20px">What strategy did you use in your selections? </p>'],
   rows: [2,2,5, 5,5, 5,5],
   columns:  [10,10,60,60,60, 60,60]
};

/* define static blocks */
var attention_check_block = {
	type: 'attention-check',
	data: {
		trial_id: 'attention_check'
	},
	timing_response: 180000,
	response_ends_trial: true,
	timing_post_trial: 200
}


var feedback_instruct_text =
	'Welcome to the experiment. This task will take about 25 minutes. \
	Press <strong>enter</strong> to begin.'
var feedback_instruct_block = {
	type: 'poldrack-text',
	data: {
		trial_id: 'instruction'
	},
	cont_key: [13],
	text: getInstructFeedback,
	timing_post_trial: 0,
	timing_response: 180000
};

/// This ensures that the subject does not read through the instructions too quickly.  If they do it too quickly, then we will go over the loop again.
var instructions_block = {
	type: 'poldrack-instructions',
	data: {
		trial_id: 'instruction'
	},
	pages: [
		"<div class = centerbox> \
		<p class = block-text> Welcome! </p> \
		<p class = block-text> Here is a example round <br> <br> <br> <br> <br> <br> <br> <br> <br> <br> <br> <br> </p> \
		<div class = decision-left style='background:" + curr_colors[0] + ";'>  \
		<img class = 'decision-stim' src= '" + curr_images[0] +"'> </img> \
		<img class = 'strategy_stim' src= '" + strategy_stim[2] +"'> </img></div> \
		<div class = decision-right style='background:" + curr_colors[0] +"; '> \
		<img class = 'decision-stim' src= '" + curr_images[1] + "'></img> \
		<img class = 'strategy_stim' src= '" + strategy_stim[0] +"'> </img> </div></div>",
		'<div class = centerbox> <p class = block-text> \
		Each second stage has its own background color and has two different abstract shapes.</p> \
		<p class = block-text> In total, the task has three "stages": a first stage which can lead to either stage 2a or stage 2b, \
		each with their own background color and shapes.</p></div>',
		'<div class = centerbox> \
		<p class = block-text> Each first-stage choice is primarily associated with one of the two second-stages. \
		This means that each first-stage choice is more likely to bring you to one of the two second-stages than the other.</p>\
		<p class = block-text>After you end these instructions you will see an example trial.</p> \
		</div>'
	],
	allow_keys: false,
	show_clickable_nav: true,
	timing_post_trial: 1000
}

var instruction_node = {
	timeline: [feedback_instruct_block, instructions_block],
	/* This function defines stopping criteria */
	loop_function: function(data) {
		for (i = 0; i < data.length; i++) {
			if ((data[i].trial_type == 'poldrack-instructions') && (data[i].rt != -1)) {
				rt = data[i].rt
				sumInstructTime = sumInstructTime + rt
			}
		}
		if (sumInstructTime <= instructTimeThresh * 1000) {
			feedback_instruct_text =
				'Read through instructions too quickly.  Please take your time and make sure you understand the instructions.  Press <strong>enter</strong> to continue.'
			return true
		} else if (sumInstructTime > instructTimeThresh * 1000) {
			feedback_instruct_text = 'Done with instructions. Press <strong>enter</strong> to continue.'
			return false
		}
	}
}

var second_instructions_block = {
	type: 'poldrack-instructions',
	data: {
		trial_id: 'instruction'
	},
	pages: [
		'<div class = centerbox><p class = block-text>As you saw, you get feedback after your second-stage choice: either a gold coin or a red "0". The gold coins determine your bonus pay, so try to get as many as possible!</p><p class = block-text>As mentioned, there are four second-stage shapes: two shapes in 2a and two shapes in 2b. These four shapes each have a different chance of paying a gold coin. You want to learn which shape is the best so you can get as many coins as possible.</p></div>',
		'<div class = centerbox><p class = block-text>The chance of getting a coin from each second-stage shape changes over the experiment, so the best choice early on may not be the best choice later.</p><p class = block-text>In contrast, the chance of going to one of the second-stages after choosing one of the first-stage choices is fixed throughout the experiment. If you find over time that one first-stage shape brings you to 2a most of the time, it will stay that way for the whole experiment.</p></div>',
		'<div class = centerbox><p class = block-text>After you end instructions we will start with some practice.</p><p class = block-text>After practice we will show you the instructions again, but please make sure you understand them as well as you can now.</p></div>'
	],
	allow_keys: false,
	show_clickable_nav: true,
	timing_post_trial: 1000
}

var end_block = {
	type: 'poldrack-text',
	data: {
		trial_id: 'end',
    	exp_id: 'two_stage_decision'
	},
	text: '<div class = centerbox><p class = center-block-text>Thanks for completing this task!</p><p class = center-block-text>Press <strong>enter</strong> to continue.</p></div>',
	cont_key: [13],
	timing_response: 180000,
	timing_post_trial: 0,
	on_finish: assessPerformance
};

var wait_block = {
	type: 'poldrack-text',
	data: {
		trial_id: 'wait'
	},
	text: '<div class = centerbox><p class = center-block-text>Take a break!</p><p class = center-block-text>Press <strong>enter</strong> to continue.</p></div>',
	cont_key: [13],
	timing_response: 120000,
	timing_post_trial: 1000
};

var start_practice_block = {
	type: 'poldrack-text',
	data: {
		trial_id: 'practice_intro'
	},
	text: '<div class = centerbox><p class = center-block-text>You will now practice for a few trials that will simulate the game to follow.</p><p class = center-block-text> Note that the colors of the fishermen and oysters differ between the practice trials and the game but the rules remain the same. </p><p class = center-block-text>Press <strong>enter</strong> to continue.</p></div>',
	cont_key: [13],
	timing_response: 180000,
	timing_post_trial: 1000
};

var start_test_block = {
	type: 'poldrack-text',
	data: {
		trial_id: 'test_intro'
	},
	text: "<div class = centerbox><p class =center-block-text>You have finished the practice.</p><p class =center-block-text> We will now start the game.</p><p class =center-block-text> We remind you that each oysters' chance of producing a pearl changes during the course of the game, so the best one early on may not be the best one later.</p><p class = center-block-text> Press <strong>enter</strong> to begin.</p></div>",
	cont_key: [13],
	timing_response: 180000,
	timing_post_trial: 1000
};

var intertrial_wait_update_FB = {
	type: "poldrack-single-stim",
	data: {
		trial_id: 'wait_update_FB'
	},
	stimulus: update_FB_data, //dummy stimulus. Returns "" but updates previous trial
	is_html: true,
	timing_post_trial: 0,
	timing_stim: 1,
	timing_response: 1
}

var intertrial_wait = {
	type: "poldrack-single-stim",
	data: {
		trial_id: 'wait'
	},
	stimulus: " ", //dummy stimulus. Returns "" but updates previous trial
	is_html: true,
	timing_post_trial: 0,
	timing_stim: 1000,
	timing_response: 1000
}

var change_phase_block = {
	type: 'call-function',
	data: {
		trial_id: 'change_phase'
	},
	func: change_phase,
	timing_post_trial: 0
}

//experiment blocks
var first_stage = {
	type: "poldrack-single-stim",
	stimulus: choose_first_stage,
	is_html: true,
	choices: choices_1,
	timing_stim: 2000,
	timing_response: 2000,
	show_response: true,
	timing_post_trial: 0,
	response_ends_trial: false,
	// stage : 1,
	data: {
		trial_id: 'first_stage'
	},
	on_finish: function() {
		jsPsych.data.addDataToLastTrial({
			exp_stage: exp_stage,
			trial_num: current_trial,
			stim_order: stim_ids,
			stage: 0
		})
	}
}

var first_stage_selected = {
	type: "poldrack-single-stim",
	data: {
		trial_id: 'first_stage_selected'
	},
	stimulus: get_first_selected_new,
	choices: 'none',
	is_html: true,
	timing_post_trial: 0,
	timing_response: 500,
	stage : 1,
	on_finish: function() {
		jsPsych.data.addDataToLastTrial({
			exp_stage: exp_stage
		})
	}
}

var second_stage = {
	type: "poldrack-single-stim",
	data: {
		trial_id: 'second_stage'
	},
	stimulus: choose_second_stage,
	is_html: true,
	choices: choices_1,
	timing_stim: 2000,
	timing_response: 2000,
	response_ends_trial: false,
	timing_post_trial: 0,
	// stage: 2,
	on_finish: function() {
		jsPsych.data.addDataToLastTrial({
			exp_stage: exp_stage,
			trial_num: current_trial,
			stim_order: stim_ids,
			stage: stage + 1,
			stage_transition: transition
		})
	}
}

var second_stage_selected = {
	type: "poldrack-single-stim",
	data: {
		trial_id: 'second_stage_selected'
	},
	stimulus: get_second_selected_new,
	choices: 'none',
	is_html: true,
	timing_post_trial: 0,
	timing_response: 500,
	on_finish: function() {
		jsPsych.data.addDataToLastTrial({
			exp_stage: exp_stage
		})
	}
}

var FB_stage = {
	type: "poldrack-single-stim",
	data: {
		trial_id: 'feedback_stage'
	},
	stimulus: get_feedback_new,
	is_html: true,
	choices: 'none',
	timing_response: 1000,
	continue_after_response: false,
	timing_post_trial: 0,
	on_finish: function() {
		jsPsych.data.addDataToLastTrial({
			exp_stage: exp_stage
		})
	}
}
// timeline can take 3 args third one is to dealay
var FB_node = {
	timeline: [second_stage_selected, FB_stage, intertrial_wait_update_FB],
	conditional_function: function() {
		return FB_on == 1
	}
}

var noFB_node = {
	timeline: [intertrial_wait],
	conditional_function: function() {
		return FB_on === 0
	}
}

var fixation = {
	type: 'poldrack-single-stim',
	//*S* is_html: true,
	stimulus:	"<img class = 'background_images' src= '" + background_Image_stage_1 +"'> </img></div>"+
	'<div class = centerbox><div class = fixation>+</div></div>',
	is_html: true,
	choices: 'none',
	data: {
		trial_id: 'fixation'
	},
	timing_post_trial: 0,
	timing_stim: 1000,
	timing_response: 1000,
	on_finish: function() {
		jsPsych.data.addDataToLastTrial({
			exp_stage: exp_stage
		})
	}
}

var almost_end_block = {
	type: 'poldrack-text',
	data: {
		trial_id: 'end',
    	exp_id: 'two_stage_decision'
	},
	text: '<div class = centerbox><p class = center-block-text>You have finished the game!</p><p class = center-block-text> Please answer the following questions and elaborate as much as possible.</p><p class =center-block-text> We remind you that all data is collected anonymously</p><p class = center-block-text>Press <strong>enter</strong> to continue.</p></div>',
	cont_key: [13],
	timing_response: 180000,
	timing_post_trial: 0,
	on_finish: assessPerformance
};


var two_stage_decision_experiment = []
//two_stage_decision_experiment.push(instruction_node);
//example trial
//two_stage_decision_experiment.push(first_stage)
//two_stage_decision_experiment.push(first_stage_selected)
//two_stage_decision_experiment.push(second_stage)
//two_stage_decision_experiment.push(FB_node)
//two_stage_decision_experiment.push(noFB_node)
//continue instructions
//two_stage_decision_experiment.push(second_instructions_block);
two_stage_decision_experiment.push(start_practice_block);
//two_stage_decision_experiment.push(attention_node)
for (var i = 0; i < practice_trials_num; i++) {
	two_stage_decision_experiment.push(fixation)
	two_stage_decision_experiment.push(first_stage)
	two_stage_decision_experiment.push(first_stage_selected)
	two_stage_decision_experiment.push(second_stage)
	two_stage_decision_experiment.push(FB_node)
	two_stage_decision_experiment.push(noFB_node)
}
two_stage_decision_experiment.push(attention_node)
two_stage_decision_experiment.push(change_phase_block)
two_stage_decision_experiment.push(start_test_block)
for (var i = 0; i < test_trials_num / 3; i++) {
	two_stage_decision_experiment.push(fixation)
	two_stage_decision_experiment.push(first_stage)
	two_stage_decision_experiment.push(first_stage_selected)
	two_stage_decision_experiment.push(second_stage)
	two_stage_decision_experiment.push(FB_node)
	two_stage_decision_experiment.push(noFB_node)
}
two_stage_decision_experiment.push(attention_node)
two_stage_decision_experiment.push(wait_block)
for (var i = 0; i < test_trials_num / 3; i++) {
	two_stage_decision_experiment.push(attention_node)
	two_stage_decision_experiment.push(fixation)
	two_stage_decision_experiment.push(first_stage)
	two_stage_decision_experiment.push(first_stage_selected)
	two_stage_decision_experiment.push(second_stage)
	two_stage_decision_experiment.push(FB_node)
	two_stage_decision_experiment.push(noFB_node)
}
two_stage_decision_experiment.push(attention_node)
two_stage_decision_experiment.push(wait_block)
for (var i = 0; i < test_trials_num / 3; i++) {
	two_stage_decision_experiment.push(attention_node)
	two_stage_decision_experiment.push(fixation)
	two_stage_decision_experiment.push(first_stage)
	two_stage_decision_experiment.push(first_stage_selected)
	two_stage_decision_experiment.push(second_stage)
	two_stage_decision_experiment.push(FB_node)
	two_stage_decision_experiment.push(noFB_node)
}
two_stage_decision_experiment.push(attention_node)
two_stage_decision_experiment.push(almost_end_block)
two_stage_decision_experiment.push(post_task_block)
//two_stage_decision_experiment.push(end_block)

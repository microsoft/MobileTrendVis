var globals = require("./globals");
var nationData = require("./data/studyData");
var taskList = require("./tasks/taskList");

function initTasks() {

  nationData.forEach(function (d){ 
    d.orig_code = d.code;
  });

  globals.trials = taskList[globals.ordering];    

  max_trials = globals.trials.length;

  // globals.trials = tutorial_tasks.concat(test_tasks);
  console.log('task_list', globals.trials);

  globals.log_message = { 
    "TimeStamp": new Date().valueOf(),
    "user_id": globals.userID, 
    "participant": globals.participant,
    "ordering": globals.ordering,
    "condition": globals.condition,
    "Event":"Load",
    "Width": window.innerWidth, 
    "Height": window.innerHeight,
    "Mode": (window.innerWidth > window.innerHeight) ? 'landscape' : 'portrait'
  };

  console.log("Load", globals.log_message);


}

module.exports = initTasks;
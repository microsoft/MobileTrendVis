var u;

var globals = {
  width: 0,
  height: 0,
  svg_dim: u,
  inner_padding: u,
  chart_dim: u,
  padding: 0,
  main_svg: u,
  defs: u,
  userID: u,
  chart_g: u,

  chart_instance: u,
  year_indicator: u,
  all_data: u,  
  hideAddressBar: u,
  shuffle: u,
  consent_complete: u,
  introduction_complete: u,
  experiment_complete: u,
  test_override: u,  
  last_pause: u,
  resumptions: u,
  trial_index: u,
  max_trials: u,
  ordering: u,
  participant: u,
  trials: u,
  trial_response: u,
  condition: u,
  lines: u,
  facets: u,
  animation: u,
  num_selected: u,
  outer_progress_circle: u,
  inner_progress_circle: u,
  param_x: 'Life Expectancy',
  param_y: 'GDP Per Capita',
  param_r: 'Population',
  param_yearMin: 1975,
  param_yearMax: 2000,
  log_message: u
};

test_override = false;
consent_complete = false;
introduction_complete = false;
experiment_complete = false;
resumptions = [];
trial_index = -1;
max_trials = 0;
condition = 'multiples';
ordering = 0;
participant = -1;
userID = -1;
tps = 0;
trial_response = [];
num_selected = 0;
log_message = '';

shuffle = function (array) {
  var currentIndex = array.length, temporaryValue, randomIndex;

  // While there remain elements to shuffle...
  while (0 !== currentIndex) {

    // Pick a remaining element...
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    // And swap it with the current element.
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }

  return array;
};

hideAddressBar = function () {
  
  setTimeout(function(){
    // Hide the address bar!
		window.scrollTo(0, 1);
  }, 10);  
  

  var touchstartHandler = function(e) {
    if (e.touches.length != 1) return;
    lastTouchY = e.touches[0].clientY;
  };
  
  var touchmoveHandler = function(e) {
    var touchY = e.touches[0].clientY;
    var touchYDelta = touchY - lastTouchY;
    lastTouchY = touchY;

    e.preventDefault();
    return;
  };

  document.addEventListener('touchstart', touchstartHandler, {passive: false });
  document.addEventListener('touchmove', touchmoveHandler, {passive: false });

};

module.exports = globals;


var d3 = require("d3");
// var io = require('socket.io-client');
var preLoader = require('pre-loader');
var globals = require("./globals");
var initTasks = require("./initTasks");
var sandbox = require("./sandbox");
var consent = require("./consent");
var introduction = require("./introduction");
var questionnaire = require("./questionnaire");
var trial = require("./trial");
var countrySelector = require("./countrySelector");
var menu = require("./menu");    
globals.userID = '1218_' + new Date().valueOf(); // CHANGE FOR PRODUCTION

if (window.location.href.indexOf('localhost') == -1) {
  var socket = io.connect({
    transports: ['websocket'],
    reconnect: true
  });
}
else {
  var socket = io.connect({
    transports: ['websocket'],
    reconnect: true
  });
}

socket.on('new_connection', function(msg) {
  console.log('new_connection', msg);
});

socket.on('new_participant', function(msg) {
  console.log('new_participant', msg);
  if (msg.user_id == globals.userID) {
    globals.participant = msg.participant;
    globals.condition = msg.condition;
    globals.ordering = msg.ordering;
    initTasks();
    loadMenu();
  }
});

socket.on('departure', function(msg) {
  console.log('departure: ', msg);
});

socket.on('completion', function(msg) {
  console.log('completion: ', msg);
});


function setCookie (c_name, value, exdays)
{
  var exdate = new Date();
  exdate.setDate(exdate.getDate() + exdays);
  var c_value = escape(value) + ((exdays==null) ? "" : "; expires="+exdate.toUTCString());
  document.cookie = c_name + "=" + c_value;
}

function getCookie (c_name)
{
   var i,x,y,ARRcookies=document.cookie.split(";");
   for (i=0;i<ARRcookies.length;i++)
    {
       x=ARRcookies[i].substr(0,ARRcookies[i].indexOf("="));
          y=ARRcookies[i].substr(ARRcookies[i].indexOf("=")+1);
          x=x.replace(/^\s+|\s+$/g,"");
          if (x==c_name)
          {
              return unescape(y);
          }
    }
 }


/* jshint ignore:end */

window.addEventListener('load', function() { 

  imagesArray = [
    "assets/battery.svg",
    "assets/brightness.svg",
    "assets/done.svg",
    "assets/fullscreen.png",
    "assets/grid.svg",
    "assets/holdingphone.svg",
    "assets/line.svg",
    "assets/na.svg",
    "assets/next_gold.svg",
    "assets/next.svg",
    "assets/nonav.svg",
    "assets/play.svg",
    "assets/portrait.svg",
    "assets/prev_gold.svg",
    "assets/prev_grey.svg",
    "assets/prev.svg",
    "assets/wifi.svg"
  ];

  new preLoader(imagesArray, {
    onProgress: function(img, imageEl, index){
        // fires every time an image is done or errors.
        // imageEl will be falsy if error
        // console.log('just ' +  (!imageEl ? 'failed: ' : 'loaded: ') + img);
        // imageContainer.appendChild(imageEl);
        // can access any propery of this
        // console.log(this.completed.length + this.errors.length + ' / ' + this.queue.length + ' done');
    },
    onComplete: function(loaded, errors){
        // fires when whole list is done. cache is primed.
        // console.log('assets loaded:', loaded);
        // imageContainer.style.display = 'block';
        if (errors){
            console.log('the following failed', errors);
        }
    }
  });
  
  resumptions = [];
  globals.last_pause = new Date().valueOf();
  

  
  hideAddressBar();   

  globals.last_pause = new Date().valueOf();

  resumptions.push({
    'resumption_time': new Date().valueOf() + 1,
    'pause_time': globals.last_pause,
    'pause_duration': 1
  }); //app resumed

    
  d3.select('body').append('input')
  .attr('id','landscape_btn')
  .attr('type','button')
  .style('color','#111')
  .style('background','#ef5350')
  .style('border-color','#fff')
  .attr('value','Hold your phone in portrait mode.')
  .attr('title','Hold your phone in portrait mode.')
  .on('touchstart', function() {  
    d3.select('#landscape_btn').remove();
  }); 
  
});  

window.onfocus = function(e) {
  var r_time = new Date().valueOf();
  if (resumptions.length == 0 || globals.last_pause > resumptions[resumptions.length - 1].resumption_time && window.innerHeight > window.innerWidth) {
    resumptions.push({
      'resumption_time': r_time,
      'pause_time': globals.last_pause,
      'pause_type':'focus',
      'pause_duration': r_time - globals.last_pause
    }); //app resumed
  }
  
  globals.log_message = { 
    "TimeStamp": new Date().valueOf(),
    "user_id": globals.userID, 
    "Event": "InFocus",
    "Focus": true ,
    'resumption_time': r_time,
    'pause_time': globals.last_pause,
    'pause_type': 'focus',
    'pause_duration': r_time - globals.last_pause
  };
  
  console.log("InFocus", globals.log_message);

};

window.onblur = function(e) {
  if (resumptions.length == 0 || resumptions[resumptions.length - 1].resumption_time > globals.last_pause) {
    globals.last_pause = new Date().valueOf(); //app paused
  }

  globals.log_message = { 
    "TimeStamp": new Date().valueOf(),
    "Event": "FocusLoss",
    "user_id": globals.userID, 
    "Focus": false 
  };
  
  console.log("FocusLoss", globals.log_message);


};

window.onbeforeunload = function() { 
  socket.emit('unload', {
    userID: globals.userID,
    userAgent: navigator.userAgent
  });
  return "Your work will be lost."; 
};

window.onresize = function(e) {
  
  d3.select('#landscape_btn').remove();

  if (document.getElementById('landscape_btn')) {
    document.getElementById('landscape_btn').remove();
  }
  hideAddressBar();

  if (window.innerHeight < window.innerWidth) {
    if (resumptions[resumptions.length - 1].resumption_time > globals.last_pause) {
      globals.last_pause = new Date().valueOf(); //app paused
    }

    globals.log_message = { 
      "TimeStamp": new Date().valueOf(),
      "user_id": globals.userID, 
      "Event": "Resized",
      "Width": window.innerWidth, 
      "Height": window.innerHeight,
      "Mode": 'landscape' 
    };
    
    console.log("Resized", globals.log_message);


    d3.select('body').append('input')
    .attr('id','landscape_btn')
    .attr('type','button')
    .style('color','#111')
    .style('background','#ef5350')
    .style('border-color','#fff')
    .attr('value','Hold your phone in portrait mode.')
    .attr('title','Hold your phone in portrait mode.')
    .on('touchstart', function() {  
      d3.select('#landscape_btn').remove();
    });        
    
  }
  else {
    d3.select('#landscape_btn').remove();
    if (document.getElementById('landscape_btn')) {
      document.getElementById('landscape_btn').remove();
    }
    var r_time = new Date().valueOf();
    if (globals.last_pause > resumptions[resumptions.length - 1].resumption_time) {
      resumptions.push({
        'resumption_time': r_time,
        'pause_time': globals.last_pause,
        'pause_type': 'orientation',
        'pause_duration': r_time - globals.last_pause
      }); //app resumed
    }

    globals.log_message = { 
      "TimeStamp": new Date().valueOf(),
      "user_id": globals.userID, 
      "Event": "Resized",
      "Width": window.innerWidth, 
      "Height": window.innerHeight,
      "Orientation": 'portrait',
      'resumption_time': r_time,
      'pause_time': globals.last_pause,
      'pause_type': 'orientation',
      'pause_duration': r_time - globals.last_pause
    };
    
    console.log("Resized", globals.log_message);


  }  
};

d3.select("body")
.on("keydown", function () {    
  
  switch(d3.event.keyCode) {    

    case 27: // test override on 'Esc' and load menu
      test_override = true;
      consent_complete = true;
      introduction_complete = true;

      globals.trial_index = -1;

      globals.animation = 'off';
      globals.lines = 'off';
      globals.facets = 'off';

      globals.log_message = { 
        "TimeStamp": new Date().valueOf(),
        "Event": "Escape",
        "user_id": globals.userID
      };
      
      console.log("Escape", globals.log_message);


      if (document.getElementById('selector_div') != undefined) {      
        document.getElementById('selector_div').remove();                    
      } 
     
      if (document.getElementsByTagName('div')[0] != undefined) {        
               
        document.getElementsByTagName('div')[0].remove();   
        
        globals.log_message = { 
          "TimeStamp": new Date().valueOf(),
          "Event": "TestOverride_Load_Menu",
          "user_id": globals.userID
        };
        
        console.log("TestOverride_Load_Menu", globals.log_message);


        loadMenu();
        hideAddressBar();
      }           
    break;    

    default:      
    break;
  }    

});

loadSandbox = function () {

  globals.trial_index = 0;
  test_override = true;    

  globals.log_message = { 
    "TimeStamp": new Date().valueOf(),
    "Event": "SandBox_Open",
    "user_id": globals.userID
  };
  
  console.log("SandBox_Open", globals.log_message);


  setTimeout(function(){
    // Hide the address bar!
		sandbox();  
  }, 100);
  hideAddressBar(); 
};

loadMenu = function () {
  
  menu(); 

  d3.select('#consent_btn')
  .on('touchstart', function() {    
    if (test_override || !consent_complete) {
      document.getElementById('menu_div').remove();
      
      globals.log_message = { 
        "TimeStamp": new Date().valueOf(),
        "user_id": globals.userID,
        "Event": "ConsentStart",
        "ordering": globals.ordering,
        "condition": globals.condition,
        "Width": window.innerWidth, 
        "Height": window.innerHeight,
        "Scene": 0
      };
      
      console.log("ConsentStart", globals.log_message);

      
      consent(0);
      hideAddressBar();
    }  
  });

  d3.select('#introduction_btn')
  .on('touchstart', function() {      
    if (test_override || (!introduction_complete && consent_complete)) {
      document.getElementById('menu_div').remove();
      
      globals.log_message = { 
        "TimeStamp": new Date().valueOf(),
        "user_id": globals.userID,
        "Event": "IntroStart",
        "Scene": 0
      };
      
      console.log("IntroStart", globals.log_message);

      
      introduction(0);
      hideAddressBar();
    }
  });

  d3.select('#trial_btn')
  .on('touchstart', function() {

    if (test_override || introduction_complete) {
      globals.trial_index = -1;  
      test_override = false;  
      
      switch (globals.condition) {
  
        case 'stepper':
  
          globals.animation = 'off';
          globals.lines = 'off';
          globals.facets = 'off';
          break;
  
        case 'animation':
  
          globals.animation = 'on';
          globals.lines = 'off';
          globals.facets = 'off';
          break;
  
        case 'multiples':
  
          globals.animation = 'off';
          globals.lines = 'on';
          globals.facets = 'on';
          break;
  
        default:
  
          globals.animation = 'off';
          globals.lines = 'off';
          globals.facets = 'off';
          break;
      }   

      globals.log_message = { 
        "TimeStamp": new Date().valueOf(),
        "Event": "Experiment_Start",
        "condition": globals.condition,
        "ordering": globals.ordering,
        "user_id": globals.userID,
        "Width": window.innerWidth, 
        "Height": window.innerHeight
      };
      
      console.log("Experiment_Start", globals.log_message);

  
      document.getElementById('menu_div').remove();   
      trial();  
      countrySelector();
      hideAddressBar();   
    }
  });

  d3.select('#questionnaire_btn')
  .on('touchstart', function() {    
    if (test_override || experiment_complete) {
      document.getElementById('menu_div').remove();

      globals.log_message = { 
        "TimeStamp": new Date().valueOf(),
        "user_id": globals.userID,
        "Event": "SurveyStart",
        "condition": globals.condition,
        "ordering": globals.ordering,
        "Width": window.innerWidth, 
        "Height": window.innerHeight,
        "Scene": 0
      };
      
      console.log("SurveyStart", globals.log_message);


      socket.emit('questionnaire', globals.userID);
      questionnaire(0);
      hideAddressBar();
    }  
  });

  d3.select('#secret_sandbox')
  .on('touchstart', function() {  
    document.getElementById('menu_div').remove(); 
    globals.trial_index = 0;
    test_override = true;    

    globals.log_message = { 
      "TimeStamp": new Date().valueOf(),
      "Event": "SandBox_Open",
      "user_id": globals.userID
    };
    
    console.log("SandBox_Open", globals.log_message);


    sandbox();
    hideAddressBar();   
  });
};

 if (getCookie('visited')) {
  // alert('You have already participated in this study.'); //COMMENT FOR TESTING

  globals.log_message = { 
    "TimeStamp": new Date().valueOf(),
    "Event": "RepeatParticipant",
    "userAgent": navigator.userAgent,
    "user_id": globals.userID
  };
  
  console.log("RepeatParticipant", globals.log_message);


  socket.emit('userID', {
    userID: globals.userID,
    userAgent: navigator.userAgent
  }); //COMMENT FOR PRODUCTION
 }
 else {
  setCookie('visited',1,365);

   globals.log_message = { 
    "TimeStamp": new Date().valueOf(),
    "Event": "NewParticipant",
    "userAgent": navigator.userAgent,
    "user_id": globals.userID
  };
  
  console.log("NewParticipant", globals.log_message);


  socket.emit('userID', {
    userID: globals.userID,
    userAgent: navigator.userAgent
  });  
 }

d3.select('body').append('svg')
.style('display','none')
.attr('xmlns','http://www.w3.org/2000/svg')
.attr('version','1.1')
.attr('height','0')
.append('filter')
.attr('id','myblurfilter')
.attr('width','110%')
.attr('height','110%')
.append('feGaussianBlur')
.attr('stdDeviation','30')
.attr('result','blur');
(function () {

  "use strict";

  var express = require('express');
  var app = express();
  var path = require('path');
  var server = require('http').createServer(app);
  var serverPort = process.env.PORT || 8080;

  app.use(express.static('public'));

  app.get('/', function(req, res) {
    res.sendFile(__dirname + '/public/index.html');
  });

  server.listen(serverPort, function() {
    console.log('Server started on port %s', serverPort);
  });

})();

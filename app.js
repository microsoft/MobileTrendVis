(function () {

  "use strict";

  var express = require('express');
  var app = express();
  var path = require('path');
  var server = require('http').createServer(app);
  var io = require('socket.io')(server);
  var serverPort = process.env.PORT || 8080;
  var count = 0;

  app.use(express.static('public'));

  app.get('/', function(req, res) {
    res.sendFile(__dirname + '/public/index.html');
  });

  server.listen(serverPort, function() {
    console.log('Server started on port %s', serverPort);
  });

  io.sockets.on('connection', function (socket) {
    io.set('transports', ['websocket']);
    console.log('new connection on socket.io');
    socket.emit('hello_from_server', { port: serverPort });
    
    socket.on('userID', function(msg){
      count++;
      console.log({ userID: msg, count: count });
      io.emit('visitor', { userID: msg, count: count });    
    });

  });

})();

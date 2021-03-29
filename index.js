const http = require('http');
const express = require('express');
const socketio = require('socket.io');

const server = express();
const socketServer = http.createServer(server);
const listener = socketio.listen(socketServer);
const clients = [];
const port = 4200;

listener.on('connection', socket => {

  socket.on('login', data => {
    listener.to(socket.id).emit('logged', {id: socket.id});
    clients.push({id: socket.id, username: data.username});
    listener.emit('users', clients);
    console.log(data.username + ' logged')
    console.log(clients)
  });

  socket.on('send', message => {
    if (message.to) {
      console.log('message from ' + message.from.username + ' to ' + message.to.username)
      socket.broadcast.to(message.to.id).emit('receive', message);
    } else {
      console.log('broadcast message from ' + message.from.username)
      listener.emit('receive', message)
    }
  });

  socket.on('disconnect', () => {
    clients.forEach((u, i) => {
      if (u.id === socket.id) {clients.splice(i, 1)}
    });
    listener.emit('users', clients);
    console.log(clients)
  });

})

socketServer.listen(port, () => console.log('Socket server running on port 4200'))

const http = require('http');
const express = require('express');
const socketio = require('socket.io');

const server = express();
const socketServer = http.createServer(server);
const io = socketio.listen(socketServer);
const users = [];

io.on('connection', socket => {

  socket.on('login', data => {
    io.to(socket.id).emit('logged', {id: socket.id});
    users.push({id: socket.id, username: data.username});
    io.emit('users', users); 
  });

  socket.on('send', message => {
    if (message.to) {
      socket.broadcast.to(message.to.id).emit('receive', message);
    } else {
      io.emit('receive', message)
    }
  });

  socket.on('disconnect', () => {
    users.forEach((u, i) => {
      if (u.id === socket.id) {users.splice(i, 1)}
    });
    io.emit('users', users); 
  });

})

socketServer.listen(4200, () => console.log('App running on port 4200'))

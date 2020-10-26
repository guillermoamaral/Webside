const http = require('http')
const express = require('express')
const socketio = require('socket.io')

const app = express()
const socketServer = http.createServer(app)

const io = socketio.listen(socketServer)

io.on('connection', socket => {
  socket.on('NEW_MESSAGE', payload => {
    io.emit('NEW_MESSAGE_RECEIVED', payload)
  })
})

socketServer.listen(4200, () => console.log('App running on port 4200'))

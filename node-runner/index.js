const WebSocket = require('ws');

const PORT = process.argv[process.argv.indexOf('--port') + 1];

RemoteJS = {
  connect() {
    const ws = new WebSocket(`ws://0.0.0.0:${PORT}/`)
      .on('message', function(buf) {
        try {
          (1, eval)(buf.toString());
        } catch (e) {
          console.error(e);
        }
      })
      .on('open', function() {
        ws.send('connected');
      })
      .on('close', function() {
        setTimeout(RemoteJS.connect, 1);
      });
    this.ws = ws;
  },

  send(msg) {
    return this.ws.send(msg);
  },
}

RemoteJS.connect();

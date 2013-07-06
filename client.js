$(document).ready(function () {
    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    var ws = new Socket('ws://localhost:9160/');

    ws.onopen = function() { ws.send('user0'); };
    ws.onmessage = function(event){
      console.log(["receive:", event.data]);
      $('#messages').text(event.data);
    };

    $('#send').click(function () {
        ws.send("MSG1");
    });
});

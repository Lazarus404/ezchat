'use strict';

window.onload = function () {
  // Getting references to page DOM.
  var sendMessage = document.getElementById("sendMessage"),
    peers = document.getElementById("peers"),
    message = document.getElementById("message"),
    messages = document.getElementById("messages"),
    login = document.getElementById("login"),
    username = document.getElementById("username"),
    roomname = document.getElementById("roomname"),
    name = "", room = "";

  var s = new $xirsys.signal();

  // When the connect button is clicked...
  login.onsubmit = function ($event) {
    $event.preventDefault();
    document.getElementById('login-btn').style.visibility = 'hidden';
    // Make the username valid.
    name = username.value.replace(/\W+/g, "");
    room = roomname.value.replace(/\W+/g, "");
    if (!name || name == "" || !room || room == "") {
      return;
    }
    var connectionProperties = {room: room, username: name};
    s.connect(connectionProperties);
  }

  // Peer connected helper function (UI only).
  var addPeer = function ($peer_name) {
    if (!!document.getElementById("peer_" + $peer_name) || $peer_name == name) {
      return;
    }
    var node = document.createElement("div"),
      btn = document.createElement("input");
    btn.setAttribute('type', 'radio');
    btn.setAttribute('name', 'peer');
    btn.setAttribute('value', $peer_name);
    node.appendChild(btn);
    node.appendChild(document.createTextNode(stripLeaf($peer_name)));
    peers.appendChild(node);
    node.id = "peer" + $peer_name;
    node.className = "peer";
  };

  // Peer removal helper function (UI only).
  var removePeer = function ($peer_name) {
    var node = document.getElementById("peer" + $peer_name);
    peers.removeChild(node);
  };

  // Peer value helper function (UI only).
  var selectedPeer = function () {
    var peer = document.getElementsByName("peer");
    console.log(peer);
    for (var i=0, l=peer.length; i<l; i++) {
      if (peer[i].checked) {
        return (peer[i].value == "__all__") ? 
          undefined : peer[i].value;
      }
    }
  };

  var addMessage = function ($msg) {
    var d = document.createElement("div");
    d.appendChild(document.createTextNode($msg));
    messages.appendChild(d);
  };

  var stripLeaf = function (p) {
     return p.substr(p.lastIndexOf("/")+1)
  };

  // When a peer connects to signalling, we
  // get notified here.
  $xirsys.events.getInstance().on($xirsys.signal.peerConnected, function ($evt, $msg) {
    addPeer($msg);
  });
  
  // When a peer leaves (disconnects) from
  // the signalling, we get notified here.
  $xirsys.events.getInstance().on($xirsys.signal.peerRemoved, function ($evt, $peer) {
    removePeer($peer);
  });
  
  // We get this when we login. There may be zero
  // to many peers at this time.
  $xirsys.events.getInstance().on($xirsys.signal.peers, function ($evt, $msg) {
    for (var i = 0; i < $msg.users.length; i++) {
      addPeer($msg.users[i]);
    }
  });

  $xirsys.events.getInstance().on($xirsys.signal.message, function ($evt, $msg) {
    console.log($msg.data);
    if ($msg.sender != name) {
      addMessage(stripLeaf($msg.sender) + ": " + $msg.data.message);
    }
  });

  $xirsys.events.getInstance().on($xirsys.signal.error, function ($evt, $msg) {
    console.error("error: ", $msg);
  });

  // Here, we do something when the send button
  // is clicked.
  sendMessage.onsubmit = function ($event) {
    $event.preventDefault();
    var peer = selectedPeer();
    addMessage(name + ": " + message.value);
    if (!!peer) {
      console.log(peer);
      s.send("message", message.value, peer);
    } else {
      console.log('no peer');
      s.send("message", message.value);
    }
    message.value = '';
  }
}
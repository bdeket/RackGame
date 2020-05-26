"use version 7";
"use strict";

import {connection, handler, newEl} from './connection.js'

let ownID=""
let ownRoom=""
const ownState=function(e){
  switch (e.state){
    case "waiting":
      butstart.classList.remove("ready");
      butready.classList.remove("ready");
      butready.textContent="ready";
      break;
    case "canjoin":
      butstart.classList.remove("ready");
      butready.classList.add("ready");
      butready.textContent="ready";
      break;
    case "ready":
      butready.textContent="wait";
      break;
    default:
      console.log(e);
  }
}
const setState=function(e){
  switch (e.state){
    case "waiting":
      rooms[allplayers[e.user]].playerWait(e.user);
      break;
    case "canjoin":
      rooms[allplayers[e.user]].playerWait(e.user);
      break;
    case "ready":
      rooms[allplayers[e.user]].playerReady(e.user);
      break;
    default:
      console.log(e);
  }
}

export const lobby=newEl("div",{id: "lobby"});
const rsgbar=newEl("div",{id: "rsgbar"});
const butcreat=newEl("button",{class: "rsgbutton",txt: "create"});
butcreat.addEventListener('click',e=>connection.send({tag: "create"}),false);
rsgbar.appendChild(butcreat);
const butready=newEl("button",{class: "rsgbutton",txt: "ready"});
butready.addEventListener('click',e=>connection.send({tag: butready.textContent}),false);
rsgbar.appendChild(butready);
const butstart=newEl("button",{class: "rsgbutton",txt: "start"});
butstart.addEventListener('click',e=>connection.send({tag: "start"}),false);
rsgbar.appendChild(butstart);

const rooms= new Object();
const allplayers= new Object()
function room(id,newname){
  const players=new Object();
  this.name=newname;
  const elem=newEl("div",{class: "room"});
  const title=newEl("div",{class: "roomtitle",txt: newname});
  this.addPlayer=function(name){
    players[name]=newEl("div",{class: "player",txt: name});
    allplayers[name]=id;
    elem.appendChild(players[name]);
  }
  this.removePlayer=function(name){
    elem.removeChild(players[name]);
    delete players[name];
  }
  this.playerReady=function(name){
    players[name].classList.add("ready");
  }
  this.playerWait=function(name){
    players[name].classList.remove("ready");
  }
  this.remove=function(){
    lobby.removeChild(elem);
  }

  elem.appendChild(title);
  title.addEventListener('click',e=>connection.send({tag: "joinroom",room: id}),false);
  lobby.insertBefore(elem,rsgbar);
}
rooms.addRoom=function(e){
  rooms[e.id]=new room(e.id, e.name);
  for(let i=0;i<(e.players?e.players.length:0);i++){
    rooms[e.id].addPlayer(e.players[i].user);
    setState(e.players[i]);
  }
}
rooms.removeRoom=function(id){
  rooms[id].remove();
  delete rooms[id];
}

lobby.register=function(snd){
  document.getElementById("theCSS").href="styles/lobby.css";
  lobby.appendChild(rsgbar);
  rooms.addRoom({id: "lobby", name: "Lobby", players: []});
connection.registerHandlers("lobby",
  new handler(e=>e.tag=="rooms",
        function(e){
          ownID=e.user;
          ownRoom="lobby";
          rooms.removeRoom("lobby");
          for(let i=0;i<e.rooms.length;i++){
            rooms.addRoom(e.rooms[i]);
          }
          butcreat.classList.add("ready");
        }),
  new handler(e=>e.tag=="roomIn",
              e=>{rooms[e.room].addPlayer(e.user);
          if(e.user==ownID){ownRoom=e.room;}}),
  new handler(e=>e.tag=="roomOut",
              e=>{rooms[allplayers[e.user]].removePlayer(e.user);}),
  new handler(e=>e.tag=="gameList",
              e=>showGames(e)),
  new handler(e=>e.tag=="newRoom",
              e=>rooms.addRoom(e)),
  new handler(e=>e.tag=="playerState",
              e=>{setState(e);if(e.user==ownID){ownState(e);}}),
  new handler(e=>e.tag=="roomState",
              e=>{if(ownRoom==e.room){
          if(e.state=="ready"){butstart.classList.add("ready")}
          else {butstart.classList.remove("ready")}
        }}),
  new handler(e=>e.tag=="deleteRoom",
              e=>{rooms.removeRoom(e.room)}),
  new handler(e=>e.tag=="startGame",
              e=>{lobby.unregister();
                  import(e.world).then(world=>{world.register(lobby)})}));
  if(snd){connection.send({tag: "lobbyReady"});}
}
connection.registerHandlers("always",
  new handler(e=>e.tag=="leaveGame",
              e=>{import(e.world).then(world=>{world.unregister()});
                  lobby.register();}));
lobby.unregister=function(){
  for(const key of Object.keys(rooms)){
    if(rooms[key].remove){
      rooms[key].remove();
      delete rooms[key];
    }
  }
  lobby.removeChild(rsgbar);
  connection.removeHandlers("lobby");
}
lobby.register(null)

const showGames=function(e){
  const gTop=newEl("div",{id: "gamelistbox",txt: "Games:"});
  for(let i=0;i<e.games.length;i++){
    const gItm=newEl("button",{class: "gamebutton",txt: e.games[i].name});
    gTop.appendChild(gItm);
    const game=e.games[i].id;
    gItm.addEventListener('click',e=>{connection.send({tag: "createRoom", game: game});lobby.removeChild(gTop)},false);
  }
  const gCancel=newEl("button",{class: "gamebutton",txt: "cancel"});
  gTop.appendChild(gCancel);
  gCancel.addEventListener('click',e=>lobby.removeChild(gTop),false);
  lobby.appendChild(gTop);
}

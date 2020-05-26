"use version 7";
"use strict";

import {connection, handler, newEl} from '../scripts/connection.js'

export function register(screen, joinLobby){
  const game=newEl("div");
  const leave=newEl("button",{txt: "leave"});
  leave.addEventListener('click',
  e=>{
  connection.send({tag: "leave"});
  screen.removeChild(game);
  joinLobby();
  },false);
  game.appendChild(leave);
  screen.appendChild(game);
  connection.send({tag: "gameReadyToStart"})
}

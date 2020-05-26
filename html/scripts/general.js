"use version 7";
"use strict";

import {chatbox} from './chatbox.js';
import {connection, handler, newEl} from './connection.js';
import {namebox} from './getname.js';
import {lobby} from './lobby.js';

const content=newEl("div",{id: "content"})
window.addEventListener('load',init,false);
function init(){
  document.body.appendChild(content);
  content.appendChild(lobby);
  content.appendChild(chatbox);
  namebox.register(content);
}

connection.registerHandlers("general",
  new handler(e=>e.tag=="serverDisconnected",
              e=>{document.body.appendChild(newEl("div",{id: "noserver",txt: "Server Disconnected"}))}));

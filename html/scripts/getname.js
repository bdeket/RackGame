"use version 7";
"use strict";

import {connection, handler, newEl} from './connection.js'

var content=false;
connection.registerHandlers("getname",
new handler(
  e=>e.tag=="nameOK",
  function(e){
  document.body.removeChild(namebox);
  connection.removeHandlers("getname");
  content.style.filter="";
  }
),
new handler(
  e=>e.tag=="nameAlreadyUsed",
  function(e){
  nameinput.classList.add("invalid");
  nameused.style.display="block";
  }
));

export const namebox=newEl("div",{id: "namebox",txt: "Choose name:"});
const nameinput=newEl("input",{id: "nameinput"})
nameinput.required=true;
const test=new RegExp('^[A-Za-z][A-Za-z0-9]+$');
nameinput.addEventListener('keydown',function(e){if(e.keyCode==13){login()}},false);
nameinput.addEventListener('keyup',testinput,false);
function testinput(e){
  if(test.exec(nameinput.value)){
    nameinput.classList.remove("invalid");
  } else {
    nameinput.classList.add("invalid");
  }
}

const namesend=newEl("button",{txt: "send"});
namesend.addEventListener('click',login,false)

const nameused=newEl("div",{id: "nameused",txt: "Name already in use"});


function login(){
  if(test.exec(nameinput.value)){
    connection.send({tag: "login",name: nameinput.value});
  }
}

namebox.appendChild(nameinput);
namebox.appendChild(nameused);
namebox.appendChild(namesend);

namebox.register=function(elem){
  content=elem
  document.body.appendChild(namebox);
  content.style.filter="blur(1px)";
  content.addEventListener('click',e=>nameinput.focus(),true);
  content.addEventListener('focus',e=>nameinput.focus(),true);
  nameinput.focus();
  //namebox.style.filter="";
}

"use version 7";
"use strict";

export function newEl(tag,opt){
  if(tag=="text"){
  if(id||txt){throw "text can't have class or txt attributes";}
    document.createTextNode(id);
  } else {
  const el=document.createElement(tag);
    if(opt){
    if(opt.id){el.id=opt.id;}
    if(opt.class){el.classList.add(opt.class);}
    if(opt.txt){el.appendChild(document.createTextNode(opt.txt));}
    if(opt.parent){opt.parent.appendChild(el);}
  }
  return el;
  }
}

let mainsite=window.location.href;
mainsite=mainsite.replace("http://","");
mainsite=mainsite.split(/(|:\d+)\/index.html/)[0];
mainsite=mainsite.split(/(|:\d+)\/RackGame/)[0];
let site="";
switch(mainsite){
  //internal test server
  case "192.168.1.2":
    site="192.168.1.30";
    break;
  default :
    site=mainsite;
}
const intern = new WebSocket('ws://'+site+':8081/game');
intern.onclose=function(e){handleMessage({tag: "serverDisconnected"})}
intern.handlers=new Object();
intern.onmessage=function(e){handleMessage(JSON.parse(e.data));}
const handleMessage= function(Je){
  let handled=false
  for(const hndl of Object.values(intern.handlers)){
    if(hndl.test(Je)){
      handled=true
      hndl.action(Je)
    }
  }
  if(!handled){
    console.log("unhandled message: "+Je.tag);
    console.log(Je);
  }
}

export const connection = new Object();
export function handler(test, action){
  this.test=test;
  this.action=action;
}
connection.registerHandlers=function(name){
  for(let i=1;i<arguments.length;i++){
    intern.handlers[name+''+i]=arguments[i];
  }
}
connection.removeHandlers=function(name){
  for(let i=1;intern.handlers[name+''+i];i++){
    delete intern.handlers[name+''+i]
  }
}
connection.send=function(msg){intern.send(JSON.stringify(msg))}

setInterval(function(){connection.send({tag: "keepalive"})},60*1000);

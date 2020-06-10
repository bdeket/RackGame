"use version 7";
"use strict";

import {connection, handler, newEl} from '../../scripts/connection.js'

const leave=newEl("button",{txt: "leave"});
leave.addEventListener('click',e=>connection.send({tag: "leaveGame"}),false);
const again=newEl("button",{txt: "again"});
again.addEventListener('click',e=>connection.send({tag: "next"}),false)
const winner=newEl("span",{txt: ""})

const game=newEl("div",{id: "gamebord"});
const player1=newEl("div",{id: "player1", class: "playerTop", parent: game});
const player2=newEl("div",{id: "player2", class: "playerTop", parent: game});
const bordbox=newEl("div",{id: "bordbox", parent: game});
const passbut=newEl("button",{id: "passbtn", txt: "pass", parent: bordbox});
passbut.addEventListener("click",e=>connection.send({tag: "pass"}));
const contbut=newEl("button",{id: "contbtn", txt: "continue", parent: bordbox});
contbut.addEventListener("click",e=>connection.send({tag: "continue"}));
const brd=newEl("div",{parent: bordbox});

//*****************************************************************************
// player state
//*****************************************************************************
const colors=["white","black"];
const players=new Object();
function playerState(e){
  let P=players[e.user];
  if(P==null){
    P=new Object();
    P.color=colors[Object.keys(players).length];
    P.name=e.user;
    players[e.user]=P;
    const elem=newEl("div",{class: "player", parent: (Object.keys(players).length==1?player1:player2)});
    const name=newEl("div",{parent: elem});
    name.appendChild(stone(P.color));
    newEl("span",{txt: P.name, parent: name});
    const points=newEl("div",{txt: "points: ", parent: elem});
    P.points=newEl("span",{txt: e.points, parent: points});
    P.state=newEl("div",{txt: e.state, parent: elem});
  }
  P.points.textContent=e.points;
  P.state.textContent=e.state;
  //console.log(P);
}
//*****************************************************************************
// Game drawing functions
//*****************************************************************************
const bord=new Object();
function doReport(e){
  cleanBord();
  contbut.disabled=true;
  const SS=e.bordSize;
  for(let i=0;i<SS;i++){
    const row=newEl("div", {class: "row", parent: brd});
    for(let j=0;j<SS;j++){
      const name=""+i+"+"+j;
      const itm=newEl("div", {class: "space", parent: row});
      if (i==0 && j==0){const svg=gridLT();itm.appendChild(svg);}
      else if (i==0 && j==SS-1){const svg=gridRT();itm.appendChild(svg);}
      else if (i==0 ){const svg=gridL();itm.appendChild(svg);}
      else if (i==SS-1 && j==0){const svg=gridLB();itm.appendChild(svg);}
      else if (i==SS-1 && j==SS-1){const svg=gridRB();itm.appendChild(svg);}
      else if (i==SS-1){const svg=gridR();itm.appendChild(svg);}
      else if (j==0){const svg=gridT();itm.appendChild(svg);}
      else if (j==SS-1){const svg=gridB();itm.appendChild(svg);}
      else {const svg=gridM();itm.appendChild(svg);}
      const tag={tag: "place", x: i, y: j};
      itm.addEventListener("click",e=>connection.send(tag),false);
      bord[name]=itm;
    }
  }
}

function bordUpdate(e){
  //console.log(players[e.user]);
  addStone(e.new.x,e.new.y,players[e.user].color);
  e.removed.forEach((item, i) => {
    removeStone(item.x,item.y);
  });
}
function addStone(x,y,color){
  const name=""+x+"+"+y;
  bord[name].stone=stone(color);
  bord[name].childNodes[0].appendChild(bord[name].stone);
}
function removeStone(x,y){
  const name=""+x+"+"+y;
  bord[name].stone.remove();
}

const check=new Object();
function bordCheck(e){
  if(contbut.disabled) contbut.disabled=false;
  const pos=""+e.pos.x+"+"+e.pos.y;
  if(e.state){
    const color=players[e.state].color;
    check[pos]=circle(color);
    bord[pos].childNodes[0].appendChild(check[pos]);
  } else {
    check[pos].remove();
    delete check[pos];
  }
}
function playAgain(){
  removeCheck();
  contbut.disabled=true;
}
function removeCheck(){
  for(const pos of Object.keys(check)){
    if(check[pos]){
      check[pos].remove();
      delete check[pos];
    }
  }
}
function cleanBord(){
  //remove players and state
  for(const play of Object.keys(players)){delete players[play];}
  player1.childNodes.forEach((item, i) => {item.remove();});
  player2.childNodes.forEach((item, i) => {item.remove();});
  //remove win info (if any)
  leave.remove();
  again.remove();
  winner.remove();
  //remove stones checks and bord state
  removeCheck();
  console.log(brd.childNodes.length);
  while(brd.firstChild){brd.removeChild(brd.lastChild);}
  console.log(brd.childNodes.length);
  for(const itm of Object.keys(bord)){delete bord[itm];}
}
//*****************************************************************************
// Register
//*****************************************************************************
export function register(screen, joinLobby){
  connection.registerHandlers("go",
  new handler(e=>e.tag=="report",doReport),
  new handler(e=>e.tag=="playerState",playerState),
  new handler(e=>e.tag=="bordUpdate",bordUpdate),
  new handler(e=>e.tag=="bordCheck",bordCheck),
  new handler(e=>e.tag=="playAgain",playAgain),
  new handler(e=>e.tag=="gameDone",e=>{
    if(e.winner){ winner.textContent=e.winner+" wins!"; }
    else { winner.textContent="Draw"; }
    bordbox.appendChild(winner);
    bordbox.appendChild(leave);
    bordbox.appendChild(again);
  }));

  document.getElementById("theCSS").href="games/go/go.css";
  screen.appendChild(game);
  connection.send({tag: "gameReadyToStart"})
}
//*****************************************************************************
// Unregister
//*****************************************************************************
export function unregister(){
  //remove handlers
  connection.removeHandlers("go");
  //remove bord state
  cleanBord();
  //remove game
  game.remove();
}


//*****************************************************************************
// code for SVG generation
//*****************************************************************************
function getNode(n, v) {
  n = document.createElementNS("http://www.w3.org/2000/svg", n);
  for (var p in v)
    n.setAttributeNS(null, p.replace(/[A-Z]/g, function(m, p, o, s) { return "-" + m.toLowerCase(); }), v[p]);
  return n
}

function makeSVG(){
  const svg=getNode("svg",{width: "100%", height: "100%"});
  svg.setAttributeNS(null ,"viewBox","0 0 100 100");
  svg.setAttributeNS(null,"preserveAspectRatio","xMinYMin meet")
  for(const e of arguments){
    if(e.tag==null){
      Object.entries(e).map(([k,v]) => svg.setAttributeNS(null,k,v));
    } else {
      const r = getNode(e.tag,e.obj);
      svg.appendChild(r);
    }
  }
  return svg;
}
function gridT(){
  return makeSVG({tag: "line", obj: {x1: "50", y1:  "0", x2:  "50", y2: "100", strokeWidth: "2", stroke: "black"}},
                 {tag: "line", obj: {x1: "50", y1: "50", x2: "100", y2:  "50", strokeWidth: "2", stroke: "black"}});
}
function gridB(){
  return makeSVG({tag: "line", obj: {x1: "50", y1:  "0", x2:  "50", y2: "100", strokeWidth: "2", stroke: "black"}},
                 {tag: "line", obj: {x1: "50", y1: "50", x2:   "0", y2:  "50", strokeWidth: "2", stroke: "black"}});
}
function gridL(){
  return makeSVG({tag: "line", obj: {x1:  "0", y1: "50", x2: "100", y2:  "50", strokeWidth: "2", stroke: "black"}},
                 {tag: "line", obj: {x1: "50", y1: "50", x2:  "50", y2: "100", strokeWidth: "2", stroke: "black"}});
}
function gridR(){
  return makeSVG({tag: "line", obj: {x1:  "0", y1: "50", x2: "100", y2:  "50", strokeWidth: "2", stroke: "black"}},
                 {tag: "line", obj: {x1: "50", y1: "50", x2:  "50", y2:   "0", strokeWidth: "2", stroke: "black"}});
}
function gridLT(){
  return makeSVG({tag: "line", obj: {x1: "50", y1: "50", x2: "100", y2:  "50", strokeWidth: "2", stroke: "black"}},
                 {tag: "line", obj: {x1: "50", y1: "50", x2:  "50", y2: "100", strokeWidth: "2", stroke: "black"}});
}
function gridRT(){
  return makeSVG({tag: "line", obj: {x1: "50", y1: "50", x2:   "0", y2:  "50", strokeWidth: "2", stroke: "black"}},
                 {tag: "line", obj: {x1: "50", y1: "50", x2:  "50", y2: "100", strokeWidth: "2", stroke: "black"}});
}
function gridLB(){
  return makeSVG({tag: "line", obj: {x1: "50", y1: "50", x2:  "50", y2:   "0", strokeWidth: "2", stroke: "black"}},
                 {tag: "line", obj: {x1: "50", y1: "50", x2: "100", y2:  "50", strokeWidth: "2", stroke: "black"}});
}
function gridRB(){
  return makeSVG({tag: "line", obj: {x1: "50", y1: "50", x2:  "50", y2:   "0", strokeWidth: "2", stroke: "black"}},
                 {tag: "line", obj: {x1: "50", y1: "50", x2:   "0", y2:  "50", strokeWidth: "2", stroke: "black"}});
}
function gridM(){
  return makeSVG({tag: "line", obj: {x1: "50", y1:  "0", x2:  "50", y2: "100", strokeWidth: "2", stroke: "black"}},
                 {tag: "line", obj: {x1:  "0", y1: "50", x2: "100", y2:  "50", strokeWidth: "2", stroke: "black"}});
}

let theCounter=0
function stone(color){
  const svg=makeSVG();
  const defs=getNode("defs", {});
  const circle=getNode("circle",{cx: "50", cy: "50", r: "50", fill: "url(#radgrad"+theCounter+")", fillOpacity: 1, opacity: 1});
  const radgrad=getNode("radialGradient", {id: "radgrad"+theCounter, fx: "0.75", fy: "0.75"})
  radgrad.appendChild(getNode("stop",{offset: "0%", stopColor:"#a0a0a0"}));
  radgrad.appendChild(getNode("stop",{offset: "100%", stopColor: color, stopOpacity: "0.9"}));
  defs.appendChild(radgrad);
  svg.appendChild(defs);
  svg.appendChild(circle);
  svg.classList.add("stone");
  theCounter++;
  return svg;
}

function circle(color){
  const svg=makeSVG({tag: "circle", obj: {cx: "50", cy: "50", r: "35", strokeWidth: "8", stroke: color, fill: "transparent"}});
  svg.classList.add("circ");
  return svg;
}

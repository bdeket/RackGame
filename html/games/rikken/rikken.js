"use version 7";
"use strict";

import {connection, handler, newEl} from '../../scripts/connection.js'

const game=newEl("div",{id: "gamebord"});
const plNord=newEl("div",{id: "plNord", class: "playerarea", parent: game});
const cdNord=newEl("div",{id: "cdNord", class: "cardarea", parent: game});
const plEast=newEl("div",{id: "plEast", class: "playerarea", parent: game});
const cdEast=newEl("div",{id: "cdEast", class: "cardarea", parent: game});
const plWest=newEl("div",{id: "plWest", class: "playerarea", parent: game});
const cdWest=newEl("div",{id: "cdWest", class: "cardarea", parent: game});
const cdSouth=newEl("div",{id: "cdSouth", class: "cardarea", parent: game});
const plLeft=newEl("div",{id: "plLeft", parent: game});
const plRight=newEl("div",{id: "plRight", parent: game});
const handbox=newEl("div",{id: "handbox", parent: game});
const trump=newEl("div",{id: "trump", parent: game});

const leave=newEl("button",{txt: "leave"})
leave.addEventListener('click',e=>connection.send({tag: "leaveGame"}),false);
const again=newEl("button",{txt: "again"})
again.addEventListener('click',e=>connection.send({tag: "next"}),false);

//Player init and point update
const players=new Object();
let self=null;
function addPlayer(e,i){
  let P=players[e.user];
  if(!P){
    players[e.user]=new Object();
    P=players[e.user];
    P.elem=newEl("div",{class: "player"})
    P.name=newEl("span",{class: "playername",txt: e.user,parent: P.elem});
    P.point=newEl("span",{class: "playerppoints", parent: P.elem});
    P.round=newEl("span",{class: "playerrpoints", parent: P.elem});
    P.state=newEl("div",{class: "playerstate", parent: P.elem})
    P.bid=newEl("div",{class: "playerbid", parent: P.elem});
    switch(i){
      case 0: plLeft.appendChild(P.elem);P.cardbox=cdSouth;self=e.user;break;
      case 1: plWest.appendChild(P.elem);P.cardbox=cdWest;break;
      case 2: plNord.appendChild(P.elem);P.cardbox=cdNord;break;
      case 3: plEast.appendChild(P.elem);P.cardbox=cdEast;break;
    }
  }
  if(e.type){P.name.classList.add("computer");}
  if(e.state!="waiting"){P.state.classList.add("ready")}
  else {P.state.classList.remove("ready")}
  P.point.textContent="p["+e.point+"]";
  P.round.textContent="r["+e.round+"]";
  P.state.textContent=e.state;
  if(e.bid){P.bid.textContent=e.bid;}else{P.bid.textContent="";}
}
function updateState(e){
  const P=players[e.user];
  if(P){
    P.state.textContent=e.state;
    if(e.type){P.name.classList.add("computer")}
    if(e.state!="waiting"){P.state.classList.add("ready")}
    else {P.state.classList.remove("ready")}
    if(e.bid){P.bid.textContent=e.bid;}else{P.bid.textContent="";}
  }
}

function card(c){
	const elem=newEl("div",{class: "card"});
	const suit=newEl("div",{class: "suit"});
	const numb=newEl("div",{class: "numb"});
	let N="";
	switch(c.number){
		case "T": N="T";break;
		case  9: N="J";break;
		case 10: N="D";break;
		case 11: N="H";break;
		case 12: N="1";break;
		default: N=""+(c.number+2);
	}
	switch(c.suit){
		case 0: elem.appendChild(heart(N));break;
		case 1: elem.appendChild(spade(N));break;
		case 2: elem.appendChild(diamond(N));break;
		case 3: elem.appendChild(club(N));break;
	}
	return elem;
}

const cards=new Object();
function addHandCard(c){
	const name="s"+c.suit+"n"+c.number;
	cards[name]=card(c);
	const tag={tag: "selectCard", card: c};
	cards[name].ticker=e=>connection.send(tag);
	cards[name].addEventListener('click',cards[name].ticker,false);
	handbox.appendChild(cards[name]);
}
function playHandCard(c){
	const name="s"+c.suit+"n"+c.number;
	if(cards[name]){
		cards[name].removeEventListener('click',cards[name].ticker,false);
		cdSouth.appendChild(cards[name]);
		delete cards[name];
	}
}
function playCard(e){
	if(e.user==self){playHandCard(e.card)}
	else{
		players[e.user].cardbox.appendChild(card(e.card));
	}
}
function cleanCards(){
  for(const key of Object.keys(cards)){
    cards[key].remove();
    delete cards[key];
  }
  while(0<trump.childElementCount){trump.childNodes[0].remove();}
  for(const key of Object.keys(players)){players[key].bid.textContent="";}
  while(0<plRight.childElementCount){plRight.childNodes[0].remove();}
  while(0<cdEast.childElementCount){cdEast.childNodes[0].remove();}
  while(0<cdNord.childElementCount){cdNord.childNodes[0].remove();}
  while(0<cdWest.childElementCount){cdWest.childNodes[0].remove();}
  while(0<cdSouth.childElementCount){cdSouth.childNodes[0].remove();}
}
function allCards(e){
  cleanCards();
  e.cards.forEach(addHandCard)
}

function lastRound(ar){
  while(0<plRight.childElementCount){plRight.childNodes[0].remove();}
	let e=null;
	if(0<ar.length){
		e=cdEast.childNodes[0];plRight.appendChild(e);e.style.gridArea="lce";
		e=cdNord.childNodes[0];plRight.appendChild(e);e.style.gridArea="lcn";
		e=cdWest.childNodes[0];plRight.appendChild(e);e.style.gridArea="lcw";
		e=cdSouth.childNodes[0];plRight.appendChild(e);e.style.gridArea="lcs";
	}
}

function bid(e){
  const bid=newEl("div",{id: "bid", parent: game});
  bid.appendChild(card(e.lastCard));
  for(const key of e.options){
    const el=newEl("button",{txt: key, parent: bid});
    el.addEventListener('click',e=>{connection.send({tag: "selectBid",bid: key});
                                    bid.remove();},false);
  }
}
function getTrump(e){
  const bid=newEl("div",{id: "bid", parent: game});
  for(const s of e.trump){
    const el=card({number: "T",suit: s})
    el.addEventListener('click',e=>{connection.send({tag: "selectTrump",suit: s});
                                    bid.remove();},false);
    bid.appendChild(el);
  }
}
function getAce(e){
  const bid=newEl("div",{id: "bid", parent: game});
  for(const s of e.ace){
    const el=card({number: 12,suit: s})
    el.addEventListener('click',e=>{connection.send({tag: "selectAce",suit: s});
                                    bid.remove();},false);
    bid.appendChild(el);
  }
}
function gameType(e){
  while(0<trump.childElementCount){trump.childNodes[0].remove();}
  newEl("span",{class: "type",txt: e.type,parent: trump});
  const crds=newEl("div",{parent: trump});
  if(e.trump!=null){
    const crd=card({number: "T",suit: e.trump});
    crds.appendChild(crd);
  }
  if(e.ace!=null){
    const crd=card({number: 12,suit: e.ace});
    crds.appendChild(crd);
  }
  e.users.forEach(e=>newEl("span",{class: "playing", txt: e, parent: trump}));
  e.team.forEach(e=>newEl("span",{class: "playing", txt: e, parent: trump}));
}

export function register(screen){
	connection.registerHandlers("rikken",
		new handler(e=>e.tag=="gameType",
                e=>gameType(e)),
    new handler(e=>e.tag=="players", e=>{e.players.forEach(addPlayer)}),
    new handler(e=>e.tag=="play", e=>{"ignore"}),
    new handler(e=>e.tag=="cards", e=>allCards(e)),
    new handler(e=>e.tag=="bid", e=>bid(e)),
    new handler(e=>e.tag=="selectTrump", e=>getTrump(e)),
    new handler(e=>e.tag=="selectAce", e=>getAce(e)),
		new handler(e=>e.tag=="lastRound", e=>lastRound(e.cards)),
    new handler(e=>e.tag=="playerState", e=>updateState(e)),
    new handler(e=>e.tag=="gameDone", e=>{cdSouth.appendChild(leave);cdSouth.appendChild(again);}),
		new handler(e=>e.tag=="cardPlayed", e=>playCard(e)))

	document.getElementById("theCSS").href="games/rikken/rikken.css";
	screen.appendChild(game);
	connection.send({tag: "gameReadyToStart"})
}
export function unregister(){
  game.remove();
  cleanCards();
  for(const key of Object.keys(players)){
    players[key].elem.remove();
    delete players[key];
  }
  const bid=document.getElementById("bid");
  if(bid){bid.remove();}
  connection.removeHandlers("rikken");
}

/*code for SVG generation*/
function getNode(n, v) {
  n = document.createElementNS("http://www.w3.org/2000/svg", n);
  for (var p in v)
    n.setAttributeNS(null, p.replace(/[A-Z]/g, function(m, p, o, s) { return "-" + m.toLowerCase(); }), v[p]);
  return n
}

function makeSVG(){
	const svg=getNode("svg",{width: "100%", height: "100%"});
	//svg.setAttributeNS(null ,"viewBox","-3000 -3000 3000 3000");
	svg.setAttributeNS(null,"preserveAspectRatio","xMinYMin meet")
	for(const e of arguments){
		const r = getNode(e.tag,e.obj);
		svg.appendChild(r);
	}
	return svg;
}
const spade=function(t){
	const svg=makeSVG({tag: "path", obj: {fill: 'black', d:"M0 0 c -87 139 -196 255 -570 600 -393 364 -533 525 -630 725 -63 130 -79 198 -79 335 0 96 4 125 24 175 115 298 460 464 797 384 91 -21 228 -95 298 -160 36 -34 65 -57 65 -53 -1 26 -32 194 -55 298 -62 278 -177 516 -302 626 -46 40 -143 92 -203 108 -23 7 206 10 675 10 689 0 708 0 655 -17 -105 -34 -171 -81 -254 -184 -122 -153 -235 -480 -281 -812 l -7 -50 54 52 c 72 71 94 87 174 127 108 54 181 70 319 70 138 0 208 -16 325 -72 98 -48 223 -169 269 -259 127 -254 48 -556 -233 -891 -86 -102 -244 -258 -516 -509 -273 -252 -347 -332 -483 -528 -13 -18 -15 -17 -42 25 z"}},
										{tag: "text", obj: {fill: 'black', x: "2800", y: "1900", fontSize: "2500", dominantBaseline: "middle", textAnchor: "middle"}})
	svg.setAttributeNS(null,"viewBox","-1500 -50 6100 3050");
	svg.childNodes[1].textContent=t;
	return svg;
}
const club=function(t){
	const svg=makeSVG({tag: "path", obj: {fill: '#003300', d:"M 0 0 c -313 70 -512 329 -497 646 8 170 63 289 208 444 47 51 105 121 129 156 47 70 145 268 136 277 -10 10 -109 -68 -166 -130 -254 -279 -699 -287 -960 -17 -255 263 -240 685 32 925 73 64 214 132 311 150 98 19 155 19 249 1 141 -27 240 -81 363 -197 85 -80 251 -196 263 -185 10 11 -27 247 -58 363 -53 202 -125 346 -227 453 -81 86 -145 128 -242 156 l -76 23 700 0 c 681 -1 698 -1 635 -18 -259 -69 -430 -313 -519 -741 -11 -52 -23 -126 -27 -164 l -7 -70 39 24 c 75 45 129 83 181 127 28 23 54 43 58 43 4 0 29 20 55 44 61 54 173 110 265 132 94 22 275 15 358 -15 107 -38 180 -83 258 -161 128 -127 178 -238 186 -415 5 -93 2 -124 -16 -195 -12 -47 -24 -94 -26 -104 -9 -42 -74 -132 -142 -196 -163 -156 -412 -217 -630 -154 -94 27 -201 89 -303 175 -52 44 -117 96 -143 115 -45 33 -48 34 -42 12 14 -48 104 -225 141 -280 22 -31 78 -99 125 -151 147 -162 193 -269 193 -452 0 -155 -33 -256 -127 -381 -147 -195 -430 -295 -677 -240 z"}},
										{tag: "text", obj: {fill: '#003300', x: "2980", y: "1960", fontSize: "2540", dominantBaseline: "middle", textAnchor: "middle"}})
	svg.setAttributeNS(null,"viewBox","-1390 -25 6200 3100");
	svg.childNodes[1].textContent=t;
	return svg;
}
const diamond=function(t){
	const svg=makeSVG({tag: "path", obj: {fill: 'darkred', d:"M 0 0 c -2 4 -12 34 -21 67 -10 32 -35 97 -57 145 -22 47 -39 88 -39 90 1 2 -3 7 -8 10 -5 3 -31 37 -57 76 -81 119 -195 254 -325 386 -187 188 -333 291 -738 520 l -202 114 47 13 c 149 40 390 152 538 249 317 210 555 480 721 818 54 108 129 308 146 390 6 23 1 35 56 -137 140 -436 429 -814 822 -1073 149 -98 395 -213 525 -246 25 -6 44 -14 41 -16 -2 -3 -56 -22 -120 -43 -430 -145 -793 -424 -1050 -808 -82 -123 -189 -335 -235 -467 -34 -96 -37 -101 -44 -88 z m -162 393 c -9 16 -18 30 -21 30 -2 0 2 -14 11 -30 9 -17 18 -30 21 -30 2 0 -2 13 -11 30 z"}},
										{tag: "text", obj: {fill: 'darkred', x: "2640", y: "1830", fontSize: "2375", dominantBaseline: "middle", textAnchor: "middle"}})
	svg.setAttributeNS(null,"viewBox","-1450 -10 5800 2900");
	svg.childNodes[1].textContent=t;
	return svg;
}
const heart=function(t){
	const svg=makeSVG({tag: "path", obj: {fill: 'red', d:"M 0 0 c -16 3 -58 14 -93 25 -216 71 -378 244 -444 474 -18 65 -22 103 -21 205 1 197 52 363 174 570 122 206 227 337 584 730 312 343 444 510 519 657 14 29 28 53 31 53 2 0 20 -30 40 -68 81 -153 190 -288 543 -675 133 -144 271 -298 307 -342 208 -250 347 -492 400 -699 24 -96 53 -373 34 -331 -6 14 -7 4 -4 -30 4 -38 3 -46 -5 -35 -9 12 -10 7 -5 -19 5 -22 4 -31 -3 -27 -6 3 -8 -1 -6 -12 3 -9 -13 -53 -35 -97 -122 -242 -338 -377 -600 -377 -117 1 -172 13 -276 63 -201 96 -344 292 -377 519 -3 18 -8 7 -17 -35 -56 -257 -229 -453 -465 -527 -60 -19 -220 -31 -281 -22 z"}},
										{tag: "text", obj: {fill: 'red', x: "3250", y: "1700", fontSize: "2240", dominantBaseline: "middle", textAnchor: "middle"}})
	svg.setAttributeNS(null,"viewBox","-605 -15 5460 2730");
	svg.childNodes[1].textContent=t;
	return svg;
}

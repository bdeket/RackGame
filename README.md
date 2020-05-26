#Racket Gameserver
made in the spirit of the 2htdp Universe library

## General outline

The main loop of this server is embedded in the server/connection-server.rkt.
It should be started with a start function and optional on-connection, on-disconnect, on-message and on-tick functions. Also the port and tick-rate can be supplied.

There is a server/lobby-server/lobby-server.rkt that implements a lobby where new instances of a game can be created, keeps track of the state of these games and delegetes all the messages of the connection server to the right game.

The server/game-server.rkt bundles the predefined games and passes it to the lobby-server.

In the games directory there is one game supplied, (a variant of Whist.) Unfortunately one with more obtuse spagetti code.

The HTML folder is where the Front end for the games is defined. In scripts and styles are the files for the main lobby server, the javascript files are found below the games folder.

To make everything available start a html/fileserver in the html folder (for example raco pollen start . 80)
and run the game-server (racket game-server.rkt)


## The connection server

This server will listen on 127.0.0.1:PORT/game for incoming websocket connections. The incomming connection wil have to provide a unique name. Once a unique name is obtained the connection (name) is passed to the on-connection function and this name will be used to handle messages etc.

There is no password login, and more generally any kind of state is lost when the server is stopped.

Messages between the server and the clients are encoded in json objects with a mandatory tag: field.
Two tag's are caught by the connection server:
+ "keepalive" that will be ignored
+ "chat" that has an extra message: field wich will be broadcasted to all other clients.

Different from the the htdp-universe the start function takes one argument, a function that can be called to produce a worker. This can be used to create for example a computer player. From the perspective of the game this worker can be treated as any other client.

Logging of all messages is done at ConServ logger. Add  -W "debug@ConServ" to see all messages generated by the connection server (this includes all messages sent from server to client and visa versa).

## The lobby server

The lobby-server is started with a list of Gameservers (structure defined in server/util.rkt). Based on these gameservers it will try to find an optimal tickrate that it will supply to the connection server.

Logged in players (handled by the connection server) will appear in the room Lobby and can create new (game) rooms. Based on the list of supplied games. Multiple rooms of the same game can be started. Once the conditions for starting a game are met (enough players in the room, all players have indicated to be "ready") the game is started, and from here it is refered to as a "bout".

When a bout is finished, all players are returned to the lobby.

## A Gameserver

A gameserver is made with:
```
#:forall (A)
(new-gameserver [name : String]
                [empty : A]
                [start : (Start-Fct A)]
                [world : Path-String]
                #:min-players [min : Positive-Integer 1]
                #:max-players [max : Positive-Integer 100]
                #:on-disconnect [disconnect : (Disconnect-Fct A) (λ ([G : A]Pid CW) (make-bundle G))]
                #:on-message [message : (Message-Fct A) (λ ([G : A]Pid M CW) (make-bundle G))]
                #:on-tick [tick : (Tick-Fct A) (λ ([G : A]CW) (make-bundle G))]
                #:tick-rate [tick-rate : Positive-Real +inf.0])
```
+ Name is the name that will be presented in the lobby server.
+ Empty is an instance of the starting world.
+ start is the function that will be called once all the clients are ready to receive messages. The start function gets as arguments the empty world, a list of clients and a worker-function for should it want to create more worker-clients. It needs to return a bundle.
+ World is the path to the javascript file that handles the client side presentation/logic of the game.
+ min/max players, the limits for clients to start the game.
+ on-disconnect, on-message, on-tick, similar to the universe functions, but all expecting an extra argument, namely a function to create more workers. Also different from the Universe is that returning a bundle is mandatory.

## Util

For the server side, the server should include server/util.rkt or server/util-untyped.rkt wich provides the above mentionod new-gameserver. It also provides bundle / make-bundle and variants to produce the return values of the gameserver functions and a MSG macro and :msg match expander to easily create and analyze the json messages.

### Bundle
```
#:forall (A)
(make-bundle [state : A]
             [mails : (Listof Mail)]
             [drops : (Listof ID)])
```

Equivalent to the universe, except that mails and drops are optional, and the ID is the name-string provided to the start function. (or by the create-worker function).

A few convenience functions/macro's are defined:
+ (defbundle (state) aBundle) (defbundle (state mails) aBundle)(defbundle (state mails drops) aBundle) will bind the mentioned parts of a bundle.
+ (bundle-update) will update a bundle based on the following key-word arguments (applied in the following order:)
    1. \#:s-state state : set the new bundle state
    2. \#:u-state (-> state state>) : update the bundle state
    3. \#:a-mails mails : append a list of mails to the end of the existing mails
    4. \#:m-mails (-> state mails) : create a new list of mails based on the (updated) state, that will be appended to the end of the existing mails
    5. \#:u-mails (-> state mails mails> : create a new list of mails based on the (updated) state and the original mails.
    6. \#a-drops drops : similar as above, but for dropping IDs
    7. \#m-drops (-> state drops) : similar as above, but for dropping IDs
    8. \#u-drops (-> state drops drops) : similar as above, but for dropping IDs
+ (bundle-combine b1 b2 ...), keep the state of the last bundle, append all the mails and drops in order

### Mail
```
(make-mail [id : ID]
           [msg : Msg])
```

prepare a message for client ID. ID's are name-strings as provided to the start function (or by the create-worker function).

### Post
```
#:forall (A)
(make-post [S : A]
           [M : (Option Msg) #f])
```
Similar to the universe function. Mandatory for each return value for a (worker)client function.

### MSG
(MSG tag \[key value\]...) is a macro to create Msg's. Value's can have nested (MSG \[key value\]...) (not the missing tag)

(msg\@ Msg key) extracts a value
(tag Msg) extracts the tag.
matching can be done with
(match-msg aMsg \[(msg: ...) ...\]...)
regular match can be used in the untyped environment.

## Javascript
the javascript (client) side is handled with modules. The module for the game shuold provide two functions
+ register: a function that receives one argument (screen) the div that can be used to draw the game
+ unregister: a function that will be called when exiting the game.

the module should import the connection module that exports
+ the connection : used to talk to the server
+ a handler : used to register for certain messages from the server
+ newEl : a convenience function to create new DOM elements

In the register function the following is typical to be called:
```
connection.registerHandlers("gamename",
    new handler(e=>e.tag=="play", e=>somefunction(e.cards),
    ...);
    connection.send({tag: "gameReadyToStart"}};
```
the last line is (unfortunately) mandatory so the server knows all client's are ready to start receiving information.

in the unregister function, unregistering for all the defined handlers can be done with
```
connection.removeHandlers("gamename");
```

the tag leaveGame can be used to signal the lobby-server that the client wants to leave the game
```
const leave=newEl("button",{txt: "leave"})
leave.addEventListener('click',e=>connection.send({tag: "leaveGame"}),false);
```

## TODO

+ Provide gameserver with options that can be chosen at game creation in the lobby.
+ Provide tools to write the client side in racket (Urlang? RacketScript?)
+ Catch the connection-lost in the message server
+ Improve (write real) documentation
+ Implement more games.
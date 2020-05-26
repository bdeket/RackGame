#! /bin/zsh
cp -r ./html/* /amarilis/web/RackGame/
pushd server
racket -W "debug@ConServ debug@LobServ" "games-server.rkt" 2>&1 | tee log

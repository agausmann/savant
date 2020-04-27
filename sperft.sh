#!/bin/sh

if [ $# -lt 2 ]
then
    echo "Usage: $0 position depth"
    exit 1
fi

fen=$1
depth=$2

stockfish <<EOF
position fen $fen
go perft $depth
EOF


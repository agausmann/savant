#!/bin/sh

cargo run -q --release --example perft -- "$@" \
    | sed '$s/.*: //;s/://'

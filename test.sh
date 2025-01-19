#!/usr/bin/env bash
cargo run && objdump -D -d target/glsc/out.o && ./harness.sh
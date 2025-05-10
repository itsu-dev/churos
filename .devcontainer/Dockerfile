FROM ubuntu:24.04

RUN apt-get update && \
    apt-get install -y \
    build-essential \
    qemu-system-x86 \
    netcat-openbsd \ 
    curl \
    git

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

RUN . $HOME/.cargo/env
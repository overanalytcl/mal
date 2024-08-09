FROM ubuntu:24.04
MAINTAINER Stefan-Iulian Alecu <uneven-shiver@protonmail.com>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get update && \
    apt-get install -y \
        make \
        python3 \
        python3-pip && \
    ln -fs /usr/bin/python3 /usr/local/bin/python

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

# Free Pascal
RUN apt-get install -y \
        libc-dev \
        fp-compiler \
        fpc-source \
        fp-units-base \
        fp-units-rtl \
        fp-units-fcl \
        fp-units-misc \
        fp-utils \
        libedit-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
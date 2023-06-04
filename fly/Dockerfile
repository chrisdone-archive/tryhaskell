FROM ubuntu:20.04

RUN apt-get update -y && apt-get install -y libtinfo-dev

COPY fly/tryhaskell /bin/tryhaskell

CMD ["/bin/tryhaskell"]

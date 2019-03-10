FROM haskell:latest

COPY . .

RUN stack init

RUN apt update && apt install -y llvm

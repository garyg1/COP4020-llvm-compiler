FROM ubuntu:16.04

WORKDIR workspace
RUN apt-get update && \
    apt-get install -y curl wget lsb-release software-properties-common apt-transport-https && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 9

apt-get update

# install stack
apt-get install -y curl wget lsb-release software-properties-common apt-transport-https
curl -sSL https://get.haskellstack.org/ | sh

# install llvm 9
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
./llvm.sh 9

alias llvm-config="/usr/bin/llvm-config-9"
# build project
stack build

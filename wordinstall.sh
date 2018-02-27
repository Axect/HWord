export DIRECTORY=$HOME/.local/lib

if [ ! -d "$DIRECTORY" ]; then
  mkdir -p ~/.local/lib
fi

cp -r ./WordLIST/* ~/.local/lib/

stack build
stack install

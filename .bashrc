if [[ $- == *i* ]]; then
    export SHELL=zsh
    exec zsh -l
fi

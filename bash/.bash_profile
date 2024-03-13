

# added by Anaconda3 5.2.0 installer
# export PATH="/anaconda3/bin:$PATH"  # commented out by conda initialize

# added by Miniconda3 4.7.12 installer
# >>> conda init >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$(CONDA_REPORT_ERRORS=false '/Users/olgabot/opt/miniconda3/bin/conda' shell.bash hook 2> /dev/null)"
if [ $? -eq 0 ]; then
    \eval "$__conda_setup"
else
    if [ -f "/Users/olgabot/opt/miniconda3/etc/profile.d/conda.sh" ]; then
# . "/Users/olgabot/opt/miniconda3/etc/profile.d/conda.sh"  # commented out by conda initialize
        CONDA_CHANGEPS1=false conda activate base
    else
        \export PATH="/Users/olgabot/opt/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda init <<<
. "$HOME/.cargo/env"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/olgabot/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/olgabot/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/olgabot/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/olgabot/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


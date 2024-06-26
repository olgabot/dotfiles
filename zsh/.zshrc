# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="agnosterzak"

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"
#
# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"
#
# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"
#
# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13
#
# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"
#
# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git macos python aws tmux common-aliases zsh-autosuggestions brew)

source $ZSH/oh-my-zsh.sh
source ~/.oh-my-zsh/plugins/fast-syntax-highlighting/F-Sy-H.plugin.zsh


# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='emacs -nw --no-init-file'
else
  export EDITOR='emacs -nw --no-init-file'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="$HOME/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias sshtscc="ssh obotvinnik@tscc-login2.sdsc.edu"

# added by Anaconda3 4.3.1 installer
#export PATH="/Users/olgabot/anaconda3/bin:$PATH"

# >:)
alias vim=emacs
alias vi=emacs


# Alias to Triton Supercomputing Cluster (TSCC)
alias tscc="ssh obotvinnik@tscc-login2.sdsc.edu"

# Alias to CZ biohub logins
alias sshdobby="ssh dobby@ds05.czbiohub.org"
alias sshutility="ssh utility@ds06.czbiohub.org"
alias sshfry="ssh botvinnik@fry.czbiohub.org"
alias fry=sshfry


# CZ Biohub vpn

alias czbvpn="sudo openconnect --user olga.botvinnik https://64.71.0.146 --servercert sha256:0b3c46f6bee2673ee9be9b4227f632fa87b52333949003f4f2d155a70241eacd --authgroup HubVPN"
alias biohubvpn=czbvpn

IPYNB_PORT=7788
alias tunnelfry="ssh -NL $IPYNB_PORT\:localhost:$IPYNB_PORT botvinnik@fry.czbiohub.org  &"

alias mux=tmuxinator


# added by Anaconda3 4.3.1 installer
# export PATH="/Users/olgabot/anaconda3/bin:$PATH"

# Don't show user@hostname
# From https://stackoverflow.com/questions/28491458/zsh-agnoster-theme-showing-machine-name
# redefine prompt_context for hiding user@hostname
prompt_context () { }


# Non-ugly colors in terminal emacs
export TERM=xterm-256color
#export PATH=/Users/olgabot/anaconda/bin:$PATH
#. $HOME/anaconda/etc/profile.d/conda.sh
#conda activate base

# added by Anaconda3 5.2.0 installer
# export PATH="$HOME/anaconda/bin:$PATH"




# Add Go packages to Path
export PATH="$PATH:$HOME/go/bin"

# Tell Reflow to use ~/.aws folder for credentials
export AWS_SDK_LOAD_CONFIG=1

alias ls="lsd --config-file ~/.config/lsd/config.yml"
alias ll="ls -lha --git --header"


# # The new way of starting anaconda
# . /anaconda3/etc/profile.d/conda.sh
# conda activate


alias globalgitignore='cat ~/.gitignore >> .gitignore && git add .gitignore && git commit -m "Add global gitignore from olga"'
export PATH="/usr/local/opt/gettext/bin:$PATH"
export PATH="/usr/local/opt/openssl/bin:$PATH"
export PATH="/usr/local/opt/ruby/bin:$PATH"


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/olga/opt/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/olga/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/olga/opt/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/olga/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

. "$HOME/.cargo/env"


all_ubuntu: setup_ubuntu exa_ubuntu zsh-extras hc-zenburn-emacs copy get_anaconda_ubuntu anaconda_install
all_mac: setup_mac get_zsh zsh-extras hc-zenburn-emacs anaconda_mac set_zsh copy_mac

all_amazon_linux: miniconda_linux setup_amazon_linux hc-zenburn-emacs zsh-extras get_zsh set_zsh_amazon_linux rust lsd copy_amazon_linux 

setup_ubuntu:
	sudo apt update
	sudo apt install --yes zsh emacs tree git-core unzip htop
	sudo usermod -s /bin/zsh ubuntu
	sudo chsh -s `which zsh`

setup_mac:
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
	brew install wget exa htop
	wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh


setup_amazon_linux:
	# install cc, gcc
	sudo yum groupinstall "Development Tools" -y

miniconda_linux:
	mkdir -p ~/miniconda3
	wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda3/miniconda.sh
	bash ~/miniconda3/miniconda.sh -b -u -p ~/miniconda3
	
get_anaconda_ubuntu:
	echo source ~/.bashrc >> ~/.bash_profile
	curl -L https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/anaconda.sh
	
get_anaconda_mac:
	https://repo.anaconda.com/archive/Anaconda3-2019.07-MacOSX-x86_64.sh -O ~/anaconda.sh
	
anaconda_install:
	# -b installs in "silent mode"
	bash ~/anaconda.sh -b -p ${HOME}/anaconda
	export PATH="${HOME}/anaconda/bin:${PATH}"

anaconda_mac: get_anaconda_mac anaconda_install


exa_ubuntu:
	wget https://github.com/ogham/exa/releases/download/v0.8.0/exa-linux-x86_64-0.8.0.zip
	unzip exa-linux-x86_64-0.8.0.zip
	sudo mv exa-linux-x86_64 /usr/local/bin/exa
	sudo chmod ugo+x /usr/local/bin/exa


rust:
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
	source "$$HOME/.cargo/env"

lsd:
	# ls deluxe, rewrite of GNU ls
	cargo install lsd


get_zsh:
	wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh

set_zsh_amazon_linux:
	sudo yum install util-linux-user -y
	sudo chsh -s $(which zsh) $(whoami)



set_zsh:
	sudo chsh -s /usr/bin/zsh $(whoami)


zsh-extras:
	git clone https://github.com/zakaziko99/agnosterzak-ohmyzsh-theme 
	git clone https://github.com/zdharma/fast-syntax-highlighting
	git clone https://github.com/zsh-users/zsh-autosuggestions
	cp -v ~/agnosterzak-ohmyzsh-theme/agnosterzak.zsh-theme ~/.oh-my-zsh/themes
	cp -r ~/fast-syntax-highlighting/F-Sy-H.plugin.zsh ~/.oh-my-zsh/plugins
	cp -r ~/zsh-autosuggestions ~/.oh-my-zsh/plugins

hc-zenburn-emacs:
	git clone https://github.com/edran/hc-zenburn-emacs
	cp -v ~/hc-zenburn-emacs/hc-zenburn-theme.el ~/.emacs.d/themes

gh-cli: 
	# install github cli
	conda install --yes -c conda-forge gh

copy_shared:
	cp -v ~/dotfiles/.screenrc ~
	cp -v ~/dotfiles/.gitconfig ~
	cp -v ~/dotfiles/.gitignore ~
	cp -v ~/dotfiles/tmux/.tmux.conf ~
	cp -vr ~/dotfiles/emacs/.emacs ~/
	cp -vr ~/dotfiles/emacs/.emacs.d ~/

copy_lsd:
	# ensure the config dir is there if it's not already
	mkdir ~/.config
	cp -vr ~/dotfiles/lsd/.config/lsd ~/.config/

copy_mac: copy_shared
	cp -v ~/dotfiles/zsh/.zshrc ~

copy_amazon_linux: copy_shared
	cp -v ~/dotfiles/zsh/.zshrc_amazon_linux ~/.zshrc
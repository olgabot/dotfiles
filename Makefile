
all_ubuntu: setup_ubuntu exa_ubuntu zsh-extras hc-zenburn-emacs copy get_anaconda_ubuntu anaconda_install
all_mac: setup_mac zsh-extras hc-zenburn-emacs copy get_anaconda_mac anaconda_install

setup_ubuntu:
	sudo apt update
	sudo apt install --yes zsh emacs tree git-core unzip htop
	sudo usermod -s /bin/zsh ubuntu
	sudo chsh -s `which zsh`

setup_mac:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	brew install wget exa htop
	wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh
	
get_anaconda_ubuntu:
	wget https://repo.anaconda.com/archive/Anaconda3-2019.07-Linux-x86_64.sh -O ~/anaconda.sh
	
get_anaconda_mac:
	https://repo.anaconda.com/archive/Anaconda3-2019.07-MacOSX-x86_64.sh -O ~/anaconda.sh
	
anaconda_install:
	# -b installs in "silent mode"
	bash ~/anaconda.sh -b -p ${HOME}/anaconda
	export PATH="${HOME}/anaconda/bin:$PATH"

exa_ubuntu:
	wget https://github.com/ogham/exa/releases/download/v0.8.0/exa-linux-x86_64-0.8.0.zip
	unzip exa-linux-x86_64-0.8.0.zip
	sudo mv exa-linux-x86_64 /usr/local/bin/exa
	sudo chmod ugo+x /usr/local/bin/exa


zsh-extras:
	git clone https://github.com/zakaziko99/agnosterzak-ohmyzsh-theme 
	git clone https://github.com/zdharma/fast-syntax-highlighting
	git clone https://github.com/zsh-users/zsh-autosuggestions


hc-zenburn-emacs:
	git clone https://github.com/edran/hc-zenburn-emacs



copy:
	cp -v ~/rcfiles/.screenrc ~
	cp -v ~/rcfiles/.zshrc ~
	cp -v ~/rcfiles/.gitconfig ~
	cp -v ~/rcfiles/.gitignore ~
	cp -v ~/rcfiles/.tmux.conf ~
	cp -vr ~/rcfiles/.emacs.d ~/
	cp -v ~/agnosterzak-ohmyzsh-theme/agnosterzak.zsh-theme ~/.oh-my-zsh/themes
	cp -v ~/hc-zenburn-emacs/hc-zenburn-theme.el ~/.emacs.d/themes
	cp -r ~/fast-syntax-highlighting ~/.oh-my-zsh/plugins
	cp -r ~/zsh-autosuggestions ~/.oh-my-zsh/plugins

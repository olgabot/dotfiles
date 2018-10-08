all: install exa agnosterzak-ohmyzsh-theme hc-zenburn-emacs copy

install:
	sudo apt update
	sudo apt install --yes zsh emacs tree git-core unzip
	wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh
	sudo chsh -s `which zsh`
	sudo usermod -s /bin/zsh ubuntu
	
	
exa:
	wget https://github.com/ogham/exa/releases/download/v0.8.0/exa-linux-x86_64-0.8.0.zip
	unzip exa-linux-x86_64-0.8.0.zip
	sudo mv exa-linux-x86_64 /usr/local/bin/exa
	sudo chmod ugo+x /usr/local/bin/exa

copy:
	cp -v ~/rcfiles/.screenrc ~
	cp -v ~/rcfiles/.zshrc ~
	cp -v ~/rcfiles/.gitconfig ~
	cp -v ~/rcfiles/.gitignore ~
	cp -vr ~/rcfiles/.emacs.d ~/
	cp -v ~/agnosterzak-ohmyzsh-theme/agnosterzak.zsh-theme ~/.oh-my-zsh/themes
	cp -v ~/hc-zenburn-emacs/hc-zenburn-theme.el ~/.emacs.d/themes

agnosterzak-ohmyzsh-theme:
	git clone https://github.com/zakaziko99/agnosterzak-ohmyzsh-theme 


hc-zenburn-emacs:
	git clone https://github.com/edran/hc-zenburn-emacs

all: install agnosterzak-ohmyzsh-theme hc-zenburn-emacs copy

install:
	sudo apt install --yes zsh emacs tree git-core
	wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh
	sudo chsh -s `which zsh`

copy:
	cp -v ~/rcfiles/.* ~/
	cp -vr ~/rcfiles/.emacs.d ~/
	cp -v ~/agnosterzak-ohmyzsh-theme/agnosterzak.zsh-theme ~/.oh-my-zsh/themes
	cp -v ~/hc-zenburn-emacs/hc-zenburn-theme.el ~/.emacs.d/themes

agnosterzak-ohmyzsh-theme:
	git clone https://github.com/zakaziko99/agnosterzak-ohmyzsh-theme 


hc-zenburn-emacs:
	git clone https://github.com/edran/hc-zenburn-emacs ~/code/hc-zenburn-emacs

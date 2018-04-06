sudo apt install --yes zsh emacs tree
sudo sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

mkdir -p ~/code
cd ~/code
git clone https://github.com/zakaziko99/agnosterzak-ohmyzsh-theme
git clone https://github.com/edran/hc-zenburn-emacs

sudo cp -v ~/code/rcfiles/.* ~/
sudo cp -vr rcfiles/.emacs.d ~/
cp -v ~/code/agnosterzak-ohmyzsh-theme/agnosterzak.zsh-theme ~/.oh-my-zsh/themes
cp -v ~/code/hc-zenburn-emacs/hc-zenburn-theme.el ~/.emacs.d/themes

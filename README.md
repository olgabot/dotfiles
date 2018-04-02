rcfiles
=======

Collection of rc files for easy portability


## Usage

1-liner to change to home directory, clone, and copy rcfiles

```
cd ~/ mkdir code && cd code && git clone https://github.com/olgabot/rcfiles && cp rcfiles/.* ~/
```

### Ubuntu
Also need to install ZSH, oh-my-zsh and [AgnosterZak theme](https://github.com/zakaziko99/agnosterzak-ohmyzsh-theme)

```
sudo apt install --yes zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
cd ~/code
git clone https://github.com/zakaziko99/agnosterzak-ohmyzsh-theme
cp ~/code/agnosterzak-ohmyzsh-theme/agnosterzak.zsh-theme ~/.oh-my-zsh/themes
```

rcfiles
=======

Collection of rc files for easy portability


## Ubuntu - step 1, install ZSH
Also need to install ZSH, oh-my-zsh and [AgnosterZak theme](https://github.com/zakaziko99/agnosterzak-ohmyzsh-theme)

```
sudo apt install --yes zsh && sudo sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)" && cd ~/code && git clone https://github.com/zakaziko99/agnosterzak-ohmyzsh-theme
```


## Usage - Step 2, after installing ZSH and theme

1-liner to change to home directory, clone, and copy rcfiles

```
cd ~/ && mkdir code && cd code && git clone https://github.com/olgabot/rcfiles && sudo cp -v ~/code/rcfiles/.* ~/ && cp -vr rcfiles/.emacs.d ~/ && cp -v ~/code/agnosterzak-ohmyzsh-theme/agnosterzak.zsh-theme ~/.oh-my-zsh/themes
```


```
cd ~/ mkdir code && cd code && git clone https://github.com/olgabot/rcfiles && bash rcfiles/init.sh
```

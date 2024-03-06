dotfiles
=======

Collection of run command (rc) / "dot" files (files that start with a `.`) for easy portability

## Usage (Amazon Machine Linux)

```
cd && sudo yum install git tmux emacs htop unzip zsh -y && git clone https://github.com/olgabot/dotfiles ~/dotfiles && cd dotfiles && make all_amazon_linux
```

## Usage (Ubuntu)

1-liner to change to home directory, clone, and run init script. Mostly used for AWS EC2 images.


```
cd && git clone https://github.com/olgabot/dotfiles ~/dotfiles && cp ~/dotfiles/Makefile ~ && cd && make all_ubuntu
```


## Usage (Mac)

1-liner to change to home directory, clone, and run init script. Mostly used for setting up new computers


```
cd && git clone https://github.com/olgabot/dotfiles ~/dotfiles && cp ~/dotfiles/Makefile ~ && cd && make all_mac
```

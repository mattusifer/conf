# from mu documentation
mu:
	autoreconf -im emacs.d/vendor/mu/

emacs:	mu
	rm -r ~/.emacs.d
	ln -s $(shell pwd)/emacs.d/ ~/.emacs.d

sbt:
	rm -r ~/.sbt
	ln -si $(shell pwd)/sbt ~/.sbt

zsh:
	ln -si $(shell pwd)/zshrc ~/.zshrc

tmux:
	ln -si $(shell pwd)/tmux.conf ~/.tmux.conf

mbsync:
	ln -si $(shell pwd)/mbsyncrc ~/.mbsyncrc

flake8:
	ln -si $(shell pwd)/config ~/.config

all: sbt zsh tmux mbsync emacs flake8

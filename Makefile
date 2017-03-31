mu:
	cd emacs.d/vendor/mu
	autoreconf -i && ./configure && make  # from mu documentation

emacs:	mu
	ln -s emacs.d ~/.emacs.d

sbt: 
	ln -s sbt ~/.sbt

zsh:
	ln -s zshrc ~/.zshrc

tmux:
	ln -s tmux.conf ~/.tmux.conf

mbsync:
	ln -s mbsyncrc ~/.mbsyncrc

all: sbt zsh tmux mbsync emacs

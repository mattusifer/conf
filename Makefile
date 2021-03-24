arch-deps:
	git submodule init
	git submodule update --remote
	sudo pacman -S isync 2> /dev/null || echo "Not on an arch system"

# from mu documentation
mu:
	rm -rf ~/gmail-cert.pem
	ln -s $(shell pwd)/emacs.d/gmail-cert.pem ~/gmail-cert.pem

	@if [ -d '~/.mbsyncmaildir' ]; then \
		mkdir -p ~/.mbsyncmaildir/personal-gmail; \
		mbsync -V personal-gmail; \
	fi

	command -v mu >/dev/null 2>&1 || { echo "I require foo but it's not installed.  Aborting." >&2; exit 1; }

	cd emacs.d/vendor/mu && autoreconf -i && ./configure && make && sudo make install; \
	mu index --maildir=~/.mbsyncmaildir; \

emacs:
	touch emacs.d/custom.el
	rm -rf emacs.d/elpa

	rm -rf ~/.emacs.d
	ln -s $(shell pwd)/emacs.d/ ~/.emacs.d

vim:
	ln -si $(shell pwd)/vimrc ~/.vimrc

psql:
	ln -si $(shell pwd)/psqlrc ~/.psqlrc


git:
	git config --global user.name "Matt Usifer"
	git config --global user.email "mattusifer@gmail.com"

scala:
	rm -rf ~/.sbt
	ln -si $(shell pwd)/sbt ~/.sbt

rust:
	curl https://sh.rustup.rs -sSf | sh
	rustup component add rust-src
	cargo install rustfmt racer || true

# flake8 config
python:
	rm -rf ~/.config
	ln -si $(shell pwd)/config ~/.config

zsh:
	chsh -s $(shell which zsh) || echo "Could not change the shell type to ZSH."

	mkdir -p ~/.zsh
	git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions

	rm -rf ~/.zshrc ~/.zshenv
	ln -si $(shell pwd)/zshrc ~/.zshrc
	ln -si $(shell pwd)/zshenv ~/.zshenv

tmux:
	rm -rf ~/.tmux.conf
	ln -si $(shell pwd)/tmux.conf ~/.tmux.conf

screen:
	rm -rf ~/.screenrc
	ln -si $(shell pwd)/screenrc ~/.screenrc

mbsync:
	rm -rf ~/.mbsyncrc
	ln -si $(shell pwd)/mbsyncrc ~/.mbsyncrc

build-lein:
	ln -s $(shell pwd)/lein ~/.lein

all: arch-deps scala rust python zsh tmux emacs vim psql

emacs:
	touch emacs.d/custom.el
	rm -rf emacs.d/elpa

	rm -rf ~/.emacs.d
	ln -sf $(shell pwd)/emacs.d/ ~/.emacs.d

vim:
	ln -sf $(shell pwd)/vimrc ~/.vimrc

psql:
	ln -sf $(shell pwd)/psqlrc ~/.psqlrc

git:
	git config --global user.name "Matt Usifer"
	git config --global user.email "mattusifer@gmail.com"

scala:
	rm -rf ~/.sbt
	ln -sf $(shell pwd)/sbt ~/.sbt

rust:
	curl https://sh.rustup.rs -sSf | sh
	rustup component add rust-src
	cargo install rustfmt racer || true

python:
	rm -rf ~/.config
	ln -sf $(shell pwd)/config ~/.config

zsh:
	chsh -s $(shell which zsh) || echo "Could not change the shell type to ZSH."

	mkdir -p ~/.zsh
	git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions || echo "Failed to install zsh-autosuggestions (it might already exist)"

	rm -rf ~/.zshrc ~/.zshenv
	ln -sf $(shell pwd)/zshrc ~/.zshrc
	ln -sf $(shell pwd)/zshenv ~/.zshenv

tmux:
	rm -rf ~/.tmux.conf
	ln -sf $(shell pwd)/tmux.conf ~/.tmux.conf

screen:
	rm -rf ~/.screenrc
	ln -sf $(shell pwd)/screenrc ~/.screenrc

all: git scala rust python zsh screen tmux emacs vim psql

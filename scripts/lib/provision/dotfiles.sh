# shellcheck shell=bash
# shellcheck disable=SC2154 # $dotfiles and the repo's URLs are set by provision
#
# provision's dotfiles steps: the SSH key for GitHub, the repo's git filters, the links into the
# home folder, and ~/.bashrc.

# An SSH key, and the checkout's remote switched to SSH once GitHub knows the key: a new machine
# clones over HTTPS, which can't push.
set_up_ssh() {
	mkdir -p ~/.ssh && chmod 700 ~/.ssh
	[ -f ~/.ssh/id_ed25519 ] ||
		ssh-keygen -q -t ed25519 -C "$(git config --global user.email)" -f ~/.ssh/id_ed25519 -N ""
	ssh-keygen -F github.com >/dev/null 2>&1 || ssh-keyscan github.com >>~/.ssh/known_hosts 2>/dev/null

	if [ "$(git -C "$dotfiles" remote get-url origin 2>/dev/null)" = "$repo_https" ]; then
		until ssh -T git@github.com </dev/null 2>&1 | grep -q 'successfully authenticated'; do
			echo -e "\nAdd this public key to https://github.com/settings/keys:\n"
			cat ~/.ssh/id_ed25519.pub
			read -p $'\nPress ENTER once it is added... ' -r
		done
		git -C "$dotfiles" remote set-url origin "$repo_ssh"
	fi
	git -C "$dotfiles" submodule update --init --recursive
}

# Git filters (see .gitattributes) that keep out of the repo what programs write into their
# tracked config files about this machine. Each is a sed script that git runs on a file's content
# when it is staged.
set_up_git_filters() {
	local filter
	local -A filters=(
		# In a scene collection: OBS's version, written into every source on save, and the
		# token that lets it capture the screen again without asking.
		[obs-scene]='s/^( *"prev_ver": )[0-9]+,$/\1536936449,/; s/^( *"RestoreToken": )"[^"]*"/\1""/'
		# The home folder in the recording paths, and the id of the profile's browser cookies.
		[obs-profile]='s|=/home/[^/]+|=/home/user|; /^CookieId=/d'
		# Device serial numbers. Not the dummy one that every ASUS Aura board reports, which
		# OpenRGB needs to match the board to its saved zone sizes.
		[openrgb-serial]='/"9876543210"/! s/^( *"serial": )"[^"]*"/\1""/'
	)
	for filter in "${!filters[@]}"; do
		git -C "$dotfiles" config "filter.$filter.clean" "sed -E '${filters[$filter]}'"
	done
	# The scene filter's earlier name.
	git -C "$dotfiles" config --unset filter.obs-prev-ver.clean
}

link_dotfiles() {
	mkdir -p ~/.config/kitty ~/.config/tmux-powerline/themes ~/.local/share/man/man1
	ln -sf "$dotfiles"/home/{.bash_aliases,.local.bashrc,.dircolors,.tmux.conf} ~
	ln -sf "$dotfiles"/home/{.wezterm.lua,.alacritty.toml} ~
	ln -sfn "$dotfiles"/bin ~/.bin
	ln -sf "$dotfiles"/config/kitty/{kitty.conf,default-kitty,resize_split.py} ~/.config/kitty
	ln -sf "$dotfiles"/config/tmux-powerline/config.sh ~/.config/tmux-powerline/
	ln -sf "$dotfiles"/config/tmux-powerline/themes/danylo-theme.sh ~/.config/tmux-powerline/themes/
	ln -sf "$dotfiles"/config/.blue-owl-custom.omp.json ~
	# `man provision`: man looks in ~/.local/share/man, next to ~/.local/bin on PATH. Without the
	# page under its earlier name.
	ln -sf "$dotfiles"/man/provision.1 ~/.local/share/man/man1/
	rm -f ~/.local/share/man/man1/start_fresh.1
	echo 'kitty.desktop' >~/.config/xdg-terminals.list

	[ -d ~/.tmux/plugins/tpm ] || git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
}

# Have ~/.bashrc load the dotfiles' shell setup.
set_up_bashrc() {
	if ! grep -qF '.local.bashrc' ~/.bashrc 2>/dev/null; then
		cat >>~/.bashrc <<'EOF'

# Binaries.
export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/.bin/git-custom-commands

# Enable fzf commands
eval "$(fzf --bash)"

# Aliases.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Custom bashrc setup.
if [ -f ~/.local.bashrc ]; then
    . ~/.local.bashrc
fi
EOF
	fi
	# Earlier versions also started Oh My Posh here, which .local.bashrc does.
	# shellcheck disable=SC2016 # the $(...) is text to match
	sed -i -e '/^# Oh-my-posh$/d' \
		-e '/^eval "\$(oh-my-posh init bash --config ~\/.blue-owl-custom.omp.json)"$/d' ~/.bashrc
}

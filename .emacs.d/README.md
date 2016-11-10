# My Emacs Configuration

I use Emacs for C/C++ and LaTeX. This configuration aims to provide functionality that I need to work with these languages, aiming to be clean, understandable and portable. Currently I use this configuration with Linux Mint 18 Mate 64-bit and Emacs 24.5.1.

## Setup
Just install Emacs and clone the repository into your `~/.emacs.d/` folder. On first load of Emacs, all necessary packages will be installed automatically (this may take a few minutes and will occur only once). Some extra things you need to do in Linux (also explained for the appropriate package in the `init.el` file):

 - For irony-mode (asynchronous support for code linting, autocompletion, etc.), you need to run:
 
   ```
   sudo apt-get install g++ clang libclang-dev
   ```
   You will need to run `M-x irony-install-server` after opening a C/C++ file for the very first time.
 - For Helm-gtags (GNU GLOBAL helm interface for code exploration), you need to run:
 
   ```
   cd ~/Downloads/
   wget -O gnu_global.tar.gz "http://tamacom.com/global/global-6.5.5.tar.gz" # or whatever the latest version is
   tar -zxvf gnu_global.tar.gz && rm gnu_global.tar.gz
   cd global-6.5.5/ # or whatever version you downloaded with wget
   sudo ./configure && sudo make && sudo make install
   rm -R ~/Downloads/global-6.5.5/ && cd ~/
   ```
 - For Flycheck in `shell-script-mode`, need to install `shellcheck`:

   ```
   sudo apt-get instal shellcheck
   ```

## Known issues

 1. [pdf-tools](https://github.com/politza/pdf-tools) package throws an error after the first run of Emacs when `use-package` auto-installs it:
    
    ```
    Error (use-package): pdf-tools :config: No executable `epdfinfo' found
    ```
    You just need to restart Emacs. The error will never appear again.
 2. If errors come up upon first run of Emacs when all packages are being auto-installed (e.g. sometimes I get an `Assertion failed: (or (= (buffer-size tar-data-buffer) (buffer-size)) ...)`), just restart Emacs. The error will go away and the installation will finish successfully.

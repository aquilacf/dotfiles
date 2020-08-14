"# First vi config 4 Aug 2020


let mapleader =","

"if ! filereadable(system('echo -n "~/.vim/autoload/plug.vim"'))
"	echo "Downloading junegunn/vim-plug to manage plugins..."
"	silent !mkdir -p ~/.vim/autoload/
"	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.vim/autoload/plug.vim
"	autocmd VimEnter * PlugInstall
"endif


"call plug#begin('~/.vim/plugged')
"	Plug 'patstockwell/vim-monokai-tasty'
"	Plug 'airblade/vim-gitgutter'
"	Plug 'editorconfig/editorconfig-vim'
"call plug#end()


" Theme
"let g:vim_monokai_tasty_italic = 0
"colorscheme vim-monokai-tasty



"# Basic
set number relativenumber
set updatetime=100
set mouse=a
set encoding=utf-8




if (has("termguicolors"))
	let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
	let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
	set termguicolors
endif





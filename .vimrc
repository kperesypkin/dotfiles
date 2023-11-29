" MAIN SECTION------------------------------------------------{{{
" Disable compatibility with vi
set nocompatible

" Enable mouse
set mouse=a

" Set file encoding
set encoding=utf-8

" Set terminal-256-colors
set t_Co=256

" Enable type file detection
filetype on"

" Set file format
set fileformat=unix

" Enable plugins and load plugin for the detected file type
filetype plugin on

" Load an indent file for the detected file type
filetype indent on

" Turn syntax highlighting on
syntax on

" Add line numbers
set number
set relativenumber

" Highlight cursor position
"set cursorline
"set cursorcolumn

" Set shift width to 4 spaces.
set shiftwidth=4

" Set tab width to 4 columns.
set tabstop=4
set softtabstop=4

" Use space characters instead of tabs.
set expandtab

" Auto indentation
set autoindent

" Do not save backup files.
set nobackup

" Do not let cursor scroll below or above N number of lines when scrolling.
set scrolloff=10

" Do not wrap lines. Allow long lines to extend as far as the line goes.
set nowrap

" While searching though a file incrementally highlight matching characters as you type.
set incsearch

" Ignore capital letters during search.
set ignorecase

" Override the ignorecase option if searching for capital letters.
" This will allow you to search specifically for capital letters.
set smartcase

" Show partial command you type in the last line of the screen.
set showcmd

" Show the mode you are on the last line.
set showmode

" Show matching words during a search.
set showmatch

" Use highlighting when doing a search.
set hlsearch

" Set the commands to save in history default number is 20.
set history=1000

" Enable auto completion menu after pressing TAB.

" Make wildmenu behave like similar to Bash completion.
set wildmode=list:longest

" There are certain files that we would never want to edit with Vim.
" Wildmenu will ignore files with these extensions.
set wildignore=*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx

" Run current script with python3 by CTRL+R in command and insert mode
set colorcolumn=79

" Folding code
set foldmethod=indent
set foldnestmax=2

" Enable italic font
set t_ZH=[3m
set t_ZR=[23m

" }}}

" PLUGINS ----------------------------------------------------------------{{{

" If Vim-plug does not exist download it from https://github.com/junegunn/vim-plug
" and put in '~/.vim/autoload'

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Autoinstall plugins into plugin folder
call plug#begin('~/.vim/plugged')

Plug 'preservim/nerdtree'
Plug 'dense-analysis/ale'
Plug 'tpope/vim-commentary'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'vim-airline/vim-airline'
Plug 'Yggdroot/indentLine'
Plug 'liuchengxu/vista.vim'

" Colorschemes
Plug 'morhetz/gruvbox'
Plug 'crusoexia/vim-monokai'

call plug#end()
" }}}

" PLUGINS CONFIGURATION --------------------------------------------------{{{
" ALE config
let g:ale_linters = {
            \ 'python': 'all',
            \ 'go': ['gopls']}
let g:ale_fixers = {'python':['isort', 'yapf', 'remove_trailing_lines', 'trim_whitespace']}
let g:ale_lsp_suggestions = 1
let g:ale_fix_on_save = 1
let g:ale_go_gofmt_options = '-s'
let g:ale_go_gometalinter_options = '— enable=gosimple — enable=staticcheck'
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] [%severity%] %code: %%s'
let g:ale_virtualenv_dir_names = ['env', 'venv', 'virtualenv']
let g:ale_completion_enabled = 1
let g:ale_completion_autoimport = 1
set completeopt=menu,menuone,preview,noselect,noinsert

" IndentLine config
let g:indentLine_enabled = 1
"let g:indentLine_char_list = ['|', '¦', '┆', '┊']
"let g:indentLine_setColors = 0

" }}}

" COLORS -----------------------------------------------------------------{{{
set background=dark
colorscheme gruvbox

" if (has('termguicolors'))
set termguicolors
" endif

" Use italic font for comments
highlight Comment cterm=italic
" }}}

" MAPPINGS ---------------------------------------------------------------{{{

" Mappings code goes here.
nnoremap <C-n> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

" Turn off search highlight
nnoremap /<space> :nohlsearch<CR>

" Type jk to exit insert mode quickly.
inoremap jk <Esc>

" Navigate the split view easier by pressing CTRL+j, CTRL+k, CTRL+h, or CTRL+l.
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
" }}}

" VIMSCRIPT --------------------------------------------------------------{{{

" This will enable code folding.
" Use the marker method of folding.
augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END


" Run current script with python3 by CTRL+R in command and insert mode
autocmd FileType python map <buffer> <C-r> :w<CR>:exec '!python3' shellescape(@%, 1)<CR>
autocmd FileType python imap <buffer> <C-r> <esc>:w<CR>:exec '!python3' shellescape(@%, 1)<CR>
" }}}

" STATUS LINE ------------------------------------------------------------{{{
" Airline configuration
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.colnr = ' ㏇:'
let g:airline_symbols.colnr = ' ℅:'
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '☰'
let g:airline_symbols.linenr = ' ␊:'
let g:airline_symbols.linenr = ' ␤:'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.maxlinenr = '㏑'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = 'Ɇ'
let g:airline_symbols.whitespace = 'Ξ'
" }}}

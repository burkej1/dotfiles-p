execute pathogen#infect()

" Enable syntax highlighting
syntax enable

" Hybrid relative line numbers
set number relativenumber

" No compatibility mode (enables some extra features)
set nocp
filetype plugin on
filetype plugin indent on

" Tab settings
" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab

" Set to auto read when a file is changed from the outside
set autoread

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = " "
let g:mapleader = " "

" Fast saving
nmap <leader>w :w!<cr>

"Always show current position
set ruler

" Height of the command bar
set cmdheight=1

" A buffer becomes hidden when it is abandoned
set hid

" Configure backspace so it acts as it should act
set backspace=eol,start,indent

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" " Highlight search results
" set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Yank to the system clipboard by default (works with MacVim)
set clipboard=unnamed

" Enable mouse
set mouse=a

" Disable text wrapping
set nowrap

" Keybindings for changing tabs
nnoremap <Leader>h :tabprevious<CR>
nnoremap <Leader>l :tabnext<CR>

" Disable arrow movement, use to resize splits instead
let g:elite_mode = 1
nnoremap <Up> :resize +2<CR> 
nnoremap <Down> :resize -2<CR>
nnoremap <Left> :vertical resize -2<CR>
nnoremap <Right> :vertical resize +2<CR>



" " " Plugin specific options (comment if not installed)

" " Colour schemes
colorscheme space-vim-dark
 
" " Vim airline
" Enabling vim airline tabline
let g:airline#extensions#tabline#enabled = 1
" Setting airline to not show buffers
let g:airline#extensions#tabline#show_buffers = 0

" " " Jedi-vim
" " Disable docstring popups on autocompletion with jedi-vim
" autocmd FileType python setlocal completeopt-=preview

" " Vim-sneak
" Make vim-sneak a bit more like easymotion
let g:sneak#label = 1

" " Dispatch
" Mapping make (with force recompile option)
" " Compiling c projects
" nnoremap <leader>c :Make -B<cr>
" Compiling and running rust projects
" nnoremap <leader>c :Dispatch cargo run<cr>

" Modify some of the keybindings for easymotion
nmap <Leader>j <Plug>(easymotion-j)
nmap <Leader>k <Plug>(easymotion-k)

" " NERDTree
" Keybinding for to open file tree
nnoremap <Leader>n :NERDTree<CR>

" " ALE
" Toggle linting
nmap <leader>at :ALEToggle<cr>

" Linter use dictionary
let g:ale_linters = {
\    'python': ['flake8', 'mypy'],
\}

" Changing ale linting colours
highlight ALEWarning ctermbg=23
highlight ALEError ctermbg=52

" Using the rust compiler for linting with ALE
" let g:ale_linters = {'rust': ['rustc']}

" Modifying indentation rules for yaml (and cwl) files
autocmd FileType yaml setlocal shiftwidth=2 tabstop=2
autocmd FileType cwl setlocal shiftwidth=2 tabstop=2

" Changing where splits appear to something more natural
set splitbelow
set splitright

" Changing commentary setting for cwl files
autocmd FileType cwl setlocal commentstring=#\ %s

" Fast make
nmap <leader>m :!make<cr>

" Vimrc
" DZ

set nocompatible
filetype off "required for neobundle
let $GIT_SSL_NO_VERIFY = 'true' " required for bad ssl certs
set runtimepath+=/usr/local/bin
let s:darwin = has('mac')

call plug#begin('~/.vim/installed')

" Core stuff
Plug 'vim-scripts/L9'
Plug 'qpkorr/vim-bufkill'
Plug 'Lokaltog/vim-easymotion'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'Raimondi/delimitMate'
Plug 'henrik/vim-indexed-search'
Plug 'vim-scripts/matchit.zip'
Plug 'sjl/gundo.vim'
Plug 'vim-scripts/YankRing.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'tpope/vim-eunuch'
Plug 'jaxbot/semantic-highlight.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" lol
Plug 'junegunn/vim-emoji'

" Journal
Plug 'junegunn/vim-journal'

" Javascript
Plug 'othree/yajs.vim'

" HTML/CSS
Plug 'kchmck/vim-coffee-script'
Plug 'groenewege/vim-less'
Plug 'tpope/vim-haml'
Plug 'tudorprodan/html_annoyance.vim'

" Ruby/Rails
Plug 'tpope/vim-rails'
Plug 'tpope/vim-endwise'

call plug#end()

filetype plugin indent on
filetype on

set background=dark
color ir_black
syntax on
set shm=atI "disable intro screen
set ttyfast " Improves redrawing for newer computers
set mouse=a
scriptencoding utf-8
set encoding=utf-8
set virtualedit=onemore
set history=1000
set nofoldenable " disable code folding
set display=lastline,uhex
set switchbuf=useopen

set backup
set backupdir=$HOME/.vimbackup//
set directory=$HOME/.vimswap//
" Create directories if they don't exist
silent execute '!mkdir -p $HOME/.vimbackup'
silent execute '!mkdir -p $HOME/.vimswap'

"typos sucks
command! -bang -nargs=* W w<bang> <args>
command! -bang -nargs=* Q q<bang> <args>
command! -bang -nargs=* Wq wq<bang> <args>
command! -bang -nargs=* WQ wq<bang> <args>

set showmode " display the current mode
set showcmd
set wildmode=longest,list,full
set wildmenu
set completeopt=menu

set wildignore+=*.o,*.fasl,CVS,*.pyc,._*,.DS_Store,*~,*.gif,*.jpg,*.png,*.pdf,*.psd,*.svn,.svn,.git,.hg

set ruler
set number
set nuw=4
set vb "set visual bell
set backspace=indent,eol,start
set laststatus=2
set showmatch

set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set bs=2

"change softwrap to sane setting
set wrap linebreak textwidth=0

"set mapleader
let mapleader = ","
let g:mapleader = ","

" allow buffers to be fully squashed
set winminheight=0
set hidden

nnoremap ' `
nnoremap ` '

" Remap control-g to esc
map <C-g> <esc>
noremap <C-g> <esc>

"scrolling offset
set scrolloff=5
set shortmess=atI

"turn off middleclicking
noremap <MiddleMouse> <LeftMouse>
noremap <2-MiddleMouse> <LeftMouse>
noremap <3-MiddleMouse> <LeftMouse>
noremap <4-MiddleMouse> <LeftMouse>

"Set to auto read when a file is changed from the outside
set autoread

" sudo write
map w! w !sudo tee % >/dev/null

" if i indent in visual mode, i want to remain in visual mode
vnoremap < <gv
vnoremap > >gv

"map control/command arrows for moving between windows
map <C-up> <C-W><up>
map <C-down> <C-W><down>
map <C-left> <C-W><left>
map <C-right> <C-W><right>
"these should ideally be in gvimrc; duplicated there
noremap <D-up> <C-W><up>
noremap <D-down> <C-W><down>
noremap <D-left> <C-W><left>
noremap <D-right> <C-W><right>

"map window manipulation to match emacs
noremap <D-1> :only<cr>
noremap <D-0> :close<cr>
noremap <D-2> :sp<cr>
noremap <D-3> :vsp<cr>
noremap! <D-1> <esc>:only<cr>
noremap! <D-0> <esc>:close<cr>
noremap! <D-2> <esc>:sp<cr>
noremap! <D-3> <esc>:vsp<cr>

command! SmartHomeKey call SmartHomeKey()
function! SmartHomeKey()
  let l:lnum	=	line('.')
  let l:ccol	=	col('.')
  execute 'normal! ^'
  let l:fcol	=	col('.')
  execute 'normal! 0'
  let l:hcol	=	col('.')
  if l:ccol != l:fcol
    call cursor(l:lnum, l:fcol)
  else
    call cursor(l:lnum, l:hcol)
  endif
endfun

" map CTRL-E to end-of-line (insert mode)
map <C-e> <esc>$<right><esc>
imap <C-e> <esc>$i<right>
" map CTRL-A to beginning-of-line (insert mode)
map <C-a> <esc>:SmartHomeKey<CR><esc>
imap <C-a> <esc>:SmartHomeKey<CR>i

ab teh the
ab fro for

set incsearch
set ignorecase
set wrapscan
set smartcase

"No scrollbars
set guioptions-=L
set guioptions-=l
set guioptions-=R
set guioptions-=r

"easily equalize windows with plus
nmap + <c-w>=

"tab key in normal mode autoindents
nmap <tab> ==
"if we rebind tab though, we need to also rebind <C-i> to something else
"so we can still use the jumplist
nnoremap <C-p> <C-i>

"fast file switching
map <C-q> <esc>:b#<esc>
nmap <C-q> <esc>:b#<cr>
noremap <C-q> <esc>:b#<cr>
noremap! <C-q> <esc>:b#<cr>

map <Leader>t :NERDTreeToggle<cr>
map <Leader>m :NERDTreeToggle<cr>
map <Leader><Leader> :NERDTreeToggle
" open nerdtree focused on the existing file
map <Leader>n :NERDTreeFind<cr>

" Quickly open/reload vim
nnoremap <leader>ev :e $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

"NERDTree options
let NERDTreeMinimalUI=1
let NERDTreeDirArrows=1
let NERDTreeWinSize=51
let NERDTreeQuitOnOpen=1

" Fugitive options
" if a fugitive buffer is hidden, just go ahead and delete it
autocmd BufReadPost fugitive://* set bufhidden=delete

"vim indent guide options
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_guide_size = 1
let g:indent_guides_start_level = 1

"yankring options
let g:yankring_enabled = 1
let g:yankring_max_history = 1000
let g:yankring_max_display = 100
let g:yankring_ignore_duplicate = 1

"Gundo options
nnoremap <leader>u :GundoToggle<CR>

"bufkill options
let g:BufKillActionWhenBufferDisplayedInAnotherWindow = "kill"
let g:BufKillActionWhenModifiedFileToBeKilled = "fail"
let g:BufKillCreateMappings = 0
noremap <D-k> :BD<CR>
noremap! <D-k> <esc>:BD<cr>

" strip whitespace
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

"ensure no tabs
autocmd BufWritePre * :set expandtab

"fzf options
let g:fzf_launcher = "in_a_new_term_function %s"
" This is the default extra key bindings
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

" Default fzf layout
let g:fzf_layout = { 'down': '~40%' }
let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'

autocmd VimEnter * command! Colors
  \ call fzf#vim#colors({'left': '15%', 'options': '--reverse --margin 30%,0'})

" LANGUAGE OPTIONS
"
"syntax color options for various languages

"syntax coloring for apache log files
autocmd BufRead,BufNewFile *access.log* set filetype=httplog
autocmd BufRead,BufNewFile *.tumblr.html setfiletype=tumblr

" these are all html really
autocmd BufRead,BufNewFile *.ss set filetype=html
autocmd BufRead,BufNewFile *.fbml set filetype=html
autocmd BufRead,BufNewFile *.lbi set filetype=html
autocmd BufRead,BufNewFile *.dtpl set filetype=html

"Google Go
autocmd BufRead,BufNewFile *.go set filetype=go

"LOLCODE
autocmd BufRead,BufNewFile *.lol set filetype=lolcodtpl

"I edit actionscript files, not atlas files, whatever the hell they are
autocmd BufRead,BufNewFile *.as set filetype=actionscript

"Json is still javascript
au BufNewFile,BufRead  *.json set filetype=javascript


"language specific options
"
"PHP
let php_sql_query=1
let php_baselib=1
let php_htmlInStrings=1
let PHP_BracesAtCodeLevel = 0
let PHP_removeCRwhenUnix = 1
let php_noShortTags = 1

"Python
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
let g:pyflakes_user_quickfix = 0
autocmd BufRead,BufNewFile *.py set autoindent
autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,with,try,except,finally,def,class

"Ruby
let g:tagbar_type_ruby = {
    \ 'kinds' : [
        \ 'm:modules',
        \ 'c:classes',
        \ 'd:describes',
        \ 'C:contexts',
        \ 'f:methods',
        \ 'F:singleton methods'
    \ ]
\ }

"HTML
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
let g:html_indent_script1 = "inc"
let g:html_indent_style1 = "inc"
"matchtagalways
let g:mta_filetypes = {
    \ 'html' : 1,
    \ 'xhtml' : 1,
    \ 'xml' : 1,
    \ 'jinja' : 1,
    \ 'eruby': 1
    \}

"CSS
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS

"Javascript
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
" Use Node.js for JavaScript interpretation
let javascript_enable_domhtmlcss=1
let g:html_indent_inctags = "html,body,head,tbody"
let g:html_indent_script1 = "inc"
let g:html_indent_style1 = "inc"
let g:tagbar_type_coffee = {
  \ 'kinds' : [
  \   'f:functions',
  \   'o:object'
  \ ],
  \ 'kind2scope' : {
  \  'f' : 'object',
  \   'o' : 'object'
  \},
  \ 'sro' : ".",
  \ 'ctagsbin' : 'coffeetags',
  \ 'ctagsargs' : '--include-vars '
  \}

" Markdown
let g:tagbar_type_markdown = {
  \ 'ctagstype' : 'markdown',
  \ 'kinds' : [
    \ 'h:Heading_L1',
    \ 'i:Heading_L2',
    \ 'k:Heading_L3'
  \ ]
\ }

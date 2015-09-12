" syntax on
" set statusline=%t[%{strlen(&fenc)?&fenc:'none'},%{&ff}]%h%m%r%y%=%c,%l/%L\ %P
"set expandtab
    syntax on

    set wildmenu

    set hidden                       " you can change buffers without saving

    set history=50                   " keep 50 lines of command line history

    set nowrap

    set ignorecase
    set smartcase
    set autoindent                   " always set autoindenting on
    set smartindent

    set laststatus=2
    set expandtab
    set smarttab
    set softtabstop=4
    set shiftwidth=4

    set langmenu=en
    set helplang=en

    set backup
    set backupdir=$HOME/backup/vim
    " following allows delete with backspace of indention
    set backspace=indent,eol,start
    set incsearch                    " do incremental searching
    " set gdefault                     " global substitution per default

    set t_Co=256
    " set background=dark
    " colorscheme lucius

    set hlsearch
    set ruler                        " show the cursor position all the time
    set showcmd                      " display incomplete commands
    set cursorline
    set relativenumber
    set novisualbell
    set scrolloff=10
    set sidescrolloff=10
    set showmatch
    set colorcolumn=80,120

    " Colors
    hi User1 term=inverse,bold cterm=bold ctermbg=darkgreen      ctermfg=white
    hi User2 term=inverse,bold cterm=bold ctermbg=darkmagenta     ctermfg=white
    hi User3 term=inverse,bold cterm=bold ctermbg=cyan     ctermfg=white
    hi User4 term=inverse,bold cterm=bold ctermbg=grey    ctermfg=white
    hi User5 term=inverse,bold cterm=bold ctermbg=red   ctermfg=white
    hi User6 term=inverse,bold cterm=bold ctermbg=darkyellow  ctermfg=white
    hi User7 term=inverse,bold cterm=bold ctermbg=blue ctermfg=white
    hi MatchParen cterm=underline ctermbg=blue ctermfg=white

    set statusline=
    set statusline +=%2*\ %{&ff}\ %*        "file format
    set statusline +=%6*\ %y\ %*            "file type
    set statusline +=%5*\ %<%F\ %*          "full path
    set statusline +=\ %m\                  "modified flag
    set statusline +=%5*%r%*                "read only flag
    set statusline +=%=                     "space
    " set statusline +=%7*%=\ pos:%12o\ %*    "current byte position
    set statusline +=%7*\ lines:%5l%*       "current line
    set statusline +=%7*/%L\ %*             "total lines
    set statusline +=%2*\ cols:%4c\ %*      "column number
    set statusline +=%1*%7P\ %*             "percentage through file
    set statusline+=%#warningmsg#
    " set statusline+=\ %{SyntasticStatuslineFlag()}\     " error counts
    set statusline+=%*

    set list
    set listchars=tab:▸\ ,trail:.,nbsp:%
    "set listchars=tab:▸\ ,eol:¬,trail:.,nbsp:%
    :let mapleader = ","

    filetype on
    au BufNewFile,BufRead *.sql_diff set filetype=sql

    call pathogen#infect()



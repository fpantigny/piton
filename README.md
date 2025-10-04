# Readme for the package piton

Author: F. Pantigny (`fpantigny@wanadoo.fr`).

CTAN page: `https://ctan.org/pkg/piton`

GitHub page: `github.com/fpantigny/piton`

## License
The LaTeX extension `piton` is distributed under the LPPL 1.3 license.

## Presentation

The package `piton` provides tools to typeset computer listings, with syntactic highlighting, by using the Lua library LPEG. It requires the use of `lualatex` and won't work with `xelatex` nor `pdflatex`. 

It requires the use of LuaLaTeX and won't work the other TeX engines (xelatex, pdflatex, etc.).

## Installation

The package `piton` is present in the distributions MiKTeX, TeXLive and MacTeX.

For a manual installation:

* put the files `piton.ins` and `piton-code.dtx` in the same directory; 
* run `latex piton.ins` in that directory.

The files `piton.sty` and `piton.lua` will be generated.

These files `piton.sty` and `piton.lua` are the only files necessary to use the extension `piton`. 
You have to put them in the same directory as your document or (best) in a `texmf` tree. 

For the documentation in English, run: `lualatex piton.tex`



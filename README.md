# Readme for the package piton

Author: F. Pantigny (`fpantigny@wanadoo.fr`).

CTAN page: `https://ctan.org/pkg/piton`

## License
The LaTeX extension `piton` is distributed under the LPPL 1.3 license.

## Presentation

The LaTeX package `piton` provides a command `\piton` and an environment `{Piton}` to typeset Python codes by using the Lua library LPEG. It requires the use of `lualatex`. It won't work with `xelatex` nor `pdflatex`. 



## Installation

The package `piton` is present in the distributions MiKTeX, TeXLive and MacTeX.

For a manual installation:

* put the files `piton.ins` and `piton.dtx` in the same directory; 
* run `latex piton.ins` in that directory.

The file `piton.sty` will be generated.

The file `piton.sty` is the only file necessary to use the extension `piton`. 
You have to put it in the same directory as your document or (best) in a `texmf` tree. 



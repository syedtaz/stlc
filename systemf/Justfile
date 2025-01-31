output_dir := "output"

clean:
	rm -f *.pdf *.out *aux *bbl *blg *log *toc *.ptb *.tod *.fls *.fdb_latexmk *.lof *.bcf *.xml *.xmpi

build filename:
  @lualatex -interaction nonstopmode {{filename}}.tex 2>&1 > /dev/null
  @biber {{filename}} 2>&1 > /dev/null
  @lualatex -interaction nonstopmode {{filename}}.tex 2>&1 > /dev/null
  @lualatex -interaction nonstopmode {{filename}}.tex 2>&1 > /dev/null
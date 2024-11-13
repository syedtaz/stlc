output_dir := "output"

clean:
	cd output && rm -f *.pdf *.out *aux *bbl *blg *log *toc *.ptb *.tod *.fls *.fdb_latexmk *.lof *.bcf *.xml *.xmpi

build filename:
  @mkdir -p {{output_dir}}
  @lualatex --output-directory {{output_dir}} -interaction nonstopmode {{filename}}.tex 2>&1 > /dev/null
  @biber {{filename}} 2>&1 > /dev/null
  @lualatex --output-directory {{output_dir}} -interaction nonstopmode {{filename}}.tex 2>&1 > /dev/null
  @mv ./{{output_dir}}/{{filename}}.pdf ./{{filename}}.pdf
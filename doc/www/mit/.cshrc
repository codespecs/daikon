# The "source pag-daikon.cshrc" command will fail until you check out your
# own copy of the "invariants" CVS repository.
if (! -f $HOME/research/invariants/scripts/pag-daikon.cshrc) then
  echo "See instructions for account setup at http://pag.csail.mit.edu/daikon/mit/"
endif
source $HOME/research/invariants/scripts/pag-daikon.cshrc

# Adjust if desired.  Printers are labeled with their names.
# setenv PRINTER carrot

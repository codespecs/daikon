# The "source pag-daikon.bashrc" command will fail until you check out your
# own copy of the "invariants" CVS repository.
if [ ! -f $HOME/research/invariants/scripts/pag-daikon.bashrc ]; then
  echo "See instructions for account setup at http://pag.csail.mit.edu/daikon/mit/"
fi

source $HOME/research/invariants/scripts/pag-daikon.bashrc

# Adjust if desired.  Printers are labeled with their names.
# export PRINTER=carrot

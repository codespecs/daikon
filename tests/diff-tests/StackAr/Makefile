include ../Makefile.common

test:
	${JAVA} daikon.PrintInvariants StackAr-1a.inv.gz  > out1a.txt
	${JAVA} daikon.PrintInvariants StackAr-1b.inv.gz  > out1b.txt
	${JAVA} daikon.PrintInvariants StackAr-1c.inv.gz  > out1c.txt
	${JAVA} daikon.PrintInvariants StackAr-2a.inv.gz  > out2a.txt
	${JAVA} daikon.PrintInvariants StackAr-2b.inv.gz  > out2b.txt
	${JAVA} daikon.PrintInvariants StackAr-2c.inv.gz  > out2c.txt

good:
	cp -f StackAr-1a.inv.gz StackAr-1.inv.gz
	cp -f StackAr-2a.inv.gz StackAr-2.inv.gz


bad:
	cp -f StackAr-1b.inv.gz StackAr-1.inv.gz
	cp -f StackAr-2b.inv.gz StackAr-2.inv.gz

worse:
	cp -f StackAr-1c.inv.gz StackAr-1.inv.gz
	cp -f StackAr-2c.inv.gz StackAr-2.inv.gz

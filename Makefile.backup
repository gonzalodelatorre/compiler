# Unix makefile for tigermain example

HOME=~
#MOSMLHOME=mosml
#MOSMLTOOLS=camlrunm /usr/share/mosml/tools
#MOSMLLEX=mosmllex
#MOSMLYACC=mosmlyac -v

MOSMLHOME=/home/cuca/mosml
MOSMLTOOLS=${MOSMLHOME}/bin/camlrunm $(MOSMLHOME)/tools
MOSMLLEX=${MOSMLHOME}/bin/mosmllex
MOSMLYACC=${MOSMLHOME}/bin/mosmlyac -v


GCC=gcc
CFLAGS= -g
#MOSMLC=mosmlc -c -liberal
#MOSMLL=mosmlc

MOSMLC=${MOSMLHOME}/bin/mosmlc -c -liberal
MOSMLL=${MOSMLHOME}/bin/mosmlc


# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigermain.uo \
	tigernlin.uo tigerpp.uo tigerescap.uo tigertab.uo topsort.uo tigerseman.uo tigertemp.uo tigertree.uo \
	tigerframe.uo tigertrans.uo tigerit.uo tigerpila.uo tigercanon.uo tigerinterp.uo tigercodegen.uo tigerassem.uo

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y 
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.ui \
    tigertemp.ui 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigerpila.uo: tigerpila.ui 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigerframe.uo: tigerframe.ui tigertree.uo tigertemp.ui 
tigersimpleregalloc.uo: tigersimpleregalloc.ui tigerassem.ui 
tigerassem.uo: tigerassem.ui tigertemp.ui 
tigergrm.ui: tigerabs.uo 
tigerpp.uo: tigerabs.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerframe.ui: tigertree.uo tigertemp.ui 
tigerescap.ui: tigerabs.uo 
tigerassem.ui: tigertemp.ui 
tigermain.uo: tigerseman.ui tigerescap.ui tigergrm.ui tigerlex.uo \
    tigerpp.uo 
tigerit.uo: tigertree.uo tigertab.ui 
tigertab.uo: tigertab.ui 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.ui 
tigertemp.uo: tigertemp.ui 
tigerseman.ui: tigerabs.uo 
tigerseman.uo: tigerseman.ui tigercodegen.ui tigersres.uo tigertab.ui \
    tigerpila.ui topsort.uo tigerframe.ui tigerit.uo tigercanon.ui \
    tigerassem.ui tigertemp.ui tigerabs.uo tigertrans.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigertree.uo: tigertemp.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigercanon.ui: tigertree.uo tigertemp.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigertemp.ui 
tigersimpleregalloc.ui: tigerframe.ui tigerassem.ui 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 

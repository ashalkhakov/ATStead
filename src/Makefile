######
#
# A simple Makefile
#
######

PATSCC=patscc
ATSCC2JS=atscc2js

######
#
all:: \
main_dats.js
#
main_dats.c: main.dats; $(PATSCC) -ccats $<
main_dats.js: main_dats.c; $(ATSCC2JS) -o $@ -i $<
#
######
# #
# all:: \
# label_dats.js
# #
# label_dats.c: label.dats; $(PATSCC) -ccats $<
# label_dats.js: label_dats.c; $(ATSCC2JS) -o $@ -i $<
# #
# ######
# #
# #
# all:: \
# tree_dats.js
# #
# tree_dats.c: tree.dats; $(PATSCC) -ccats $<
# tree_dats.js: tree_dats.c; $(ATSCC2JS) -o $@ -i $<
# #
# #
######

RMF=rm -f

######

clean:: ; $(RMF) *~
clean:: ; $(RMF) *_dats.c

######

cleanall:: clean
cleanall:: ; $(RMF) *_dats.js

###### end of [Makefile] ######

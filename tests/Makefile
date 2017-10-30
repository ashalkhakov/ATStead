######
#
# A simple Makefile
#
######

NODEJS=nodejs
PATSCC=patscc
ATSCC2JS=atscc2js

######
#
all:: \
test_tree_dats.js test_sdom_dats.js
#
test_tree_dats.c: test_tree.dats; $(PATSCC) -ccats $<
test_tree_dats.js: test_tree_dats.c; $(ATSCC2JS) -o $@ -i $<
#
regress:: test_test_tree
test_test_tree:: test_tree_dats.js ; $(NODEJS) $< 23
cleanall:: ; $(RMF) test_tree_dats.js
#
######
#
test_sdom_dats.c: test_sdom.dats; $(PATSCC) -ccats $<
test_sdom_dats.js: test_sdom_dats.c; $(ATSCC2JS) -o $@ -i $<
#
regress:: test_test_sdom
test_test_sdom:: test_sdom_dats.js ; $(NODEJS) $< 23
cleanall:: ; $(RMF) test_sdom_dats.js
#
######
#
RMF=rm -f

######

clean:: ; $(RMF) *~
clean:: ; $(RMF) *_dats.c

######

cleanall:: clean
cleanall:: ; $(RMF) *_dats.js

###### end of [Makefile] ######

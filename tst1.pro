/*
.PN1
.PO0 */

signs :- nl,
  print( 'Does the patient exhibit any of the following signs:' ), nl,
  print( 'weakness, lightheadedness, syncope, cardiac awareness,
  pallor, tachycardia, jaundice.' ),
  affirm,
  syndrome( 1 ).

syndrome( 1 ) :- 'undist anemia'.

'undist anemia' :-
   anemia( RBC ),
   print( 'Patient has anemia. We now try to diagnose the specific type.' ),
  'anemia subtype'( RBC ).

'anemia subtype'( RBC ) :-  'congenital hemolytic anemia'( RBC ).
'anemia subtype'( RBC ) :-  'acquired hemolytic anemia'.

'acquired hemolytic anemia' :-
   ldh( high ),  nl,
   print('Based upon a diagnosis of anemia and ' ),
   print(' high LDH we obtain acquired hemolytic anemia.' ).

'congenital hemolytic anemia'( low )  :-
   'congenital hemolytic history',
   'congenital hemolytic determinant', nl,
   print('Based upon a diagnosis of anemia and '),
   print( 'the just named symptom we diagnose congenital hemolytic anemia.' ).

'deficiency anemia' :- nl,
  print( 'Diagnosis is a deficiency anemia.' ).


'congenital hemolytic history' :- jaundice.
'congenital hemolytic history' :- gallstones.
'congenital hemolytic history' :- sphenomegally.
'congenital hemolytic history' :- hepatomegally.
'congenital hemolytic history' :- 'bony malformations'.
'congenital hemolytic history' :- 'mental retardation'.

'congenital hemolytic determinant' :- microcytosis.
'congenital hemolytic determinant' :- eliptocytosis.
'congenital hemolytic determinant' :- spherocytosis.
'congenital hemolytic determinant' :- anisopoikilocytosis.
'congenital hemolytic determinant' :- 'anemia related to food'.

microcytosis :- labfindings( microcytosis ).
eliptocytosis :- labfindings( eliptocytosis ).
anisopoikilocytosis :- labfindings( anisopoikilocytosis ).
'anemia related to food' :- evidence( 'anemia related to food' ).
spherocytosis :- nl,
     print( 'Is the % of spherocytosis > 50%' ), affirm.

anemia( RBC ) :- symptom( anemic ), rbc( RBC ).

symptom( anemic ) :- hemoglobin( low ).
symptom( anemic ) :- hematocrit( low ).

evidence( X ) :- nl,
  print('Has the patient evidence of '),
  print( X ), affirm.

labfindings( X ) :- nl,
  print('Are there laboratory findings of ' ),
  print( X ), affirm.

jaundice :- evidence( jaundice ).
gallstones :- evidence( gallstones ).
sphenomegally :- evidence( sphenomegally ).
hepatomegally :- evidence( hepatomegally ).
'bony malformations' :- evidence( 'bony malformations' ).
'mental retardation' :- evidence(  'mental retardation' ).
'retarded growth and development' :-
     evidence( 'retarded growth and development' ).
'crisis of viscera, bones' :-
     evidence( 'crisis of viscera, bones' ).


/* Laboratory measurements: */

rbc( HLN ) :- rbcmeas( RBC ), rbccat( RBC, HLN ).
rbccat( RBC, low ) :- RBC < 4.
rbccat( RBC, high) :- RBC > 6.
rbccat( RBC, norm ) :- RBC = 5.
rbcmeas(RBC) :- nl,
     print( 'Input the RBC in millions/microliter:' ),
     read( RBC ).

hematocrit( HLN ) :- hematocrtmeas( HEMAT ), hematcat( HEMAT, HLN ).
hematcat( HEMAT, low ) :- HEMAT < 36.
hematocrtmeas( HEMAT ) :- nl,
     print( 'What is the hematocrit level % per deciliter?:' ),
     read( HEMAT ).

mcv( low  ) :- mcv1( low ).
mcv( high ) :- mcv1( high ), not( arct( high ) ).


mcv1( HLN ) :- mcvmeas( MCV ), mcvcat( MCV, HLN ).
mcvcat( MCV, high) :- MCV > 96.
mcvcat( MCV, low ) :- MCV < 85.
mcvmeas( MCV ) :- nl,
  print( 'What is the level of MCV in cubic microns:' ),
  read( MCV ).

ldh( LDH ) :- nl,
  print( 'What is the level of LDH (high,low, or norm)?: ' ),
  read( LDH ).

arct( HLN ) :- arctmeas( ARCT ), arctcat( ARCT, HLN ).
arctmeas( ARCT ) :- nl,
  print( 'What is the absolute reticulocyte count in units of thousands:'),
  nl,
  read( ARCT ).

affirm :- nl, print( '(y./n.) ?:- ' ),  read( ANS ), nl, yes( ANS ).

yes( y ).

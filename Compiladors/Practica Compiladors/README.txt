
Alguns aclariments i comentaris:

- Si es fa SPLIT d'un tube amb longitud 1, salta excepció i no es creen els tubs (ni es destrueix el que s'anava a partir en dos).

- Es permet afegir tubs/connectors amb length i/o diameter 0, ja que es mostra a la practica i no es diu que generi cap error (FT), com a mínim amb el "length". Seguint el mateix model es permet un vector de mida 0, encara que no tingui gaire sentit (tampoc la te un tub de mida 0 o diametre 0, encara que pugui ser útil per a fer algunes operacions).


----ALTRES COSES----

- "chivatos":

-> Per comentar els "chivatos" i fer que no apareguin per consola:
 find and replace:
 	find: /*CHI:*/
 	replace: /*CHI:

-> Per descomentar i poder veure per consola el resultat dels "chivatos":
find and replace:
	find: /*CHI:
	replace: /*CHI:*/


- excepcions/errors: per evitar que el programa pari en cas d'error s'ha afegit catch d'excepcions per defecte (a part de les que hi ha per quan falla alguna instruccio perque no es troba un ID o s'intenta crear un tub amb mida menor a 1, per exemple). Per treure-ho i que només es faci "catch" de les excepcions que afegits expressament i no qualsevol:

-> Per afegir catch de qualsevol excepcio:
 find and replace:
 	find: /*UEXCEPT:*/
 	replace: /*UEXCEPT:

-> Per treure catch de qualsevol excepcio:
find and replace:
	find: /*UEXCEPT:
	replace: /*UEXCEPT:*/



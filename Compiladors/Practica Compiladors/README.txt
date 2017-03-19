
Alguns aclariments i comentaris:

- Si es fa SPLIT d'un tube amb longitud 1, salta excepció i no es creen els tubs (ni es destrueix el que s'anava a partir en dos).

- Es permet afegir tubs/connectors amb length i/o diameter 0, ja que es mostra a la practica i no es diu que generi cap error (FT), com a mínim amb el "length". Seguint el mateix model es permet un vector de mida 0, encara que no tingui gaire sentit (tampoc la te un tub de mida 0 o diametre 0, encara que pugui ser útil per a fer algunes operacions).


---- OTROS ----

- Per veure després de cada instrucció com ha quedat el STOCK:

Find and replace:
	-find: /*PRINTSTOCK:*/
	-replace: /*PRINTSTOCK:

- Per treure el print de com ha quedat el STOCK després de cada instrucció:

Find and replace:
	-find: /*PRINTSTOCK:
	-replace: /*PRINTSTOCK:*/
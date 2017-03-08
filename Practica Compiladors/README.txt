-> Per comentar els chivatos i fer que no apareguin per consola:
 find and replace:
 	find: /**CHI:*/
 	replace: /**CHI:
 find and replace:
 	find: /*ENDCHI*/
 	replace: ENDCHI*/

-> Per descomentar i poder veure per consola el resultat dels chivatos:
find and replace:
	find: /**CHI:
	replace: /**CHI:*/
 find and replace:
 	find: ENDCHI*/
 	replace: /*ENDCHI*/
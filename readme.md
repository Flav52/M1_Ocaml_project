# Projet: M1 Ocaml
Stéganographie : programmation en language CAML d'un cryptage RSA d'un message dans une image.
<br><br>
RIGLET FLAVIEN<br>
LARDI NICOLAS<br>
<br>
makefile:   -all<br>
            -clean<br>
            -flush : Nettoie les clée générées. (sauf paire de base)<br>
<br>
./rsa:  -c: Lancer le cryptage.<br>
        -d: Lancer de décryptage.<br>
        -k: Lancer la génération des clés.<br>
        -h: Afficher cette aide.<br><br>

La taille des nombres premiers ainsi que le numero du jeu de clés doivent
être précisé dans la méthode main avant compilation.<br>

Le fichier Static permet de générer des données de base après un clean.<br>
(jeu de clés RSA par défaut)

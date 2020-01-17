Sujet 21: Stéganographie
RIGLET FLAVIEN
LARDI NICOLAS

makefile:   -all
            -clean
            -flush : Nettoie les clée générées. (sauf paire de base)

./rsa:  -c: Lancer le cryptage.
        -d: Lancer de décryptage.
        -k: Lancer la génération des clés.
        -h: Afficher cette aide.

La taille des nombres premiers ainsi que le numero du jeu de clés doivent
être précisé dans la méthode main avant compilation.

Le fichier Static permet de générer des données de base après un clean.
(jeu de clés RSA par défaut)

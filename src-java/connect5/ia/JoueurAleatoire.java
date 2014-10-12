package connect5.ia;

/*
 * Si vous utilisez Java, vous devez modifier ce fichier-ci.
 *
 * Vous pouvez ajouter d'autres classes sous le package connect5.ia.
 *
 * Prénom Nom    (CODE00000001)
 * Prénom Nom    (CODE00000002)
 */

import connect5.Grille;
import connect5.Joueur;
import connect5.Position;

import java.util.ArrayList;
import java.util.Random;


public class JoueurAleatoire implements Joueur {

    private final Random random = new Random();

    /**
     * Voici la fonction à modifier.
     * Évidemment, vous pouvez ajouter d'autres fonctions dans JoueurArtificiel.
     * Vous pouvez aussi ajouter d'autres classes, mais elles doivent être
     * ajoutées dans le package connect5.ia.
     * Vous de pouvez pas modifier les fichiers directement dans connect., car ils seront écrasés.
     *
     * @param grille Grille reçu (état courrant). Il faut ajouter le prochain coup.
     * @param delais Délais de rélexion en temps réel.
     * @return Retourne le meilleur coup calculé.
     */
    @Override
    public Position getProchainCoup(Grille grille, int delais) {
        ArrayList<Integer> casesvides = new ArrayList<Integer>();
        int nbcol = grille.getData()[0].length;
        for (int l = 0; l < grille.getData().length; l++)
            for (int c = 0; c < nbcol; c++)
                if (grille.getData()[l][c] == 0)
                    casesvides.add(l * nbcol + c);
        int choix = random.nextInt(casesvides.size());
        choix = casesvides.get(choix);
        return new Position(choix / nbcol, choix % nbcol);
    }

    @Override
    public String getAuteurs() {
        return "Prénom1 Nom1 (CODE00000001)  et  Prénom2 Nom2 (CODE00000002)";
    }


}

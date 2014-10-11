/*
 * INF4230 - Intelligence artificielle
 * TP2 - Algorithme minimax avec élagage alpha-beta appliqué au jeu Connect5GUI
 *
 * (C) Éric Beaudry 2014.
 * UQAM - Département d'informatique
 */

package connect5;

/**
 *
 * @author Éric Beaudry
 */
public class GrilleVerificateur {

    public int determineGagnant(Grille grille) {
        quiGagne = lastValue = 0; // reset status

        // horizontal
        for (int l = 0; l < grille.data.length; l++) {
            for (int c = 0; c < grille.data[0].length; c++) {
                check(grille.data[l][c]);
            }
            check(0);
        }

        // vertical
        for (int c = 0; c < grille.data[0].length; c++) {
            for (int l = 0; l < grille.data.length; l++) {
                check(grille.data[l][c]);
            }
            check(0);
        }

        // Diagonal \\\\\\\
        for (int c = -grille.data.length; c < grille.data[0].length; c++) {
            int c2 = c;
            int l = 0;
            if (c2 < 0) {
                l = -c2;
                c2 = 0;
            }
            for (; c2 < grille.data[0].length && l < grille.data.length; c2++, l++) {
                check(grille.data[l][c2]);
            }
            check(0);
        }

        // Diagonal //////
        for (int c = -grille.data.length; c < grille.data[0].length; c++) {
            int c2 = c;
            int l = grille.data.length - 1;
            if (c2 < 0) {
                l += c2;
                c2 = 0;
            }
            for (; c2 < grille.data[0].length && l >= 0; c2++, l--) {
                check(grille.data[l][c2]);
            }
            check(0);
        }

        return quiGagne;
    }

    private void check(int value) {
        if (value == lastValue) {
            count++;
        } else {
            if (lastValue > 0 && (compteExact ? count == nombreGagnant : count >= nombreGagnant)) {
                quiGagne = lastValue;
            }
            count = 1;
            lastValue = value;
        }
    }
    protected int nombreGagnant = 5;
    protected boolean compteExact = true;
    protected int lastValue = 0;
    protected int count = 0;
    protected int quiGagne = 0;
   
}

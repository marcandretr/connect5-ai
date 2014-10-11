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
 * @author Eric Beaudry
 */
public interface JeuObserver {

    public void grilleChanged(Grille g);
    
    public void message(String msg);
    
}

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
public class Position {
    public Position(){
        
    }
    public Position(int l, int c){
        ligne = l;
        colonne = c;
    }
    @Override
    public String toString(){
        return ligne + " " + colonne;
    }
    public int ligne, colonne;
}

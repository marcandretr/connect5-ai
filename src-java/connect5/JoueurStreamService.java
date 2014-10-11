/*
 * INF4230 - Intelligence artificielle
 * TP2 - Algorithme minimax avec élagage alpha-beta appliqué au jeu Connect5GUI
 *
 * (C) Éric Beaudry 2014.
 * UQAM - Département d'informatique
 */

package connect5;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.StringTokenizer;

/**
 *
 * @author Eric Beaudry
 */
public class JoueurStreamService {

    public static void runInterface(Joueur joueur, PrintStream out, BufferedReader reader) throws IOException{
        out.println("Joueur artificiel pour Connect5");
        out.println("INF4230 - Intelligence Artificielle");
        out.println("Département d'informatique");
        out.println("UQAM");
        out.println(joueur.getAuteurs());
        out.println("");
        out.println("READY");
        out.flush();


        while(true){
            int nbligne, nbcol;
            String ligne = reader.readLine();
            if(ligne==null)
                return;
            if(ligne.equalsIgnoreCase("shutdown")){
                System.exit(0);
                return;
            }
            StringTokenizer tokens = new StringTokenizer(ligne);
            nbligne = Integer.parseInt(tokens.nextToken());
            if(nbligne==0)
                return;
            nbcol = Integer.parseInt(tokens.nextToken());

            Grille grille = new Grille(nbligne, nbcol);

            for(int l=0;l<nbligne;l++){
                ligne = reader.readLine();
                for(int c=0;c<nbcol;c++){
                    switch(ligne.charAt(c)){
                        case '0':
                            grille.set(l, c, 0);
                            break;
                        case 'N':
                            grille.set(l, c, 1);
                            break;
                        case 'B':
                            grille.set(l, c, 2);
                            break;
                    }
                }
            }

            int delais = Integer.parseInt(reader.readLine());
            Position coup = joueur.getProchainCoup(grille, delais);
            out.println(coup);
            out.flush();
            System.gc();
        }
    }

    public static void main(String args[]) throws Exception{
        Joueur joueur = new connect5.ia.JoueurArtificiel();
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        runInterface(joueur, System.out, reader);
    }

}

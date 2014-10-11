/*
 * INF4230 - Intelligence artificielle
 * TP2 - Algorithme minimax avec élagage alpha-beta appliqué au jeu Connect5GUI
 *
 * (C) Éric Beaudry 2014.
 * UQAM - Département d'informatique
 */

package connect5;

import java.io.*;
import java.net.Socket;
import java.util.StringTokenizer;

/**
 *
 * @author Eric Beaudry
 */
public class JoueurClientSocket implements Joueur {


    public JoueurClientSocket(String host, int port) throws Exception{
        socket = new Socket(host, port);
        fromDistant = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        toProcess = new PrintWriter(socket.getOutputStream());

        System.out.println("Initialisation du joueur distant");
        String ligne;
        do{
            ligne = fromDistant.readLine();
            System.out.println(" STDOUT: " + ligne);
        }while(!ligne.equalsIgnoreCase("READY"));

        System.out.println("Joueur artificiel initialisé.");

    }

    protected Socket         socket;
    protected PrintWriter    toProcess;
    protected BufferedReader fromDistant;
    protected boolean        debug = false;

    @Override
    public Position getProchainCoup(Grille g, int delais) {
        Position coup = new Position();
        if(debug){
            System.out.println("----- Texte envoye au STDIN -------");
            System.out.print(g.toString());
            System.out.println("" + delais);
            System.out.println("-----------------------------------");
        }
        toProcess.print(g.toString());
        toProcess.println("" + delais);
        toProcess.flush();

        try{
            String ligne = fromDistant.readLine();
            if(debug)
                System.out.println(" Ligne recu du STDOUT: \"" + ligne + "\"");
            StringTokenizer tokens = new StringTokenizer(ligne);
            coup = new Position(Integer.parseInt(tokens.nextToken()), Integer.parseInt(tokens.nextToken()));
        }catch(Exception e){
            System.out.println("Joueur fail!");
            e.printStackTrace();
        }
        return coup;
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        socket.close();
        socket = null;
    }

    @Override
    public String getAuteurs() {
        return "?";
    }
}

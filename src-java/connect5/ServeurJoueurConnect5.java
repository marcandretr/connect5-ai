/*
 * INF4230 - Intelligence artificielle
 * TP2 - Algorithme minimax avec élagage alpha-beta appliqué au jeu Connect5GUI
 *
 * (C) Éric Beaudry 2014.
 * UQAM - Département d'informatique
 */

package connect5;

import connect5.ia.JoueurArtificiel;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Date;

/**
 *
 * @author Eric Beaudry
 */
public class ServeurJoueurConnect5 {

    public static void main(String args[]) throws Exception{
        int port = args.length>0 ? Integer.parseInt(args[0]) : 1199;
		System.out.println("Ouverture du port " + port);
        ServerSocket serversocket = new ServerSocket(port);
        System.out.println("Attente de connection...");

        while(true){
            Socket socket = serversocket.accept();

            if(JoueurRunner.compteur > 50){
                socket.close();
            }

            System.out.println("Client: " + socket.toString() +"  @ " + new Date() );
            JoueurRunner jr = new JoueurRunner(socket);
            Thread t = new Thread(jr);
            t.start();
        }

    }

    public static class JoueurRunner implements Runnable{

        Socket         socket;
        int            id;
        static int     nextID=1;

        public JoueurRunner(Socket socket){
            this.socket = socket;
            id=nextID++;
        }

        @Override
        public void run() {
            changeCompteur(+1);
            try{
                PrintStream out = new PrintStream(socket.getOutputStream());
                BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                JoueurArtificiel joueur;
                joueur = new JoueurArtificiel();
                //joueur.showTrace = true;
                JoueurStreamService.runInterface(joueur, out, reader);
            }catch(Exception e){
                e.printStackTrace();
            }catch(Error e){
                e.printStackTrace();;
            }

            changeCompteur(-1);

            try{
                socket.close();
            }catch(IOException ioe){
            }
        }
        
        public static synchronized  void changeCompteur(int diff){
            compteur += diff;
            System.out.println(" #clients actifs:" + JoueurRunner.compteur);
        }

        public static int   compteur=0;

    }

}

/*
 * INF4230 - Intelligence artificielle
 * TP2 - Algorithme minimax avec élagage alpha-beta appliqué au jeu Connect5GUI
 *
 * (C) Éric Beaudry 2014.
 * UQAM - Département d'informatique
 */

package connect5;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.StringTokenizer;



/**
 *
 * @author Eric Beaudry
 */
public class JoueurClientCmd implements Joueur {

    public JoueurClientCmd(String command) throws IOException {
        ProcessBuilder pb = new ProcessBuilder(command);
        File file = new File(command);
        File parent = file.getParentFile();
        if(parent.exists())
            pb.directory(parent);
        process = pb.start();
        
        fromProcess = new BufferedReader(new InputStreamReader(process.getInputStream()));
        new Thread(new Runnable(){
            @Override
            public void run() {
                BufferedReader err = new BufferedReader(new InputStreamReader(process.getErrorStream()));
                try{
                    String ligne;
                    do{
                        ligne=err.readLine();
                        if(debug)
                            System.err.println(" STDERR: " + ligne);
                    }while(ligne!=null);
                    process.getErrorStream().close();
                }catch(Exception e){e.printStackTrace();}
            }
        }).start();
        
        toProcess = new PrintWriter(process.getOutputStream());
        
        System.out.println("Initialisation du joueur artificiel");
        String ligne;
        do{
            ligne = fromProcess.readLine();
            System.out.println(" STDOUT: " + ligne);
        }while(!ligne.equalsIgnoreCase("READY"));
        
        System.out.println("Joueur artificiel initialisé.");
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        toProcess.println("0");
        toProcess.flush();
        for(int i=0;i<20;i++){
            try{
                process.exitValue();
                return;
            }catch(IllegalThreadStateException e)
            {
                try{
                    Thread.sleep(1000);
                }catch(InterruptedException ie){}
            }
        }
        process.destroy();
    }
    
    @Override
    public Position getProchainCoup(Grille g, int delais){
        Position coup = new Position(0, 0);
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
            String ligne = fromProcess.readLine();
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
    
    public void terminate(){
        toProcess.println("0");
        toProcess.flush();
    }
    
    public void forceTerminate(){
        toProcess.println("0");
        toProcess.flush();
        for(int i=0;i<5;i++){
            try{
                process.exitValue();
                return;
            }catch(IllegalThreadStateException e)
            {
                try{
                    Thread.sleep(1000);
                }catch(InterruptedException ie){}
            }
        }
        process.destroy();        
    }
    
    public void setDebug(boolean d){
        debug = d;
    }
    
    protected Process        process;
    protected PrintWriter    toProcess;
    protected BufferedReader fromProcess;
    protected BufferedReader fromProcessErr;
    protected boolean        debug=true;
    
    
    public static void main(String args[]) throws Exception{
        JoueurClientCmd j = new JoueurClientCmd("/home/user/inf4230/tp1/tp1");
        Grille g = new Grille(10, 10);
        GrilleVerificateur verif = new GrilleVerificateur();
        int next=0;
        while(true){
            if(g.nbLibre()==0){
                System.out.println("Partie nulle");
                break;
            }
            Position coup = j.getProchainCoup(g, 2000);
            if(g.get(coup)!=0){
                System.out.println("Placement illégal!");
                break;
            }
            g.set(coup, next+1);
            int winner = verif.determineGagnant(g);
            if(winner>0){
                System.out.println(g);
                System.out.println("Gagant: " + winner);
                break;
            }
            next = (next+1)%2;
        }
        j = null;
      
    }

    @Override
    public String getAuteurs() {
        return "?";
    }
    
}

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
public class Grille {

    public Grille(int nblignes, int nbcols){
        data = new byte[nblignes][nbcols];
    }

    public Grille(int nblignes, int nbcols, int[] data1d){
        data = new byte[nblignes][nbcols];
        for(int l=0;l<nblignes;l++)
            for(int c=0;c<nbcols;c++){
                data[l][c] = (byte) (data1d[l*nbcols+c] + 1);
            }
    }
    public void set(int l, int c, int v){
        data[l][c] = (byte) v;
    }
    public void set(Position p, int v){
        data[p.ligne][p.colonne] = (byte)v;
    }
    
    public int get(int l, int c){
        return data[l][c];
    }
    public int get(Position p){
        return data[p.ligne][p.colonne];
    }
    
    public void reset(){
        for(byte[] b : data)
            for(int i=0;i<b.length;i++)
                b[i] = 0;
    }
    
    public int nbLibre(){
        int n=0;
        for(byte[] b : data)
            for(byte bb : b)
                if(bb==0)
                    n++;
        return n;
    }

    public int getSize(){
        return data.length * data[0].length;
    }
    
    @Override
    public String toString(){
        char[] table = {'0', 'N', 'B' };
        String result = "" + data.length + " " + data[0].length + "\n";
        for(byte[] b : data){
            char[] c = new char[b.length];
            for(int i=0;i<b.length;i++)
                c[i] = table[b[i]];
            result += new String(c);
            result += '\n';
        }
        return result;
    }
    
    public byte[][] getData(){
        return data;
    }
    
    private Grille(){
        
    }

    @Override
    public Grille clone(){
        Grille copie = new Grille(data.length, data[0].length);
        for(int l=0;l<data.length;l++)
            System.arraycopy(data[l], 0, copie.data[l], 0, data[l].length);
        return copie;
    }
    
    protected byte[][]     data;
    
}

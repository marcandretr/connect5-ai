package connect5.ia;

/*
 * Si vous utilisez Java, vous devez modifier ce fichier-ci.
 *
 * Vous pouvez ajouter d'autres classes sous le package connect5.ia.
 *
 * Marc-Antoine Sauve   (SAUM13119008)
 * Marc-Andre Tremblay  (TREM22129101)
 */


import clojure.lang.*;
import com.google.common.base.Joiner;
import connect5.Grille;
import connect5.Joueur;
import connect5.Position;


import java.util.ArrayList;
import java.util.Random;


public class JoueurArtificiel implements Joueur {

    static Var CljPlayer;

    static {
        IFn require = RT.var("clojure.core", "require").fn();
        require.invoke(Symbol.intern("connect5-ai.core"));
        CljPlayer = RT.var("connect5-ai.core", "-getNextMove");
    }

    @Override
    public Position getProchainCoup(Grille grille, int delais) {
        PersistentVector res = (PersistentVector) CljPlayer.invoke(grille.getData(), delais);
        return new Position(
                ((Long) res.get(1)).intValue(),
                ((Long) res.get(0)).intValue());
    }

    @Override
    public String getAuteurs() {
        return Joiner.on("\n").join(AUTHORS);
    }

    private class Author {
        public String name;
        public String permCode;
        public String githubUID;

        public Author setName(String name) {
            this.name = name;
            return this;
        }

        public Author setPermCode(String permCode) {
            this.permCode = permCode;
            return this;
        }

        public Author setGithubUID(String githubUID) {
            this.githubUID = githubUID;
            return this;
        }

        @Override
        public String toString() {
            return this.name + "\t (" + this.permCode + ") \t" + githubUID;
        }
    }
    final Author[] AUTHORS = {
            new Author()
                    .setName("Marc-Antoine Sauve")
                    .setPermCode("SAUM13119008")
                    .setGithubUID("madeinqc"),
            new Author()
                    .setName("Marc-Andre Tremblay")
                    .setPermCode("TREM22129101")
                    .setGithubUID("marcandretr")
    };


}

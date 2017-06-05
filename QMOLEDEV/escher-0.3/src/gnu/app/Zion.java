package gnu.app;

import gnu.util.Misc;
import java.io.BufferedReader;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;


/**
 * The last mainframe on earth. 
 *
 * Allow execute multiple JVM processors in a single VM, hence acting as
 * "the last mainframe" (from the movie Matrix). This can be helpful for
 * frequent invocations of Java applications during development, especially
 * when (my IBM 1.3) JVM is horribly slow for startup. It also helps if you
 * run a destop-full of Java programs, which will otherwise drain all your
 * system resources. This program is inspired by the full-blown "free
 * multiprocess system in Java", <a
 * href="http://www.javagroup.org/echidna/">Echidna</a>, by Luke Gorrie.
 *
 * <p>At the <code>zion&gt;</code> prompt, simply type in the classname
 * optionally with its command-line arguments, Zion will run it as a
 * sub-thread. Exit Zion with the command <code>"exit"</code>. For example,
 * to (1) display the help of <code>gnu.x11.test.Hello</code>, (2) run it,
 * and (3) run <code>gnu.app.displayhack.Munch</code> all concurrently in the
 * same JVM session.
 *
 * <p>Because of the way Java loads and caches classes, Zion provides
 * another command <code>"reset"</code>, such that, for instance, old
 * versions of development classes can be discarded and then new versions
 * can be executed without exiting and re-entering Zion. A shortcut command
 * to reset and execute a class is prepended the command with
 * <code>"="</code>. For example, to reset and load
 * <code>gnu.x11.test.Hello</code>, do <code>zion>
 * =gnu.x11.test.Hello</code>.
 * 
 * @see <a href="../../../etc/screenshot/gnu/app/Zion.output">
 * sample output</a>
 *
 * @see <a href="../../../etc/screenshot/gnu/app/Zion.help">
 * help output</a>
 */
public class Zion extends Application {
  public Zion (String [] args) {
    super (args);

    about ("0.1", "the last mainframe on earth",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nCommands: "
      + "\n  exit: terminate Zion"
      + "\n  reset: clear cached classes"
      + "\n  CLASSNAME [OPTION]...: execute class with options");

    if (help_option) return;
  }


  public void exec () throws IOException, ClassNotFoundException {
    System.setSecurityManager (new SecurityManager ());
    BufferedReader br = new BufferedReader (
      new java.io.InputStreamReader (System.in));

    while (true) {
      System.out.print ("zion> ");
      String command = br.readLine ();
      if (command == null) exit ();
      if (Misc.empty (command)) continue;
      
      if (command.equals ("reset")) 
        gnu.util.ReloadableClassLoader.reset ();
      else if (command.equals ("exit")) exit ();

      else {
        // possbily reset
        if (command.charAt (0) == '=') {
          gnu.util.ReloadableClassLoader.reset ();
          command = command.substring (1, command.length ());
        }

        // execute subvm
        int from = command.indexOf (' ');
        if (from == -1)
          new SubVM (command, new String [0]);
        else {
          String args0 = command.substring (from, command.length ());
          String [] args1 = Misc.tokenize (args0);
          String name = command.substring (0, from);
          new SubVM (name, args1);
        }
      }
    }
  }


  public void exit () {
    System.setSecurityManager (null);
    System.exit (0);
  } 


  public static void main (String [] args) 
    throws IOException, ClassNotFoundException {

    new Zion (args).exec ();
  }
}


class SubVM extends Thread {
  public String [] args;
  public java.lang.reflect.Method main;


  public SubVM (String name, String [] args) {
    this.args = args;
    
    try {               
      // Class.forName() uses loaded class instead of asking class loader
      Class klass = gnu.util.ReloadableClassLoader
        .instance.loadClass (name);

      Class [] para_types = new Class [] {String [].class};
      main = klass.getDeclaredMethod ("main", para_types);        
      start ();

    } catch (Exception e) {
      e.printStackTrace ();
    }
  }


  public void run () {
    if (main == null) return;

    try {
      main.invoke (null, new Object [] {args});

    } catch (Exception e) {
      // safely ignored sub vm exit
      // FIXME: it can't catch exceptions thrown in other threads
      if (e instanceof InvocationTargetException) {
        InvocationTargetException e0 = (InvocationTargetException) e;
        Throwable e1 = e0.getTargetException ();
        if (e1 instanceof SecurityManager.SubVMExitException) return;
      }

      e.printStackTrace ();
    }
  }
}


/** A dummy security manager for Zion. */
class SecurityManager extends java.lang.SecurityManager {

  /** Thrown when a sub VM calls {@link java.lang.System#exit(int)}. */
  public class SubVMExitException extends SecurityException {}
    

  public void checkExit (int status) {
    // throw an exception such that `System.exit()' fails
    throw new SubVMExitException ();
  }


  /** Override to disable all permission checkings for Zion. */
  public void checkPermission (java.security.Permission perm) {}
}

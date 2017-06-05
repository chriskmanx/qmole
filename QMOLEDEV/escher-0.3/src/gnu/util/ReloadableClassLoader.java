package gnu.util;

import java.io.File;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;


/** 
 * A neat hack to do class reloading.
 *
 * <p>Class reloading is, if not impossible and ugly, inconvenient in
 * current class loader architecture. Sun should _really_ provide a
 * standard way to do it. Many packages have implemented class reloading of
 * their own, but none of them are cleanly designed to be reused IMHO. I
 * ended up hacking my own version.
 *
 * <p>The problem of class reloading is that JVM does not provision the
 * need. Once a class is loaded into JVM, it stays until some
 * non-deterministic garbage collect time when no ones references a class
 * anymore. However, JVM _does_ provide "namespace", to which different
 * versions of the same class data can be loaded. A namespace is actually
 * the "local scope" of an instance of a class loader. That is, a class
 * can be loaded once and only once by the same intance of a class loader;
 * but a new instance of a class loader can be created to load a new
 * version of a class. Therefore, the trick of class reloading is to create a
 * new instance of a class loader, and cache the loaded and reloaded
 * classes in a global static place.
 *
 * <p>Note that dependent classes must be reloaded and resolved manually
 * after the modified class is reloaded. For instance, given Class A and
 * Class B (which depends on Class A) are loaded, and then Class A is
 * modified and reloaded, Class B has to be reloaded to get the new version
 * of Class A. It is because we can only force resolution of the new Class
 * A for Class B through reloading Class B, as implied in <cite>The JavaTM
 * Virtual Machine Specificatio</cite>.
 *
 * <p>Each of the following packages has its own reloadable class loader:
 * BeanShell, GNU Server Pages, Jigsaw HTTPD, Resin's DynamicClassLoader,
 * and Echidna multi-process system.
 *
 * @see <a href="
 * http://www.javaworld.com/javaworld/jw-01-1999/jw-01-techniques.html">
 * bill's article</a>
 *
 * @see <a href="
 * http://www.artima.com/insidejvm/ed2/ch08TheLinkingModelPrint.html">
 * bill's book</a>
 *
 * @see <a href="
 * http://java.sun.com/people/gbracha/classloaders.ps">
 * Liang and Bracha</a>
 */
public class ReloadableClassLoader extends ClassLoader {  
  public static Hashtable classes = new Hashtable ();
  public static ReloadableClassLoader instance 
    = new ReloadableClassLoader ();

  
  /** 
   * Load class file data into namespace of this class loader.
   *
   * @param name fully-qualified class name
   * @param data raw bytes of class file
   */
  public Class load (String name, byte [] data, boolean resolve) {
    Class klass = defineClass (name, data, 0, data.length);
    if (resolve) resolveClass (klass);
    classes.put (name, klass);
    return klass;
  }
    
    
  /**
   * Force loading a class. Consider using {@link #reload_classes(String[],
   * boolean)} for efficiency.
   * 
   * @see #reload_classes(String[], boolean)
   */
  public static void reload_class (String name, boolean resolve) 
    throws ClassNotFoundException {

    String [] names = {name};
    reload_classes (names, resolve);
  }


  /**
   * Force loading a list of classes.
   *
   * @param wildnames a list of wildnames; a wildname can be a class name
   * to indicate a class to be reloaded, or a package name appended with
   * ".*" to indicate a package to be reloaded
   */
  public static void reload_classes (String [] wildnames, 
    boolean resolve) throws ClassNotFoundException {

    Vector file_pool = new Vector ();
    Vector name_pool = new Vector ();

    for (int i=0; i<wildnames.length; i++) {
      if (wildnames [i].endsWith (".*")) { // package        
        String package_name = wildnames [i].substring (
          0, wildnames [i].length () - ".*".length ());
        File [] files = Classpath.find_package (package_name);        
        String [] names = Classpath.to_jvm_name (package_name, files);
        for (int j=0; j<files.length; j++) file_pool.add (files [j]);
        for (int j=0; j<names.length; j++) name_pool.add (names [j]);

      } else {                  // individual class
        file_pool.add (Classpath.find_class (wildnames [i]));
        name_pool.add (wildnames [i]);
      }
    }

    reload (file_pool, name_pool, resolve);
  }


  /**
   * Force loading a package. Consider using {@link #reload_classes(String[],
   * boolean)} for efficiency.
   * 
   * @see #reload_classes(String[], boolean)
   */
  public static void reload_package (String name, boolean resolve)
    throws ClassNotFoundException {
    
    String [] names = {name + ".*"};
    reload_classes (names, resolve);
  }
      

  public static Object INVALID = new Object ();


  /** 
   * Force loading a list of classes into namespace of this class loader. 
   * If a class file is not found, exception will be thrown and classes in
   * the rest of the list will not be loaded.
   *
   * Because each reloading creates a new instance of class loader (which
   * will not be garbaged collected until all classes it loads are garbaged
   * collected), requests of reloading should be grouped together (into
   * longer vectors) instead of doing it separate. In particular, {@link
   * #reload_classes(String[], boolean)} should be used instead of
   * {@link #reload_class(String, boolean)} or {@link
   * #reload_package(String, boolean)} whenever possible.
   *
   * @param names fully-qualified class names
   */
  public static void reload (Vector files, Vector names, boolean resolve) 
    throws ClassNotFoundException {

    // try remove from cache before reload
//     for (Iterator it=names.iterator (); it.hasNext ();)
//       classes.put ((String) it.next (), INVALID);

    ReloadableClassLoader dummy = new ReloadableClassLoader ();

    for (Iterator file_it=files.iterator (), name_it=names.iterator (); 
         file_it.hasNext ();) {

      byte [] data = read ((File) file_it.next ());
      if (data == null) throw new ClassNotFoundException ();

      /* There may exist linkage dependency in the given list of classes. 
       * For instance, loading Class A may load Class B due to resolution
       * and linkage; then loading Class B explicitly will generate
       * LinkageError because one class loader can only load any class
       * once. We can safely ignore the error. Another way is to keep track
       * of which classes are already loaded and which classes are being
       * reloaded, but this work is unnecessary.
       */
      try {
        dummy.load ((String) name_it.next (), data, resolve);
      } catch (LinkageError e) {
        // ignored
      }
    }
  }


  /**
   * Load class facility for JVM.
   */
  public Class loadClass (String name, boolean resolve)     
    throws ClassNotFoundException {

    Object value = classes.get (name);
    if (value != null && value != INVALID) {
      Class klass = (Class) value;
      if (resolve) resolveClass (klass);
      return klass;

    } else {
      byte [] data = read (Classpath.find_class (name));
      if (data == null) return super.loadClass (name, resolve);
      else return instance.load (name, data, resolve);
    }
  }


  /**
   * Read a class file.
   *
   * @param name fully-qualified class name
   * 
   * @return raw bytes of class file, or <code>null</code> if class not
   * found 
   */
  public static byte [] read (File file) {
    if (file == null) return null;
    byte [] data = new byte [(int) file.length ()];

    try {
      new java.io.FileInputStream (file).read (data);
      return data;

    } catch (java.io.IOException e) {
      return null;
    }
  }


  /**
   * There are several mysterious problems relating to VerifyError and
   * LinkageError because of dependency and type-safety of reloading
   * classes. I cannot understand the issue well enough to solve the problem. This
   * method resets all caches and starts with a brand new classloader so
   * as to trade caching for correctness.
   */
  public static void reset () {
    instance = new gnu.util.ReloadableClassLoader ();
    classes.clear ();
  }
}

package gnu.util;

import java.util.Vector;


/**
 * Primitive command-line option parsing.
 *
 * <p>Only long option is supported, for clarity and easy parsing. GNU long
 * option requires a equal sign "=" between the option name and option
 * value. No equal sign is required (allowed) here, for clarity and easy
 * parsing, again.
 *
 * <p>Usual approach to option parsing such as that in GNU
 * <code>getopt</code> may not be the best in Java because Java does not
 * support pointer needed for uniform argument type and pass-by-reference. 
 * Instead this library uses "explicit method call per option". By this
 * type checking can be strictly enforced and return value can be directly
 * assigned to the desired variable. No boxing of primitive types or
 * un-boxing of wrapper classes will take place. Moreover, instead of
 * combining arrays of option table, option can be specified in super
 * classes and subclasses easily. One caveat is that option values cannot
 * be used in initializations of class member variables because parsing
 * takes place outside constructors. For instance, {@link
 * gnu.x11.Application} cannot initialize and create a window member
 * variable because parsing is not done yet and <code>Display</code> may
 * not be initialized; a window can be created only after all parsings.
 */
public class Option {
  public String [] args;
  public boolean noisy;
  public StringBuffer spec = new StringBuffer ();
  public StringBuffer invalidNames = new StringBuffer ();
  public Vector validNames = new Vector ();


  public Option (String [] args) {
    this.args = args;

    noisy = booleann ("noisy-option-parser", 
      "show working of option parsing", false);
  }


  /** 
   * Print "about" and list possible options. This method
   * should be called only once after all options are specified.
   *
   * <p>Note that this package is under GNU Public License, and thus all
   * programs using this package MUST be under GNU Public License.
   */
  public void about (Object object, String version, String description,
    String author, String url, String usage, String extra) {

    String name = object.getClass ().getName ();

    System.out.println ("\n" + name + " (" + version + ") - " + description
      + ".\nCopyright (C) " + author
      + ". Under GNU Public License.\n" + url
      + ".\n\nUsage: java " + name + " " + usage
      + "\n\nPossible options:\n"
      + spec + extra);

    spec = null;                // help garbage collection
  }


  public void addSpec (String name, String type, String description,
    String default_value, String return_value) {

    spec.append ("  --" + name + " [" + type + "] " + description
      + " (" + default_value + ")\n");
    validNames.add ("--" + name);

    if (noisy) System.out.println (name + " --> " + return_value);
  }


  public boolean booleann (String name, String description, 
    boolean default_value) {

    boolean retval = default_value;

    try {
      String opt = option (name);
      if (opt != null && opt.equals ("true")) retval = true;
      if (opt != null && opt.equals ("false")) retval = false;

    } catch (RuntimeException e) {
      invalidNames.append (name + ", ");
      // fall through
    }

    addSpec (name, "boolean", description, 
      String.valueOf (default_value), String.valueOf (retval));
    return retval;
  }
 


  protected int index (String name) {
    for (int i=0; i<args.length; i++)
      if (args [i].equals ("--" + name))
        return i;

    return -1;
  }


  public String option (String name) {
    int index = index (name);
    if (index == -1 || index == args.length-1) return null;
    return args [index+1];
  }

}

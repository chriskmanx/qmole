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
  public StringBuffer invalid_names = new StringBuffer ();
  public Vector valid_names = new Vector ();


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


  public void add_spec (String name, String type, String description,
    String default_value, String return_value) {

    spec.append ("  --" + name + " [" + type + "] " + description
      + " (" + default_value + ")\n");
    valid_names.add ("--" + name);

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
      invalid_names.append (name + ", ");
      // fall through
    }

    add_spec (name, "boolean", description, 
      String.valueOf (default_value), String.valueOf (retval));
    return retval;
  }
 

  public double doublee (String name, String description,
    double default_value) {

    double retval = default_value;

    try {
      String opt = option (name);
      if (opt != null) retval = Double.parseDouble (opt);

    } catch (RuntimeException e) {
      invalid_names.append (name + ", ");
      // fall through
    }

    add_spec (name, "double", description, 
      String.valueOf (default_value), String.valueOf (retval)); 
    return retval;
  }


  public int enumerate (String name, String description,
    String [] enum_strings, int default_value) {     

    int retval = default_value;

    try {
      String opt = option (name);
      boolean successful = false;
      if (opt != null) {
        for (int i=0; i<enum_strings.length; i++)
          if (opt.equals (enum_strings [i])) {           
            successful = true;
            retval = i;            
            break;
          }
        if (!successful) throw new RuntimeException ();
      }

    } catch (RuntimeException e) {
      invalid_names.append (name + ", ");
      // fall through
    }


    StringBuffer sb = new StringBuffer ("enum: ");
    for (int i=0; i<enum_strings.length; i++)
      sb.append (enum_strings [i] + ", ");    
    sb.delete (sb.length ()-2, sb.length ()); // delete last ", "

    add_spec (name, sb.toString (), description, 
      enum_strings [default_value], enum_strings [retval]);
    return retval;
  }    


  public float floatt (String name, String description,
    float default_value) {

    float retval = default_value;

    try {
      String opt = option (name);
      if (opt != null) retval = Float.parseFloat (opt);

    } catch (RuntimeException e) {
      invalid_names.append (name + ", ");
      // fall through
    }

    add_spec (name, "float", description, 
      String.valueOf (default_value), String.valueOf (retval)); 
    return retval;
  }


  public float floatt (String name, String description,
    float default_value, float low, float high) {

    float retval = default_value;

    try {
      String opt = option (name);
      if (opt != null) retval = Float.parseFloat (opt);

    } catch (RuntimeException e) {
      invalid_names.append (name + ", ");
      // fall through
    }

    String full_description = description + " from " + low + " to " + high;
    retval = Math.clamp (retval, 0.0f, 1.0f);
    add_spec (name, "float", full_description, 
      String.valueOf (default_value), String.valueOf (retval)); 
    return retval;
  }


  protected int index (String name) {
    for (int i=0; i<args.length; i++)
      if (args [i].equals ("--" + name))
        return i;

    return -1;
  }


  public boolean invalid () {
    for (int i=0; i<args.length; i++) {
      String arg = args [i];

      if (arg.startsWith ("--")) // option
        if (arg.length () == 2) // end-option-mark
          break;

        else if (!valid_names.contains (arg))
          invalid_names.append (arg + ", ");
    }

    int n = invalid_names.length ();
    boolean any = n > 2;
    if (any) {
      invalid_names.delete (n-2, n); // delete last ", "
      System.out.println ("\nInvalid options: " + invalid_names);
      System.out.println ("Try --help for a list of valid options.");
    }

    invalid_names = null;       // help garbage collection
    valid_names = null;         // help garbage collection
    return any;
  }


  public int intt (String name, String description, 
    int default_value) {
 
    int retval = default_value;

    try {
      String opt = option (name);
      if (opt != null) retval = Integer.parseInt (opt);

    } catch (RuntimeException e) {
      invalid_names.append (name + ", ");
      // fall through
    }

    add_spec (name, "int", description, 
      String.valueOf (default_value), String.valueOf (retval)); 
    return retval;
  }


  public int intt (String name, String description, 
    int default_value, int low, int high) {

    int retval = default_value;

    try {
      String opt = option (name);
      if (opt != null) retval = Integer.parseInt (opt);

    } catch (RuntimeException e) {
      invalid_names.append (name + ", ");
      // fall through
    }

    String full_description = description + " from " + low + " to " + high;
    retval = Math.clamp (retval, low, high);
    add_spec (name, "int", description, 
      String.valueOf (default_value), String.valueOf (retval)); 
    return retval;
  }


  public long longg (String name, String description,
    long default_value) {

    long retval = default_value;

    try {
      String opt = option (name);
      if (opt != null) retval = Long.parseLong (opt);

    } catch (RuntimeException e) {
      invalid_names.append (name + ", ");
      // fall through
    }

    add_spec (name, "long", description, 
      String.valueOf (default_value), String.valueOf (retval)); 
    return retval;
  }


  public String option (String name) {
    int index = index (name);
    if (index == -1 || index == args.length-1) return null;
    return args [index+1];
  }


  public boolean flag (String name, String description) {
    boolean retval = index (name) != -1;
    add_spec (name, "flag", description, "absent",
      retval ? "present" : "absent");
    return retval;
  }


  public float scale (String name, String description,
    float default_value) {

    return floatt (name, description, default_value, 0.0f, 1.0f);
  }
  

  public String string (String name, String description, 
    String default_value) {

    String retval = default_value;

    try {
      String opt = option (name);
      if (opt != null) retval = opt;

    } catch (RuntimeException e) {
      invalid_names.append (name + ", ");
      // fall through
    }

    add_spec (name, "String", description, default_value, retval);
    return retval;
  }
}

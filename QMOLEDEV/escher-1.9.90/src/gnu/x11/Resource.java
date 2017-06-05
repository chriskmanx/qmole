package gnu.x11;


/** X ID resource. */
public abstract class Resource {
  protected Display display;
  protected int id;


  /** Predefined. */  
  public Resource (int id) { this.id = id; }

  /** Create. */
  public Resource (Display display) {
    this.display = display;
    id = display.allocateID (this);
  }


  /** Intern. */
  public Resource (Display display, int id) {
    this.display = display;
    this.id = id;
    display.getResources().put (new Integer (id), this);
  }


  /* Java cannot enforce the presence of static method in subclasses. But
   * subclasses of this class should implement the following.
   *
   * public static Object intern (Display display, int id);
   */


  public void unintern () {
    display.getResources().remove (new Integer (id));
  }

  /**
   * Returns the resource ID of this resource.
   *
   * @return the resource ID of this resource
   */
  public int getID () {
    return id;
  }
  
  /**
   * Returns the Display of this resource
   * 
   * @return the display of this resource
   */
  public Display getDisplay() {
    return display;
  }
}

package gnu.x11;


/** X ID resource. */
public abstract class Resource {
  public Display display;
  public int id;


  /** Predefined. */  
  public Resource (int id) { this.id = id; }

  /** Create. */
  public Resource (Display display) {
    this.display = display;
    id = display.allocate_id (this);
  }


  /** Intern. */
  public Resource (Display display, int id) {
    this.display = display;
    this.id = id;
    display.resources.put (new Integer (id), this);
  }


  /* Java cannot enforce the presence of static method in subclasses. But
   * subclasses of this class should implement the following.
   *
   * public static Object intern (Display display, int id);
   */


  public void unintern () {
    display.resources.remove (new Integer (id));
  }

  /**
   * Returns the resource ID of this resource.
   *
   * @return the resource ID of this resource
   */
  public int id () {
    return id;
  }
}

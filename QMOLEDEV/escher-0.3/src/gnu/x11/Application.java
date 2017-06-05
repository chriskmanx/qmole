package gnu.x11;


/**
 * X application.
 *
 * <p>A basic X application is expected to extend this class and call the
 * constructor with its command-line arguments. See <a
 * href="../../../gnu/x11/test/Hello.java"><code>gnu/x11/test/Hello.java</code></a>
 * for an example.
 *
 * <p>To run an X application, the name of the X server to be connected to
 * must be given. By default, the value is fetched from the environment
 * variable $DISPLAY. Users can override this by specifying --display
 * option. Simply <code>java gnu.x11.test.Hello</code> will display a
 * window in the default X server.
 *
 * <p>Users can also specify --send-mode option. For instance, <code>java
 * gnu.x11.test.Hello --send-mode sync</code> will display the same
 * window but the protocol requests are sent synchronously. See {@link
 * Connection#send_mode} and {@link Connection#SEND_MODE_STRINGS} for more
 * info.
 */
public class Application extends gnu.app.Application {
  /**
   * X server connection.
   *
   * <p>Note that this variable (X connection) should not be used before
   * calling {@link #about} since it is possible that users specify
   * <code>"--help"</code> option and no connection is made at all.
   */
  public Display display;

  protected Option option;


  protected Application (String [] args) {
    super (args, new Option (args));

    // cast `gnu.util.Option option' to `gnu.x11.Option'
    option = (Option) super.option;
    String env = gnu.util.Environment.value ("DISPLAY");
    Display.Name display_name = option.display_name ("display",
      "X server to connect to", new Display.Name (env));

    if (help_option) return;
    display = new Display (display_name);
  }
}

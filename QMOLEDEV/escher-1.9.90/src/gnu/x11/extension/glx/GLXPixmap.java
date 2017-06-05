package gnu.x11.extension.glx;

import gnu.x11.RequestOutputStream;


/** GLX pixmap. */
public class GLXPixmap extends gnu.x11.Resource implements GLXDrawable {
  public GLX glx;


  // glx opcode 13 - create glx pixmap
  /**
   * @see <a href="glXCreateGLXPixmap.html">glXCreateGLXPixmap</a>
   */
  public GLXPixmap (GLX glx, int screen_no, XVisualInfo visual, 
    gnu.x11.Pixmap pixmap) {

    super (glx.getDisplay());
    this.glx = glx;
    
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (glx.getMajorOpcode(), 13, 5);
      
      o.writeInt32 (screen_no);
      o.writeInt32 (visual.getID());
      o.writeInt32 (pixmap.getID());
      o.writeInt32 (id);
      o.send ();
    }
  } 


  // glx opcode 15 - destroy glx pixmap
  /**
   * @see <a href="glXDestroyContext.html">glXDestroyContext</a>
   */
  public void destroy () {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (glx.getMajorOpcode(), 15, 2);
      o.writeInt32 (id);
      o.send ();
    }
  }    

  public int id () {
    return id;
  }
}
